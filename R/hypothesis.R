#' Retrieve path to hypothesis classification model
#'
#' Retrieves the path to the hypothesis classification model. This prevents a
#' hard path being defined, which would cause an error when verifying
#' staged installation.
#'
#' @noRd
get_path_ft_model <- function() {
  system.file("extdata", "models","fasttext_model.bin",
              package = 'HypothesisReader')
}

ft_model <- fastTextR::ft_load(get_path_ft_model())

# REGEX Strings ---------------------------------------------------------------
split_tag <- "<split>"
hypothesis_tag <- "hypo (.*?):\\s*"
hypothesis_split_tag <- paste(split_tag, hypothesis_tag, sep = "")
regex_non_numeric <- "[^0-9.-]"
regex_return_num <- "(\\d)+"

# Functions -------------------------------------------------------------------

#' Generate fastText model input
#'
#' Performs pre-processing steps to prepare fastText model input.
#'
#' @param input_text input text for processing, character vector
#' @noRd

gen_fasttext_model_input <- function(input_text) {
  # For R CMD Checks
  output_text <-  NULL

  # Remove hypothesis tag
  output_text <- gsub(
    pattern     = hypothesis_tag,
    replacement = "",
    x           =  input_text
  )

  # Return output
  output_text

}


#' Filter hypothesis statements with fastText classification model
#'
#' Removes statements if not classified as hypothesis by the
#' hypothesis classification model.
#'
#' @param input_text possible hypothesis statements, character vector
#' @noRd

apply_fasttext_model <- function(input_text) {
  # For R CMD Checks
  col_names <- no <- Response <- yes <- NULL

  # Process model input data
  model_input <- gen_fasttext_model_input(input_text)

  # Generate hypothesis predictions
  hypothesis_pred <- fastTextR::ft_predict(
    model   = ft_model,
    newdata = model_input,
    rval    = "dense"
  ) %>%
    as.data.frame()

  # Rename columns
  col_names <- names(hypothesis_pred)

  if ("__label__1" %in% col_names) {
    hypothesis_pred <- hypothesis_pred %>%
      dplyr::rename(yes = "__label__1")
  }

  if ("__label__0" %in% col_names) {
    hypothesis_pred <- hypothesis_pred %>%
      dplyr::rename(no = "__label__0")
  }

  # Generate logical vector indicting if a vector element is a hypothesis
  col_names <- names(hypothesis_pred)

  ## If no column not found, all elements are hypothesis
  if (!("no" %in% col_names)) {
    response <- vector(
      mode   = "logical",
      length = length(model_input)
    )

    for (i in seq_along(model_input)) (response[i] <- TRUE)

    ## If yes column not found, all elements are not hypothesis
  } else if (!("yes" %in% col_names)) {
    response <- vector(
      mode   = "logical",
      length = length(model_input))

    for (i in seq_along(model_input)) (response[i] <- FALSE)

  } else {
    response <- hypothesis_pred %>%
      dplyr::mutate(
        Response = dplyr::if_else(
          condition = yes >= no,
          true      = TRUE,
          false     = FALSE
        )
      ) %>%
      dplyr::pull(Response)

  }

  # Return filtered input hypothesis statements with logical vector
  input_text[response]
}


#' Reduce to acceptable hypothesis labels
#'
#' Reduces the list of identified hypotheses to unique labels. For this 
#' process, we have a specific logic to determine if a label is unique when
#' dealing with both numeric and alpha-numeric labels. 
#' 
#' When the first instance of a hypothesis number occurs in an alpha-numeric
#' label (the number 1 in Hypothesis 1a), then subsequent uses of that
#' number that are strictly numeric (Hypothesis 1) ARE NOT allowed. But, when
#' the first instance of a hypothesis number occurs in a numeric label
#' (the number 1 in Hypothesis 1), then subsequent uses of that
#' number that are alphanumeric (Hypothesis 1a) ARE allowed. 
#' 
#' For example, the following list of hypothesis labels are allowed:
#'  - Hypothesis 1      ALLOWED
#'  - Hypothesis 1a     ALLOWED
#'  - Hypothesis 1b     ALLOWED
#'  
#' However, the following list of hypothesis contains labels that are not 
#' allowed, and would therefore be dropped.
#'  - Hypothesis 1a     ALLOWED
#'  - Hypothesis 1      NOT ALLOWED
#'  - Hypothesis 1b     ALLOWED
#' 
#'
#' @param hypothesis_labels Vector of identified hypothesis labels
#' @noRd

acceptable_hypothesis_labels <- function(hypothesis_labels) {
  h_id <- hypothesis <- NULL

  hypothesis_numbers <- stringr::str_extract(
    string = hypothesis_labels,
    pattern = regex_return_num
  )

  # Check if hypothesis label contains letters
  logical_hypothesis_labels_alpha <- grepl("[a-zA-Z]", hypothesis_labels)

  # Initialize
  h_num_output <- c()
  h_label_output <- c()

  for (i in seq_along(logical_hypothesis_labels_alpha)) {

    # Extract values at index i
    h_label_alpha <- logical_hypothesis_labels_alpha[i]
    h_num <- hypothesis_numbers[i]
    h_label <- hypothesis_labels[i]

    # If label contains a letter
    if (h_label_alpha) {

      # # Check if number already used in label
      # if (!(h_num %in% h_label_output)) {
      # 
      #   h_label_output <- c(h_label_output, h_label)
      #   h_num_output <- c(h_num_output, h_num)
      # 
      # }
      
      # Append to output vectors
      h_label_output <- c(h_label_output, h_label)
      h_num_output <- c(h_num_output, h_num)

    } else {

      if (!(h_num %in% h_num_output)) {
        
        # Append to output if number has not already
        # used in alphanumeric label
        h_label_output <- c(h_label_output, h_label)
        h_num_output <- c(h_num_output, h_num)

      }
    }

    h_label_output <- stringr::str_sort(x = h_label_output, numeric = TRUE)

  }

  # Return
  h_label_output

}


#' Drop hypothesis sentences with fewer than minimum token threshold
#'
#' Removes sentences that contain a hypothesis tag, and contain fewer than
#' the minimum threshold of tokens. This is a method to assist in removing
#' erroneous hypothesis identification.
#'
#' @param input_text Processed input text, one sentence per vector element
#' @param min_threshold Minimum threshold of tokens in a sentence.
#' @noRd

drop_hypothesis_below_min_threshold <- function(
  input_text,
  min_threshold = 5
) {
  # For R CMD Checks
  extract_hypothesis <- index <- n <- n_tokens <- pass <- token <- NULL

  # Remove hypothesis tag and trim white space
  extract_hypothesis <- input_text %>%
    stringr::str_remove_all(pattern = hypothesis_tag) %>%
    stringr::str_trim()

  # Create tibble and add index
  hypothesis.tb <- dplyr::tibble(extract_hypothesis) %>%
    dplyr::mutate(
      index = dplyr::row_number()
    )

  # Insert dummy token for observations with zero tokens to avoid NA drop
  hypothesis.tb <- hypothesis.tb %>%
    dplyr::mutate(
      extract_hypothesis = dplyr::if_else(
        condition = extract_hypothesis == "",
        true      = "dummy",
        false     = extract_hypothesis
        )
    )

  # Generate vector of sentences with token counts above minimum threshold
  idx_above_min_threshold <- hypothesis.tb %>%
    tidytext::unnest_tokens(                          # Convert to tokens
      output = token,
      input  = extract_hypothesis
    ) %>%
    dplyr::group_by(index) %>%
    dplyr::summarise(n_tokens = dplyr::n()) %>%      # Count tokens per index
    dplyr::ungroup() %>%
    dplyr::mutate(
      pass = dplyr::if_else(                          # ID index pass/fail
        condition = n_tokens > min_threshold,
        true      = 1,
        false     = 0
      )
    ) %>%
    dplyr::filter(pass == 1) %>%                      # Filter passing index
    dplyr::pull(index)                                # Extract as vector

  # Return hypothesis statements with token count greater than threshold
  input_text[idx_above_min_threshold]

}


#' Remove hypothesis statements with unreasonably large label numbers.
#'
#' Hypothesis number sanity check. Some false hypothesis identifications
#' result in numbers far larger than the true hypothesis labels. This 
#' function removes hypothesis labels that are larger than one could
#' reasonably expect.
#'
#' @param input_hypothesis hypothesis statements, character vector
#' @param threshold maximum difference allowable between consecutive 
#'  hypothesis numbers
#' @noRd

hypothesis_sanity_check <- function(
  input_hypothesis,
  threshold = 3
  ) {
  # Generate vector of all hypothesis labels
  h_label <- input_hypothesis %>%
    stringr::str_match(
      pattern = hypothesis_tag
    )
  h_label <- h_label[,2]

  # Extract hypothesis label numbers 
  h_num <- gsub(
    pattern = regex_non_numeric, 
    replacement =  "",
    x =  h_label
  ) %>% as.numeric()
  
  # Sort ascending, then append zero
  ## Adding zero is for offsetting the diff function, which will reduce our
  ## vector length from n to n-1.
  h_num_ascend <- sort(h_num)
  h_num_ascend_app_0 <- c(0, h_num_ascend)
  
  # Determine difference between next closest hypothesis number
  h_num_diff <- diff(h_num_ascend_app_0)
  
  # Check if any differences exceed threshold
  logical_h_num_sanity <- h_num_diff > threshold
  
  if (any(logical_h_num_sanity)) {
    
    # Determine index of first instance of exceeding threshold
    index_cutoff <- min(which(logical_h_num_sanity))
    
    # Determine hypothesis number at first index
    value_cutoff <- h_num_ascend[index_cutoff]
    
    # Filter hypothesis labels
    input_hypothesis[h_num < value_cutoff] 
    
  } else {
    
    input_hypothesis
    
  }
}


#' Format hypothesis output table
#'
#' Creates the output table for hypothesis statements
#'
#' @param input_hypothesis hypothesis statements, character vector
#' @noRd

hypothesis_output_table <- function(input_hypothesis) {
  
  # Extract hypothesis label/number
  h_id <- input_hypothesis %>%
    stringr::str_extract("hypo (.*?):") %>%
    stringr::str_remove_all("hypo ") %>%
    stringr::str_remove_all(":")
  
  # Drop ~Hypo #:~ for entity extraction input
  hypothesis <- gsub(
    pattern     = hypothesis_tag,
    replacement = "",
    x           =  input_hypothesis
  )
  
  # Create Dataframe with hypothesis number and hypothesis
  df_hypothesis <- data.frame(
    h_id,
    hypothesis,
    stringsAsFactors = FALSE
  )
  
  # Rename and add hypothesis Number
  df_hypothesis <- df_hypothesis %>%
    dplyr::mutate(
      h_id = paste0("h_", h_id)
    ) %>%
    dplyr::select(h_id, hypothesis)
  
  df_hypothesis
}


#' Format hypothesis output table
#'
#' Creates the output table for hypothesis statements
#'
#' @param input_hypothesis hypothesis statements, character vector
#' @noRd

hypothesis_output_table <- function(input_hypothesis) {
  
  # Extract hypothesis label/number
  h_id <- input_hypothesis %>%
    stringr::str_extract("hypo (.*?):") %>%
    stringr::str_remove_all("hypo ") %>%
    stringr::str_remove_all(":")
  
  # Drop ~Hypo #:~ for entity extraction input
  hypothesis <- gsub(
    pattern     = hypothesis_tag,
    replacement = "",
    x           =  input_hypothesis
  )
  
  # Create Dataframe with hypothesis number and hypothesis
  df_hypothesis <- data.frame(
    h_id,
    hypothesis,
    stringsAsFactors = FALSE
  )
  
  # Rename and add hypothesis Number
  df_hypothesis <- df_hypothesis %>%
    dplyr::mutate(
      h_id = paste0("h_", h_id)
    ) %>%
    dplyr::select(h_id, hypothesis)
  
  df_hypothesis
}
  
  
#' Extract hypothesis statements
#'
#' Wrapper function. Executes all steps in the hypothesis extraction process.
#'
#' @param input_text PDF text as processed by [process_text()].
#' @param apply_model Boolean tag for whether to filter hypothesis statements
#'  with the hypothesis classification model.
#' @noRd

hypothesis_extraction <- function(input_text, apply_model = FALSE){
  # For R CMD Checks
  h_id <- hypothesis <- NULL

  # Reduce to Hypothesis Statements --------------------------------------------
  # Split vector elements with multiple hypothesis tags
  split_text <- stringr::str_split(
    string  = input_text,
    pattern = split_tag) %>%
    unlist()

  # Select vector elements which contain hypothesis tags
  logical_hypothesis_tag <- stringr::str_detect(
    string  = split_text,
    pattern = hypothesis_tag
  )

  hypothesis <- split_text[logical_hypothesis_tag]

  # Minimum Token Threshold ----------------------------------------------------
  hypothesis <- drop_hypothesis_below_min_threshold(hypothesis)

  # fastText -------------------------------------------------------------------
  # Filter vector elements based on hypothesis prediction model
  if (apply_model) {
    if (!(purrr::is_empty(hypothesis))) {

      hypothesis <- apply_fasttext_model(hypothesis)

    }
  }
  
  # Sanity Check ---------------------------------------------------------------
  hypothesis <- hypothesis_sanity_check(hypothesis)

  # Unique Hypothesis Labels ---------------------------------------------------
  ## Extract hypotheses label/number
  h_match <- hypothesis %>%
    stringr::str_match(
      pattern = hypothesis_tag
    )

  h_match_lbl <- h_match[,2]

  ## Identify unique hypothesis labels
  h_match_lbl_unq <- unique(h_match_lbl)

  ## Remove known erroneous hypothesis formats
  error_hypothesis <- c("na")
  h_match_lbl_unq <- setdiff(h_match_lbl_unq, error_hypothesis)
  h_match_lbl_unq <- h_match_lbl_unq[!is.na(h_match_lbl_unq)]

  # Acceptable Hypothesis Labels -----------------------------------------------
  ### i.e.: Hypothesis 1 not selected if Hypothesis 1a appears earlier
  h_match_lbl_unq <- acceptable_hypothesis_labels(h_match_lbl_unq)

  # Initial Instance -----------------------------------------------------------
  ## Determine vector index of initial hypothesis statements
  h_initial <- c()

  for (i in h_match_lbl_unq){

    intial_idx <- tapply(
      X     = seq_along(h_match_lbl),
      INDEX = h_match_lbl,
      FUN   = min
      )[i]

    h_initial <- c(h_initial, intial_idx)
  }

  ## Reduce to only initial hypothesis instances
  hypothesis <- hypothesis[h_initial]

  # Create Output Table -------------------------------------------------------
  df_hypothesis <- hypothesis_output_table(hypothesis)
  
  df_hypothesis

}
