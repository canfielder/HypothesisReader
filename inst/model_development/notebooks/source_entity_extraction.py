"""
The following script contains all functions related to the entity
extraction notebooks.
"""
### IMPORT #####################################################################
# General
import numpy as np
import pandas as pd
import re
from itertools import chain

# Anaysis
from sklearn.metrics import \
    accuracy_score, classification_report, confusion_matrix, \
    precision_recall_fscore_support

# NLP
import nltk
from nltk.corpus import stopwords
from nltk.stem import WordNetLemmatizer 

# Downloads
nltk.download('wordnet')
nltk.download('stopwords')

# Deep Learning
import tensorflow as tf

from keras.initializers import Constant
from keras.layers import \
    Bidirectional, Dense, Dropout, Embedding, LSTM, \
    SpatialDropout1D, TimeDistributed 
from keras.models import Input, Model, Sequential
from keras.preprocessing.sequence import pad_sequences
from keras.utils import to_categorical

### FUNCTIONS ##################################################################
# General ----------------------------------------------------------------------
"""
Flatten lists

Flattens a shallow list of lists into a single list.
"""
def flatten_list(list_of_lists):
    list_flat = list(chain(*list_of_lists))
    return list_flat


# Text Processing --------------------------------------------------------------

"""
Remove white space between tokens greater than one space.
"""
def remove_whitespace(text_string):
    # Drop extra whitespace
    while "  " in text_string:
        text_string = text_string.replace('  ', ' ')

    return text_string


"""
Remove stopwords
"""
def remove_stopwords(input_string):
    stop_words = set(stopwords.words('english')) 

    # Split into tokens
    tokens = input_string.split()

    # Remove stopwords
    tokens_filtered = [w for w in tokens if not w in stop_words]
    # Recombine into string
    output_string = " ".join(tokens_filtered)

    return output_string


"""
Lemmatize text
"""
def lemmatize_text(input_string):
    
    # Initialize Lemmatizer
    lemmatizer = WordNetLemmatizer()

    # Split into tokens
    tokens = input_string.split()

    # Lemmatize each token
    tokens_lemm = [lemmatizer.lemmatize(t) for t in tokens]

    # Recombine tokens into string
    output_string = " ".join(tokens_lemm)

    return output_string
    

""" 
Remove hypothesis tag

Removes standardized hypothesis tag and removes other characters at the start
of the hypothesis string determined to be unnecessary.
"""
def remove_hypothesis_tag(input_list):
    hypo_tag_pattern = "^(hypo|hypothesis|hypotheis) (.*?):\\s*"

    output_list = []
    # Remove hypothesis tag and other text
    for i, sentence in enumerate(input_list):
        sentence = re.sub(hypo_tag_pattern, "", sentence)
        sentence = re.sub("^\d:", "", sentence)
        sentence = re.sub("^\d{1,2}", "", sentence)
        sentence = re.sub("^,", "", sentence)
        sentence = sentence.strip()
        
        output_list.append(sentence)
    
    return output_list


"""
Performs basic text processing on Nodes and Hypotheses before converting into
numerical indexes
"""
def process_text(
    df_input, 
    stopwords = False,
    lemmatize = False
    ):
   
    # Regex patterns
    whitespace_pattern = re.compile(r'\s+')
   
    # Extract columns as lists
    description_list = df_input['text'].tolist()
    node1_list = df_input['node1'].tolist()
    node2_list = df_input['node2'].tolist()

    # Remove hypothesis tags
    description_list = remove_hypothesis_tag(description_list)

    # Text Processing
    for i, n in enumerate(node1_list): 
        n_iter=n.lower() \
                .replace('.', '') \
                .replace(',', '') \
                .strip()
        
        # Replace &
        n_iter = re.sub("&", "and", n_iter)
        
        # Remove stopwords
        if stopwords:
            n_iter = remove_stopwords(n_iter)
        
        # Lemmatize Text
        if lemmatize:
            n_iter = lemmatize_text(n_iter)

        # Remove extra whitespace
        n_iter=remove_whitespace(n_iter)

        # Replace with text processed string
        node1_list[i] = n_iter

    for j, n in enumerate(node2_list): 
        n_iter=n.lower() \
                .replace('.', '') \
                .replace(',', '') \
                .strip()
        
        # Replace &
        n_iter = re.sub("&", "and", n_iter)

        # Remove stopwords
        if stopwords:
            n_iter = remove_stopwords(n_iter)
        
        # Lemmatize Text
        if lemmatize:
            n_iter = lemmatize_text(n_iter)

        # Remove extra whitespace
        n_iter=remove_whitespace(n_iter)
        
        # Replace with text processed string
        node2_list[j] = n_iter

    for k, n in enumerate(description_list):        
        n_iter = n.lower() \
                  .replace('.', '') \
                  .replace(',', '') \
                  .replace(':', '') \
                  .strip()
        
        # Replace &
        n_iter = re.sub("&", "and", n_iter)

        # Remove stopwords
        if stopwords:
            n_iter = remove_stopwords(n_iter)

        # Lemmatize Text
        if lemmatize:
            n_iter = lemmatize_text(n_iter)

        # Remove extra whitespace
        n_iter=remove_whitespace(n_iter)

        # Replace with text processed string
        description_list[k] = n_iter        

    df_output = pd.DataFrame({
      'text': description_list,
      'node1': node1_list,
      'node2': node2_list,
    })

    return df_output


"""
Performs basic text processing on Nodes and Hypotheses before converting into
numerical indexes, specifically for the word identification split method.
"""
def process_text_split_method(
    df_input
    ):
   
    # Regex patterns
    whitespace_pattern = re.compile(r'\s+')
   
    # Extract columns as lists
    description_list = df_input['text'].tolist()
    node1_list = df_input['node1'].tolist()
    node2_list = df_input['node2'].tolist()

    # Remove hypothesis tags
    description_list = remove_hypothesis_tag(description_list)

    # Text Processing
    for i, n in enumerate(node1_list): 
        n_iter=n.lower() \
                .replace('.', '') \
                .strip()


        # Remove extra whitespace
        n_iter=remove_whitespace(n_iter)

        # Replace with text processed string
        node1_list[i] = n_iter

    for j, n in enumerate(node2_list): 
        n_iter=n.lower() \
                .replace('.', '') \
                .strip()

        # Remove extra whitespace
        n_iter=remove_whitespace(n_iter)
        
        # Replace with text processed string
        node2_list[j] = n_iter

    for k, n in enumerate(description_list):        
        n_iter = n.lower() \
                  .replace('.', '') \
                  .replace(':', '') \
                  .strip()

        # Remove extra whitespace
        n_iter=remove_whitespace(n_iter)

        # Replace with text processed string
        description_list[k] = n_iter        

    df_output = pd.DataFrame({
      'text': description_list,
      'node1': node1_list,
      'node2': node2_list,
    })

    return df_output

# Target Development-----------------------------------------------------------
"""
Identifies starting index of entity inside description token list. Both inputs
are token list representations of the intial strings.
"""
def find_node(node, description):
    len_node=len(node)
    for idx in (i for i, e in enumerate(description) if e==node[0]):
        if description[idx:idx+len_node]==node:
            return idx


"""
Replaces label list values with node representations at the correct index
"""
def replace_label_index(
    labels, node_tokens, description_tokens, replacement_value
    ):
    
    # Determine start/end index
    n_start = find_node(node_tokens, description_tokens)
    n_end = n_start + len(node_tokens)

    # Create replacement list
    label_replacement = [replacement_value] * len(node_tokens)

    labels[n_start:n_end] = label_replacement

    return labels


"""
Converts description tokens into numerical representations, indicating if the 
token is part of node 1 (1), node 2 (2), or neither (0).
"""
def target_gen_labels(df_input):
    
    # Extract columns as lists
    description_list = df_input['text'].tolist()
    node1_list = df_input['node1'].tolist()
    node2_list = df_input['node2'].tolist()

    # Intialize output labels
    labels = []

    for i, description in enumerate(description_list):
        # print(f"Iteration: {i}")

        # Extract nodes
        node1 = node1_list[i]
        node2 = node2_list[i]

        # Convert to token lists
        description_tokens = description.split(" ")
        n1_tokens = node1.split(" ")
        n2_tokens = node2.split(" ")

        # Initialize labels, set all values to 0
        labels_iter = [0] * len(description_tokens)

        # Replace node 1 labels
        labels_iter = replace_label_index(
            labels_iter, n1_tokens, 
            description_tokens, 1
            )

        # Replace node 2 labels
        labels_iter = replace_label_index(
            labels_iter, n2_tokens, 
            description_tokens, 2
            )
        
        labels.append(labels_iter)

    # Store output as dataframe
    df_output = pd.DataFrame({
        'text': description_list,
        'node1': node1_list,
        'node2': node2_list,
        'labels': labels,
    })
    
    return df_output


"""
Wrapper - Target Data Generation

Wrapper function for executing all steps in the target data generation
for entity extraction.
"""
def target_gen_wrapper(
    df_input, 
    max_length
    ):

    # Generate text labels
    df_label = target_gen_labels(df_input)

    # Extract labels
    labels = df_label['labels'].tolist()

    # Pad labels for equal length features
    labels_padded = pad_sequences(
        maxlen=max_length, 
        sequences=labels, 
        padding="post", 
        truncating="post",
        value=0
        )
    
    # Define target as numerical classes
    target_labels = labels_padded.tolist()
   
    # Generate Output Dataframe
    df_output = df_input.copy()
    df_output['target_labels'] = target_labels

    return df_output


"""
Encode Target
"""
def encode_target(target_labels):
    # Define number of classes
    num_classes = len(set(chain(*target_labels)))

    target_encoded =  [
        to_categorical(i, num_classes=num_classes) for i in target_labels
    ]

    return target_encoded


# Model Construction -----------------------------------------------------------
"""
Embedding Matrix
"""
def gen_embedding_matrix(
    dct_embedding_index, 
    embed_label,
    vocabulary_length,
    word_index
    ):

    # Define dimension and index
    embedding_dim = dct_embedding_index[embed_label]["dimension"]
    embeddings_index = dct_embedding_index[embed_label]["index"]

    if embedding_dim != None:

        # Create matrix
        num_tokens = vocabulary_length + 2

        # Initialize Matrix
        embedding_matrix = np.zeros((num_tokens, embedding_dim))
        
        # Fill with pretrain values
        for word, i in word_index.items():
            embedding_vector = embeddings_index.get(word)
            if embedding_vector is not None:
                # Words not found in embedding index will be all-zeros.
                # This includes the representation for "padding" and "OOV"
                embedding_matrix[i] = embedding_vector

    else:
        embedding_matrix = None

    return embedding_matrix

"""
Embedding Layer
"""
def gen_embedding_layer(
    label,
    input_dimension,
    max_length, 
    embedding_matrix,
    output_dimension_wo_init = 64,
    mask_zero = True,
    trainable = True,
    ):

    # Text Vectorization (Internal)
    if np.any(embedding_matrix) == None:
        embedding_layer = Embedding(
            input_dim = input_dimension, 
            output_dim = output_dimension_wo_init,
            input_length = max_length, 
            mask_zero = mask_zero,
            name="embeddings_" + label, 
            )
        
    else:
        # Define output dimension
        embedding_dim = len(embedding_matrix[0])

        embedding_layer = Embedding(
            input_dim = input_dimension + 2, 
            output_dim = embedding_dim,
            embeddings_initializer = Constant(embedding_matrix), 
            trainable = trainable, 
            mask_zero = mask_zero,
            name="embeddings_" + label
            )
    
    return embedding_layer


"""
Compile Model Architecture
"""
def compile_model(
    vectorization_layer,
    embedding_layer,
    lstm_stack,
    hidden_dimension_1,
    hidden_dimension_2,
    sample_weights,
    time_distributed = True,
    dropout=True,
    dropout_rate=0.5,
    optimizer="rmsprop",
    loss="categorical_crossentropy",
    metrics=["accuracy"]
    ):
    
    model = Sequential()
    model.add(
        tf.keras.Input(
            shape=(1,), 
            dtype=tf.string
            )
        )

    model.add(vectorization_layer)

    # Embedding
    model.add(embedding_layer)

    if dropout:
        model.add(SpatialDropout1D(dropout_rate))

    if lstm_stack:
        model.add(
            Bidirectional(
                LSTM(
                    units=hidden_dimension_1, 
                    return_sequences=True, 
                    recurrent_dropout=0.1)
                )
            )

    model.add(
        Bidirectional(
            LSTM(
                units=hidden_dimension_2, 
                return_sequences=True, 
                recurrent_dropout=0.1)
            )
        )
    
    # Time Distributed Layer
    if time_distributed:
        model.add(
            TimeDistributed(
                Dense(
                    units=3, 
                    activation="softmax"
                    )
                )
            )
    else:
        model.add(
            Dense(
                units=3, 
                activation="softmax"
            )
        )
        
        
    # Sample Weights
    if sample_weights:
        model.compile(
            optimizer=optimizer, 
            loss=loss, 
            sample_weight_mode="temporal",
            metrics=metrics
        )

    else:
        model.compile(
            optimizer=optimizer, 
            loss=loss, 
            metrics=metrics
        )
        
    return model

# Evaluation -------------------------------------------------------------------
"""
Generate evalution metrics.

y_test and y_pred should be flattened lists.
"""
def gen_eval_metrics(
    dct_summary,
    y_test, y_pred,
    embedding,
    stop_words,
    lemmatization,
    hidden_dim_1,
    hidden_dim_2,
    lstm_stack,
    dropout,
    dropout_rate,
    sample_weights,
    trainable, 
    optimizer, 
    time_distributed
    ):

    # Calculate evaluation metrics
    prfs_macro = precision_recall_fscore_support(
        y_test, y_pred, average="macro"
        )

    prfs = precision_recall_fscore_support(y_test, y_pred)

    acc = accuracy_score(y_test, y_pred)

    # Store in evaluation tracking table
    dct_summary["embedding"].append(embedding)
    dct_summary["stop_words"].append(stop_words) 
    dct_summary["lemmatization"].append(lemmatization)
    dct_summary["hidden_dim_1"].append(hidden_dim_1)
    dct_summary["hidden_dim_2"].append(hidden_dim_2)
    dct_summary["lstm_stack"].append(lstm_stack)
    dct_summary["dropout"].append(dropout)
    dct_summary["dropout_rate"].append(dropout_rate)
    dct_summary["sample_weights"].append(sample_weights)
    dct_summary["trainable"].append(trainable)
    dct_summary["optimizer"].append(optimizer)
    dct_summary["time_distributed"].append(time_distributed)
    dct_summary["accuracy"].append(acc)
    dct_summary["precision_0"].append(prfs[0][0])
    dct_summary["precision_1"].append(prfs[0][1])
    dct_summary["precision_2"].append(prfs[0][2])
    dct_summary["precision_macro"].append(prfs_macro[0])
    dct_summary["recall_0"].append(prfs[1][0])
    dct_summary["recall_1"].append(prfs[1][1])
    dct_summary["recall_2"].append(prfs[1][2])
    dct_summary["recall_macro"].append(prfs_macro[1])
    dct_summary["f1_0"].append(prfs[2][0])
    dct_summary["f1_1"].append(prfs[2][1])
    dct_summary["f1_2"].append(prfs[2][2])
    dct_summary["f1_macro"].append(prfs_macro[2])

    return dct_summary

    
def get_classification_report(y_test, y_pred):
    '''Source: https://stackoverflow.com/questions/39662398/scikit-learn-output-metrics-classification-report-into-csv-tab-delimited-format'''

    report = classification_report(y_test, y_pred, output_dict=True)
    df_classification_report = pd.DataFrame(report).transpose()
    df_classification_report['index'] = df_classification_report.index
    df_classification_report = df_classification_report.sort_values("index")

    return df_classification_report


# Archive ----------------------------------------------------------------------
"""
The following functions are no longer used, but are maintained for archival
purposes.
"""
"""
Label connections

Labels the tokens between nodes 1 and 2 as a unique label (3). If no tokens 
exist between the two defined nodes, no label is applied
"""
def label_node_connect(input_labels):
    # Determine location of nodes
    idx_1 =  [i for i, x in enumerate(input_labels) if x == 1]
    idx_2 =  [i for i, x in enumerate(input_labels) if x == 2]

    min_idx_1 = min(idx_1)
    max_idx_1 = max(idx_1)
    min_idx_2 = min(idx_2)
    max_idx_2 = max(idx_2)

    # If Node 1 Occurs First
    if min_idx_2 > max_idx_1:
        idx_start = max_idx_1+1
        idx_end = min_idx_2
    else:
        idx_start = max_idx_2+1
        idx_end = min_idx_1

    # Determine length of connection labels
    idx_connect = list(range(idx_start,idx_end))
    
    # Create Copy
    output_labels = input_labels.copy()

    # If connection indexes are returned
    if idx_connect:
        # Create replacement labels
        label_connect = [3] * (idx_end - idx_start)

        # Replace labels
        output_labels[idx_start:idx_end] = label_connect

    return output_labels


"""
Convert connection labels

Converts labels than have been processed by function label_node_connect.
Labels with values 1 or 2 are converted to 0. Labels with value 3 are set to 1.
"""
def replace_labels(input_labels):
    temp_labels = []
    for label in input_labels:
        if label == 1 or label == 2:
            temp_labels.append(0)
        else:
            temp_labels.append(label)

    output_labels = []
    for label in temp_labels:
        if label == 3:
            output_labels.append(1)
        else:
            output_labels.append(label)
    
    return output_labels


"""
Label connective tokens - wrapper

Wrapper function for returning labeled connective tokens.
"""
def label_node_connect_wrapper(labels_entites):
    
    labels_connect = []
    for label in labels_entites:      
        # Label connective tokens
        label = label_node_connect(label)

        # Convert to binary
        label = replace_labels(label)

        labels_connect.append(label)

    return labels_connect






