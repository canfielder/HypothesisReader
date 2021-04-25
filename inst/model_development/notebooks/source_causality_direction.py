"""
The following script contains functin related to the data processing and 
modeling of the Causality and Direction classification exercises.
"""
# Import ---------------------------------------------------------------------
# General
import re
import string

# NLP
import gensim

from nltk import word_tokenize
from nltk.corpus import stopwords
from nltk.stem import WordNetLemmatizer
from nltk.stem import SnowballStemmer

# Modeling
from imblearn.over_sampling import SMOTE, ADASYN
from imblearn.pipeline import make_pipeline
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.pipeline import Pipeline

# Processing ------------------------------------------------------------------
### General ### 
# Normalization - Lower Case
def normalize_text_case(input_list): 
    output_list=[]
    for n in input_list:
        output_list.append(n.lower())
    
    return output_list


# Entity Replacement
def replace_entities(features_list, node1_list, node2_list):
    output_list=[]
    for idx, line in enumerate(features_list): 
        temp_string=features_list[idx] \
            .replace(node1_list[idx], "node1") \
            .replace(node2_list[idx], "node2") 
            
        output_list.append(temp_string)
    
    return output_list


# Remove hypothesis tag
def remove_hypothesis_tag(input_list):
    hypothesis_tag="hypo [1-9][:]?\\s*"
    output_list=[]
    for string in input_list:
        temp_str=re.sub(hypothesis_tag, "", string)
        output_list.append(temp_str)
    
    return output_list       


# Tokenization
def tokenize_strings(input_list):
    output_list=[word_tokenize(sent) for sent in input_list]   
    return output_list
   
    
# Remove puntuation and stopwords
def remove_punct_sw(input_list):
    punctuation=list(string.punctuation)
    remove=stopwords.words("english") + punctuation + [""]
    output_list=[]
    for i in range(len(input_list)): 
        temp_list=[item for item in input_list[i] if item not in remove]
        output_list.append(temp_list)
    return output_list


# Token normalization
def normalize_tokens(input_list, method="lemm"):
    lemmatizer=WordNetLemmatizer() 
    stemmer=SnowballStemmer(language="english")

    if method == "stem":
        tn=stemmer.stem
    else:
        tn=lemmatizer.lemmatize

    # Execute
    output_list=[]
    for i in range(len(input_list)): 
        temp_list=[tn(token) for token in input_list[i]]
        output_list.append(temp_list)
    
    return output_list


# Generate feature strings (concatenate after node2)
def gen_feature_strings(features_list):
    full_list=[]
    for i in range(len(features_list)):
        list_hold=features_list[i]
        index1=list_hold.index("node1")
        index2=list_hold.index("node2")
        result1=""
        result2=""
        result3=""
        hold=[]

        for j in range(index1):
            result1 += list_hold[j]
            if(j < index1 - 1):
                result1 += " "

        for j in range(index1 + 1, index2):
            result2 += list_hold[j]
            if(j < index2 - 1):
                result2 += " "

        if(index2 + 1 < len(list_hold)):
            for j in range(index2 + 1, len(list_hold)):
                result3 += list_hold[j]

        if result1 != "":
            hold.append(result1)

        hold.append(list_hold[index1])

        if result2 != "":
            hold.append(result2)

        hold.append(list_hold[index2])

        if result3 != "":
            hold.append(result3)

        full_list.append(hold)
    
    return full_list


# Convert tokens to string
def convert_to_string(full_list):
    
    full_list_str=[]
    for tokens in full_list:
        statement=" ".join(tokens)
        full_list_str.append(statement)

    return full_list_str

# Wrapper - Text Processing
def gen_target_features(
    df, 
    target,
    token_method="lemm", 
    feature_strings=False, 
    balance=False, 
    entity_replacement=True
):
    
    if target == "causality":
        # Drop unused target
        df.drop(columns=["direction"], inplace=True)
    else:
         df.drop(columns=["causality"], inplace=True)
        
    
    # Extract Inputs
    df=df.dropna()
    features_list=df["text"].tolist()
    node1_list=df["node1"].tolist()
    node2_list=df["node2"].tolist()
    target_list=df[target].tolist()
    
    # Lower Case
    node1_list=normalize_text_case(node1_list)
    node2_list=normalize_text_case(node2_list)
    features_list=normalize_text_case(features_list)
    
    # Entity Replacement
    if entity_replacement:
        features_list=replace_entities(features_list, node1_list, node2_list)
    
    # Remove hypothesis tag
    features_list=remove_hypothesis_tag(features_list)
    
    # Tokenization
    features_list=tokenize_strings(features_list)
    
    # Remove punctuation / stopwords
    features_list=remove_punct_sw(features_list)
    
    # Token normalization
    features_list=normalize_tokens(features_list, method=token_method)
    
    # Generate feature strings
    if feature_strings:
        features_list=gen_feature_strings(features_list)
    
    # Concatenat tokens into strings
    features_list=convert_to_string(features_list)
        
    return (target_list, features_list)


### Doc2Vec ### 
# Read Corpus
def read_corpus(str_list):
    for idx, string in enumerate(str_list):
        tokens=gensim.utils.simple_preprocess(string)
        yield gensim.models.doc2vec.TaggedDocument(tokens, [idx])
        
        
# Transform
def doc2vec_xfrm(lst_of_strings, model):
    embeddings=[]
    for sentence in lst_of_strings:
        tokens=gensim.utils.simple_preprocess(sentence)
        vector=model.infer_vector(tokens)
        embeddings.append(vector)
        
    return(embeddings)


# Modeling --------------------------------------------------------------------
# Define Pipeline Function
def gen_pipeline(clf, balance, random_state):
    
    # Define Vectorizer
    vect_count=CountVectorizer(
        analyzer="word", 
        binary=True, 
        ngram_range=(1,3)
    )
    
    if balance == "none":
        pipeline=Pipeline(
            [
                ("vectorizer", vect_count), 
                ("clf", clf)
            ]
        )
    else:
        if balance == "ada":
            bal=ADASYN(random_state=random_state)
            
        elif balance == "smote":
            bal=SMOTE(random_state=random_state)
        
        pipeline=make_pipeline(vect_count, bal, clf)
        
    return pipeline