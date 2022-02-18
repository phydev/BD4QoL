# file: reg_rnn.py
# date: 18.02.2022
# author(s): Moreira-Soares, M
# Sensitive(?): No
# Description:
# Collection of functions needed to transform and encode the HN5000 data into a 3D tensor 
# required to use recurrent neural network. This code is ready to work on the real data.


def attribute_dtypes(df, dtypes):
    """
    pandas cannot open and infer column types for too many columns
    so we will open the data attributing type object to every column
    and then run this function to provide the correct dtype
    
    :param df: pd.DataFrame
    :param dtypes: dictionary with types 
    :return df: DataFrame with properly dtypes
    """
    for column in df.columns:
        df[column] = df[column].astype(dtypes[column])
    return df


def map_feature_to_index(features):
    """
    generates a dictionary that maps the feature name and the index position in the tensor
    """
    map_ = {}   
    for n, attribute in enumerate(features):
        map_[attribute] = n 
    return map_


def map2unique(features, map_time2single):
    """
    extracts unique feature name for variables defined at several time points
    for example: `hn1_qol`, `hn2_qol`, `hn3_qol` and `hn4_qol` will return only `qol`
    """
    attributes = []
    for feature in features["variable"]:
    
        if "hn" in feature:
            map_time2single[feature] = feature[4:]
            attributes.append(feature[4:])
        else:
            map_time2single[feature] = feature
            attributes.append(feature)    
    
    return list(dict.fromkeys(attributes)) # remove duplicates

def convertType(x):
    """
    convert columns object into categories
    """
    import warnings
    
    warnings.warn("Use attribute_dtypes() providing the types for every column instead.", DeprecationWarning)
    
    if x.dtype == 'O':
        return x.astype('category')
    else:
         return x


def encode_categorical(df):
    """
    Encode all object dtype pd.DataFrame columns with OneHotEncoder and return also a DataFrame
    """
    from sklearn.preprocessing import OneHotEncoder
    enc = OneHotEncoder(drop='if_binary') # if categorical variable is binary, then drop one of the categories
    X_transformed = enc.fit_transform(df.select_dtypes("category")).toarray()
    encoded_categories = enc.get_feature_names_out()
    X_numeric_data = df.select_dtypes(exclude=['category']) # select only numeric and drop non-encoded categorical variables
    X_dataframe = pd.DataFrame(data=X_transformed, columns = encoded_categories) # transform the encoded variables into a dataframe
    df_encoded = X_numeric_data.join(X_dataframe)   
    return df_encoded

def df2tensor(df, features):
    """
    :pd.DataFrame df: data frame
    :pd.DataFrame features: list of variables with a single column `variable`
    :dict map: dictionary that maps variable names and tensor index
    :return input: transformed data into a 3D tensor with shape (samples,time points, n features) 
    """
    
    # we will create a list of features colapsing all time points in a single variable
    
    df = df.reset_index(drop=True) # because the indexes were shuffled and sliced after train_test_split
    
    attributes = []
    map_time2single = dict.fromkeys(features['variable']) # dictionary to map 
    attributes = map2unique(features, map_time2single) 
    map_ = map_feature_to_index(attributes)
    
    timesteps = 4
    batch = len(df)
    timeDict = {"hn1": 0,
                "hn2": 1,
                "hn3": 2,
                "hn4": 3}
    
    inputs  = np.zeros((batch, timesteps, len(map_)))
    inputs[:,:,:] = -100000
    

    for feature in features["variable"]:

        if any(x in feature for x in timeDict.keys()):
            for timeId in timeDict.keys():

                if timeId in feature:
                
                    for sample in range(batch):
                        inputs[sample, timeDict[timeId], map_[map_time2single[feature]]] = df[feature][sample]
                    
        # variables that are not associated to a time point will be evaluated in this condition       
        else: 
            for sample in range(batch):           
                    inputs[sample, 0, map_[feature]] = df[feature][sample]

    return inputs
    
    
def tensor_train_test_split(df, features, outcome, **params):
    """
    splits a dataframe into train and test set by encapsulating sklearn.model_selection.train_test_split
    then converts the two sets into 3D tensor format for keras rnn models
    
    :param df: pandas data frame
    :param features: data frame with the variable list
    :param outcome: string with the variable name used as outcome
    """
    
    from sklearn.model_selection import train_test_split
    
    map_time2single = dict.fromkeys(features['variable']) # dictionary to map 
    attributes = map2unique(features, map_time2single)
    map_ = map_feature_to_index(attributes)

    df_train, df_test, _, _ = train_test_split(df, df[outcome], **params)
    
    train_set = df2tensor(df_train, features)
    test_set = df2tensor(df_test, features)
    
    X_train, y_train = np.delete(train_set, obj=map_[map_time2single[outcome]], axis=2), train_set[:, 3, map_[map_time2single[outcome]]]
    X_test, y_test = np.delete(test_set, obj=map_[map_time2single[outcome]], axis=2), test_set[:, 3, map_[map_time2single[outcome]]]
    
    return X_train, X_test, y_train, y_test



if __name__ == "__main__":
    import sys
    import pickle

    import numpy as np
    import pandas as pd
    
    from keras.models import Sequential
    from keras.layers import Dense

    # Recurrent Neural Networks
    from keras.models import Model
    from keras.layers import Input
    from keras.layers import Dense
    from keras.layers.recurrent import LSTM
    from keras.layers import TimeDistributed

    variables = pd.read_csv("../data/consolidated_variables.csv")
    
    variables = variables.loc[  
                               (variables["type"]!="index") &
                               (variables["type"]!="excluded")]

    dtypes_dict = dict(zip(variables['variable'].to_dict().values(),
                           variables['dtype'].to_dict().values() ))


    df = pd.read_csv("../data/imputed/"+sys.argv[1], dtype=object)
    print("dataset:", sys.argv[1])
    
    df = df[variables['variable']] # select only the variables in the list
    df = attribute_dtypes(df, dtypes_dict) # attribute dtypes to every column
    #print(df.dtypes)
    #df = df.apply(lambda x: convertType(x))
    df = encode_categorical(df)
    
    features = pd.DataFrame({"variable": df.columns})

    X_train, X_test, y_train, y_test = tensor_train_test_split(df, features, outcome = "hn4_dv_c30_summary", test_size = 0.2)
    
    n_features = len(variables["variable"])
    timesteps = 4

    model = Sequential([
        LSTM(units = 10, input_shape=(timesteps, n_features), return_sequences=True),
        TimeDistributed(Dense(10, activation='relu')),
        TimeDistributed(Dense(1)),    
    ])

    model.compile(loss='MeanSquaredError', optimizer='adam')
    
    history = model.fit(X_train, y_train, verbose=1, validation_split=0.20, epochs=100) 

    with open('rnn_model.pickle', 'wb') as handle:
        pickle.dump(history, handle, protocol=pickle.HIGHEST_PROTOCOL)
