from typing import Type, Dict
import sklearn
import pandas as pd
import numpy as np
import os
import pickle


def mkdir_safe(folder_name = '', prefix = 'results/'):
    '''
     check if directory exists and add the suffix _{i} 
    '''
    i = 1
    temp = folder_name
    while os.path.isdir(prefix + temp):
        i += 1
        print(f"Warning: the directory {prefix}{temp} already exists!")
        temp = folder_name  + '_' + str(i)
    
    os.mkdir('results/' + temp)
    print('Results will be saved at /results/' + temp)

    return temp 

def preprocess(data: Type[pd.DataFrame], 
              features: Type[pd.DataFrame], 
              target: str = None) -> Type[pd.DataFrame]:
    '''
    simple data preprocessing
        1. make all column names lower case
        2. make all categorical variables lower case
        3. subset the data
        4. add the correct types to the data frame
    :param data: data frame
    :param features: data frame with the variables list and enconding
    :param target: outcome variable name, if provided missing values are removed
    :return data: transformed data frame
    '''

    # make all variables lower case
    data.columns = map(str.lower, data.columns)

    # make all categories lower case
    data = data.apply(lambda x: x.str.lower() if(x.dtype == 'object') else x)

    # subset data
    data = data.loc[:, features.loc[:, "variable"]]

    # encode variables type according to variables list
    types = dict(zip(features.loc[:, "variable"], features.loc[:, "dtype"]))

    data = data.astype(types, errors='ignore')
    
    if target is not None:
        # filter missing outcomes
        data = data[~data[target].isna()]

    return data



def load_preprocessor(X: Type[pd.DataFrame]):
    """
    preprocess data for neural network training
    Numerical transformations:
        - Impute with median
        - Standard scale
    Categorical transformations:
        - Impute with missing indicator
        - Encode with OneHotEncoder

    :param X: training data
    :return preprocessor: ColumnTransformer object
    
    """
    from sklearn.compose import ColumnTransformer
    from sklearn.pipeline import Pipeline
    from sklearn.preprocessing import OneHotEncoder, StandardScaler
    from sklearn.impute import SimpleImputer, MissingIndicator, KNNImputer

    # define the numerical and categorical features
    numeric_features = [col for col in X.columns if X[col].dtype != "category"]
    categorical_features = [col for col in X.columns if col not in numeric_features]

    # define the preprocessing steps
    # scale first and impute after since KNN works best with standardized features
    numeric_transformer = Pipeline(steps=[
        ('scale', StandardScaler()),
        ('num_imputer', KNNImputer(n_neighbors=5, weights="uniform"))
        ])

    # create the missing indicator first and encode after
    categorical_transformer = Pipeline(steps=[
        ('cat_imputer', SimpleImputer(strategy='constant', fill_value='missing')),
        ('onehot', OneHotEncoder(handle_unknown='ignore', drop='if_binary'))
        ])

    # apply the preprocessing steps to the features
    preprocessor = ColumnTransformer(
        transformers=[
            ('num', numeric_transformer, numeric_features),
            ('cat', categorical_transformer, categorical_features)])

    #preprocessor = preprocessor.fit(X)
    
    return preprocessor



def kfold_indices(X: Type[np.ndarray], 
                  y: Type[np.ndarray] = None, 
                  n_splits: int = 5, 
                  shuffle: bool = False, 
                  random_state: int = None) -> Dict:
    """
    Extract the indices from the generator function KFold from sklearn as dictionary.
    
    :param X numpy.ndarray: features
    :param y numpy.ndarray: response
    :param n_splits int: number of folds
    :param shuffle bool: shuffle the data before splitting
    :param random_state: seed for the randoom generator
    :return folds dict: dictionary with folds indices

    Example:
    >>> X = np.random.rand(10,3)
    >>> y = np.random.rand(10)
    >>> kfold_indices(X, y)
        {'0': {'Train': array([2, 3, 4, 5, 6, 7, 8, 9]), 'Test': array([0, 1])},
         '1': {'Train': array([0, 1, 4, 5, 6, 7, 8, 9]), 'Test': array([2, 3])},
         '2': {'Train': array([0, 1, 2, 3, 6, 7, 8, 9]), 'Test': array([4, 5])},
         '3': {'Train': array([0, 1, 2, 3, 4, 5, 8, 9]), 'Test': array([6, 7])},
         '4': {'Train': array([0, 1, 2, 3, 4, 5, 6, 7]), 'Test': array([8, 9])}}
          
    """
    
    from sklearn.model_selection import KFold
    
    kf = KFold(n_splits= n_splits, shuffle=shuffle, random_state=random_state)
    
    folds = {}
    for i, (train_index, test_index) in enumerate(kf.split(X, y)):
        folds[str(i)] = {}
        folds[str(i)]['Train'] = train_index
        folds[str(i)]['Test'] = test_index

    return folds


def cross_validate(X, y, model, loss, n_splits = 5, random_state = None, weighting_function = None, **kwargs):
    """
    
    k-fold cross validation from scratch

    if the estimator is set to QuantileRegressor then you should pass quantile = 0.5 through **kwargs
    
    """
    
    folds = kfold_indices(X, y, n_splits = n_splits, shuffle = True, random_state = random_state)

    losses = np.zeros(n_splits)

    weights_train = None
    weights_test = None
    
    for  n in folds.keys():
        X_train = X.iloc[folds[str(n)]['Train']]
        y_train = y.iloc[folds[str(n)]['Train']]
        X_test = X.iloc[folds[str(n)]['Test']]
        y_test = y.iloc[folds[str(n)]['Test']]

        if weighting_function is not None:
            weights_train = weighting_function(y_train)
            weights_test = weighting_function(y_test)
        
        model.fit(X_train, y_train, regressor__sample_weight = weights_train)

        y_pred = model.predict(X_test)

        losses[int(n)] = loss(y_true = y_test, y_pred = y_pred, sample_weight = weights_test, **kwargs)
        
    return losses


def get_oob_samples(original_data: Type[pd.DataFrame], bootstrap_case_ids: Type[np.ndarray]) -> Type[pd.DataFrame]:
    """
    :param original_data: dataframe with the original data
    :param bootstrap_case_ids: observation ids in the boostrap
    """
    oob = original_data[~original_data["studyid_hn057"].isin(bootstrap_case_ids)]
    
    return oob

def get_optimum_threshold(fpr, tpr, thresholds):
    i = np.arange(len(tpr)) # index for df
    roc = pd.DataFrame({
        'fpr' : pd.Series(fpr, index=i),
        'tpr' : pd.Series(tpr, index = i), 
        '1-fpr' : pd.Series(1-fpr, index = i), 
        'tf' : pd.Series(tpr - (1-fpr), index = i), 
        'thresholds' : pd.Series(thresholds, index = i)
        }
        )
    return roc.iloc[(roc.tf-0).abs().argsort()[:1]]

def dot632_estimator(theta_origin, theta_oob):
    """
    compute the .632 bootstrap estimator
    """
    return 0.368 * theta_origin + 0.632 * np.mean(theta_oob)



def plot_prediction_intervals(y_true, y_pred, y_lower, y_upper) -> None:
    """
    plot prediction intervals accross the whole range of predictions
    and the actual measurements

    """
    import plotly.graph_objects as go

    fig = go.Figure()

    trace1 = go.Scatter(
        x = y_pred,
        y = y_pred,

       mode = 'markers',
                error_y = dict(
                    type = 'data',
                    symmetric = False,
                    array = y_upper,
                    arrayminus = y_lower),
        name = 'prediction'
        )
    
    trace2 = go.Scatter(
        x = y_pred,
        y = y_true,
        mode = 'markers',
        marker_size = 5,
        marker_symbol = 'circle',
        name = 'data',
        )

    fig.add_trace(trace1)
    fig.add_trace(trace2)
  
    fig.show()


def plot_prediction_bands(preds: Type[pd.DataFrame]) -> None:
    """
    plot prediction intervals with error bands accross the prediction range
    with plotly.

    :param preds DataFrame: A data frame with the following columns
                            y_pred: predicted value
                            y_true: observed true value 
                            y_lower: predicted lower 5% quantile 
                            y_upper: predicted upper 95% quantile
    """
    import plotly.graph_objects as go
    
    fig = go.Figure([
            go.Scatter(
                name='Measurement',
                x=preds['y_pred'],
                y=preds['y_true'],
                mode='markers+lines',
            line=dict(color='rgb(31, 119, 180)'),
            ),
            go.Scatter(
                name='Upper Bound',
                x=preds['y_pred'],
                y=preds['y_upper'],
                mode='lines',
                marker=dict(color="#444"),
                line=dict(width=0),
                showlegend=False
            ),
            go.Scatter(
                name='Lower Bound',
                x=preds['y_pred'],
                y=preds['y_lower'],
                marker=dict(color="#444"),
                line=dict(width=0),
                mode='lines',
                fillcolor='rgba(68, 68, 68, 0.3)',
                fill='tonexty',
                showlegend=False
            )
    ])
    fig.update_layout(
    yaxis_title='Prediction interval',
    xaxis_title='Prediction',
    hovermode="x"
    )
    fig.show()

def plot_calibration(y_true, y_pred, mode='percentile', show_hist = True):
    
    import matplotlib.pyplot as plt
    from pygam import GAM, s, l

    if mode=='percentile' or mode=='both':
        percs = np.linspace(0,100,21)
    
        predicted_quantiles = np.percentile(y_pred, percs)
        sample_quantiles = np.percentile(y_true, percs)

        gam = GAM(s(0) + l(0), distribution = 'normal', verbose= False ).fit(predicted_quantiles, sample_quantiles)
        smooth = gam.predict(predicted_quantiles)
        ci = gam.confidence_intervals(predicted_quantiles)
    
        plt.plot(predicted_quantiles, ci, ls = '--', color = 'black')
        plt.plot(predicted_quantiles, smooth, color = 'red')    
    
        plt.scatter(predicted_quantiles, sample_quantiles, color='black', marker="o", s=5)
        
        x = np.linspace(np.min((predicted_quantiles.min(), sample_quantiles.min())), 
                    np.max((predicted_quantiles.max(), sample_quantiles.max())))

        plt.plot(x,x, color="k", ls=":", alpha=0.2)
        if show_hist:
            plt.hist(predicted_quantiles, alpha = 0.5)
        plt.ylabel('Sample quantiles')
        plt.xlabel('Predicted quantiles')
        plt.show()
        
    if mode=='raw' or mode=='both':
        x = np.linspace(np.min(y_true),np.max(y_true), 100)
        #preds = pd.DataFrame(np.concatenate((y_pred.reshape(-1,1), y_true.reshape(-1,1)), axis = 1), columns = ['y_pred', 'y_true'])
        
        gam = GAM(s(0) + l(0), distribution = 'normal', verbose= False ).fit(y_pred, y_true)
        smooth = gam.predict(x)
        ci = gam.confidence_intervals(x)

        plt.plot(x, ci, ls = '--', color = 'black')
        plt.plot(x, smooth, color = 'red')    
        plt.scatter(x = y_pred, y = y_true, s = 4.0, c = 'black', alpha=0.2)
        plt.ylabel('$Sample$')
        plt.xlabel('$Predicted$')
        plt.plot(x, x, linestyle = ':')

        plt.show()



def predict_wrapper(predict):
    '''
    Decorator to make the predict function general for binary and continuous outcomes
    '''
    def predict_general(X):
        prediction = predict(X)
        if prediction.shape[1] == 2:
            prediction = prediction[:, 0]

        return prediction

    return predict_general


def compute_bootstrap_metrics(n_bootstraps, 
                              df, 
                              bs, 
                              covariates, 
                              outcome, 
                              variable_id,
                              metrics_dict,
                              models_root_directory = 'results/models/reduced/',
                              model_directory = 'logreg_phys_func/',
                              threshold = None, 
                              outcome_type = 'binary', 
                              prediction_function = 'predict_proba',
                              ):
    '''
    This function computes the bootstrap metrics for a given model and a given set of metrics
    provided by the metrics_dict. The metrics_dict is a dictionary with the name of the metric
    and the function that computes the metric. The function should have the following signature:
    metric_function(y_true, y_pred) where y_true is the true outcome and y_pred is the predicted
    outcome. The function should return a single value.

    // TODO:
        This function be generalized further:
        - Allow metrics that take the classification instead of the probability

    '''


    df_orig = df.copy(deep=True)

    # filter missing outcomes
    df_orig = df_orig[~df_orig[outcome].isna()]


    # separate predictors and outcome
    X_orig = df_orig[covariates]

    # check if the outcome is binary or continuous
    if outcome_type == 'binary':
        #  define labels
        df_orig = df_orig.assign(label = np.where(df_orig[outcome]<threshold, 1, 0))

        y_orig = df_orig['label']
    else:
        y_orig = df_orig[outcome]    
    
    bootstrap_metrics = {}

    model_file = models_root_directory + model_directory + 'model_orig.pkl'

    with open(model_file, 'rb') as file:
        model = pickle.load(file)

    # decorate the predict function to make it general for binary and continuous outcomes
    predict = predict_wrapper(getattr(model, prediction_function))

    # Make predictions on the original data
    y_pred_orig = predict(X_orig)

    for name, metric in metrics_dict.items():
        print(name)
        bootstrap_metrics[name] = {}

        # Calculate the metrics for original data
        bootstrap_metrics[name]['orig'] = metric(y_orig, y_pred_orig)

        # train in the bootstrap and test in the oob samples
        bootstrap_metrics[name]['oob'] = np.zeros(n_bootstraps)

        # train in the bootstrap and test in the bootstrap
        bootstrap_metrics[name]['bs'] = np.zeros(n_bootstraps)

        # train in the bootstrap and test in the original data
        bootstrap_metrics[name]['bs_orig'] = np.zeros(n_bootstraps)

    for bootstrap in range(n_bootstraps):
        print('Bootstrap', bootstrap)

        # load model from results/models/reduced
        model_file = models_root_directory + model_directory + 'model_bs' + str(bootstrap + 1) + '.pkl'

        with open(model_file, 'rb') as file:
            model = pickle.load(file)
        
        # decorate the predict function to make it general for binary and continuous outcomes
        predict = predict_wrapper(getattr(model, prediction_function))

        # select observations in the bootstrap
        bs_n = df.iloc[bs[bs['bs_id'] == bootstrap + 1].loc[:, 'studyid'], :].copy(deep=True)

        # filter missing outcomes
        bs_n = bs_n[~bs_n[outcome].isna()]

        oob = get_oob_samples(df, bs_n.loc[:, variable_id])
        
        # bootstrap data
        X_train = bs_n.loc[:, covariates]

        # check if the outcome is binary or continuous
        if outcome_type == 'binary':

            # define labels
            bs_n = bs_n.assign(label = np.where(bs_n[outcome] < threshold, 1, 0))
            oob = oob.assign(label = np.where(oob[outcome] < threshold, 1, 0))

            # select binary outcome
            y_train = bs_n.loc[:, 'label']
            y_oob = oob.loc[:, 'label']

        else:
            
            # select continuous outcome
            y_train = bs_n.loc[:, outcome]
            y_oob = oob.loc[:, outcome]

        # bootstrap out-of-bag samples
        X_oob = oob.loc[:, covariates]
        

        # Make predictions on the test data
        predictions_oob = predict(X_oob)
        predictions_bs = predict(X_train)
        predictions_orig = predict(X_orig)

        for metric_name, metric_function in metrics_dict.items():
            # Calculate metrics
            bootstrap_metrics[metric_name]['oob'][bootstrap] = metric_function(y_oob, predictions_oob)
            bootstrap_metrics[metric_name]['bs'][bootstrap] = metric_function(y_train, predictions_bs)
            bootstrap_metrics[metric_name]['bs_orig'][bootstrap] = metric_function(y_orig, predictions_orig)

    return bootstrap_metrics