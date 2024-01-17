
import pandas as pd
import numpy as np
from functools import partial
from sklearn import metrics
from sklearn.model_selection import cross_val_score
from sklearn.preprocessing import OneHotEncoder
from sklearn.pipeline import Pipeline
from hyperopt import fmin, tpe, hp, Trials
from sklearn.metrics import mean_squared_error, r2_score
from xgboost import XGBRegressor
import json
from functions import *

def objective(params, X, y, n_splits = 5):
    '''
    combine k-fold cross validation with hyperparameter optimization
    using hyperopt
    '''

    
    specs = {
        'objective': 'reg:quantileerror',
        'quantile_alpha': np.array([0.05, 0.5, 0.95]),
        'tree_method': 'hist',
        'n_estimators': int(params['n_estimators']),
        'max_depth': int(params['max_depth']),
        'gamma': params['gamma'],
        'learning_rate': params['learning_rate'],
        'subsample': params['subsample'],
        'colsample_bytree' : params['colsample_bytree'],
        'min_child_weight': params['min_child_weight'],
        'eta': params['eta'],
        'random_state':42
    }

    
    folds = kfold_indices(X, y, n_splits = n_splits, shuffle = True, random_state = 42)

    losses = np.zeros(n_splits)
    
    for  n in folds.keys():
        X_train, y_train, X_test, y_test = X.iloc[folds[str(n)]['Train']], y.iloc[folds[str(n)]['Train']], X.iloc[folds[str(n)]['Test']], y.iloc[folds[str(n)]['Test']]
        
        Xy_train = xgb.QuantileDMatrix(X_train, y_train, enable_categorical=True)
    
        # use Xy_train as a reference
        Xy_test = xgb.QuantileDMatrix(X_test, y_test, ref=Xy_train, enable_categorical=True)

        model = xgb.train(
            specs,
            Xy_train,
            num_boost_round = specs[],
            early_stopping_rounds=2,
            # The evaluation result is a weighted average across multiple quantiles.
            evals=[(Xy_train, "Train"), (Xy_test, "Test")],
            evals_result=evals_result,
            verbose_eval=False,
        )

        losses[int(n)] = evals_result['Test']['quantile'][-1]

    mean_loss = np.mean(losses)
    
    return mean_loss


# Define the hyperparameter search space
space = {
    'n_estimators': hp.quniform('n_estimators', 100, 1000, 1),
    'max_depth': hp.quniform('max_depth', 1, 15, 1),
    'gamma': hp.quniform ('gamma', 0, 1, 0.05),
    'learning_rate': hp.loguniform('learning_rate', -5, 0),
    'subsample': hp.quniform('subsample', 0.5, 1, 0.05),
    'colsample_bytree': hp.quniform('colsample_bytree', 0.5, 1, 0.05),
    'min_child_weight' : hp.quniform('min_child_weight', 0, 6, 1),
    'eta': hp.quniform('eta', 0.025, 0.5, 0.025)
}



df = pd.read_csv("data/BD4QoL_150523_encoded.csv") # original data
bs = pd.read_csv("data/bootstrap_ids.csv")

# load variable list
variable_list = pd.read_csv("data/prospective_variables.csv")

# clean the data
df = preprocess(data = df, features = variable_list, target = None)

df_orig = df.copy(deep=True)

# filter missing outcomes
df_orig = df_orig[~df_orig['hn4_dv_c30_ghs'].isna()]

# define the independent variables
covariates = variable_list[variable_list['predictor']=='yes'].variable.values.tolist()

# separate predictors and outcome
X_orig = df_orig[covariates]
y_orig = df_orig['hn4_dv_c30_ghs']


pipeline = Pipeline(
    [
        ('xgboost', best_model)
    ]
    )

pipeline.fit(X_train, y_train)


# Perform hyperparameter optimization on the original data
trials = Trials()
best = fmin(fn=partial(objective, X=X_orig, y=y_orig), 
            space=space, 
            algo=tpe.suggest, 
            max_evals=max_evals, 
            trials=trials)

best_model = XGBRegressor(
        n_estimators = int(best['n_estimators']),
        max_depth = int(best['max_depth']),
        gamma = best['gamma'],
        learning_rate = best['learning_rate'],
        subsample = best['subsample'],
        colsample_bytree = best['colsample_bytree'],
        min_child_weight = best['min_child_weight'],
        eta = best['eta'],
        random_state=42
    )
    

pipeline_best = Pipeline(
        [
        ('xgboost', best_model)
        ]
    )
    
pipeline_best.fit(X_orig, y_orig)




# compute relevance weights
relevance = phi(y_orig)

pipeline.fit(X_orig, y_orig, quantile_regressor__sample_weight = relevance)

# save model
with open('results/' + model_results_folder + '/model_'+str(quantile)+'_orig.pkl', 'wb') as model_file:
    pickle.dump(pipeline, model_file)

for bootstrap in range(0, n_bootstraps):
    print("Bootstrap:", bootstrap)
    # select observations in the bootstrap
    bs_n = df.iloc[bs[bs['bs_id'] == bootstrap + 1].loc[:, 'studyid'], : ].copy(deep=True)

    # filter missing outcomes
    bs_n = bs_n[~bs_n['hn4_dv_c30_ghs'].isna()]

    #  get out-of-bag observations
    # oob = get_oob_samples(df, bs_n.loc[:, "studyid_hn057"])
    
    # bootstrap data
    X_train = bs_n.loc[:, covariates]
    y_train = bs_n.loc[:, "hn4_dv_c30_ghs"]

    # bootstrap out-of-bag samples
    # X_oob = oob.loc[:, covariates]
    # y_oob = oob.loc[:, "hn4_dv_c30_ghs"]

    # hyperparametrization on the bootstrap
    # preprocessing is done inside the objective function
    # cross-validation loop to avoid data leakage
    trials = Trials()
    best = fmin(fn=partial(objective, X=X_train, y=y_train, n_splits = 5, quantile = quantile), 
            space=space, 
            algo=tpe.suggest, 
            max_evals=max_evals, 
            trials=trials)
    
    model_best = QuantileRegressor(
                        alpha = best['alpha'], 
                        quantile = quantile, 
                        solver = 'highs'
                        )
    

    # load preprocesssing pipeline
    preprocessor = load_preprocessor(X_train)

    # define pipeline
    pipeline = Pipeline( steps = [
                    ('preprocessing', preprocessor),
                    ('quantile_regressor', model_best)]
                     )

    # compute relevance weights
    relevance = phi(y_train)

    # fit the bootstrap model with optimum parameters
    pipeline.fit(X_train, y_train, quantile_regressor__sample_weight = relevance, quantile_regressor__quantile = quantile)

    # save model
    with open('results/' + model_results_folder + '/model_'+str(quantile)+'_bs' + str(bootstrap + 1) + '.pkl', 'wb') as model_file:
        pickle.dump(pipeline, model_file)

