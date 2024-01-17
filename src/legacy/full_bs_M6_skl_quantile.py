from sklearn.linear_model import QuantileRegressor
from sklearn.pipeline import Pipeline
from sklearn.metrics import d2_pinball_score, mean_pinball_loss
from functools import partial
import pandas as pd
import numpy as np
from hyperopt import fmin, tpe, hp, Trials
import pickle

# load relevance function
file = open('phi.pkl', 'rb')
phi = pickle.load(file)

from functions import *

def objective(params, X, y, n_splits, quantile = 0.5):
    """
    this is the objective function that will be minimized by hyperopt
    to estimate the best hyperparameters, in this case only the l1-penalizaton alpha
    """
    # load preprocessor pipeline
    preprocessor = load_preprocessor(X)

    model = QuantileRegressor(alpha = params['alpha'], quantile = quantile, solver = 'highs')
    
    # define pipeline
    pipeline = Pipeline( steps = [
                    ('preprocessing', preprocessor),
                    ('quantile_regressor', model)]
                     )

    losses = cross_validate(X, y, model = pipeline, loss = mean_pinball_loss, n_splits = n_splits, weighting_function = phi)

    mean_loss = np.mean(losses)
    
    return mean_loss

n_bootstraps = 1
max_evals = 30
model_results_folder = 'linear_quantile'
quantile = 0.5


df = pd.read_csv("data/BD4QoL_150523_encoded.csv") # original data
bs = pd.read_csv("data/bootstrap_samples.csv")

# load variable list
variable_list = pd.read_csv("data/prospective_variables.csv")

# bootstrap variables list
bs_variables = variable_list.copy(deep = True)
bs_variables.loc[len(bs_variables)] = {'variable': 'bs_id', 'type': 'index', 'dtype': 'int64', 'rtype':'int'}

df = preprocess(data = df, features = variable_list, target = "hn4_dv_c30_ghs")
bs = preprocess(data = bs, features = bs_variables, target = "hn4_dv_c30_ghs")

# define the independent variables
#covariates = ["hn1_dv_age_cons", "hn1_na8_cb_sex", "hn1_icd_group_conf", "hn1_tnm_stage_best", "hn1_nb4_cb_comorb_index"]
covariates = variable_list[variable_list['predictor']=='yes'].variable.values.tolist()

# separate predictors and outcome
X_orig = df[covariates]
y_orig = df['hn4_dv_c30_ghs']

# Define the hyperparameter search space
space = {
    'alpha': hp.loguniform('alpha', -10, 0),

}

# Perform hyperparameter optimization on the original data
trials = Trials()
best = fmin(fn=partial(objective, X=X_orig, y=y_orig, n_splits = 5), 
            space=space, 
            algo=tpe.suggest, 
            max_evals=max_evals, 
            trials=trials)

# define model with best hyperparameters
model_best = QuantileRegressor(alpha = best['alpha'], quantile = quantile, solver = 'highs')

# load preprocessor pipeline
preprocessor = load_preprocessor(X_orig)

# define pipeline
pipeline = Pipeline( steps = [
                    ('preprocessing', preprocessor),
                    ('quantile_regressor', model_best)]
                     )

# compute relevance weights
relevance = phi(y_orig)

pipeline.fit(X_orig, y_orig, quantile_regressor__sample_weight = relevance)

# save model
with open('results/' + model_results_folder + '/model_'+str(quantile)+'_orig.pkl', 'wb') as model_file:
    pickle.dump(pipeline, model_file)

for bootstrap in range(0, n_bootstraps):
    bs_n = bs[bs["bs_id"] == bootstrap + 1]
    oob = get_oob_samples(df, bs_n.loc[:, "studyid_hn057"])
    
    # bootstrap data
    X_train = bs_n.loc[:, covariates]
    y_train = bs_n.loc[:, "hn4_dv_c30_ghs"]

    # bootstrap out-of-bag samples
    X_oob = oob.loc[:, covariates]
    y_oob = oob.loc[:, "hn4_dv_c30_ghs"]

    # hyperparametrization on the bootstrap
    # preprocessing is done inside the objective function
    # cross-validation loop to avoid data leakage
    trials = Trials()
    best = fmin(fn=partial(objective, X=X_train, y=y_train, n_splits = 5), 
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
    pipeline.fit(X_train, y_train, quantile_regressor__sample_weight = relevance)

    # save model
    with open('results/' + model_results_folder + '/model_'+str(quantile)+'_bs' + str(bootstrap + 1) + '.pkl', 'wb') as model_file:
        pickle.dump(pipeline, model_file)

