"""
In this script we load the models trained with bootstrap data
and compute some statistics such as performance metrics.

load models with pickle
make predictions
evaluate

"""


from sklearn.linear_model import QuantileRegressor
from sklearn.metrics import d2_pinball_score, mean_pinball_loss
from functools import partial
import pandas as pd
import numpy as np
from hyperopt import fmin, tpe, hp, Trials
import pickle

# load relevance function
file = open('phi.pkl', 'rb')
phi = pickle.load(file)
file.close()
from src.functions import *

def coefficients_from(model: Type[sklearn.linear_model._quantile.QuantileRegressor], preprocessor: Type[sklearn.compose.ColumnTransformer]) -> Type[pd.DataFrame]:
    """
    retrieve the weights from sklearn quantile linear model
    combined with the feature names processed with sklearn's ColumnTransformer
    """
    weights = pd.DataFrame(zip(preprocessor.get_feature_names_out(), model.coef_), columns = ['feature', 'weight'])
    return weights




df = pd.read_csv("../data/BD4QoL_150523_encoded.csv") # original data
bs = pd.read_csv("../data/bootstrap_samples.csv")

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

preprocessor = train_preprocessor(X_orig)

X_orig = preprocessor.transform(X_orig)

# Make predictions on the original data
y_pred_orig = model_best.predict(X_orig)

# Calculate Mean Squared Error
origorig_mse = mean_squared_error(y_orig, y_pred_orig)
origorig_r2 = r2_score(y_orig, y_pred_orig) 

# train in the bootstrap and test in the oob samples
oob_r2 = np.zeros(n_bootstraps)
oob_mse = np.zeros(n_bootstraps)

# train in the bootstrap and test in the bootstrap
bsbs_r2 = np.zeros(n_bootstraps)
bsbs_mse = np.zeros(n_bootstraps)

# train in the bootstrap and test in the original data
bsorig_r2 = np.zeros(n_bootstraps)
bsorig_mse = np.zeros(n_bootstraps)


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
    best = fmin(fn=partial(objective, X=X_train, y=y_train), 
            space=space, 
            algo=tpe.suggest, 
            max_evals=max_evals, 
            trials=trials)
    
    model_best = QuantileRegressor(
                        alpha = best['alpha'], 
                        quantile = 0.5, 
                        solver = 'highs'
                        )
    

    # apply preprocessing 
    preprocessor = train_preprocessor(X_train)

    X_train = pd.DataFrame(X_train, columns = preprocessor.get_feature_names_out())

    # fit the bootstrap model with optimum parameters
    model_best.fit(X_train, y_train)


    # Make predictions on the test data
    predictions_oob = model_best.predict(X_oob)
    predictions_bs = model_best.predict(X_train)
    predictions_orig = model_best.predict(X_orig)

    # Calculate Mean Squared Error
    oob_mse[bootstrap] = mean_squared_error(y_oob, predictions_oob)
    oob_r2[bootstrap] = r2_score(y_oob, predictions_oob) 
    
    bsbs_mse[bootstrap] =  mean_squared_error(y_train, predictions_bs)
    bsbs_r2[bootstrap] = r2_score(y_train, predictions_bs)
    
    bsorig_mse[bootstrap] =  mean_squared_error(y_orig, predictions_orig)
    bsorig_r2[bootstrap] = r2_score(y_orig, predictions_orig)

    

mse_362 = dot632_estimator(origorig_mse, oob_mse)
r2_362 = dot632_estimator(origorig_r2, oob_r2)
    
results = {
           'metrics' : 
            {
               'oob_mse': oob_mse.tolist(),
               'oob_r2': oob_r2.tolist(),
               'origorig_mse': origorig_mse.tolist(), 
               'bsbs_mse': bsbs_mse.tolist(), 
               'bsorig_mse': bsorig_mse.tolist(), 
               'bsorig_r2': bsorig_r2.tolist(),
               'mse_362': mse_362,
               'r2_362': r2_362
            },
            'description': 
            {
                'oob': 'Trained on the BS and evaluated on OOB sample',
                'origorig': 'Trained on the original data and evaluated on the original data',
                'bsbs': 'Trained on the BS and evaluated on the BS sample',
                '_362': 'Bootstrap .362 estimator'
            }
        }

with open('results_extensive.json', 'w') as file:
    json.dump(results, file)