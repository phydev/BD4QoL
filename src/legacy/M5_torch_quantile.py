import torch
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from src.functions import *
import pickle

# load relevance function
file = open('phi.pkl', 'rb')
phi = pickle.load(file)

df = pd.read_csv("data/BD4QoL_150523_encoded.csv") # original data
#bs = pd.read_csv("../data/bootstrap_samples.csv")

# load variable list
variable_list = pd.read_csv("data/prospective_variables.csv")

# bootstrap variables list
#bs_variables = variable_list.copy(deep = True)
#bs_variables.loc[len(bs_variables)] = {'variable': 'bs_id', 'type': 'index', 'dtype': 'int64', 'rtype':'int'}

df = preprocess(data = df, features = variable_list, target = "hn4_dv_c30_ghs")
#bs = preprocess(data = bs, features = bs_variables, target = "hn4_dv_c30_ghs")

# define the independent variables
#covariates = ["hn1_dv_age_cons", "hn1_na8_cb_sex", "hn1_icd_group_conf", "hn1_tnm_stage_best", "hn1_nb4_cb_comorb_index"]
covariates = variable_list[variable_list['predictor']=='yes'].variable.values.tolist()

# separate predictors and outcome
#X = df[['hn3_dv_c30_ghs']]
X = df [covariates]
#X = df[['hn3_dv_c30_ghs', 'hn1_dv_age_cons', 'hn1_icd_group_conf']]
y = df['hn4_dv_c30_ghs']