import pandas as pd
from sklearn.linear_model import LogisticRegression


df = pd.read_csv('../data/BD4QoL_150523_encoded.csv') # original data

variables_list = pd.read_csv('../data/prospective_variables.csv')

df = df.assign(y = np.where(np.isnan(df['hn4_dv_c30_ghs']), 1, 0))

# filter only alive at prediction time
df = df[ df['hn4_dv_status'] == '1 - Alive']

df = preprocess(df, variables_list, variables_list)


from sklearn.preprocessing import OneHotEncoder, Normalizer

model = LogisticRegression(penalty ='l1', solver ='saga', max_iter=1000)


propensity = Pipeline([
                ('encoder', OneHotEncoder(drop=None, handle_unknown='ignore')),
                ('normalize', Normalizer()),
                ('logistic', model)

            ])
