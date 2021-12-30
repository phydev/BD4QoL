# file: sim_qol.py
# date: 30.12.2021
# author(s): Moreira-Soares, M
# sensitive(?): No
# Description:
# Generates synthetic date for Quality of Life studies based on statistics from Head and & Neck 5000 data (http://www.headandneck5000.org.uk/)
# and census data from the UK government. The same ontology from HN5000 is used in the variable names.  

import pandas as pd
import numpy as np
from scipy import stats


def qol_penalization(n, df, penalization):
    """
        decrease quality of life score for
        :param penalization: dictionary with the penalization for each time point
    """

    for time_point in range(1, 5):
        t = "hn"+str(time_point)+'_qol'
        df.at[n, t] = np.int(df[t][n] - np.random.rand() * penalization[str(time_point)])

    return df


def expand_multinomial(num_samples, probs):
    """
        expand a multinomial distribution of num_samples according to
        the provided probabilities probs.

    :param num_samples: number of samples
    :param probs: array with probabilities
    :return:
    """

    dist = stats.multinomial.rvs(n=num_samples, p=probs)
    expanded = np.zeros(num_samples, dtype=np.int64)

    a = 0
    b = 0
    for k, n in enumerate(dist):
        b += n
        for i in range(a, b):
            expanded[i] = k
        a += n

    np.random.shuffle(expanded)

    return expanded


def state_transition_matrix(n_states):
    """
        computes the state transition matrix 
        based on the distance between the states. 
        states separated by larger distances have lower 
        probability with A_ij = 1/|i-j|
    """
    A = np.zeros((n_states, n_states))
    
    for i in range(0, n_states):
        for j in range(0, n_state):
            if np.abs(i-j)>0:
                A[i, j] = (1/np.abs(i-j))
            else:
                A[i, j] = 1.0

    for i in range(0, n_states):
        A[i, :] = A[i, :]/A[i, :].sum()
        
    return A


def markov_process(p_initial, n_steps, dt = 1.):
    """
        integrates the master equation with euler method
        and returns the normalised probability state

        :param p_initial: initial probability distribution per state
        :param n_steps: number of time steps
        :param dt: time interval
    """
    n_states = len(p_initial)
    A = state_transition_matrix(n_states)
    p = np.copy(p_initial)
    pt = np.zeros((n_steps, n_states))
    
    # time propagation
    for i in range(0, n_steps):
        pt[i, :] = p/p.sum()
        p = p + dt * np.dot(A, p)
    
    
    return p/p.sum(), pt


def sim_data(num_samples, num_time_points, seed):
    """
        clinical samples: baseline
        clinical data from notes: baseline, 4 months, 12 months, 24 months
        QoL questionnaires: baseline, 4 months, 12 months, 24 months

    :param num_samples: number of samples to be simulated
    :param num_time_points: number of time points
    :return:
    """

    np.random.seed(seed)

    oc_tmn_dist = {'I': 0.337, 'II': 0.226, 'III': 0.08, 'IV': 0.357}
    sex_dist = {'male': 0.728, 'female': 0.272}
    # I am ... anxious or depressed 
    hn1_depression_dist = {'not': 0.452,  'slightly': 0.132, 
                       'moderately': 0.332, 'severely': 0.014, 
                       'extremely':0.031, 'missing':0.039}

    hn2_depression_dist = {'not': 0.221,  'slightly': 0.175, 
                       'moderately': 0.082, 'severely': 0.014, 
                       'extremely':0.05, 'missing':0.505}
    scale = 10
    data = {'id': np.int64(np.random.rand(num_samples) * 10e5),
            'hn1_age': 55 + stats.truncnorm(a=-50 / scale, b=50 / scale, scale=scale).rvs(size=num_samples).round().astype(
                np.int64),
            'hn1_sex': expand_multinomial(num_samples, np.array(list(sex_dist.values()))),
            'hn1_tnm': expand_multinomial(num_samples, np.array(list(oc_tmn_dist.values()))),
            'hn2_surgery': np.zeros(num_samples, dtype=np.int64),
            'hn2_radio': np.zeros(num_samples, dtype=np.int64),
            'hn2_chemo': np.zeros(num_samples, dtype=np.int64),
            'hn1_smoke': np.zeros(num_samples, dtype=np.int64),
            'hn1_depression': expand_multinomial(num_samples, np.array(list(hn1_depression_dist.values()))),
            'hn2_depression': expand_multinomial(num_samples, np.array(list(hn2_depression_dist.values()))),
            'hn3_depression': expand_multinomial(num_samples, np.array(list(hn2_depression_dist.values()))),
            'hn4_depression': expand_multinomial(num_samples, np.array(list(hn1_depression_dist.values()))),
            'hn1_alcohol': np.zeros(num_samples, dtype=np.int64),
            'hn1_qol': np.ones(num_samples, dtype=np.int64) * 100,
            'hn2_qol': np.ones(num_samples, dtype=np.int64) * 100,
            'hn3_qol': np.ones(num_samples, dtype=np.int64) * 100,
            'hn4_qol': np.ones(num_samples, dtype=np.int64) * 100}

    df = pd.DataFrame(data=data, columns=data.keys())

    for n in range(num_samples):
         
        smoke_prob_m = 0.45  # UK ever smoked? - 45% male, 35% female
        smoke_prob_f = 0.35  # current smokers 18% male, 15% female
                           # used to smoke - 26% male, 21% female
                           # hn5000 cohort -
                           # https://www.statista.com/statistics/376602/cigarette-smoking-status-by-gender-in-england/

        alc_prob = 0.4
        sur_prob = 0.5
        rad_prob = 0.2
        chem_prob = 0.8

        if df['hn1_age'][n] >= 50:
            dcr = df['hn1_age'][n] * 0.1
            df = qol_penalization(n, df, {'1': 0, '2': dcr, '3': dcr, '4': dcr})

        if df['hn1_sex'][n] == 0:
            smoke_prob = smoke_prob_m
            alc_prob += 0.1
        else:
            smoke_prob = smoke_prob_f
            alc_prob -= 0.1

        if df['hn1_tnm'][n] >= 3:
            smoke_prob += 0.2
            alc_prob += 0.2
            sur_prob += 0.2
            chem_prob += 0.18

        if np.random.rand() <= smoke_prob:
            df.at[n, 'hn1_smoke'] = 1
        if np.random.rand() <= alc_prob:
            df.at[n, 'hn1_alcohol'] = 1
        if np.random.rand() <= sur_prob:
            df.at[n, 'hn2_surgery'] = 1
            df = qol_penalization(n, df, {'1': 0, '2': 30, '3': 20, '4': 10})
        if np.random.rand() <= rad_prob:
            df.at[n, 'hn2_radio'] = 1
            df = qol_penalization(n, df, {'1': 0, '2': 20, '3': 10, '4': 8})
        if np.random.rand() <= chem_prob or df['hn2_radio'][n] == 0 and df['hn2_surgery'][n] == 0:
            df.at[n, 'hn2_chemo'] = 1
            df = qol_penalization(n, df, {'1': 0, '2': 15, '3': 5, '4': 0})

    df = df.replace({ "hn2_radio":{0:"No", 1:"Yes"},
                       "hn1_smoke": {0:"No", 1:"Yes"},
                       "hn1_alcohol": {0:"No", 1:"Yes"},
                      "hn2_chemo":{0:"No", 1:"Yes"},
                      "hn2_surgery":{0:"No", 1:"Yes"},
                      "hn1_depression":{0:'not at all',  1:'slightly', 
                       2:'moderately', 3:'severely', 
                       4:'extremely', 5:'a. - Missing'}, 
                     "hn2_depression":{0:'not',  1:'slightly', 
                       2:'moderately', 3:'severely', 
                       4:'extremely', 5:'a. - Missing'},
                    "hn3_depression":{0:'not at all',  1:'slightly', 
                       2:'moderately', 3:'severely', 
                       4:'extremely', 5:'a. - Missing'}})
    return df
  
if __name__ == "__name__":
    
    df = sim_data(num_samples = 3083, num_time_points = 4, seed = 42)
    
    df.to_csv(df, "simulated_data.csv")
    
