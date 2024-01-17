from sklearn import metrics
from sklearn.calibration import calibration_curve, CalibrationDisplay

n_bootstraps = 200

# make sure to use the right threshold according to the selected scale
threshold = 83 # Gessinger et al.
outcome = 'hn4_dv_c30_phys_func'
model_folder = 'lasso_phys_func'



def bootstrapped_roc_curve(df, outcome, model_folder, n_bootstraps, bs):
    # plot roc curve for out-of-bag performance using bootstraps

    preds = np.zeros((0, 3))
    auc_curve = np.zeros((0, 4))


    tprs = []
    aucs = []
    mean_fpr = np.linspace(0, 1, 50)

    for bootstrap in range(0, n_bootstraps):
        print(bootstrap)
        bs_n = df.iloc[bs[bs['bs_id'] == bootstrap+1].loc[:, 'studyid'], : ].copy(deep=True)

        # out-of-bag samples
        oob = get_oob_samples(df, bs_n.loc[:, "studyid_hn057"])
        
        # bootstrap data
        X_train = bs_n.loc[:, covariates]
        y_train = bs_n.loc[:, outcome]

        # bootstrap out-of-bag samples
        X_oob = oob[~oob[outcome].isna()].loc[:, covariates]
        y_oob = oob[~oob[outcome].isna()].loc[:, outcome]

        with open('results/' +model_folder+ '/model_bs'+str(bootstrap+1)+'.pkl', 'rb') as file:
            bs_model = pickle.load(file)
            
        
        label = np.where(y_oob <= threshold, 1, 0)

        y_pred_prob = bs_model.predict_proba(X_oob, lower = threshold)
        

        fpr, tpr, thresholds = metrics.roc_curve(label, y_pred_prob, pos_label=1)
        auc = metrics.roc_auc_score(label, y_pred_prob)

        interp_tpr = np.interp(mean_fpr, fpr, tpr)
        interp_tpr[0] = 0.0
        tprs.append(interp_tpr)
        aucs.append(auc)

        auc_curve = np.vstack((auc_curve, np.vstack((fpr, tpr, thresholds, np.ones(fpr.shape[0])*(bootstrap+1))).T))
        
        prob_true, prob_pred = calibration_curve(label, y_pred_prob, n_bins=10)

        preds = np.vstack((preds, np.vstack((prob_true, prob_pred, np.ones(prob_true.shape[0])*(bootstrap+1))).T))

    auc_curve = pd.DataFrame(auc_curve, columns = ['fpr', 'tpr', 'thresholds', 'bs'])
    preds = pd.DataFrame(preds, columns = ['y_true', 'y_pred', 'bs'])
 
    return (preds, auc_curve, tprs, aucs, thresholds)

def plot_bootstrapped_roc_curve(aucs, tprs)

    mean_tpr = np.mean(tprs, axis=0)
    mean_tpr[-1] = 1.0
    mean_auc = metrics.auc(mean_fpr, mean_tpr)
    std_auc = np.std(aucs)



    std_tpr = np.std(tprs, axis=0)
    tprs_upper = np.minimum(mean_tpr + 1.96*std_tpr, 1)
    tprs_lower = np.maximum(mean_tpr - 1.96*std_tpr, 0)

    optimum = get_optimum_threshold(mean_fpr, mean_tpr, thresholds)


    fig, ax = plt.subplots(figsize=(6, 6))

    ax.plot(
        mean_fpr,
        mean_tpr,
        color="black",
        label=r"Mean ROC (AUC = %0.2f [%0.2f, %0.2f])" % (mean_auc, mean_auc-1.96*std_auc, mean_auc + 1.96*std_auc),
        lw=2,
        alpha=0.8,
    )



    std_tpr = np.std(tprs, axis=0)
    tprs_upper = np.minimum(mean_tpr + 1.96*std_tpr, 1)
    tprs_lower = np.maximum(mean_tpr - 1.96*std_tpr, 0)

    ax.fill_between(
        mean_fpr,
        tprs_lower,
        tprs_upper,
        color="grey",
        alpha=0.2,
        #label=r"$\pm$ 2 std. dev.",
        label=r"$95\%$ Confidence Interval",
    )

    ax.set(
        xlim=[-0.05, 1.05],
        ylim=[-0.05, 1.05],
        xlabel="False Positive Rate",
        ylabel="True Positive Rate",
        #title=f"Mean and Bootstrapped 95% Confidence Intervals",
    )

    ax.plot(mean_tpr, mean_tpr, ls=':', color='black')

    #cutoff = '(%.2f, %.2f, %.2f)' % (optimum['fpr'].to_numpy()[0], optimum['tpr'].to_numpy()[0], optimum['thresholds'].to_numpy()[0] )
    cutoff = 'Optimum cut-off = %.5f' %  optimum['thresholds'].to_numpy()[0]
    ax.scatter(optimum['fpr'].to_numpy()[0],  optimum['tpr'].to_numpy()[0], marker='x', color='red', s=100, label=cutoff, zorder=200) 
    #plt.text(x = 0.1 +optimum['fpr'].to_numpy()[0], y = optimum['tpr'].to_numpy()[0], s=cutoff)
    #ax.get_figure()
    ax.axis("square")
    ax.legend(loc="lower right")
    handles, labels = ax.get_legend_handles_labels()
    ax.legend(handles[-4:], labels[-4:])
    plt.show()

