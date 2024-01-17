from typing import List, Type, Union

def estimate_control_points(y: Type[np.ndarray[np.float64]], adjusted: bool = True) -> Type[np.ndarray[np.float64]]:
    """
    estimate the robust control points for detecting outliers in skewed distributions
    these cut-offs can be used later for defining the relevance function used on
    unbalanced regression models
    :param y: array
    :param bounds: if bounded variable pass boundaries as array/list
    :return control_points: array with [lower_extreme, median, upper_extreme]
    """
    quantiles = np.quantile(y, [0.25, 0.50, 0.75])
    IQR = quantiles[2] - quantiles[0]
    if adjusted:
        MC = medcouple(y)
    else:
        MC = 0.0

    if MC>= 0.0:
        control_points = np.array([quantiles[0] - 1.5 * np.exp(-4 * MC) * IQR, quantiles[1],  quantiles[2] +  1.5 * np.exp(3 * MC) * IQR])
    else:
        control_points = np.array([quantiles[0] - 1.5 * np.exp(-3 * MC) * IQR, quantiles[1],  quantiles[2] +  1.5 * np.exp(4 * MC) * IQR])
            
    return control_points


if __name__ == '__main__':
    from scipy.interpolate import PchipInterpolator


    control_points = np.array([0, 29.16666412,  83.33333588, 129.16666412])

    relevance = np.array([1.0, 1.0, 0.5, 1.0])

    phi =  PchipInterpolator(control_points, relevance, extrapolate = False)
    phi_y = phi(y)
    plt.plot(y, phi_y)
    plt.ylim(0, 1.5)
    plt.xlabel('y')
    plt.ylabel('$\phi$')
    plt.show()
    #sns.boxplot(x=df['hn4_dv_c30_ghs'],  color='skyblue')
    #plt.show()

    import pickle 

    filehandler = open('phi.pkl', 'wb') 
    pickle.dump(phi, filehandler)
    filehandler.close()