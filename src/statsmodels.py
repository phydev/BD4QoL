


def _medcouple_1d(y):
    """
    Calculates the medcouple robust measure of skew.

    Parameters
    ----------
    y : array_like, 1-d
        Data to compute use in the estimator.

    Returns
    -------
    mc : float
        The medcouple statistic

    Notes
    -----
    The current algorithm requires a O(N**2) memory allocations, and so may
    not work for very large arrays (N>10000).

    .. [*] M. Hubert and E. Vandervieren, "An adjusted boxplot for skewed
       distributions" Computational Statistics & Data Analysis, vol. 52, pp.
       5186-5201, August 2008.
    """

    # Parameter changes the algorithm to the slower for large n

    y = np.squeeze(np.asarray(y))
    if y.ndim != 1:
        raise ValueError("y must be squeezable to a 1-d array")

    y = np.sort(y)

    n = y.shape[0]
    if n % 2 == 0:
        mf = (y[n // 2 - 1] + y[n // 2]) / 2
    else:
        mf = y[(n - 1) // 2]

    z = y - mf
    lower = z[z <= 0.0]
    upper = z[z >= 0.0]
    upper = upper[:, None]
    standardization = upper - lower
    is_zero = np.logical_and(lower == 0.0, upper == 0.0)
    standardization[is_zero] = np.inf
    spread = upper + lower
    h = spread / standardization
    # GH5395
    num_ties = np.sum(lower == 0.0)
    if num_ties:
        # Replacements has -1 above the anti-diagonal, 0 on the anti-diagonal,
        # and 1 below the anti-diagonal
        replacements = np.ones((num_ties, num_ties)) - np.eye(num_ties)
        replacements -= 2 * np.triu(replacements)
        # Convert diagonal to anti-diagonal
        replacements = np.fliplr(replacements)
        # Always replace upper right block
        h[:num_ties, -num_ties:] = replacements

    return np.median(h)



def medcouple(y, axis=0):
    """
    Calculate the medcouple robust measure of skew.

    Parameters
    ----------
    y : array_like
        Data to compute use in the estimator.
    axis : {int, None}
        Axis along which the medcouple statistic is computed.  If `None`, the
        entire array is used.

    Returns
    -------
    mc : ndarray
        The medcouple statistic with the same shape as `y`, with the specified
        axis removed.

    Notes
    -----
    The current algorithm requires a O(N**2) memory allocations, and so may
    not work for very large arrays (N>10000).

    .. [*] M. Hubert and E. Vandervieren, "An adjusted boxplot for skewed
       distributions" Computational Statistics & Data Analysis, vol. 52, pp.
       5186-5201, August 2008.
    """
    y = np.asarray(y, dtype=np.double)  # GH 4243
    if axis is None:
        return _medcouple_1d(y.ravel())

    return np.apply_along_axis(_medcouple_1d, axis, y)


