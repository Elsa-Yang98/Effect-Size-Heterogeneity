# Effect-Size-Heterogeneity-Matters-in-High-Dimensions
A finer-grained understanding of the trade-off between type I and type II errors for variable selection.

The functions:


1. calctpp(alpha, delta, eps, gamma, M):
    #The function that computes tpp, using the equation: P(|Pi^* + tau Z| > alpha tau)
    ## Dependent on function calctau

2. calctau(alpha, delta, eps, gamma, M):
    #The function calculates tau via bi-search (for reasonable input of alpha)
    #The equation is AMP I.
    ## Dependent on function res, funcA

3. res(alpha, tau, eps, delta, gamma,  M):
    #The function that rearranges AMP I, and it should = 0. This solves tau using bi-search.

4. funcA(alpha, x):
    #The expansion of E[eta_alpha(x+z)-x]^2, where z ~ N(0, 1) and `eta_alpha` is the soft-threshold function at alpha.

5. calcalpha0(delta):
    #The function calculate alpha0: the solution of (1 + t^2) Phi(-t) - t phi(t) = delta/2

