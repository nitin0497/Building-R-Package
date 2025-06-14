# Building-R-Package

Final Project

The final project will be evaluated on 10 points and the goal is to develop an R package implementing logistic regression using numerical optimization. The package must contain the basic functions to perform logistic regression (e.g. estimate the coefficient vector β
 which includes the independent variables/predictors plus the intercept) and obtain different outputs from the procedure. The estimator to be computed using numerical optimization is the following:

β^:=argminβ∑i=1n(−yi⋅ln(pi)−(1−yi)⋅ln(1−pi)),
where
pi:=11+exp(−xTiβ),
and yi
 and xi
 represent the ith
 observation and row of the response and the predictors respectively.

Without using any of the logistic regression or classification functions already available in R (i.e. all outputs must be produced using formulas provided in this document), the basic outputs from the procedure must be the following:

Initial values for optimization obtained from the least-squares formula (XTX)−1XTy
.
Bootstrap Confidence intervals: the user must be able to choose (i) the significance level α
 to obtain for the 1−α
 confidence intervals for β
, and (ii) the number of bootstraps which by default will be 20.
Plot of the fitted logistic curve to the actual values (see this link or this link for example).
The resulting ``Confusion Matrix’’ (see this link) using a cut-off value for prediction at 0.5 (i.e. assign value 1 for predictions above 0.5 and value 0 for prediction below or equal to 0.5). In addition, based on this cut-off value, also output the following metrics:
Prevalence
Accuracy
Sensitivity
Specificity
False Discovery Rate
Diagnostic Odds Ratio
The possibility for the user to plot of any of the above metrics evaluated over a grid of cut-off values for prediction going from 0.1 to 0.9 with steps of 0.1.
Help documentation for all functions (for example using the roxygen2 package)
The package will be made available for download on a public GitHub repository in the AU-R-Data-Science organization, meaning that it should be downloadable using the function install_github(). The final submission on Canvas will be an html file consisting in a so-called vignette (see e.g. this link) which indicates the name of the GitHub repository (and package). This document explains and gives examples of how to use the package functions for all the desired outputs using one of the datasets on the Canvas course page.

Up to 5 bonus points will be given for the final projects if other pertinent/useful features are added for the package (e.g. a website with vignette, an example Shiny app that uses the package).
