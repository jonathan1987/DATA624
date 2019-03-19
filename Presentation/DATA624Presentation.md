---
title: "CUNYSPS DATA624 Presentation"
author: "Jonathan Hernandez"
date: "March 13, 2019"
output: slidy_presentation
  toc: true
---

CUNY SPS DATA624 Spring 2019 Presentation
Chapter 6 of Applied Predictive Modeling: Linear Regression and It's Cousins
========================================================

========================================================

This presentation will about Linear Regression and it's alternatives like partial
least squares and Penalized regression models. I will go over each one in detail
starting with

- Linear Regression
- Partial Least Squares (PLS)
- Penalized Regression Models (LASSO, Ridge Regression and elastic net)

Linear Regression
========================================================

- Linear Regression is one of the most basic tools a Data Scientist or Analyst can
use for analyzing data. It is also used as a prerequiste for other advanced regression and
machine learning techniques.
- In its general form it is the formula


$$
\begin{align}
y_i = b_0 +b_1x_{i1} + b_2x_{i2} + ... + b_px_{iP} + \epsilon_i \tag{1}
\end{align}
$$

where 

- $y_i$ represents the numerical response of observation i
- $b_0$ represents the estimated intercept
- $b_j$ is the estimated coefficient of the jth predictor
- $x_{ij}$ is the value of the jth feature of observation i
- P is the number of predictors or explanatory variables
- $\epsilon_i$ is random error that can't be explained by the model

Ordinary Least Squares
========================================================

- When performing linear regression, the goal is to find coefficients that minimize the sum-of-squared
errors (SSE) between the actual value of the response and the predicted response.

- The (SSE) is defined as the following

$$
\begin{align}
SSE = \sum_{i=1}^n (y_i - \hat{y_i})^2 \tag{1}
\end{align}
$$

- This equation contains the coefficient estimates for each predictor in a vector.

- It can be shown that the coefficients that minimize the SSE is the matrix equation
(Note this would involve a combination of Matrix Algebra and Solving a system of 
partial derivatives to derive). There is a problem with this solution which will be addressed
later in the presentation.

$$
\begin{align}
(X^TX)^{-1}X^Ty
\end{align}
$$

Multiple Linear Regression Caveats
========================================================

- Several flaws in multiple linear regression. For example, the response variable
is a linear model and that if the data have some non-linear structure in the mix,
the multiple linear regression technique may not be accurate.

- There are many statistics that can tell if the multiple linear regression model you
are using is a good one or not.

- Having data with large residuals when doing a multiple regression model can negatively
impact your model.

- If the data are shown to not have near constant variance, this is also an indictor that
the model needs to be adjusted and may need transformations, feature extraction etc.

- If the residual plot does not follow a normal or near-normal distribution, that is also
a bad sign.

- Using Q-Q plots can also be a good metric to test and see as well.

- If there are more predictors than observations, Eq. (2) cannot be inverted.

- Other metrics like $R^2$, $R^2_{adj}$, p-value and F-test come in handy as well.
R's lm() function gives you these statistics.