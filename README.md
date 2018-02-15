# Likelihood-of-a-Customer-Cancelling-Insurance-Policy
The task is to identify home insurance policies that will cancel before the end of their term. Need to build a strong predictive model to predict those policies that are most likely to cancel as well as understand what variables are most influential in causing a policy cancellation.

01. Data_Preprocessing.R:
Before jumping into creating models and making recommendations, getting familiar with the data is of utmost importance. Some times, hidden trends and patterns in the data can reveal valuable insights which could help us making decisions. Here I am showing the different steps of data pre processing including missing value treatment, outlier removal, visualization etc.

02. Feature Engineering.R:
One way to create a strong predictive model is to create new features from the existing ones. Initial visualizations have helped me deciding what features to create which will be significant to the model

03. Logistic Regression.R:
Simple Logistic Regression using glm

04. L1 Lasso Regularization.R:
Introducing penalty parameters (mod of coefficients) to reduce overfitting and make the model more general

05. L2 Ridge Regularization.R
Introducing penalty parameters (square of coefficients) to reduce overfitting and make the model more general

06. Elastic Net.R
Introducing penalty parameters (combination of mod of coefficients and square of coefficients) to reduce overfitting and make the model more general

And boosting algorithms (gradient boosting and XGBoost). Gradient boosting fared better than all other algorithms.
