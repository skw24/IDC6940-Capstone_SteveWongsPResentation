
# Stroke Prediction Analysis Using Logistic Regression

## Project Overview
In this project, we analyzed a Kaggle dataset to predict the likelihood of stroke in individuals based on 11 different predictor variables. The goal was to apply logistic regression, evaluate the assumptions, and develop multiple models to handle issues like small sample bias and skewed predicted probabilities.

## Data Preparation
The dataset consists of 3357 observations and 11 predictor variables. Here's a quick overview of how the variables were recoded:

1. **Age**: Continuous, recoded to numeric with two decimal places.
2. **Gender**: Categorical, recoded as 1 (male) and 2 (female), with 'other' recoded to N/A.
3. **Hypertension, Heart Disease, Marital Status, Work Type, Residence Type, Smoking Status**: Categorical variables recoded to numeric values.
4. **BMI, Glucose Levels**: Continuous, recoded to numeric with two decimal places.

### Data Summary:
After cleaning the data, we were left with 3357 cases, which is sufficient to proceed with logistic regression (15 cases per predictor variable rule).

## Logistic Regression Assumptions
Before fitting the logistic regression model, we verified the following assumptions:
1. The outcome variable has 2 categories: Stroke (1) and No Stroke (0).
2. There is a linear relationship between predictor variables and the outcome variable, confirmed through residual plots.
3. There are no substantial outliers, with Cook's D ranging from 0 to 0.0122.
4. No multicollinearity was found, with VIF values between 1.01 and 1.21.

### Conclusion: All assumptions were met, making the dataset suitable for logistic regression.

## Model Development
We developed three logistic regression models:
1. **Base Logistic Regression**: Standard logistic regression.
2. **Firth Regression**: Adjusts for small sample bias in rare events datasets.
3. **FLIC**: A refinement of Firth regression that adjusts the intercept to match observed event rates.

## Model Evaluation
We evaluated the models using ROC curves and Youden’s J statistics. All three models showed similar performance, indicating that the dataset was balanced enough to distinguish between stroke and non-stroke cases.

### ROC Curves and Youden’s J:
The area under the curve (AUC) was consistent across all models at 0.8285, indicating good model performance.

![ROC Curve for Logistic Regression](./extracted_images/image4.png)

### Confusion Matrices:
Confusion matrices for all models were identical, confirming no significant bias in the dataset.

![Confusion Matrix](./extracted_images/image9.png)

## Conclusion
The analysis showed that the dataset was balanced enough to distinguish between stroke and non-stroke events. The three models provided similar results, confirming the reliability of the dataset and logistic regression approach.
