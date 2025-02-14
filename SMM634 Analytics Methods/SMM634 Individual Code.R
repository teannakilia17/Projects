library(mgcv)
library(car)
library(glmnet)
library(pROC)
library(statmod)

## load the data
data <-read.table("hospital.txt")

# Inspect the structure of the data
str(data)
summary(data)

data$gender <- as.factor(data$gender) # M = 0
data$severity <- as.factor(data$severity)
data$avpu <- as.factor(data$avpu)
data$risk <- as.factor(data$risk)

set.seed(123)
trainIndex <- createDataPartition(data$los, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Checking for multicolinearity 
model1_vif <-glm(died ~ age + gender + bmi  + sp02 + sbp + dbp + pulse + respiratory + temp,
                 family = binomial(link = "logit"), data = data)

vif <- vif(model1_vif)
print(vif)

## Model Selection for Mortality (Binary Logistic Regression)---------------------------------------------------------------
# Logistic Regression Model for Mortality (died)
logit_model <- glm(died ~ age + gender + bmi + severity + risk + sp02 + sbp + dbp + pulse + respiratory + avpu + temp,
                       family = binomial(link = "logit"), data = trainData)


# Convert coefficients to Odds Ratios (exp(coef)) to interpret the effect of predictors
mortality_odds_ratios <-exp(coef(logit_model))
print(mortality_odds_ratios)

logit_Pprob <- predict(logit_model, newdata = testData, type = "response")
logit_pred <- ifelse(logit_Pprob > 0.5, 1, 0)

testData$died <- factor(testData$died, levels = c(0, 1))

roc_curve <- roc(response = testData$died, predictor = logit_Pprob, levels = rev(levels(testData$died)), direction = ">")

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve for Logistic Regression", col = "blue")

# Calculate the AUC
auc_value <- auc(roc_curve) #0.81

#Prepare data for LASSO
X <- model.matrix(died ~ age + gender + bmi + severity + risk + sp02 + sbp + dbp + pulse + respiratory + avpu + temp, trainData)[, -1]
y <- trainData$died

#Fit LASSO model
lasso_model <- cv.glmnet(X, y, alpha = 1, family = "binomial", standardize = FALSE)
summary(lasso_model)

#Plot cross-validated error
plot(lasso_model, main = "Cross-Validated Error Curve",pch=10)

#Coefficients of the best model
lasso_coefs <- coef(lasso_model, s = "lambda.1se")
print(lasso_coefs)

#Identifying variables using LASSO logistic regression
lasso_coefs_df <- as.data.frame(as.matrix(lasso_coefs))
non_zero_coefs <- lasso_coefs_df[lasso_coefs_df != 0, , drop = FALSE]
print(non_zero_coefs)

#Prepare model for Lasso
X_test <- model.matrix(died ~ age + gender + bmi + severity + risk + sp02 + sbp + dbp + pulse + respiratory + avpu + temp, 
                       data = testData)[, -1]
lasso_Pprob <- predict(lasso_model, newx = X_test, s = "lambda.min", type = "response")
lasso_pred <- ifelse(lasso_Pprob > 0.5, 1, 0)

testData$died <- factor(testData$died, levels = c(0, 1))

#Fit LASSO model with cross validation 
lasso_roc <- roc(response = testData$died, predictor = as.numeric(lasso_Pprob), levels = rev(levels(testData$died)), direction = ">")
plot(lasso_roc, main = "ROC Curve for LASSO Logistic Regression")
auc(lasso_roc) #0.96

## Model Selection for Length of Stay (LOS)----------------------------------------------------
#Model 2 - length of stay (los)----
#Fit a Generalized Additive Model
gam_model <- gam(los ~ s(age) + s(bmi) + severity + risk + s(sp02) + s(sbp) + s(dbp) + s(pulse) + respiratory + avpu + s(temp),
                 data = data, family = gaussian())

#Summary of the model
summary(gam_model)

#Predict and evaluate performance
gam_pred <- predict(gam_model, testData)
cor(gam_pred, testData$los) # Check correlation between predictions and actual values

#Evaluate performance
mse <- mean((gam_pred - testData$los)^2)
rmse <- sqrt(mse)
paste0("MSE:", round(mse,2), " and RMSE:", round(rmse,2))

## Diagnostic Plots ----------------------------------------
# Model Diagnostics for Logistical Regression
par(mfrow =c(2,2))
plot(logit_model)

# Model Diagnostics for LASSO Regression
par(mfrow =c(1,1))
plot(lasso_model)

#2. Residual Plot for GAM Model
par(mfrow =c(1,1))
plot(
  gam_model$residuals,
  main = "Residuals of GAM Model",
  xlab = "Observation",
  ylab = "Residuals",
)

abline(h = 0, col = "red")



