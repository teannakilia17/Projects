## Load libraries

library(tidyverse)
library(corrplot) 
library(ggplot2)
library(caret)
library(glmnet)
library(smotefamily)
library(pROC)    
library(randomForest)
library(e1071)
library(lattice)
library(class)

### Data Preprocessing ###----------------------------------------------------
# Load and clean dataset
df <- read.csv("heart-disease.csv") %>%
  drop_na() %>%  # Remove missing values
  mutate(
    famhist = ifelse(famhist == "Present", 1, 0),
    chd = factor(chd)  # Ensure factor conversion in one step
  )

str(df)      # Check data types (numeric, factor, etc.)
summary(df)  # Get summary statistics (min, max, median, mean)


### Exploratory Data Analysis ###-------------------------------------------------------
# Convert dataset into long format for visualization
df_long <- df %>%
  pivot_longer(cols = where(is.numeric), names_to = "variable", values_to = "value")

# Boxplot for each numeric variable
ggplot(df_long, aes(x = variable, y = value)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red", outlier.shape = 16) +
  theme_minimal() +
  coord_flip() +
  labs(title = "Boxplots of Numeric Variables (Detecting Outliers)", x = "Variable", y = "Value")

# Histograms for numeric variables
par(mfrow=c(1,1))
ggplot(df_long, aes(x = value)) +
  geom_histogram(fill = "blue", bins = 30, alpha = 0.7) +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of Numeric Variables")

# Compute correlation matrix
cor_matrix <- df %>%
  select(where(is.numeric)) %>% 
  cor(use = "pairwise.complete.obs")

# Visualize the correlation matrix
corrplot(cor_matrix, method = "color", type = "upper",
         tl.cex = 0.8, tl.col = "black",
         col = colorRampPalette(c("blue", "white", "red"))(200),
         addCoef.col = "black", number.cex = 0.8,
         title = "Correlation between Variables", mar = c(0, 0, 2, 0))

## Remove features if correlation is 0.8 or higher as that shows multicollinearity

#PCA
pca_data <- df %>% select_if(is.numeric)

preprocess_params <- preProcess(pca_data, method = c("center", "scale"))
pca_scaled <- predict(preprocess_params, pca_data)

pca_model <- prcomp(pca_scaled, center = TRUE, scale. = TRUE)
summary(pca_model)
pca_model$rotation

pca_df <- data.frame(
  PC1 = pca_model$x[,1], 
  PC2 = pca_model$x[,2], 
  CHD = df$chd  # Add CHD labels back for coloring
)

ggplot(pca_df, aes(x = PC1, y = PC2, color = CHD)) +
  geom_point(alpha = 0.7, size = 3) +
  labs(title = "PCA of Heart Disease Data",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Heart Disease Status") +
  theme_minimal()

## Data Processing --------------------------------------------------------------
# Stratified Train-Test Split (80-20)
set.seed(345)
trainIndex <- createDataPartition(df$chd, p = 0.8, list = FALSE)
train <- df[trainIndex, ]
test <- df[-trainIndex, ]

table(train$chd)
table(test$chd)

# Balance Training Data with SMOTE
set.seed(345)
smote_data <- SMOTE(train[, -ncol(train)], train$chd, K = 5, dup_size = 0.5)
train_smote <- smote_data$data
colnames(train_smote)[ncol(train_smote)] <- "chd"
train_smote$chd <- as.factor(train_smote$chd)  # Convert back to factor

# Controlled undersampling to prevent oversampling
train_smote <- train_smote %>%
  group_by(chd) %>%
  slice_sample(n = min(table(train_smote$chd))) %>%
  ungroup()

# Check Class Distributions
table(train_smote$chd)
table(test$chd)

# Converting Binary Data to Factors
train_smote$chd <- as.factor(train_smote$chd)
test$chd <- as.factor(test$chd)

train_smote$famhist <- as.factor(train_smote$famhist)
test$famhist <- as.factor(test$famhist)

### Model 1: Logistic Regression with Ridge Penalty ------------------------------------------
### Prepare Data for Ridge Regression
ridge_x_train <- model.matrix(chd ~ ., data = train_smote)[, -1]  # Remove intercept
ridge_x_test  <- model.matrix(chd ~ ., data = test)[,-1]

ridge_y_train <- train_smote$chd
ridge_y_test  <- test$chd

# Verify Data Consistency
str(ridge_x_train)
str(ridge_x_test)

dimnames(ridge_x_train)
dimnames(ridge_x_test)

### Train Ridge Regression Model with Cross-Validation
set.seed(345)  
cv_ridge <- cv.glmnet(ridge_x_train, ridge_y_train, alpha = 0, family = "binomial")

# Extract Best Lambda (Regularization Parameter)
best_lambda_ridge <- cv_ridge$lambda.1se
print(paste("Best Lambda for Ridge:", best_lambda_ridge))

# Train Final Ridge Model with Best Lambda
ridge_model <- glmnet(ridge_x_train, ridge_y_train, alpha = 0, lambda = best_lambda_ridge, family = "binomial")

### Predict Probabilities on Train and Test Sets
ridge_train_probs <- predict(ridge_model, newx = ridge_x_train, type = "response")
ridge_test_probs  <- predict(ridge_model, newx = ridge_x_test, type = "response")

### Convert Probabilities to Binary Predictions (Threshold = 0.5)
ridge_train_preds <- ifelse(ridge_train_probs > 0.5, 1, 0)
ridge_test_preds  <- ifelse(ridge_test_probs > 0.5, 1, 0)

# Confusion Matrix for Test Set
ridge_train_probs <- as.vector(ridge_train_probs)
ridge_test_probs <- as.vector(ridge_test_probs)

conf_matrix_ridge <- confusionMatrix(as.factor(ridge_test_preds), as.factor(ridge_y_test))
print(conf_matrix_ridge)

### Compute AUC for Ridge Regression
roc_ridge_train <- roc(ridge_y_train, ridge_train_probs,  direction = "<")
roc_ridge_test  <- roc(ridge_y_test, ridge_test_probs,  direction = "<")

ridge_train_auc <- auc(roc_ridge_train)
ridge_test_auc  <- auc(roc_ridge_test)

# Print Train and Test AUC
print(paste("Ridge Train AUC:", round(ridge_train_auc, 3)))
print(paste("Ridge Test AUC:", round(ridge_test_auc, 3)))   

### Prepare ROC Data for ggplot
ridge_roc_train_df <- data.frame(
  tpr = roc_ridge_train$sensitivities,
  fpr = 1 - roc_ridge_train$specificities,
  set = "Train Set"
)

ridge_roc_test_df <- data.frame(
  tpr = roc_ridge_test$sensitivities,
  fpr = 1 - roc_ridge_test$specificities,
  set = "Test Set"
)

ridge_roc_data <- rbind(ridge_roc_train_df, ridge_roc_test_df)

### Plot ROC Curves for Train vs Test
ggplot(ridge_roc_data, aes(x = fpr, y = tpr, color = set)) +
  geom_line(size = 1) +  # Line for each set
  geom_abline(linetype = "dashed", color = "gray") +  # Diagonal reference line
  labs(title = "Ridge Logistic Regression ROC Curves: Train vs Test",
       x = "False Positive Rate (1 - Specificity)", 
       y = "True Positive Rate (Sensitivity)",
       color = "Dataset") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"))  # Set colors: Blue for Train, Red for Test


### Model 2: kNN-------------------------------------------------------------------
### Define kNN-Specific Data 
## Numeric Features only

train_smote$famhist <-as.numeric(train_smote$famhist)
test$famhist <-as.numeric(test$famhist)

knn_x_train <- train_smote[, !names(train_smote) %in% "chd"]
knn_x_test <- test[, !names(test) %in% "chd"]

knn_y_train <- factor(train_smote$chd, labels = c("No", "Yes"))
knn_y_test <- factor(test$chd, labels = c("No", "Yes"))

# Normalize Data (Min-Max Scaling)
normalize <- function(x) (x - min(x)) / (max(x) - min(x))
knn_train_scaled <- as.data.frame(lapply(knn_x_train, normalize))
knn_test_scaled <- as.data.frame(lapply(knn_x_test, normalize))

# Verify normalization
summary(knn_train_scaled)
summary(knn_test_scaled)

### Tune k using cross-validation (Odd k-values from 1 to 30)
set.seed(42)
knn_control <- trainControl(
  method = "cv", 
  number = 10, 
  classProbs = TRUE,  
  summaryFunction = twoClassSummary
)

knn_train_final <- cbind(knn_train_scaled, chd = knn_y_train)

# Train kNN Model
set.seed(42)
knn_model <- train(
  chd ~ ., 
  data = knn_train_final,  # Use the final combined dataset
  method = "knn",
  tuneGrid = expand.grid(k = seq(1, 30, by = 2)),  # Tune k automatically
  trControl = knn_control,
  metric = "ROC"  
)

# Extract the Best k Value
best_k <- knn_model$bestTune$k
print(paste("Best k:", best_k))

auc_scores <- knn_model$results$ROC  # Extract AUC values
k_values <- knn_model$results$k       # Extract k values
best_auc <- max(auc_scores)           # Get the best AUC score

# Plot AUC scores vs. k values
plot(k_values, auc_scores, type = "b", col = "blue", pch = 16,
     xlab = "k (Number of Neighbors)", ylab = "AUC Score",
     main = "Tuning k in kNN")
abline(v = best_k, col = "red", lwd = 2, lty = 2)
text(best_k, best_auc, labels = paste("Best k =", best_k), pos = 4, col = "red")

# Predict Probabilities
knn_train_probs <- predict(knn_model, knn_train_final, type = "prob")[, "Yes"]
knn_test_probs <- predict(knn_model, knn_test_scaled, type = "prob")[, "Yes"]

# Compute Train AUC
roc_knn_train <- roc(knn_y_train, knn_train_probs, direction = "<")
train_auc <- auc(roc_knn_train)
print(paste("kNN Train AUC:", round(train_auc, 3)))

# Compute Test AUC
roc_knn_test <- roc(knn_y_test, knn_test_probs, direction = "<")
test_auc <- auc(roc_knn_test)
print(paste("kNN Test AUC:", round(test_auc, 3)))

# Confusion Matrix
knn_test_preds <- ifelse(knn_test_probs > 0.5, "Yes", "No")
conf_matrix_knn <- confusionMatrix(factor(knn_test_preds, levels = c("No", "Yes")), knn_y_test)
print(conf_matrix_knn)

knn_roc_train_df <- data.frame(
  tpr = roc_knn_train$sensitivities,
  fpr = 1 - roc_knn_train$specificities,
  set = "Train Set"
)

knn_roc_test_df <- data.frame(
  tpr = roc_knn_test$sensitivities,
  fpr = 1 - roc_knn_test$specificities,
  set = "Test Set"
)

knn_roc_data <- rbind(knn_roc_train_df, knn_roc_test_df)

### Plot ROC Curves for Train vs Test (kNN)
ggplot(knn_roc_data, aes(x = fpr, y = tpr, color = set)) +
  geom_line(size = 1) +  # Line for each set
  geom_abline(linetype = "dashed", color = "gray") +  # Diagonal reference line
  labs(title = "kNN ROC Curves: Train vs Test",
       x = "False Positive Rate (1 - Specificity)", 
       y = "True Positive Rate (Sensitivity)",
       color = "Dataset") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"))

### Model 3: Random Forest------------------------------------------------------
# Prepare Data
rf_x_train <- model.matrix(chd ~ ., train_smote)[, -1]  # Remove intercept
rf_x_test  <- model.matrix(chd ~ ., test)[, colnames(rf_x_train), drop = FALSE]  # Keep same columns

rf_y_train <- train_smote$chd
rf_y_test  <- test$chd

set.seed(345)

### Define Hyperparameter Grid
rf_ntree_values <- c(10, 25, 50, 100, 200, 300, 500)  # Number of trees
rf_mtry_values <- c(1, 2, 3, 4, 5)  # Features per split
rf_maxnodes_values <- c(5, 10, 15, 20)  # Max terminal nodes
rf_nodesize_values <- c(5, 10, 15, 20)  # Minimum node size

# Initialize Results Data Frame
rf_results <- data.frame(
  ntree = integer(),
  mtry = integer(),
  maxnodes = integer(),
  nodesize = integer(),
  Train_AUC = numeric(),
  Test_AUC = numeric()
)

### Cross-Validation Setup
rf_control <- trainControl(
  method = "cv",       # Cross-validation
  number = 10,         # 10-Fold CV
  classProbs = TRUE,   # Enables probability estimation
  summaryFunction = twoClassSummary  # Optimizes for AUC
)

### Hyperparameter Tuning Loop
for (ntree in rf_ntree_values) {
  for (mtry in rf_mtry_values) {
    for (maxnodes in rf_maxnodes_values) {
      for (nodesize in rf_nodesize_values) {
        
        # Train Random Forest Model
        set.seed(42)  # Ensure reproducibility
        rf_model <- randomForest(
          rf_x_train, rf_y_train,
          ntree = ntree,
          mtry = mtry,
          maxnodes = maxnodes,
          nodesize = nodesize,
          importance = TRUE
        )
        
        # Get Predicted Probabilities
        rf_train_probs <- predict(rf_model, rf_x_train, type = "prob")[,2]
        rf_test_probs  <- predict(rf_model, rf_x_test, type = "prob")[,2]
        
        # Compute AUC for Train and Test Sets
        roc_rf_train <- roc(rf_y_train, rf_train_probs, direction = "<")
        roc_rf_test  <- roc(rf_y_test, rf_test_probs, direction = "<")
        
        # Store Results
        rf_results <- rbind(rf_results, data.frame(
          ntree = ntree,
          mtry = mtry,
          maxnodes = maxnodes,
          nodesize = nodesize,
          Train_AUC = auc(roc_rf_train),
          Test_AUC = auc(roc_rf_test)
        ))
        
      }
    }
  }
}

### Get Best Model (Highest Test AUC)
best_rf_model <- rf_results[which.max(rf_results$Test_AUC), ]
print("Best Hyperparameters for Random Forest:")
print(best_rf_model)  # Example: Train AUC: 0.93, Test AUC: 0.811

# Extract Best Hyperparameters
best_ntree <- best_rf_model$ntree
best_mtry <- best_rf_model$mtry
best_maxnodes <- best_rf_model$maxnodes
best_nodesize <- best_rf_model$nodesize

# Train the Best Random Forest Model
set.seed(42)
best_rf <- randomForest(
  rf_x_train, rf_y_train,
  ntree = best_ntree,
  mtry = best_mtry,
  maxnodes = best_maxnodes,
  nodesize = best_nodesize,
  importance = TRUE
)

print(varImp(best_rf))

# Predict Probabilities for Train & Test Sets
rf_train_probs <- predict(best_rf, rf_x_train, type = "prob")[,2]
rf_test_probs  <- predict(best_rf, rf_x_test, type = "prob")[,2]

# Compute ROC Curves
roc_rf_train <- roc(rf_y_train, rf_train_probs, direction = "<")
roc_rf_test  <- roc(rf_y_test, rf_test_probs, direction = "<")

# Print AUC Scores
cat("RF Train AUC:", round(auc(roc_rf_train), 3), "\n")
cat("RF Test AUC:", round(auc(roc_rf_test), 3), "\n")

# Predict Class Labels (Convert Probabilities to Class Labels)
rf_test_preds  <- predict(best_rf, rf_x_test, type = "class")
conf_matrix_rf  <- confusionMatrix(rf_test_preds, rf_y_test)
print(conf_matrix_rf)

# Create DataFrames for ROC Curves (Train & Test)
rf_roc_train_df <- data.frame(
  tpr = roc_rf_train$sensitivities,
  fpr = 1 - roc_rf_train$specificities,
  set = "Train Set"
)

rf_roc_test_df <- data.frame(
  tpr = roc_rf_test$sensitivities,
  fpr = 1 - roc_rf_test$specificities,
  set = "Test Set"
)

# Combine Data for Plotting
rf_roc_data <- rbind(rf_roc_train_df, rf_roc_test_df)

# Plot ROC Curves for Train & Test
ggplot(rf_roc_data, aes(x = fpr, y = tpr, color = set)) +
  geom_line(size = 1) +  
  geom_abline(linetype = "dashed", color = "gray") +  # Diagonal reference line
  labs(title = "Random Forest ROC Curves: Train vs. Test",
       x = "False Positive Rate (1 - Specificity)", 
       y = "True Positive Rate (Sensitivity)",
       color = "Dataset") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red")) 

### Model 4 : XGB (with & without Feature Selection)------------------------------------------------------
### Prepare Data for XGBoost
xgb_x_train <- model.matrix(chd ~ ., data = train_smote)[, -1]  # Remove intercept
xgb_x_test  <- model.matrix(chd ~ ., data = test)[, colnames(xgb_x_train), drop = FALSE]  # Keep same columns

xgb_y_train <- factor(train_smote$chd, labels = c("No", "Yes"))
xgb_y_test <- factor(test$chd, labels = c("No", "Yes"))

### Define Hyperparameter Grid for Tuning
xgb_grid <- expand.grid(
  nrounds = c(100, 150, 200),       # Limit boosting rounds (prevents overfitting)
  max_depth = c(2, 4, 6),           # Tree depth (controls complexity)
  eta = c(0.005, 0.01, 0.015),      # Learning rate (prevents overfitting)
  gamma = c(5, 10, 15),             # Minimum loss reduction (higher = more regularization)
  colsample_bytree = c(0.5, 0.75),  # Feature sampling (reduces correlation among trees)
  min_child_weight = c(10, 20, 30), # Prevents small splits
  subsample = c(0.5, 0.75)          # Fraction of data per tree (reduces variance)
)

xgb_control <- trainControl(
  method = "cv",       
  number = 10,          
  classProbs = TRUE,   
  summaryFunction = twoClassSummary
)

### Train XGBoost with Full Feature Set
set.seed(42)
xgb_full <- train(
  x = xgb_x_train, 
  y = xgb_y_train,  
  method = "xgbTree",              
  trControl = xgb_control,            
  tuneGrid = xgb_grid,            
  metric = "ROC"                  
)

# Print Best Hyperparameters for Full XGBoost Model
print(xgb_full$bestTune)

# Predict Probabilities
xgb_train_probs <- predict(xgb_full, xgb_x_train, type = "prob")[,2]
xgb_test_probs  <- predict(xgb_full, xgb_x_test, type = "prob")[,2]

# Compute ROC
roc_xgb_train <- roc(xgb_y_train, xgb_train_probs, direction = "<")
roc_xgb_test  <- roc(xgb_y_test, xgb_test_probs, direction = "<")

# Print AUC Scores
cat("XGB Full Train AUC:", round(auc(roc_xgb_train), 3), "\n")
cat("XGB Full Test AUC:", round(auc(roc_xgb_test), 3), "\n")  

xgb_test_preds <- ifelse(xgb_test_probs > 0.5, "Yes", "No")
conf_matrix_xgb1 <- confusionMatrix(factor(xgb_test_preds, levels = c("No", "Yes")), xgb_y_test)
print(conf_matrix_xgb1)

# Feature Importance
print(varImp(xgb_full))

## XGB With Feature Selection (Removing variables with 0 impact)

### Train XGBoost with Selected Features
xgb_selected_features <- c("tobacco", "age", "ldl", "alcohol", "famhist")

xgb_x_train_selected <- xgb_x_train[, xgb_selected_features]
xgb_x_test_selected  <- xgb_x_test[, xgb_selected_features]

### Define Updated Hyperparameter Grid
xgb_grid_selected <- expand.grid(
  nrounds = c(100, 150, 200),       
  max_depth = c(2, 4, 6),           
  eta = c(0.005, 0.01, 0.015),      
  gamma = c(10, 20, 30),            
  colsample_bytree = c(0.5, 0.75),  
  min_child_weight = c(10, 20, 30), 
  subsample = c(0.5, 0.75)          
)

### Train XGBoost with Selected Features
set.seed(42)
xgb_selected <- train(
  x = xgb_x_train_selected, 
  y = xgb_y_train,  
  method = "xgbTree",              
  trControl = xgb_control,            
  tuneGrid = xgb_grid_selected,            
  metric = "ROC"                  
)

# Print Best Hyperparameters for Selected Features
print(xgb_selected$bestTune)

# Predict Probabilities for Selected Features
xgb_selected_train_probs <- predict(xgb_selected, as.matrix(xgb_x_train_selected), type = "prob")[,2]
xgb_selected_test_probs  <- predict(xgb_selected, as.matrix(xgb_x_test_selected), type = "prob")[,2]

# Compute ROC
roc_xgb_selected_train <- roc(xgb_y_train, xgb_selected_train_probs, direction = "<")
roc_xgb_selected_test  <- roc(xgb_y_test, xgb_selected_test_probs, direction = "<")

# Print AUC Scores
cat("XGB Selected Train AUC:", round(auc(roc_xgb_selected_train), 3), "\n")
cat("XGB Selected Test AUC:", round(auc(roc_xgb_selected_test), 3), "\n")  

# Confusion Matrix
xgb_selected_test_preds <- ifelse(xgb_selected_test_probs > 0.5, "Yes", "No")
conf_matrix_xgb <- confusionMatrix(factor(xgb_selected_test_preds, levels = c("No", "Yes")), xgb_y_test)
print(conf_matrix_xgb)

### Plot ROC Curves
# Create DataFrames for ROC Curves (Before Feature Selection)
roc_train_before <- data.frame(fpr = 1 - roc_xgb_train$specificities, tpr = roc_xgb_train$sensitivities, set = "Train")
roc_test_before <- data.frame(fpr = 1 - roc_xgb_test$specificities, tpr = roc_xgb_test$sensitivities, set = "Test")

# Create DataFrames for ROC Curves (After Feature Selection)
roc_train_after <- data.frame(fpr = 1 - roc_xgb_selected_train$specificities, tpr = roc_xgb_selected_train$sensitivities, set = "Train with Selected Features")
roc_test_after <- data.frame(fpr = 1 - roc_xgb_selected_test$specificities, tpr = roc_xgb_selected_test$sensitivities, set = "Test with Selected Features")

# Combine Data for Plotting
roc_data <- rbind(roc_train_before, roc_test_before, roc_train_after, roc_test_after)

ggplot(roc_data, aes(x = fpr, y = tpr, color = set, group = set)) +
  geom_line(size = 1, aes(linetype = set)) +  # Keep lines visually distinct
  geom_abline(linetype = "dashed", color = "gray") +  
  labs(title = "XGBoost ROC Curves: Before vs. After Feature Selection",
       x = "False Positive Rate (1 - Specificity)", 
       y = "True Positive Rate (Sensitivity)", 
       color = "Dataset Type") +  # ✅ Set explicit legend title
  theme_minimal() +
  
  # Define Colors for Different Curves
  scale_color_manual(values = c("Train" = "blue", "Test" = "red", "Train with Selected Features" = "darkgreen", "Test with Selected Features" = "purple")) +
  
  # Define Line Types but Remove from Legend
  scale_linetype_manual(values = c("Train" = "dashed", "Test" = "dashed", "Train with Selected Features" = "solid", "Test with Selected Features" = "solid"), 
                        guide = "none") +  # Removes linetype from legend
  
  # Clean Up the Legend (Only Keep Color)
  guides(color = guide_legend(title = "Dataset Type")) +  # Set clean title
  
  # Improve Legend Appearance
  theme(legend.position = "bottom")

### Model 5 : SVM (Linear. & Radial)------------------------------------------------------
### Prepare Data for SVM Linear & Radial
svm_x_train <- train_smote[, !names(train_smote) %in% "chd"]
svm_x_test <- test[, !names(test) %in% "chd"]

# Normalize Data (Min-Max Scaling)
normalize <- function(x) (x - min(x)) / (max(x) - min(x))
svm_linear_x_train <- as.data.frame(lapply(svm_x_train, normalize))
svm_linear_x_test <- as.data.frame(lapply(svm_x_test, normalize))

# Verify normalization
summary(svm_linear_x_train)
summary(svm_linear_x_test)

svm_y_train <- factor(train_smote$chd, labels = c("No", "Yes"))
svm_y_test <- factor(test$chd, labels = c("No", "Yes"))

svm_radial_x_train <- svm_linear_x_train  # Same data for both kernels
svm_radial_x_test  <- svm_linear_x_test

### Define Cross-Validation Strategy
svm_control <- trainControl(
  method = "cv",       
  number = 10,          
  classProbs = TRUE,   
  summaryFunction = twoClassSummary
)

## Linear SVM
### **Train & Tune Linear SVM**
svm_linear_grid <- expand.grid(
  C = c(0.01, 0.1, 1, 10, 100)
)

set.seed(42)
svm_linear <- train(
  x = svm_linear_x_train,
  y = svm_y_train,  
  method = "svmLinear",
  trControl = svm_control,    
  tuneGrid = svm_linear_grid,
  metric = "ROC"
)

# Print Best Hyperparameters for Linear SVM
print(paste("Best Cost for SVM Linear:", svm_linear$bestTune$C))

# Predict Probabilities for Linear SVM
svm_linear_train_probs <- predict(svm_linear, svm_linear_x_train, type = "prob")[,2]
svm_linear_test_probs  <- predict(svm_linear, svm_linear_x_test, type = "prob")[,2]

# Compute ROC & AUC for Linear SVM
roc_svm_linear_train <- roc(svm_y_train, svm_linear_train_probs,  direction = "<")
roc_svm_linear_test  <- roc(svm_y_test, svm_linear_test_probs,  direction = "<")

# Print Linear SVM AUC Scores
cat("SVM Linear Train AUC:", round(auc(roc_svm_linear_train), 3), "\n")
cat("SVM Linear Test AUC:", round(auc(roc_svm_linear_test), 3), "\n")

# Confusion Matrix
svm_linear_test_preds <- ifelse(svm_linear_test_probs > 0.5, "Yes", "No")
conf_matrix_svml <- confusionMatrix(factor(svm_linear_test_preds, levels = c("No", "Yes")), svm_y_test)
print(conf_matrix_svml)

varImp(svm_linear)

### Plot ROC Curves for Train vs. Test Performance
roc_train_df <- data.frame(
  fpr = 1 - roc_svm_linear_train$specificities, 
  tpr = roc_svm_linear_train$sensitivities, 
  set = "Train Set"
)

roc_test_df <- data.frame(
  fpr = 1 - roc_svm_linear_test$specificities, 
  tpr = roc_svm_linear_test$sensitivities, 
  set = "Test Set"
)

roc_data <- rbind(roc_train_df, roc_test_df)

ggplot(roc_data, aes(x = fpr, y = tpr, color = set)) +
  geom_line(size = 1) +
  geom_abline(linetype = "dashed", color = "gray") +
  labs(title = "SVM Linear Kernel ROC Curve",
       x = "False Positive Rate (1 - Specificity)", 
       y = "True Positive Rate (Sensitivity)",
       color = "Dataset") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"))


## Radial SVM
### **Train & Tune Radial SVM**
svm_radial_grid <- expand.grid(
  sigma = 10^seq(-3, 1, length = 5),  
  C = 10^seq(-1, 2, length = 5)
)

set.seed(42)
svm_radial <- train(
  x = svm_radial_x_train,
  y = svm_y_train,  
  method = "svmRadial",
  trControl = svm_control,    
  tuneGrid = svm_radial_grid,
  metric = "ROC"
)

# Print Best Hyperparameters for Radial SVM
print(paste("Best Cost for SVM Radial:", svm_radial$bestTune$C))
print(paste("Best Gamma for SVM Radial:", svm_radial$bestTune$sigma))

# Predict Probabilities for Radial SVM
svm_radial_train_probs <- predict(svm_radial, svm_radial_x_train, type = "prob")[,2]
svm_radial_test_probs  <- predict(svm_radial, svm_radial_x_test, type = "prob")[,2]

# Compute ROC & AUC for Radial SVM
roc_svm_radial_train <- roc(svm_y_train, svm_radial_train_probs, direction = "<")
roc_svm_radial_test  <- roc(svm_y_test, svm_radial_test_probs,  direction = "<")

# Print Radial SVM AUC Scores
cat("SVM Radial Train AUC:", round(auc(roc_svm_radial_train), 3), "\n")
cat("SVM Radial Test AUC:", round(auc(roc_svm_radial_test), 3), "\n")

# Confusion Matrix
svm_radial_test_preds <- ifelse(svm_radial_test_probs > 0.5, "Yes", "No")
conf_matrix_svmr <- confusionMatrix(factor(svm_radial_test_preds, levels = c("No", "Yes")), svm_y_test)
print(conf_matrix_svmr)

varImp(svm_radial)

### Plot ROC Curves for Train vs. Test Performance
roc_train_df <- data.frame(
  fpr = 1 - roc_svm_radial_train$specificities, 
  tpr = roc_svm_radial_train$sensitivities, 
  set = "Train Set"
)

roc_test_df <- data.frame(
  fpr = 1 - roc_svm_radial_test$specificities, 
  tpr = roc_svm_radial_test$sensitivities, 
  set = "Test Set"
)

roc_data <- rbind(roc_train_df, roc_test_df)

ggplot(roc_data, aes(x = fpr, y = tpr, color = set)) +
  geom_line(size = 1) +
  geom_abline(linetype = "dashed", color = "gray") +
  labs(title = "SVM Radial Kernel ROC Curve",
       x = "False Positive Rate (1 - Specificity)", 
       y = "True Positive Rate (Sensitivity)",
       color = "Dataset") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"))

### Appendix --------------------------------------------------------------------
# Model Improvements - Implementing Dynamic Threshold using Youden’s J Statistic
## SVM Linear
roc_svm_linear_test  <- roc(svm_y_test, svm_linear_test_probs,  direction = "<")
auc_svm_linear_test <- auc(roc_svm_linear_test)

optimal_threshold_svm <- as.numeric(coords(roc_svm_linear_test, "best", ret = "threshold", best.method = "youden"))
svm_test_preds_dynamic  <- factor(ifelse(svm_linear_test_probs  >= optimal_threshold_svm, "Yes", "No"))
conf_matrix_svm_linear  <- confusionMatrix(svm_test_preds_dynamic, svm_y_test)

cat("Optimal Threshold:", round(optimal_threshold_svm, 3), "\n")
cat("SVM Linear Test AUC:", round(auc_svm_linear_test, 3), "\n")
print(conf_matrix_svm_linear)

## SVM Radial
roc_svm_radial_test  <- roc(svm_y_test, svm_radial_test_probs,  direction = "<")
auc_svm_radial_test <- auc(roc_svm_radial_test)

optimal_threshold_svmr <- as.numeric(coords(roc_svm_radial_test, "best", ret = "threshold", best.method = "youden"))
svmr_test_preds_dynamic  <- factor(ifelse(svm_radial_test_probs  >= optimal_threshold_svmr, "Yes", "No"))
conf_matrix_svm_radial  <- confusionMatrix(svmr_test_preds_dynamic, svm_y_test)

cat("Optimal Threshold:", round(optimal_threshold_svmr, 3), "\n")
cat("SVM Linear Test AUC:", round(auc_svm_radial_test, 3), "\n")
print(conf_matrix_svm_radial)

