# Summary of Machine Learning Assignments

## 1. Coronary Heart Disease Prediction 

**Dataset:** Males from a high-risk region in Western Cape, South Africa  
**Objective:** Predict likelihood of coronary heart disease (CHD)  

### Methods & Models:
- **Exploratory Data Analysis (EDA):** Identification of skewness, outliers (alcohol, LDL, tobacco), and class imbalance  
- **Preprocessing:** SMOTE for training set, MinMax scaling for models requiring standardised input  
- **Baseline Model:** Logistic Regression with Ridge Regularisation  
- **Alternative Models Compared:**
  - k-Nearest Neighbours (kNN)
  - Random Forest
  - Extreme Gradient Boosting (XGBoost)
  - Support Vector Machine (Linear and Radial Kernels)

### Evaluation Metric:
- **Primary Metric:** Area Under the Curve (AUC)  
- **Secondary Metrics:** Accuracy, False Negatives

### Key Findings:
- **Best Model:** SVM with Radial Basis Function kernel (AUC = 81.8%)  
- **Feature Importance:** Age, LDL, Tobacco, and Adiposity were most influential across models  
- **Insights:**  
  - SVM provided robust generalisation and minimised false negatives, which is critical in medical diagnosis  
  - Random Forest performed well but showed signs of overfitting  

---

## 2. Clustering and PCA of IMDB Movies

**Dataset:** 50 Top-rated IMDB Movies (Cleaned to 47 rows)  
**Objective:** Uncover latent patterns using unsupervised learning  

### Methods & Models:
- **Principal Component Analysis (PCA):**
  - PC1 (Popularity) – Negatively correlated with audience engagement  
  - PC2 (Commercial vs. Critical Reception) – Trade-off between revenue and rating/runtime  
- **Clustering Approaches:**
  - K-Means (k=5 chosen via Elbow Method and Silhouette Score)  
  - Hierarchical Clustering (Complete Linkage preferred)  
  - DBSCAN (poor separation due to density limitations)

### Evaluation Metrics:
- **Silhouette Score:** K-Means = 0.48 (better cohesion and separation)  
- **WCSS:** Lower for K-Means, supporting more compact clusters  

### Key Findings:
- **Best Clustering:** K-Means produced well-separated, interpretable clusters (e.g., high-budget sci-fi, Indian cinema, animated features)  
- **Comparison with ChatGPT:** Human-led methods yielded more coherent and better-validated clustering than ChatGPT-4o, especially when evaluating cohesion and inter-cluster distance  

---

## 3. Shiny App for Diabetes Prediction 

**Dataset:** Pima Indians Diabetes Dataset  
**Objective:** Build an interactive Shiny application to visualise and compare Decision Tree and Random Forest classifiers  

### App Design:
- **UI:** Parameter sliders, decision boundary plots, model tabs  
- **Server Logic:** Dynamic model updating and real-time result rendering  

### Methods & Models:
- **Decision Tree:** Interpretable splits using Gini Impurity; good recall but higher false negatives  
- **Random Forest:** Ensemble of trees; more stable and accurate, handles non-linearity better  
- **SMOTE:** Applied to balance training data  
- **Cross-Validation:** 10-fold for robust performance estimation  

### Evaluation Metrics:
- Accuracy, Precision, Recall, F1-Score  
- AUC Scores: Random Forest = 0.73, Decision Tree = 0.64  

### Key Findings:
- **Best Model:** Random Forest for its superior generalisation and AUC score  
- **Feature Importance:** Glucose, BMI, and Age were the strongest predictors  
- **Ethical Considerations:** Emphasis on reducing false negatives in medical prediction, balanced with interpretability for clinician trust  

---

## Conclusion
These assignments showcase a comprehensive application of supervised and unsupervised learning methods.
