# 📊 **Data Analysis & Model Selection Projects Overview**

## 1️⃣ **Group Project: Healthcare Expenditure & Doctor Visits**

### Problem Statement:
🩺💰 The task is to analyze healthcare expenditure on doctor visits using data from over 10,000 individuals. We aim to understand the socio-economic and demographic factors influencing **healthcare spending** 🏥, particularly focusing on doctor visits and expenditures.

### Steps Taken:

1. **Data Preprocessing**:  
   - Cleaned the dataset (with variables like **general health**, **bmi**, **income**, etc.) using `R`'s `read.table()` and ensured proper variable naming 📝.
   - Applied **linear regression** to analyze **doctor visit expenditure** (`dvexpend`) based on independent variables 🧮.

2. **Modeling**:  
   - Built a **Multiple Linear Regression** model to predict the expenditure on doctor visits based on variables like **general health**, **income**, and **hypertension**. 
   - Used **VIF (Variance Inflation Factor)** to check multicollinearity between predictors with `vif()` 🧠🔍.

3. **Results**:  
   - Identified key factors influencing healthcare costs: **number of doctor visits** 🏥, **income** 💵, and **general health** 💪.  
   - Found that **more doctor visits** directly correlated with **higher expenditures**. 

4. **Business Implications**:  
   - **Policy recommendations**: Insights can inform healthcare policy, especially regarding **accessibility** and **spending efficiency** 🔄💡.
   - Consider **improving access to healthcare** in underprivileged regions to **reduce overall spending** 🌍.

---

## 2️⃣ **Individual Project: Predicting Hospital Mortality & Length of Stay**

### Problem Statement:
🏥🔬 This analysis aims to predict **in-hospital mortality** and **length of stay (LOS)** using a dataset from a **Virginia medical center**. The goal is to identify key **patient characteristics** that influence these outcomes for better **resource management** and **patient care**.

### Steps Taken:

1. **Data Preprocessing**:  
   - Loaded and cleaned the dataset with variables such as **age**, **BMI**, **severity**, **risk**, and **vital signs** using `read.table()` in `R` 🧹.
   - Split data into training and testing sets using `createDataPartition()` for cross-validation 🗂️.

2. **Modeling**:  
   - For **mortality prediction**, applied **Logistic Regression** with **Lasso** regularization (`glmnet`) to improve interpretability and remove irrelevant variables ⚖️.
   - For **LOS prediction**, used a **Generalized Additive Model (GAM)** (`mgcv`) to capture **non-linear relationships** between continuous predictors like **temperature** 🌡️ and **severity** ⚠️.

3. **Results**:  
   - **Logistic regression** identified **severity** and **oxygen saturation (spO2)** as strong predictors of mortality 🧠💡.
   - **GAM** revealed that **severity** and **temperature** were critical in predicting **length of stay** 🔑🏥.

4. **Evaluation**:  
   - **ROC curve** analysis showed a high **AUC** for both **Lasso logistic regression** and **GAM**, proving their strong predictive capabilities 📈.
   - Model diagnostics suggested improvements for **heteroskedasticity** and **non-normal residuals** using **log transformations** or **interaction terms** 🔧.

---

## 3️⃣ **Individual Assignment: Hospital Mortality & Length of Stay Analysis**

### Problem Statement:
📊💉 Analyze the effect of multiple factors on **mortality** and **length of stay** in the hospital. Focus is on **age**, **BMI**, **severity**, and other physiological measures.

### Steps Taken:

1. **Model Selection for Mortality**:  
   - Chose **Logistic Regression** for **binary mortality outcomes** (alive or dead) 🏥💀.
   - Applied **Lasso regularization** to improve model performance and identify significant predictors 🧮.

2. **Model Selection for Length of Stay (LOS)**:  
   - Fitted a **Generalized Additive Model (GAM)** to capture both **linear** and **non-linear relationships** for **LOS prediction** ⏳.

3. **Results & Interpretation**:  
   - Key findings: **Severity** strongly predicts **both mortality** and **LOS**. **Oxygen saturation** also showed a strong relationship with mortality 🔬.
   - Found that **older patients** had higher mortality risks, but **temperature** was a better predictor of **LOS** 🏥🌡️.

4. **Evaluation**:  
   - **Logistic regression** model had a solid **AUC** score of **0.96**, while the **GAM** for LOS had an **RMSE** of **2.94 days** 📊✅.

5. **Business Implications**:  
   - Hospitals could optimize **resource allocation** by focusing on **severity** and **age** to predict **mortality risks** and **LOS** 🏥💡.

---
