# 📦 DTVC Project Portfolio – Strategic Business Analytics
## 📌 Project 1: **Restructuring Strategy for SFB**

**Objective:**  
Support SFB's restructuring by predicting which employees are likely to accept a voluntary exit offer (RCC) and use optimisation to minimise severance costs while maintaining operational balance.

### 🔍 Techniques Used:
- **📊 Predictive Modelling (Python, `prediction.ipynb`):**
  - Built and validated an XGBoost classifier to predict RCC acceptance.
  - Evaluated model with cross-validation and SHAP analysis to identify key drivers: `JobSatisfaction`, `DistanceFromHome`, `StockOptionLevel`.

- **📈 Dimensionality Reduction & Clustering:**
  - Applied PCA to reduce dimensionality.
  - Used K-Means clustering to define objective, non-discriminatory employee groups based on latent characteristics (e.g., tenure, satisfaction).

- **📉 Optimisation (Excel, `optimisation.xlsx`):**
  - Formulated a binary optimisation problem to offer RCCs to groups of employees to:
    - Minimise total salary costs.
    - Cut at least €3M in payroll and achieve 40+ employee exits.
    - Preserve at least 80% departmental staffing.
  - Solved using Excel Solver's Evolutionary Algorithm.

### ✅ Key Findings:
- Optimal solution offered RCCs to 4 job roles, saving **€6.57M** in salaries.
- Group-based RCC offers balance fairness and administrative feasibility.
- Small, targeted groups are efficient but risk bias; larger groups reduce legal exposure but dilute impact.
- Ensured demographic parity to avoid discrimination risks.

---

## 📌 Project 2: **Market Research for a New Business Using Web Scraping**

**Objective:**  
Demonstrate how real-world data collection via web scraping and APIs can support competitive analysis for a new small business venture.

### 🔍 Techniques Used:
- **🌐 Web Scraping:**
  - Tools: `BeautifulSoup`, `Selenium`
  - Scraped data from review platforms, competitor websites, and public listings.

- **🧼 Data Pipeline:**
  - Developed a modular pipeline for:
    - Data extraction
    - Cleaning and preprocessing
    - Feature engineering
  - Built lexicons describing each variable for clarity and interpretability.

- **📊 Exploratory Analysis:**
  - Conducted:
    - Summary statistics
    - Price distribution comparisons
    - Sentiment analysis from reviews

- **🎯 Business Application:**
  - Defined industry context and core question (e.g., identifying optimal pricing strategies for boutique fitness studios).
  - Mapped how collected data answers key market positioning challenges.

### 🎥 Deliverables:
- Presentation link: https://youtu.be/pPGAslWoR20


