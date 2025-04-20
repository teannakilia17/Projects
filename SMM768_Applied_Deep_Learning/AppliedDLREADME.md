## 🚀 Project 1: Diabetes Detection Neural Network & HealthTech Business Model

**Assignment Type:** Group Project – Business-Focused Neural Network Deployment  
**Dataset Used:** [Diabetes Prediction Dataset on Kaggle](https://www.kaggle.com/datasets/iammustafatz/diabetes-prediction-dataset)  
**Business Idea:** AI-powered early-warning system for Type II Diabetes screening in primary care and telemedicine clinics.

### 🔍 Project Objective:
Design and pitch a business model centred on a deep learning solution that predicts diabetes from health indicators. Train and validate a functional neural network as a proof-of-concept for investors.

### 🧠 Techniques Used:
- **Data Preprocessing:**
  - Null value handling, normalization of continuous variables.
  - Train-validation-test split (70/15/15).
  - Balanced dataset through resampling.

- **Neural Network Architecture (TensorFlow/Keras):**
  - Fully connected feedforward network.
  - ReLU activations, dropout for regularisation.
  - Binary crossentropy loss and Adam optimiser.
  - Early stopping and validation monitoring to prevent overfitting.

- **Model Evaluation:**
  - Accuracy, Precision, Recall, F1-Score, AUC-ROC.
  - Confusion matrix analysis to evaluate Type II error minimisation.

### 💼 Business Model Summary:
- **Customers:** Telehealth providers, clinics, national health systems.
- **Value Proposition:** Rapid, low-cost, scalable diabetic screening tool.
- **Revenue Model:** Subscription-based API for integration with electronic health record systems.
- **Differentiation:** Model interprets early symptoms, enabling proactive interventions, reducing hospitalisations.

### 📈 Key Results:
- Achieved ~84% AUC on test set.
- Demonstrated practical viability in a clinical screening setting.
- Next steps: acquire real-time patient data via EHR integration to improve generalisation.

---

## ✈️ Project 2: BERTopic-Based Review Mining for Singapore Airlines

**Assignment Type:** Individual Project – NLP and Topic Modelling for Strategic Insight  
**Dataset Used:** Publicly scraped Singapore Airlines customer reviews  
**Objective:** Analyse post-pandemic customer sentiment shifts and prioritise areas for service investment.

### 🔍 Problem Statement:
Singapore Airlines has faced increased customer dissatisfaction since 2021. This project aims to uncover the most negatively discussed service themes using unsupervised NLP techniques.

### 🧠 Techniques Used:
- **BERTopic for Topic Modelling:**
  - Uses contextual embeddings and class-based TF-IDF to group reviews by semantic similarity.
  - Clustered 80% of review content into four coherent topics.

- **Sentiment Analysis:**
  - Merged topic clusters with star ratings and helpful vote data.
  - Calculated volume-weighted impact scores to identify high-priority issues.

- **Visualization & Interpretation:**
  - Temporal trend plots.
  - Topic-by-sentiment heatmaps.
  - Volume and severity scoring for prioritisation.

### 🔍 Key Findings:
1. **Customer Service & Refunds**:
   - Largest contributor to negative sentiment (28% of reviews, avg. rating 1.35).
   - Most helpful votes received → highest urgency.

2. **Inflight Economy Experience**:
   - Second-highest negative trend post-2021.
   - Key complaint: comfort and IFE system quality.

3. **Outlier Topics**:
   - Captured in ungrouped review clusters (54% of volume).
   - Recommended manual categorisation and system redesign.

### Recommendations:
- Automate refund and escalation processes.
- Upgrade Economy Class seating and entertainment.
- Assign human team to review unclustered service complaints.
