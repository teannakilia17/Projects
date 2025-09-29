# Earnings Call Sentiment Analysis

This repository contains data and analysis for sentiment extraction from earnings call transcripts of three companies: **Amazon, Apple, and Nvidia**.  

For each company folder, three main types of files are included:

---

## File Descriptions

### 1. Models File (`*_Models.ipynb`)
- Purpose: Preprocessing and model construction.
- Tasks:
  - Clean and chunk raw transcripts into analysable segments.
  - Apply **FinBERT** and **DeepSeek** sentiment models.
  - Save sentiment outputs for further analysis.

### 2. Statistical Analysis File (`*_StatsAnalysis.ipynb`)
- Purpose: Performance evaluation and relationship testing.
- Tasks:
  - Generate visualisations (graphs, plots).
  - Run **confusion matrices** to compare model predictions with manual annotations.
  - Conduct **linear regression** to test the link between transcript sentiment and market sentiment.
  - Market sentiment is derived from **Yahoo Finance** data.

### 3. Full Dataset (`*_full.csv`)
- Purpose: Centralised dataset for each company.
- Contents:
  - Manual sentiment annotations.
  - Sentiment scores from **FinBERT**.
  - Sentiment scores from **DeepSeek**.
- Acts as the base reference for statistical analysis.

---

## Company Folders
Each company folder (`AMAZON/`, `APPLE/`, `NVIDIA/`) contains:
- `*_Models.ipynb`
- `*_StatsAnalysis.ipynb`
- `*_full.csv`
- `clean_transcripts/` (cleaned earnings call transcripts used as input)
