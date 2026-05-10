# House Prices — Advanced Regression Techniques in R

This repository contains an R-based exploratory modelling workflow for Kaggle's [House Prices: Advanced Regression Techniques](https://www.kaggle.com/competitions/house-prices-advanced-regression-techniques) competition.

The project explores how housing characteristics can be cleaned, engineered, and modelled to predict residential sale prices.

## Overview

The workflow includes:

- exploratory data analysis
- missing-value review and treatment
- categorical and ordinal feature encoding
- feature engineering for property characteristics
- sale-price transformation
- regression modelling
- Kaggle-style prediction output

## Techniques used

The script includes examples of:

- feature engineering for tabular data
- ordinal encoding of quality/condition variables
- dummy-variable encoding
- correlation review
- Lasso / Elastic Net modelling with `caret` and `glmnet`
- gradient boosting with `xgboost`
- prediction blending for submission output

## Data

The Kaggle data is not included in this repository.

Download the data from Kaggle and place it in a local `data/` folder:

```text
data/train.csv
data/test.csv
```

Dataset page:

<https://www.kaggle.com/competitions/house-prices-advanced-regression-techniques/data>

## How to run

Install the main R dependencies:

```r
install.packages(c(
  "data.table",
  "Matrix",
  "xgboost",
  "randomForest",
  "dplyr",
  "plyr",
  "ggplot2",
  "stringr",
  "caret",
  "scales",
  "psych",
  "corrplot",
  "ggrepel",
  "Ckmeans.1d.dp"
))
```

Then run:

```r
source("House Predictions.R")
```

## Repository structure

```text
.
├── House Predictions.R  # Main exploratory modelling script
├── README.md
└── .gitignore
```

## Notes

This is an exploratory Kaggle modelling project rather than a packaged application. The main script is intended to show the modelling process from data preparation through prediction generation.
