# House Prices — Advanced Regression Techniques in R

This repository contains an exploratory **R modelling script** for Kaggle's [House Prices: Advanced Regression Techniques](https://www.kaggle.com/competitions/house-prices-advanced-regression-techniques) competition.

It is an older learning project focused on feature engineering, exploratory analysis, and regression modelling for residential house-price prediction.

## Portfolio value

This is **not a headline portfolio project**. It is better used as a supporting/archival project showing earlier work with:

- R-based data exploration
- feature engineering for tabular data
- missing-value handling
- categorical encoding
- model training with `caret`
- regularised regression with `glmnet`
- gradient boosting with `xgboost`
- Kaggle-style prediction output

For a professional portfolio, this should sit below stronger projects like `TheSpreadsheet`, `agentic-data-scientist`, and real ETL/data engineering work.

## What the script does

`House Predictions.R` includes:

- loading Kaggle train/test datasets
- exploratory plots for sale price and feature relationships
- manual feature cleaning and ordinal encodings
- feature engineering, including:
  - total bathrooms
  - house age/remodelling indicators
  - porch indicators
  - combined quality score
  - combined square-footage feature
- log transformation of `SalePrice`
- dummy-variable encoding for categorical predictors
- model training using:
  - Lasso regression via `caret`/`glmnet`
  - XGBoost
  - Elastic Net regression
- blended prediction outputs for Kaggle submission

## Data

The Kaggle data is not included in this repository.

Download the data from Kaggle and place it here:

```text
data/train.csv
data/test.csv
```

Kaggle competition page:

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

Then run the script in R/RStudio:

```r
source("House Predictions.R")
```

## Current limitations

This is a legacy exploratory script, not a polished reproducible analysis pipeline.

Known limitations:

- the workflow is written as one long script rather than modular functions
- several transformations are manual and competition-specific
- there is no saved model object or reproducible report output
- no tests are included
- the script has not been runtime-tested during this cleanup because R is not currently installed in the local assistant environment
- some modelling choices would need review before using this as a serious modern portfolio piece

## Recommended next improvements

If this project were polished further, the best next steps would be:

1. convert the script into an R Markdown/Quarto report
2. separate data cleaning, feature engineering, modelling, and evaluation into sections or functions
3. add a clear validation metric table
4. save example plots under `outputs/`
5. document final Kaggle score if available
6. compare simple baseline, Lasso, Elastic Net, Random Forest, and XGBoost in one table
7. remove or justify fragile/manual imputations

## Honest positioning

Use this repo as evidence of earlier R/Kaggle practice, not as a flagship project. Its value is in showing learning history and feature-engineering practice; it should not be presented as production-quality modelling work.
