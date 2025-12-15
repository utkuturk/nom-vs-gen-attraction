# Nominal vs Genitive Attraction

This repository contains data, scripts, and analysis files for the Nominal vs Genitive Attraction experiment.

## Directory Structure

- **`paper/`**: Contains the main manuscript and analysis.
    - `paper.qmd`: JML-style manuscript written in Quarto (using `apaquarto`). Includes full text, inline R analysis, and dynamic plots.
- **`data/`**: Contains all data files.
    - `experiment/`: Raw experimental data and results.
    - `attention/`: Data related to attention analysis.
    - `models/`: RDS files for trained models (Note: Large model files may be excluded from the repo).
- **`scripts/`**: Scripts for data processing and experimentation.
    - `experiment/`: R scripts for the experiment.
    - `attention/`: Python notebooks for attention analysis.
- **`figures/`**: Generated plots and figures.
- **`doc/`**: Documentation and references.

## How to Run
The main analysis and manuscript generation are handled by `paper/paper.qmd`. 
To render the full paper (PDF):
```bash
quarto render paper/paper.qmd
```
Ensure you have the `apaquarto` extension installed (included in `paper/_extensions`).

## Modular Analysis Scripts
For running the analysis without rendering the entire Quarto document, we have extracted the R code chunks into individual scripts located in `paper/paper_chunks/`.
These scripts should be run in the following order:

1.  **`001_setup.R`**: Loads required packages (`tidyverse`, `brms`, etc.).
2.  **`002_attention_plot.R`**: Generates the BERT attention difference plot.
3.  **`003_load_data.R`**: Loads and cleans the experimental data (including filler accuracy calculation).
4.  **`004_descriptive_stats_calcs.R`**: Calculates means and standard errors for the behavioral data.
5.  **`005_descriptive_plot.R`**: Generates the descriptive results plot.
6.  **`006_bayesian_model_setup.R`**: Configures and fits the Bayesian GLMM (or loads existing models).
7.  **`007_coef_plot.R`**: Extracts coefficients and plots the posterior distributions.

## Large Files & Exclusions
- The file `tur_tr_web_2015_1M.conllu` is excluded from git due to size (1.3GB).
- **Model Results**: The fitted Bayesian model files (`*.rds`) and other large intermediate model outputs are **not included** in this repository due to their size. The `paper.qmd` is configured to look for them in `data/models/`, but if they are missing, it may skip those blocks or require re-running the models.
