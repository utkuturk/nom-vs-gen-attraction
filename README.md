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

## Large Files & Exclusions
- The file `tur_tr_web_2015_1M.conllu` is excluded from git due to size (1.3GB).
- **Model Results**: The fitted Bayesian model files (`*.rds`) and other large intermediate model outputs are **not included** in this repository due to their size. The `paper.qmd` is configured to look for them in `data/models/`, but if they are missing, it may skip those blocks or require re-running the models.
