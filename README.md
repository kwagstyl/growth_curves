MRI growth curves
==============================

Neurodevelopment Growth Charts Project (v 1)

"Modelling Normative Growth in Paediatric MRI for the Mapping of Neurodevelopmental Abnormalities",
4th April 2024.

[**For research use only**]

A Keenlyside,
Dept. Medical Physics and Biomedical Engineering, UCL. 
andrew.keenlyside.23@ucl.ac.uk

Supervisor: K Wagstyl,
Great Ormond Street Institute of Child Health, UCL.

This project aims to model normal neurodevelopment and compare either individual scans or 
cohorts of scans to a control dataset to identify focal abnormalities.

**Code for normative modelling is within growth_charts/scr/R Normative Modelling Scrips**




Included R Model Scripts:


function_extract_synthseg.R - extracts BIDS formatted segmentation volumes into a R dataframe and       creates a .csv dataset. 

compare_individual_scan.R - A structure to call the scripts for analysis a new volume dataset for an individual patient.

compare_new_cohort.R - A structure to call the scripts for analysis of a cohort of segmented patient volumes. 

function_extract_z.R - forms normative models and extracts z scores for each structure in a new set of patient volumes or cohort of new patients. Produces results-read outs for each and can form new clinical cohort datasets.

growth_chartss.R - Uses control data to build centile-based growth curves for 35 neuroantomical regions.

create_z_distributions.R - Builds cohort-wide distributions from 2 cohort datasets. Works best in raw and scan-normalised z scores.

plot_longitudinal_z.R - Plots 50th centile mean z score curves over age.

compare_two_strucures.R - Compares the same structure(s) for 2 cohorts over time. Can be used for both volume and z scores.

multiple_testing_correction.R - Applies Holm multiple testing correction to linear regression data.

multivariate_logistic_regression.R - Trains a LASSO weighted multivariate logistic regression model for detection of a pathology using every available structure. 

ROC_logistic_regression.R - Carries out structure-by-structre linear regressions between two cohorts and produces and ROC curve for selected structures or to compare data types.

multigroup_descrimination_analysis.R - Trains a MGDA model to descriminate between 2 or more pathological groups vs control. Creates probability map based on 2 key vaiables. Best used for comparing diseases with variants by hemisphere.




Notes:

Please check the formatting of segmented cohorts (BIDS formatting).
These scripts produce various files, please create a new folder as a destination.

Project Organization
------------

    ├── LICENSE
    ├── Makefile           <- Makefile with commands like `make data` or `make train`
    ├── README.md          <- The top-level README for developers using this project.
    ├── data
    │   ├── external       <- Data from third party sources.
    │   ├── interim        <- Intermediate data that has been transformed.
    │   ├── processed      <- The final, canonical data sets for modeling.
    │   └── raw            <- The original, immutable data dump.
    │
    ├── docs               <- A default Sphinx project; see sphinx-doc.org for details
    │
    ├── models             <- Trained and serialized models, model predictions, or model summaries
    │
    ├── notebooks          <- Jupyter notebooks. Naming convention is a number (for ordering),
    │                         the creator's initials, and a short `-` delimited description, e.g.
    │                         `1.0-jqp-initial-data-exploration`.
    │
    ├── references         <- Data dictionaries, manuals, and all other explanatory materials.
    │
    ├── reports            <- Generated analysis as HTML, PDF, LaTeX, etc.
    │   └── figures        <- Generated graphics and figures to be used in reporting
    │
    ├── requirements.txt   <- The requirements file for reproducing the analysis environment, e.g.
    │                         generated with `pip freeze > requirements.txt`
    │
    ├── setup.py           <- makes project pip installable (pip install -e .) so src can be imported
    ├── src                <- Source code for use in this project.
    │   ├── __init__.py    <- Makes src a Python module
    │   │
    │   ├── data           <- Scripts to download or generate data
    │   │   └── make_dataset.py
    │   │
    │   ├── features       <- Scripts to turn raw data into features for modeling
    │   │   └── build_features.py
    │   │
    │   ├── models         <- Scripts to train models and then use trained models to make
    │   │   │                 predictions
    │   │   ├── predict_model.py
    │   │   └── train_model.py
    │   │
    │   └── visualization  <- Scripts to create exploratory and results oriented visualizations
    │   │  └── visualize.py
    │   └── R              <- R scripts
    │
    └── tox.ini            <- tox file with settings for running tox; see tox.readthedocs.io


--------

<p><small>Project based on the <a target="_blank" href="https://drivendata.github.io/cookiecutter-data-science/">cookiecutter data science project template</a>. #cookiecutterdatascience</small></p>
