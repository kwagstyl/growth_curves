MRI growth curves
==============================


"Modelling Normative Growth in Paediatric MRI for the Mapping of Neurodevelopmental Abnormalities",
4th April 2024.

[**For research use only**]
   

A Keenlyside,

Dept. Medical Physics and Biomedical Engineering, UCL. 
andrew.keenlyside.23@ucl.ac.uk

Supervisor: K Wagstyl,

Great Ormond Street Institute of Child Health, UCL.

This project aims to model normal neurodevelopment and compare either individual scans or 
cohorts of scans to a control dataset to identify focal abnormalities (Code for normative modelling is within "growth_charts/scr/R Normative Modelling Scrips")
   
   
   

## Included R Model Scripts:
   

*function_extract_synthseg.R* - extracts BIDS formatted segmentation volumes into a R dataframe and creates a .csv dataset. 

*compare_individual_scan.R* - A structure to call the scripts for analysis a new volume dataset for an individual patient.

*compare_new_cohort.R* - A structure to call the scripts for analysis of a cohort of segmented patient volumes. 

*function_extract_z.R* - forms normative models and extracts z scores for each structure in a new set of patient volumes or cohort of new patients. Produces results-read outs for each and can form new clinical cohort datasets.

*growth_chartss.R* - Uses control data to build centile-based growth curves for 35 neuroantomical regions.

*create_z_distributions.R* - Builds cohort-wide distributions from 2 cohort datasets. Works best in raw and scan-normalised z scores.

*plot_longitudinal_z.R* - Plots 50th centile mean z score curves over age.

*compare_two_strucures.R* - Compares the same structure(s) for 2 cohorts over time. Can be used for both volume and z scores.

*multiple_testing_correction.R* - Applies Holm multiple testing correction to linear regression data.

*multivariate_logistic_regression.R* - Trains a LASSO weighted multivariate logistic regression model for detection of a pathology using every available structure. 

*ROC_logistic_regression.R* - Carries out structure-by-structre linear regressions between two cohorts and produces and ROC curve for selected structures or to compare data types.

*multigroup_descrimination_analysis.R* - Trains a MGDA model to descriminate between 2 or more pathological groups vs control. Creates probability map based on 2 key vaiables. Best used for comparing diseases with variants by hemisphere.




## Notes:

Please check the formatting of segmented cohorts (BIDS formatting).
These scripts produce various files, please create a new folder as a destination before use.


Project Organization
------------

    ├── LICENSE
    ├── README.md          <- The top-level README for developers using this project.
    │   
    ├── R Normative Modelling Scripts               <- All code relevant to normative modelling 
    │   │  
    │   ├── Growth Curves     <- Scripts to extract volumes and build centile-based growth curves
    │   │   ├── function_extract_synthseg.R 
    │   │   ├── growth_charts.R
    │   │   └── plot_logitudinal.R
    │   │   
    │   │  
    │   ├── Formatting     <- Scripts for formatting datasets
    │   │   └── modify_ATP1A3_for_timepoints.R
    │   │   
    │   │  
    │   ├── Individual Analysis     <- Scripts to analyse 1 new scan           
    │   │   ├── compare_individual_scan.R
    │   │   ├── extract_z_scores.R
    │   │   ├── z_barplot.R
    │   │   ├── make_gif.R
    │   │   └── custom_MRI_mask.R
    │   │  
    │   │  
    │   ├── Cohort Analysis     <- Scripts to analysed a clinical cohort of new scans (>20)
    │   │   │           
    │   │   ├── create_z_datasets.R
    │   │   ├── create_z_distributions.R
    │   │   ├── compare_two_structures.R
    │   │   ├── ROC_logistic_regression.R
    │   │   ├── multiple_test_correction.R
    │   │   ├── multivariate_logistic_regression_ATP1A3.R
    │   │   └── multigroup_discrimination_analysis_HS.R
    │
    └── R Normative Model Flowchart.pdf   <- schematic of code interactions 


--------

Control and limited clinical cohort data may be made available at a later date.
