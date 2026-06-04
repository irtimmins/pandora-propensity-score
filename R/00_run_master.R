
# R packages and data preparation
# (Imputation takes ~60 mins).
source("R/01_setup.R")
source("R/02_data_preparation.R")
source("R/03_derive_outcome_variables.R")
source("R/04_run_multiple_imputation.R")

# Primary and sensitivity analysis, gather results
source("R/05_propensity_score_matching.R")
source("R/06_standard_regression.R")
source("R/07_doubly_robust_sensitivity_analysis.R")
source("R/08_results_odds_ratios.R")

# Results, tables, figures.
source("R/09_results_fishers_exact_mortality.R")
source("R/10_figure_love_plot.R")
source("R/11_table_1_cohort_with_matching.R")
source("R/12_table_2_glp1_characteristics.R")
source("R/13_table_3_lab_findings.R")
source("R/14_table_4_severity.R")
source("R/15_supp_table_aetiology.R")
