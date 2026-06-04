
# Data preparation
source("01_setup.R")
source("02_data_prep.R")
source("03_derive_outcome_variables.R")
source("03_analytic_sample.R")

# Primary analysis
source("04_ps_matching_cc.R")
source("05_outcomes_cc.R")

# Sensitivity analyses
source("06_imputation.R")
source("07_ps_matching_mi.R")
source("08_outcomes_mi.R")
source("09_balancing_weights.R")
source("10_regression_full.R")
source("11_evalues_mortality.R")

# Tables
source("12_table1.R")
source("13_loveplots.R")
source("14_results_summary.R")
source("15_atlanta_validation.R")
source("16_aetiology.R")
source("17_glp1_characteristics.R")
source("18_admission_bloods.R")
source("19_organ_failure_detail.R")
source("20_validation_kappa.R")