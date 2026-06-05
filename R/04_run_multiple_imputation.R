# =============================================
# 04 - Perform multiple imputation
# m=20
# =============================================

# Core variables for covariate adjustment
ps_vars <- c("GLP1_use", "gender", "age_cat",
             "bmi_cat", "cci_cat", "smoking",
             "alcohol_cat", "prev_pancreatitis",
             "gallstones_imaging")

# Include further (auxiliary) variables to improve imputation model
aux_vars <- c("crp", "los", "diabetes",
              "resp_failure", "cvs_failure",
              "renal_failure", "referral")

outcome_vars <- c("composite", "severity_bin", "mort90",
                  "readm90", "critical_care_adm_bin",
                  "local_complication", "hospital")

df_mice <- df %>%
  dplyr::select(id,
                all_of(ps_vars),
                all_of(aux_vars),
                all_of(outcome_vars)) %>%
  haven::zap_labels()    %>%
# Ensure bmi_cat has correct levels including underweight
  mutate(
  bmi_cat = factor(bmi_cat,
                   levels = c("<18.5 Underweight",
                              "18.5-24.9 Normal",
                              "25-29.9 Overweight",
                              "30-34.9 Class 1 Obesity",
                              "35-39.9 Class 2 Obesity",
                              ">40 Class 3 Obesity"))
)

cat("Missing per variable:\n")
print(sapply(df_mice, function(x) sum(is.na(x))))

# ----- Imputation methods -----
# Clean method assignment -- only variables with missing data
imp_methods <- make.method(df_mice)

imp_methods["bmi_cat"]     <- "polr"
imp_methods["smoking"]     <- "polr"
imp_methods["alcohol_cat"] <- "polr"
imp_methods["crp"]         <- "pmm"
imp_methods["los"]         <- "pmm"

# Confirm nothing else assigned
cat("Imputation methods assigned:\n")
print(imp_methods[imp_methods != ""])

# ----- Predictor matrix -----
ini  <- mice(df_mice, maxit = 0, printFlag = FALSE)
pred <- ini$predictorMatrix
pred[, "id"]       <- 0
pred["id", ]       <- 0
pred["hospital", ] <- 0

# ----- Run using mice -----
set.seed(12510)
imp <- mice(
  df_mice,
  m               = 20,
  method          = imp_methods,
  predictorMatrix = pred,
  maxit           = 20,
  printFlag       = TRUE
)

# -----Check it's worked -----
cat("\nLogged events:", nrow(imp$loggedEvents), "\n")

cat("\nObserved BMI:\n")
print(table(df_mice$bmi_cat, useNA = "always"))
cat("Imputed BMI dataset 1:\n")
print(table(mice::complete(imp, 1)$bmi_cat))

plot(imp, c("bmi_cat", "smoking", "alcohol_cat", "crp"))

# -----Save imputed datasets -----
saveRDS(imp, "Data/pandora_imp.rds")
cat("\nSaved pandora_imp.rds\n")

#imp <- readRDS("Data/pandora_imp.rds")
