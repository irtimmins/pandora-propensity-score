# =============================================
# 04 - Perform multiple imputation
# m=20, auxiliary variables
# =============================================

ps_vars <- c("GLP1_use", "gender", "age_cat",
             "bmi_cat", "cci_cat", "smoking",
             "alcohol_cat", "prev_pancreatitis",
             "gallstones_imaging")

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
  haven::zap_labels()

cat("Missing per variable:\n")
print(sapply(df_mice, function(x) sum(is.na(x))))

# -----METHODS-----
imp_methods <- make.method(df_mice)
imp_methods["bmi_cat"]       <- "polr"
imp_methods["smoking"]       <- "polr"
imp_methods["alcohol_cat"]   <- "polr"
imp_methods["crp"]           <- "pmm"
imp_methods["los"]           <- "pmm"
imp_methods["diabetes"]      <- "polyreg"
imp_methods["resp_failure"]  <- "polr"
imp_methods["cvs_failure"]   <- "polr"
imp_methods["renal_failure"] <- "polr"

cat("\nMethods assigned:\n")
print(imp_methods[imp_methods != ""])

# -----PREDICTOR MATRIX-----
ini  <- mice(df_mice, maxit = 0, printFlag = FALSE)
pred <- ini$predictorMatrix
pred[, "id"]       <- 0
pred["id", ]       <- 0
pred["hospital", ] <- 0

# -----RUN-----
set.seed(12510)
imp <- mice(
  df_mice,
  m               = 20,
  method          = imp_methods,
  predictorMatrix = pred,
  maxit           = 20,
  printFlag       = FALSE
)

# -----DIAGNOSTICS-----
cat("\nLogged events:", nrow(imp$loggedEvents), "\n")

cat("\nObserved BMI:\n")
print(table(df_mice$bmi_cat, useNA = "always"))
cat("Imputed BMI dataset 1:\n")
print(table(mice::complete(imp, 1)$bmi_cat))

plot(imp, c("bmi_cat", "smoking", "alcohol_cat", "crp"))

saveRDS(imp, "pandora_imp.rds")
cat("\nSaved pandora_imp.rds\n")




