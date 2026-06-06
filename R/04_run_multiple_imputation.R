# =============================================
# 04 - Multiple imputation
# m = 20, predictive mean matching for continuous
# variables and proportional odds for ordered
# categoricals
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
  dplyr::select(id, all_of(ps_vars),
                all_of(aux_vars),
                all_of(outcome_vars))

cat("Missing per variable:\n")
print(sapply(df_mice, function(x) sum(is.na(x))))

# Only variables have missing data and need a
# method, pmm is the default. 
# mice leaves complete variables untouched.
# change to polr method for ordered categoricals.
imp_methods <- make.method(df_mice)
imp_methods["bmi_cat"]     <- "polr"
imp_methods["smoking"]     <- "polr"
imp_methods["alcohol_cat"] <- "polr"

# id and hospital are kept in the data so they carry
# through to the matched datasets, but neither should
# drive the imputation: id is just a row label, and
# hospital (103 levels) destabilises the per-variable
# models. quickpred with these excluded handles this
# in one step.
pred <- quickpred(df_mice, exclude = c("id", "hospital"))

# Underweight patients violate positivity (no
# underweight GLP-1 users) so they are excluded from
# the propensity analysis. ignore = TRUE imputes them
# but leaves them out of the imputation models, so all
# datasets keep identical row counts.
ignore_vec <- df_mice$bmi_cat == "<18.5 Underweight" &
  !is.na(df_mice$bmi_cat)

cat("Underweight patients removed:", sum(ignore_vec), "\n")

set.seed(12510)
imp <- mice(df_mice,
            m               = 3,
            method          = imp_methods,
            predictorMatrix = pred,
            maxit           = 5,
            ignore          = ignore_vec,
            printFlag       = TRUE)

saveRDS(imp, "Data/pandora_imp.rds")
cat("Saved pandora_imp.rds\n")