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


# =============================================
# Pool imputed results using Rubin's rules 
# =============================================

run_pooled <- function(outcome_var, matched_list) {
  
  ests <- numeric(length(matched_list))
  vars <- numeric(length(matched_list))
  
  for (i in seq_along(matched_list)) {
    mdf <- matched_list[[i]]
    fit <- glm(
      as.formula(paste(outcome_var, "~ GLP1_use")),
      data    = mdf,
      family  = quasibinomial,
      weights = weights
    )
    ct <- lmtest::coeftest(
      fit,
      vcov = sandwich::vcovCL(fit,
                              cluster = ~ hospital,
                              type    = "HC3")
    )
    ests[i] <- ct["GLP1_useYes", "Estimate"]
    vars[i] <- ct["GLP1_useYes", "Std. Error"]^2
  }
  
  # Event counts from dataset 1
  mdf1 <- matched_list[[1]]
  out  <- mdf1[[outcome_var]]
  glp1 <- mdf1$GLP1_use
  n_glp1  <- sum(glp1 == "Yes")
  n_ctrl  <- sum(glp1 == "No")
  ev_glp1 <- sum(out == 1 & glp1 == "Yes")
  ev_ctrl <- sum(out == 1 & glp1 == "No")
  
  res <- pool_rubin(ests, vars, outcome_var, "MI PS matching")
  res$glp1_n <- paste0(ev_glp1, "/", n_glp1)
  res$ctrl_n <- paste0(ev_ctrl, "/", n_ctrl)
  res
}

outcomes <- c("composite", "severity_bin", "mort90",
              "readm90", "critical_care_adm_bin",
              "local_complication")

results_mi <- do.call(rbind,
                      lapply(outcomes, run_pooled,
                             matched_list = matched_list))

cat("\n-----Pooled MI results-----\n")
print(results_mi, row.names = FALSE)

write.csv(results_mi, "pandora_results_mi.csv",
          row.names = FALSE)

