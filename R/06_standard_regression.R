# =============================================
# 06 - Full cohort regression
# Multilevel logistic regression for binary
# Multilevel linear regression for LOS
# Hospital random effect
# Pooled across MI datasets via Rubin's rules
# =============================================

# -----BINARY OUTCOMES: MULTILEVEL LOGISTIC-----

run_melogit_mi <- function(outcome_var, imp,
                           n_imp = 20) {
  
  ests <- numeric(n_imp)
  vars <- numeric(n_imp)
  
  for (i in 1:n_imp) {
    
    cat("Regression dataset", i,
        "outcome:", outcome_var, "... ")
    
    df_i <- mice::complete(imp, i) %>%
      haven::zap_labels() %>%
      mutate(
        bmi_cat = factor(bmi_cat,
                         levels = c("18.5-24.9 Normal",
                                    "<18.5 Underweight",
                                    "25-29.9 Overweight",
                                    "30-34.9 Class 1 Obesity",
                                    "35-39.9 Class 2 Obesity",
                                    ">40 Class 3 Obesity")),
        smoking_cat = factor(smoking,
                             levels = c("Never smoker",
                                        "Current Smoker",
                                        "Ex-smoker")),
        alcohol_cat2 = factor(alcohol_cat,
                              levels = c("None", "1-14",
                                         "15-35", ">35")),
        GLP1_use = factor(GLP1_use,
                          levels = c("No", "Yes"))
      )
    
    fit <- tryCatch({
      glmer(
        as.formula(paste(outcome_var,
                         "~ GLP1_use + gender + age_cat +
                       bmi_cat + cci_cat + smoking_cat +
                       alcohol_cat2 + prev_pancreatitis +
                       gallstones_imaging +
                       (1 | hospital)")),
        data    = df_i,
        family  = binomial,
        control = glmerControl(
          optimizer = "bobyqa",
          optCtrl   = list(maxfun = 2e5)
        )
      )
    }, error = function(e) {
      cat("bobyqa failed, trying nloptwrap ... ")
      glmer(
        as.formula(paste(outcome_var,
                         "~ GLP1_use + gender + age_cat +
                       bmi_cat + cci_cat + smoking_cat +
                       alcohol_cat2 + prev_pancreatitis +
                       gallstones_imaging +
                       (1 | hospital)")),
        data    = df_i,
        family  = binomial,
        control = glmerControl(
          optimizer = "nloptwrap"
        )
      )
    })
    
    co      <- summary(fit)$coefficients
    ests[i] <- co["GLP1_useYes", "Estimate"]
    vars[i] <- co["GLP1_useYes", "Std. Error"]^2
    
    cat("OR =", round(exp(ests[i]), 3), "\n")
  }
  
  m     <- n_imp
  qbar  <- mean(ests)
  ubar  <- mean(vars)
  b     <- var(ests)
  t_var <- ubar + (1 + 1/m) * b
  se    <- sqrt(t_var)
  re_var <- as.data.frame(VarCorr(fit))$vcov[1]
  
  data.frame(
    outcome = outcome_var,
    method  = "MI melogit (hospital RE)",
    OR      = round(exp(qbar), 3),
    lower   = round(exp(qbar - 1.96 * se), 3),
    upper   = round(exp(qbar + 1.96 * se), 3),
    p       = round(2 * (1 - pnorm(
      abs(qbar/se))), 3),
    re_var  = round(re_var, 3)
  )
}

reg_outcomes <- c("composite", "severity_bin",
                  "mort90", "local_complication",
                  "critical_care_adm_bin",
                  "readm90")

cat("\n-----Binary outcomes: melogit-----\n")
cat("Pooling", imp$m,
    "imputed datasets via Rubin's rules\n\n")

results_reg_mi <- do.call(rbind,
                          lapply(reg_outcomes,
                                 run_melogit_mi,
                                 imp   = imp,
                                 n_imp = imp$m))

cat("\n-----MI regression results-----\n")
print(results_reg_mi, row.names = FALSE)

saveRDS(results_reg_mi,
        "Results/standard_regression_OR_mi.rds")
write.csv(results_reg_mi,
          "Results/standard_regression_OR_mi.csv",
          row.names = FALSE)

# -----LENGTH OF STAY: MULTILEVEL LINEAR-----

cat("\n-----LOS: multilevel linear regression-----\n")
cat("Pooling", imp$m,
    "imputed datasets via Rubin's rules\n\n")

los_data <- df %>%
  haven::zap_labels() %>%
  dplyr::select(id, los)

ests_los <- numeric(imp$m)
vars_los <- numeric(imp$m)

for (i in 1:imp$m) {
  
  cat("LOS regression dataset", i, "... ")
  
  df_i <- mice::complete(imp, i) %>%
    haven::zap_labels() %>%
    dplyr::select(-any_of("los")) %>%
    dplyr::left_join(los_data, by = "id") %>%
    mutate(
      bmi_cat = factor(bmi_cat,
                       levels = c("18.5-24.9 Normal",
                                  "<18.5 Underweight",
                                  "25-29.9 Overweight",
                                  "30-34.9 Class 1 Obesity",
                                  "35-39.9 Class 2 Obesity",
                                  ">40 Class 3 Obesity")),
      smoking_cat = factor(smoking,
                           levels = c("Never smoker",
                                      "Current Smoker",
                                      "Ex-smoker")),
      alcohol_cat2 = factor(alcohol_cat,
                            levels = c("None", "1-14",
                                       "15-35", ">35")),
      GLP1_use = factor(GLP1_use,
                        levels = c("No", "Yes"))
    )
  
  fit_los <- tryCatch({
    lmer(
      los ~ GLP1_use + gender + age_cat +
        bmi_cat + cci_cat + smoking_cat +
        alcohol_cat2 + prev_pancreatitis +
        gallstones_imaging +
        (1 | hospital),
      data    = df_i,
      REML    = TRUE,
      control = lmerControl(
        optimizer = "bobyqa")
    )
  }, error = function(e) {
    cat("lmer failed, using lm ... ")
    lm(los ~ GLP1_use + gender + age_cat +
         bmi_cat + cci_cat + smoking_cat +
         alcohol_cat2 + prev_pancreatitis +
         gallstones_imaging,
       data = df_i)
  })
  
  co_los      <- summary(fit_los)$coefficients
  ests_los[i] <- co_los["GLP1_useYes", "Estimate"]
  vars_los[i] <- co_los["GLP1_useYes",
                        "Std. Error"]^2
  
  cat("est =", round(ests_los[i], 2), "\n")
}

m_los     <- imp$m
qbar_los  <- mean(ests_los)
ubar_los  <- mean(vars_los)
b_los     <- var(ests_los)
tvar_los  <- ubar_los + (1 + 1/m_los) * b_los
se_los    <- sqrt(tvar_los)
p_los     <- 2 * (1 - pnorm(abs(qbar_los / se_los)))

cat("\nPooled LOS estimate:",
    round(qbar_los, 2),
    "SE:", round(se_los, 2),
    "p:", round(p_los, 3), "\n")

results_los_reg <- data.frame(
  outcome  = "los",
  method   = "MI lmer (hospital RE)",
  estimate = round(qbar_los, 3),
  lower    = round(qbar_los - 1.96 * se_los, 3),
  upper    = round(qbar_los + 1.96 * se_los, 3),
  p        = round(p_los, 3)
)

saveRDS(results_los_reg,
        "Results/los_regression_mi.rds")

cat("\nSaved Results/los_regression_mi.rds\n")
cat("Saved Results/standard_regression_OR_mi.csv\n")


