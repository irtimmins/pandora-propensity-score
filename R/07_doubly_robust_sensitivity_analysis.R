# =============================================
# 07 - Doubly robust sensitivity analysis
# Balancer weights approach
# Binary outcomes + LOS
# Pooled over multiple imputed datasets
# =============================================

# -----BINARY OUTCOMES-----

run_balancer_weights <- function(outcome_var, imp,
                                 n_imp = 20) {
  
  ests_unadj <- numeric(n_imp)
  vars_unadj <- numeric(n_imp)
  ests_adj   <- numeric(n_imp)
  vars_adj   <- numeric(n_imp)
  
  for (i in 1:n_imp) {
    
    cat("Balancer dataset", i,
        "outcome:", outcome_var, "... ")
    
    df_i <- mice::complete(imp, i) %>%
      haven::zap_labels() %>%
      dplyr::filter(
        bmi_cat != "<18.5 Underweight" |
          is.na(bmi_cat)) %>%
      mutate(
        bmi_cat = factor(bmi_cat,
                         levels = c("18.5-24.9 Normal",
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
    
    trt <- as.integer(df_i$GLP1_use == "Yes")
    
    ind_covs <- model.matrix(
      ~ gender + age_cat + bmi_cat + cci_cat +
        smoking_cat + alcohol_cat2 +
        prev_pancreatitis + gallstones_imaging - 1,
      data = df_i
    ) %>% scale()
    
    hosp_summary <- df_i %>%
      dplyr::group_by(hospital) %>%
      dplyr::summarise(
        hosp_volume    = n(),
        hosp_glp1_rate = mean(as.integer(
          GLP1_use == "Yes")),
        .groups = "drop"
      )
    df_i <- df_i %>%
      dplyr::left_join(hosp_summary,
                       by = "hospital")
    
    clus_covs <- model.matrix(
      ~ hosp_volume + hosp_glp1_rate - 1,
      data = df_i
    ) %>% scale()
    
    clusters <- as.integer(factor(df_i$hospital))
    
    out <- cluster_weights(
      ind_covs  = ind_covs,
      clus_covs = clus_covs,
      trt       = trt,
      clusters  = clusters,
      lambda    = 0,
      verbose   = FALSE
    )
    
    df_i$bw_weights <- pmax(out$weights, 0)
    df_i$bw_weights[trt == 1] <- 1
    
    # IPW only
    fit_unadj <- glm(
      as.formula(paste(outcome_var,
                       "~ GLP1_use")),
      data    = df_i,
      family  = quasibinomial,
      weights = bw_weights
    )
    ct_u <- lmtest::coeftest(fit_unadj,
                             vcov = sandwich::vcovCL(fit_unadj,
                                                     cluster = ~ hospital,
                                                     type    = "HC3"))
    ests_unadj[i] <- ct_u["GLP1_useYes",
                          "Estimate"]
    vars_unadj[i] <- ct_u["GLP1_useYes",
                          "Std. Error"]^2
    
    # DR: weights + outcome regression
    fit_adj <- glm(
      as.formula(paste(outcome_var,
                       "~ GLP1_use + age_cat + bmi_cat +
                   cci_cat + gender + smoking_cat +
                   alcohol_cat2 + prev_pancreatitis +
                   gallstones_imaging")),
      data    = df_i,
      family  = quasibinomial,
      weights = bw_weights
    )
    ct_a <- lmtest::coeftest(fit_adj,
                             vcov = sandwich::vcovCL(fit_adj,
                                                     cluster = ~ hospital,
                                                     type    = "HC3"))
    ests_adj[i] <- ct_a["GLP1_useYes", "Estimate"]
    vars_adj[i] <- ct_a["GLP1_useYes",
                        "Std. Error"]^2
    
    cat("OR(IPW)=", round(exp(ests_unadj[i]), 3),
        "OR(DR)=",  round(exp(ests_adj[i]), 3),
        "\n")
  }
  
  rbind(
    pool_rubin(ests_unadj, vars_unadj,
               outcome_var,
               "Balancing weights (IPW)"),
    pool_rubin(ests_adj, vars_adj,
               outcome_var,
               "Balancing weights + regression (DR)")
  )
}

bw_outcomes <- c("composite", "severity_bin",
                 "mort90", "local_complication",
                 "critical_care_adm_bin",
                 "readm90")

for (i in 1:imp$m) {
  imp$data <- imp$data %>% haven::zap_labels()
}

results_bw <- do.call(rbind,
                      lapply(bw_outcomes,
                             run_balancer_weights,
                             imp   = imp,
                             n_imp = imp$m))

cat("\n-----Balancing weights results-----\n")
print(results_bw, row.names = FALSE)

saveRDS(
  results_bw %>%
    dplyr::filter(method ==
                    "Balancing weights + regression (DR)"),
  "Results/doubly_robust_OR_mi.rds")

# -----LOS: DOUBLY ROBUST-----

cat("\n-----LOS: doubly robust-----\n")

los_data <- df %>%
  haven::zap_labels() %>%
  dplyr::select(id, los)

ests_dr_los <- numeric(imp$m)
vars_dr_los <- numeric(imp$m)

for (i in 1:imp$m) {
  
  cat("DR LOS dataset", i, "... ")
  
  df_i <- mice::complete(imp, i) %>%
    haven::zap_labels() %>%
    dplyr::filter(
      bmi_cat != "<18.5 Underweight" |
        is.na(bmi_cat)) %>%
    dplyr::select(-any_of("los")) %>%
    dplyr::left_join(los_data, by = "id") %>%
    mutate(
      bmi_cat = factor(bmi_cat,
                       levels = c("18.5-24.9 Normal",
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
  
  trt <- as.integer(df_i$GLP1_use == "Yes")
  
  ind_covs <- model.matrix(
    ~ gender + age_cat + bmi_cat + cci_cat +
      smoking_cat + alcohol_cat2 +
      prev_pancreatitis + gallstones_imaging - 1,
    data = df_i
  ) %>% scale()
  
  hosp_summary <- df_i %>%
    dplyr::group_by(hospital) %>%
    dplyr::summarise(
      hosp_volume    = n(),
      hosp_glp1_rate = mean(as.integer(
        GLP1_use == "Yes")),
      .groups = "drop"
    )
  df_i <- df_i %>%
    dplyr::left_join(hosp_summary, by = "hospital")
  
  clus_covs <- model.matrix(
    ~ hosp_volume + hosp_glp1_rate - 1,
    data = df_i
  ) %>% scale()
  
  clusters <- as.integer(factor(df_i$hospital))
  
  out <- cluster_weights(
    ind_covs  = ind_covs,
    clus_covs = clus_covs,
    trt       = trt,
    clusters  = clusters,
    lambda    = 0,
    verbose   = FALSE
  )
  
  df_i$bw_weights <- pmax(out$weights, 0)
  df_i$bw_weights[trt == 1] <- 1
  
  # DR linear model for LOS
  fit_los <- lm(
    los ~ GLP1_use + age_cat + bmi_cat +
      cci_cat + gender + smoking_cat +
      alcohol_cat2 + prev_pancreatitis +
      gallstones_imaging,
    data    = df_i,
    weights = bw_weights
  )
  
  ct <- lmtest::coeftest(
    fit_los,
    vcov = sandwich::vcovCL(fit_los,
                            cluster = ~ hospital,
                            type    = "HC3")
  )
  
  ests_dr_los[i] <- ct["GLP1_useYes", "Estimate"]
  vars_dr_los[i] <- ct["GLP1_useYes",
                       "Std. Error"]^2
  
  cat("est =", round(ests_dr_los[i], 1), "\n")
}

dr_los_pooled <- pool_rubin(
  ests_dr_los, vars_dr_los,
  "los",
  "Balancing weights + regression (DR)")

cat("\nDR LOS result:\n")
print(dr_los_pooled)

results_los_dr <- data.frame(
  outcome  = "los",
  method   = "Balancing weights + regression (DR)",
  estimate = round(dr_los_pooled$OR, 3),
  lower    = round(dr_los_pooled$lower, 3),
  upper    = round(dr_los_pooled$upper, 3),
  p        = round(dr_los_pooled$p, 3)
)

saveRDS(results_los_dr,
        "Results/los_doubly_robust_mi.rds")

cat("\nSaved Results/doubly_robust_OR_mi.rds\n")
cat("Saved Results/los_doubly_robust_mi.rds\n")