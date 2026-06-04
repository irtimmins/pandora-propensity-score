# =============================================
# 07 - Doubly robust sensitivity analysis
# use balancer weights approach
# balancer::cluster_weights, pooled over MI
# IPW-only and DR (with outcome regression)
# =============================================

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
    
    # Unadjusted (IPW)
    fit_unadj <- glm(
      as.formula(paste(outcome_var, "~ GLP1_use")),
      data    = df_i,
      family  = quasibinomial,
      weights = bw_weights
    )
    ct_u <- lmtest::coeftest(fit_unadj,
                             vcov = sandwich::vcovCL(fit_unadj,
                                                     cluster = ~ hospital, type = "HC3"))
    ests_unadj[i] <- ct_u["GLP1_useYes", "Estimate"]
    vars_unadj[i] <- ct_u["GLP1_useYes", "Std. Error"]^2
    
    # DR (weights + outcome regression)
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
                                                     cluster = ~ hospital, type = "HC3"))
    ests_adj[i] <- ct_a["GLP1_useYes", "Estimate"]
    vars_adj[i] <- ct_a["GLP1_useYes", "Std. Error"]^2
    
    cat("OR(IPW)=", round(exp(ests_unadj[i]), 3),
        "OR(DR)=",  round(exp(ests_adj[i]), 3), "\n")
  }
  
  rbind(
    pool_rubin(ests_unadj, vars_unadj, outcome_var,
               "Balancing weights (IPW)"),
    pool_rubin(ests_adj, vars_adj, outcome_var,
               "Balancing weights + regression (DR)")
  )
}

bw_outcomes <- c("composite", "severity_bin",
                 "mort90", "local_complication",
                 "critical_care_adm_bin")

# Convert outcomes to integer once to avoid haven issues
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

saveRDS(results_bw, "pandora_results_balancer.rds")