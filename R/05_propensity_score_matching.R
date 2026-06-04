# =============================================
# 05 - Propensity Score Matching
# Match within each imputed dataset
# =============================================

matchit_list <- vector("list", imp$m)
matched_list <- vector("list", imp$m)

for (i in 1:imp$m) {
  
  cat("Matching imputed dataset", i, "... ")
  
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
  
  m_i <- matchit(
    GLP1_use ~ gender + age_cat + bmi_cat + cci_cat +
      smoking_cat + alcohol_cat2 +
      prev_pancreatitis + gallstones_imaging,
    data        = df_i,
    method      = "nearest",
    distance    = "glm",
    link        = "logit",
    ratio       = 3,
    replace     = FALSE,
    caliper     = 0.1,
    std.caliper = TRUE,
    m.order     = "random"
  )
  
  matchit_list[[i]] <- m_i
  matched_list[[i]] <- match.data(m_i)
  
  cat("N=", nrow(matched_list[[i]]),
      "GLP1=", sum(matched_list[[i]]$GLP1_use == "Yes"),
      "Controls=", sum(matched_list[[i]]$GLP1_use == "No"),
      "GLP1 deaths=",
      sum(matched_list[[i]]$mort90 == 1 &
            matched_list[[i]]$GLP1_use == "Yes"),
      "\n")
}

saveRDS(matchit_list, "Data/pandora_matchit_list.rds")
saveRDS(matched_list, "Data/pandora_matched_list.rds")
cat("\nMI matching complete\n")

# =============================================
# Pool results
# apply Rubin's rules over matched imputed datasets
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
saveRDS(results_mi, "Results/propensity_score_OR_mi.rds")
write.csv(results_mi, "Results/propensity_score_OR_mi.csv",
          row.names = FALSE)

