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
    # Exclude underweight -- positivity violation
    # No underweight GLP-1 users so PS cannot
    # estimate overlap for this group
    dplyr::filter(bmi_cat != "<18.5 Underweight" |
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
      family  = binomial,
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

########################################################
# Length of stay
########################################################
# Linear mixed effects pooled via Rubin's rules

los_data <- df %>%
  haven::zap_labels() %>%
  dplyr::select(id, los)

ests_los <- numeric(length(matched_list))
vars_los <- numeric(length(matched_list))

for (i in seq_along(matched_list)) {
  
  cat("LOS dataset", i, "... ")
  
  # Join LOS from original df -- do this first
  # before any other operations
  mdf_i <- matched_list[[i]] %>%
    haven::zap_labels() %>%
    dplyr::select(-any_of("los")) %>%
    dplyr::left_join(los_data, by = "id") %>%
    mutate(GLP1_use = factor(GLP1_use,
                             levels = c("No", "Yes")))
  
  # Check LOS joined correctly
  if (i == 1) {
    cat("\nLOS present:", "los" %in% names(mdf_i),
        "non-missing:", sum(!is.na(mdf_i$los)), "\n")
  }
  
  fit_i <- tryCatch({
    lmer(los ~ GLP1_use + (1 | hospital),
         data    = mdf_i,
         REML    = TRUE,
         control = lmerControl(
           optimizer = "bobyqa"))
  }, error = function(e) {
    cat("lmer failed:", conditionMessage(e), "\n")
    cat("trying lm ... ")
    lm(los ~ GLP1_use,
       data    = mdf_i,
       weights = weights)
  })
  
  co_i        <- summary(fit_i)$coefficients
  ests_los[i] <- co_i["GLP1_useYes", "Estimate"]
  vars_los[i] <- co_i["GLP1_useYes", "Std. Error"]^2
  
  cat("est =", round(ests_los[i], 2), "\n")
}
# Rubin's rules
m_los     <- length(ests_los)
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

# Descriptive statistics -- use same join approach as loop
mdf1_los <- matched_list[[1]] %>%
  haven::zap_labels() %>%
  dplyr::select(-any_of("los")) %>%
  dplyr::left_join(los_data, by = "id")

los_glp1 <- as.numeric(
  mdf1_los$los[mdf1_los$GLP1_use == "Yes"])
los_ctrl <- as.numeric(
  mdf1_los$los[mdf1_los$GLP1_use == "No"])

cat("LOS GLP-1 mean:", round(mean(los_glp1,
                                  na.rm = TRUE), 2), "\n")
cat("LOS Control mean:", round(mean(los_ctrl,
                                    na.rm = TRUE), 2), "\n")
los_glp1_desc <- paste0(
  formatC(mean(los_glp1, na.rm = TRUE),
          format = "f", digits = 1),
  " (",
  formatC(sd(los_glp1, na.rm = TRUE),
          format = "f", digits = 1),
  ")")

los_ctrl_desc <- paste0(
  formatC(mean(los_ctrl, na.rm = TRUE),
          format = "f", digits = 1),
  " (",
  formatC(sd(los_ctrl, na.rm = TRUE),
          format = "f", digits = 1),
  ")")

los_row <- data.frame(
  Outcome   = "Total length of stay (days)",
  GLP1_n    = los_glp1_desc,
  Ctrl_n    = los_ctrl_desc,
  Statistic = paste0(
    formatC(qbar_los,
            format = "f", digits = 1),
    " (",
    formatC(qbar_los - 1.96 * se_los,
            format = "f", digits = 1),
    " to ",
    formatC(qbar_los + 1.96 * se_los,
            format = "f", digits = 1),
    ")"),
  p         = fmt_p(p_los),
  stringsAsFactors = FALSE
)

#los_row
# Save pooled LOS result
saveRDS(
  data.frame(
    outcome  = "los",
    method   = "MI PS matching (lmer)",
    estimate = round(qbar_los, 3),
    lower    = round(qbar_los - 1.96 * se_los, 3),
    upper    = round(qbar_los + 1.96 * se_los, 3),
    p        = round(p_los, 3)
  ),
  "Results/los_mi.rds"
)

cat("\nLOS result saved\n")
