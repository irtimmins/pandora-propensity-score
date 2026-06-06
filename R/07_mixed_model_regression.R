# =============================================
# 06 - Full cohort regression
# Multilevel models with hospital random effect,
# pooled across imputed datasets with mice::pool().
# Binary outcomes use logistic regression, length
# of stay uses linear regression.
# =============================================

# ps_vars is previously defined and includes GLP1_use
# plus all covariates. Strip GLP1_use out to get just
# the adjustment variables, then build the right-hand side
# of the model formula as a single string. 

covars <- setdiff(ps_vars, "GLP1_use")

# The hospital random intercept (1 | hospital) 
# accounts for clustering of patients within hospitals.

rhs <- paste(c("GLP1_use", covars,
               "(1 | hospital)"),
             collapse = " + ")

cat("Right-hand side of all models:\n", rhs, "\n\n")

reg_outcomes <- c("composite", "severity_bin",
                  "mort90", "local_complication",
                  "critical_care_adm_bin", "readm90")

results_reg_mi <- data.frame()

for (outcome in reg_outcomes) {
  
  # with() runs the glmer call in every imputed dataset
  # and returns a list of fitted models. pool() then
  # combines the estimates using Rubin's rules.
  # exponentiate = TRUE converts log-odds to odds ratios.
  
  cat("Fitting:", outcome, "...")
  t0 <- proc.time()["elapsed"]
  
  fit <- with(imp,
              glmer(as.formula(paste(outcome, "~", rhs)),
                    family  = binomial,
                    control = glmerControl(
                      optimizer = "bobyqa",
                      optCtrl   = list(maxfun = 2e5))))
  
  est <- summary(pool(fit),
                 conf.int     = TRUE,
                 exponentiate = TRUE)
  est <- est[est$term == "GLP1_useYes", ]
  est$outcome <- outcome
  
  results_reg_mi <- rbind(results_reg_mi, est)
  
  elapsed <- round(proc.time()["elapsed"] - t0, 1)
  cat(" done (", elapsed, "s)",
      " OR =", round(est$estimate, 2),
      " p =",  round(est$p.value, 3), "\n")
}

cat("\nBinary regression results:\n")
print(results_reg_mi[, c("outcome", "estimate",
                         "conf.low", "conf.high",
                         "p.value")],
      row.names = FALSE)

saveRDS(results_reg_mi,
        "Results/standard_regression_OR_mi.rds")

# Length of stay uses the same formula and pooling
# approach but with lmer (linear mixed model) rather
# than glmer. No exponentiation -- the estimate is a
# mean difference in days.
cat("\nFitting LOS linear model...")
t0 <- proc.time()["elapsed"]

fit_los <- with(imp,
                lmer(as.formula(paste("los ~", rhs)),
                     REML    = TRUE,
                     control = lmerControl(optimizer = "bobyqa")))

results_los_reg <- summary(pool(fit_los), conf.int = TRUE)
results_los_reg <- results_los_reg[
  results_los_reg$term == "GLP1_useYes", ]
results_los_reg$outcome <- "los"

elapsed <- round(proc.time()["elapsed"] - t0, 1)
cat(" done (", elapsed, "s)",
    " est =", round(results_los_reg$estimate, 2),
    " p =",   round(results_los_reg$p.value, 3), "\n")

cat("\nLOS regression result:\n")
print(results_los_reg[, c("outcome", "estimate",
                          "conf.low", "conf.high",
                          "p.value")],
      row.names = FALSE)

saveRDS(results_los_reg,
        "Results/los_regression_mi.rds")

cat("\nSaved Results/standard_regression_OR_mi.rds\n")
cat("Saved Results/los_regression_mi.rds\n")
