# =============================================
# 06 - Doubly robust sensitivity analysis
# Same propensity score matching as script 05,
# but outcome models include covariate adjustment.
# Consistent if either the PS model or the outcome
# model is correctly specified (doubly robust).
# =============================================

# imp <- readRDS("Data/pandora_imp.rds")

imp_ps <- mice::filter(imp, !imp$ignore)

cat("Excluded for positivity:", sum(imp$ignore), "\n")
cat("PS sample size:",
    nrow(mice::complete(imp_ps, 1)), "\n")

ps_covariates <- setdiff(ps_vars, "GLP1_use")
match_formula <- reformulate(ps_covariates,
                             response = "GLP1_use")

matched <- matchthem(
  match_formula,
  datasets = imp_ps,
  approach = "within",
  method   = "nearest",
  distance = "glm",
  link     = "logit",
  ratio    = 3,
  caliper  = 0.1
)

# Adding all covariates to the outcome model alongside
# GLP1_use gives doubly robust estimates -- the matching
# weights balance confounders and the outcome regression
# adjusts for any residual imbalance
adj_rhs <- paste(c("GLP1_use", ps_covariates),
                 collapse = " + ")

binary_outcomes <- c("composite", "severity_bin",
                     "mort90", "readm90",
                     "critical_care_adm_bin",
                     "local_complication")

results_dr <- data.frame()

for (outcome in binary_outcomes) {
  
  fit <- with(matched,
              glm(as.formula(
                paste(outcome, "~", adj_rhs)),
                family  = quasibinomial,
                weights = weights))
  
  est <- summary(pool(fit), conf.int = TRUE,
                 exponentiate = TRUE)
  est <- est[est$term == "GLP1_useYes", ]
  est$outcome <- outcome
  
  results_dr <- rbind(results_dr, est)
}

cat("Doubly robust binary outcomes:\n")
print(results_dr, row.names = FALSE)

results_dr <- results_dr %>%
  mutate(
    OR    = round(estimate,  2),
    lower = round(conf.low,  2),
    upper = round(conf.high, 2),
    p     = round(p.value,   3)
  ) %>%
  dplyr::select(outcome, OR, lower, upper, p)

saveRDS(results_dr,
        "Results/doubly_robust_OR_mi.rds")

results_dr_cont <- data.frame()

for (outcome in c("los")) {
  
  fit <- with(matched,
              lm(as.formula(paste(outcome, "~", adj_rhs)),
                 weights = weights))
  
  est <- summary(pool(fit), conf.int = TRUE)
  est <- est[est$term == "GLP1_useYes", ]
  est$outcome <- outcome
  
  results_dr_cont <- rbind(results_dr_cont, est)
}

cat("Doubly robust LOS:\n")
print(results_dr_cont, row.names = FALSE)

results_dr_cont <- results_dr_cont %>%
  mutate(
    estimate = round(estimate,  1),
    lower    = round(conf.low,  1),
    upper    = round(conf.high, 1),
    p        = round(p.value,   3)
  ) %>%
  dplyr::select(outcome, estimate, lower, upper, p)

saveRDS(results_dr_cont,
        "Results/los_doubly_robust_mi.rds")

cat("Saved doubly_robust_OR_mi.rds and",
    "los_doubly_robust_mi.rds\n")