# =============================================
# 05 - Propensity score matching (primary analysis)
# matchthem() to match within each imputed dataset,
# then mice::pool() for Rubin's rules.
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

print(summary(matched))

# Balance diagnostics -- love plot shows standardised
# mean differences before and after matching for each
# covariate. Values inside the 0.1 threshold indicate
# good balance.
love.plot(matched,
          thresholds   = c(m = 0.1),
          abs          = TRUE,
          var.order    = "unadjusted",
          colors       = c("grey50", "#E69F00"),
          shapes       = c("circle", "diamond"),
          sample.names = c("Unmatched", "Matched"),
          title        = "Covariate balance: propensity score matching")

ggsave("Results/love_plot_ps_matching.png",
       width = 8, height = 6, dpi = 300)

# Detailed balance table across all imputations
bal.tab(matched,
        thresholds = c(m = 0.1),
        un         = TRUE)

binary_outcomes <- c("composite", "severity_bin",
                     "mort90", "readm90",
                     "critical_care_adm_bin",
                     "local_complication")

results_ps <- data.frame()

for (outcome in binary_outcomes) {
  
  fit <- with(matched,
              glm(as.formula(
                paste(outcome, "~ GLP1_use")),
                family  = quasibinomial,
                weights = weights))
  
  est <- summary(pool(fit), conf.int = TRUE,
                 exponentiate = TRUE)
  est <- est[est$term == "GLP1_useYes", ]
  est$outcome <- outcome
  
  results_ps <- rbind(results_ps, est)
}

cat("PS matched binary outcomes:\n")
print(results_ps, row.names = FALSE)

results_ps <- results_ps %>%
  mutate(
    OR    = round(estimate,  2),
    lower = round(conf.low,  2),
    upper = round(conf.high, 2),
    p     = round(p.value,   3)
  ) %>%
  dplyr::select(outcome, OR, lower, upper, p)

saveRDS(results_ps,
        "Results/propensity_score_OR_mi.rds")

results_cont <- data.frame()

for (outcome in c("los")) {
  
  fit <- with(matched,
              lm(as.formula(paste(outcome, "~ GLP1_use")),
                 weights = weights))
  
  est <- summary(pool(fit), conf.int = TRUE)
  est <- est[est$term == "GLP1_useYes", ]
  est$outcome <- outcome
  
  results_cont <- rbind(results_cont, est)
}

cat("PS matched continuous outcomes:\n")
print(results_cont, row.names = FALSE)

results_cont <- results_cont %>%
  mutate(
    estimate = round(estimate,  1),
    lower    = round(conf.low,  1),
    upper    = round(conf.high, 1),
    p        = round(p.value,   3)
  ) %>%
  dplyr::select(outcome, estimate, lower, upper, p)

saveRDS(results_cont, "Results/los_mi.rds")

# Save matched datasets for descriptive table scripts
matched_list <- lapply(
  seq_along(matched$models),
  function(i) MatchIt::match.data(matched$models[[i]]))

saveRDS(matched_list, "Data/pandora_matched_list.rds")
saveRDS(matched,      "Data/pandora_matchthem.rds")

cat("Saved propensity_score_OR_mi.rds, los_mi.rds,",
    "matched_list, matchthem object\n")