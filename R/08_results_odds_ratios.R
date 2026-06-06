# =============================================
# 08 - Results summary
# Compares odds ratios across all three methods:
# propensity score matching (primary), doubly
# robust matching, and multilevel regression.
# All based on multiple imputation.
# =============================================

results_ps  <- readRDS("Results/propensity_score_OR_mi.rds")
results_dr  <- readRDS("Results/doubly_robust_OR_mi.rds")
results_reg <- readRDS("Results/standard_regression_OR_mi.rds")

outcomes <- c("composite", "severity_bin",
              "mort90", "local_complication",
              "critical_care_adm_bin", "readm90")

# Join all three result sets on outcome name.
# results_reg uses estimate/conf.low/conf.high/p.value
# from summary(pool()) directly, so we rename to match
# the OR/lower/upper/p convention from scripts 05/06.
results_reg_clean <- results_reg %>%
  mutate(
    OR    = round(estimate,  2),
    lower = round(conf.low,  2),
    upper = round(conf.high, 2),
    p     = round(p.value,   3)
  ) %>%
  dplyr::select(outcome, OR, lower, upper, p)

comparison <- results_ps %>%
  dplyr::select(outcome,
                ps_OR    = OR,
                ps_lower = lower,
                ps_upper = upper,
                ps_p     = p) %>%
  dplyr::left_join(
    results_dr %>%
      dplyr::select(outcome,
                    dr_OR    = OR,
                    dr_lower = lower,
                    dr_upper = upper,
                    dr_p     = p),
    by = "outcome") %>%
  dplyr::left_join(
    results_reg_clean %>%
      dplyr::select(outcome,
                    reg_OR    = OR,
                    reg_lower = lower,
                    reg_upper = upper,
                    reg_p     = p),
    by = "outcome") %>%
  dplyr::filter(outcome %in% outcomes) %>%
  dplyr::arrange(match(outcome, outcomes))

write.csv(comparison,
          "Results/all_methods_OR.csv",
          row.names = FALSE)

cat("\nSaved Results/all_methods_OR.csv\n")