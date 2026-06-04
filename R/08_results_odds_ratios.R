# =============================================
# 08 - Combine ORs results summary
# All methods (Propensity score matching, 
# logistic regression, doubly robust)
# All based on multiple imputation
# =============================================

# -----Load results -----
results_ps  <- readRDS("Results/propensity_score_OR_mi.rds")
results_reg <- readRDS("Results/standard_regression_OR_mi.rds")
results_dr  <- readRDS("Results/doubly_robust_OR_mi.rds")

# -----Outomces -----
outcomes <- c("composite", "severity_bin",
              "mort90", "local_complication",
              "critical_care_adm_bin")

# ----------
# Helper to extract OR and p by outcome
extract <- function(results_df, outcome_vec,
                    or_col = "OR", p_col = "p") {
  idx <- match(outcome_vec, results_df$outcome)
  list(
    OR = results_df[[or_col]][idx],
    p  = results_df[[p_col]][idx]
  )
}

ps  <- extract(results_mi,  outcomes)
reg <- extract(results_reg, outcomes)
dr  <- extract(results_dr,  outcomes)

comparison_full <- data.frame(
  outcome = outcomes,
  
  # Primary: MI PS matching
  ps_mi_OR = ps$OR,
  ps_mi_p  = ps$p,
  
  # Sensitivity 1: MI melogit
  reg_mi_OR = reg$OR,
  reg_mi_p  = reg$p,
  
  # Sensitivity 2: Doubly robust
  dr_OR = dr$OR,
  dr_p  = dr$p
)

# ----- Print to console -----
cat("\n=============================================\n")
cat("Convergence across methods (all MI-based)\n")
cat("=============================================\n\n")

cat("Outcomes:\n")
cat("  composite             = primary outcome\n")
cat("  severity_bin          = severe vs mild/moderate\n")
cat("  mort90                = 90-day mortality\n")
cat("  local_complication    = local complication\n")
cat("  critical_care_adm_bin = critical care admission\n\n")

cat("Methods:\n")
cat("  ps_mi   = PS matching + MI (PRIMARY)\n")
cat("  reg_mi  = Logistic regression + MI (sensitivity 1)\n")
cat("  dr      = Doubly robust, Balancing weights + regression (sensitivity 3)\n\n")

print(comparison_full, row.names = FALSE)

# ----- Composite outcome -----
cat("\n-----Composite outcome OR across methods-----\n")

composite_row <- comparison_full %>%
  dplyr::filter(outcome == "composite")

cat(sprintf("  PS matching (MI):        OR %.3f  p=%.3f\n",
            composite_row$ps_mi_OR,
            composite_row$ps_mi_p))
cat(sprintf("  Regression (MI):         OR %.3f  p=%.3f\n",
            composite_row$reg_mi_OR,
            composite_row$reg_mi_p))
cat(sprintf("  Doubly robust:           OR %.3f  p=%.3f\n",
            composite_row$dr_OR,
            composite_row$dr_p))

# -----Save-----
write.csv(comparison_full,
          "Results/all_methods_OR.csv",
          row.names = FALSE)

cat("\nSaved all_methods_OR.csv\n")
