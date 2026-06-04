# =============================================
# 14 - FINAL RESULTS SUMMARY
# Convergence table across all methods
# =============================================

bw_main <- results_bw %>%
  dplyr::filter(method == "Balancing weights (IPW)")
bw_dr <- results_bw %>%
  dplyr::filter(method ==
                  "Balancing weights + regression (DR)")

bw_outcomes <- c("composite", "severity_bin",
                 "mort90", "local_complication",
                 "critical_care_adm_bin")

comparison_full <- data.frame(
  outcome = bw_outcomes,
  
  # Full cohort regression
  reg_OR = c(0.608, 0.873, NA, NA, NA),
  reg_p  = c(0.035, 0.651, NA, NA, NA),
  
  # Complete case PS matching
  cc_OR  = c(0.692, 0.826, NA, 0.772, 0.478),
  cc_p   = c(0.037, 0.415, NA, 0.344, 0.102),
  
  # MI PS matching
  mi_OR  = results_mi$OR[match(bw_outcomes,
                               results_mi$outcome)],
  mi_p   = results_mi$p[match(bw_outcomes,
                              results_mi$outcome)],
  
  # Balancing weights IPW
  bw_OR  = bw_main$OR[match(bw_outcomes,
                            bw_main$outcome)],
  bw_p   = bw_main$p[match(bw_outcomes,
                           bw_main$outcome)],
  
  # Doubly robust
  dr_OR  = bw_dr$OR[match(bw_outcomes, bw_dr$outcome)],
  dr_p   = bw_dr$p[match(bw_outcomes, bw_dr$outcome)]
)

cat("\n-----Convergence across all methods-----\n")
print(comparison_full, row.names = FALSE)

write.csv(comparison_full,
          "pandora_comparison_all_methods.csv",
          row.names = FALSE)

cat("\nFinal summary saved\n")