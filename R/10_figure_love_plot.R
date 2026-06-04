# =============================================
# 10 - Love plot
# For propensity matching analysis.
# Produced for single imputed dataset
# =============================================

# Single dataset, not averaged -- avoids hiding
# cross-imputation variation
love.plot(
  matchit_list[[1]],
  threshold    = 0.1,
  abs          = FALSE,
  var.order    = "unadjusted",
  colors       = c("red", "blue"),
  shapes       = c("circle filled", "triangle filled"),
  title        = NULL,
  sample.names = c("Unmatched", "Matched")
)
ggsave("Results/propensity_score_loveplot_imp1.png", width = 8, height = 7, dpi = 300)

cat("Love plots saved\n")
