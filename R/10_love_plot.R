# =============================================
# 13 - LOVE PLOTS
# Complete case + single imputed dataset
# =============================================

# -----MI DATASET 1-----
# Single dataset, not averaged -- avoids hiding
# cross-imputation variation
love.plot(
  matchit_list[[1]],
  threshold    = 0.1,
  abs          = FALSE,
  var.order    = "unadjusted",
  colors       = c("red", "blue"),
  shapes       = c("circle filled", "triangle filled"),
  title        = "Covariate balance (imputed dataset 1)",
  sample.names = c("Unmatched", "Matched")
)
ggsave("loveplot_imp1.png", width = 8, height = 7, dpi = 300)

cat("Love plots saved\n")