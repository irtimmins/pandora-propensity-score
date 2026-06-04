# =============================================
# 20 - VALIDATION KAPPA STATISTICS
# Inter-rater agreement on key variables
# Only run if validation fields exist in data
# =============================================

library(irr)

# Check validation variables exist
val_pairs <- list(
  list(var1 = "age",    var2 = "age_validation",
       label = "Age"),
  list(var1 = "gender", var2 = "gender_validation",
       label = "Gender"),
  list(var1 = "bmi",    var2 = "bmi_validation",
       label = "BMI"),
  list(var1 = "diabetes", var2 = "diabetes_validation",
       label = "Diabetes"),
  list(var1 = "USS_adm", var2 = "USS_adm_validation",
       label = "Ultrasound")
)

# Filter to pairs that exist in data
val_pairs <- val_pairs[sapply(val_pairs, function(p)
  all(c(p$var1, p$var2) %in% names(df)))]

if (length(val_pairs) == 0) {
  cat("No validation variables found in dataset\n")
} else {
  
  kappa_results <- do.call(rbind, lapply(val_pairs,
                                         function(p) {
                                           
                                           d <- df %>%
                                             dplyr::select(v1 = all_of(p$var1),
                                                           v2 = all_of(p$var2)) %>%
                                             haven::zap_labels() %>%
                                             dplyr::filter(!is.na(v1) & !is.na(v2))
                                           
                                           # Exclude code 7/99 (not available)
                                           d <- d %>%
                                             dplyr::filter(v1 != 7, v1 != 99,
                                                           v2 != 7, v2 != 99)
                                           
                                           k <- irr::kappa2(d)
                                           
                                           data.frame(
                                             variable = p$label,
                                             n        = nrow(d),
                                             kappa    = round(k$value, 3),
                                             p        = round(k$p.value, 3)
                                           )
                                         }))
  
  cat("\n-----Validation kappa statistics-----\n")
  print(kappa_results, row.names = FALSE)
  
  write.csv(kappa_results,
            "table_validation_kappa.csv",
            row.names = FALSE)
  
  cat("\nSaved table_validation_kappa.csv\n")
}