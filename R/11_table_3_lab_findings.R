# =============================================
# Table 3 - Lab findings
# Continuous: median (IQR) by GLP-1 status
# Kruskal-Wallis tests
# =============================================

# -----SUMMARY TABLE-----
blood_vars <- c("wcc_adm", "crp", "urea_adm",
                "glucose_adm", "bili_adm",
                "lactate_adm", "ews_adm")

# Check which exist
blood_vars <- blood_vars[blood_vars %in% names(df)]

# Median (IQR) by GLP-1 group
summarise_blood <- function(var, data) {
  
  overall <- data[[var]]
  glp1    <- data[[var]][data$GLP1_use == "Yes"]
  ctrl    <- data[[var]][data$GLP1_use == "No"]
  
  fmt_miqr <- function(x) {
    paste0(
      formatC(median(x, na.rm = TRUE),
              format = "f", digits = 1),
      " (",
      formatC(quantile(x, 0.25, na.rm = TRUE),
              format = "f", digits = 1),
      "-",
      formatC(quantile(x, 0.75, na.rm = TRUE),
              format = "f", digits = 1),
      ")"
    )
  }
  
  # Kruskal-Wallis
  kw <- kruskal.test(
    data[[var]] ~ data$GLP1_use
  )
  
  data.frame(
    variable = var,
    overall  = fmt_miqr(overall),
    glp1     = fmt_miqr(glp1),
    control  = fmt_miqr(ctrl),
    n_miss   = sum(is.na(overall)),
    p_kw     = round(kw$p.value, 3)
  )
}

results_bloods <- do.call(rbind,
                          lapply(blood_vars, summarise_blood, data = df))

results_bloods
cat("\n-----Admission bloods: median (IQR)-----\n")
print(results_bloods, row.names = FALSE)

write.csv(print(results_bloods, smd = TRUE,
                printToggle = FALSE, noSpaces = TRUE),
          "Results/table1_matched_mi.csv")