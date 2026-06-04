# =============================================
# 13 - Table 3, Lab findings
# Continuous: median (IQR) by GLP-1 status
# Kruskal-Wallis tests
# =============================================

# -----SUMMARY TABLE-----
lab_vars <- c("wcc_adm", "crp", "urea_adm",
                "glucose_adm", "bili_adm",
                "lactate_adm", "ews_adm")

# Check which exist
lab_vars <- lab_vars[lab_vars %in% names(df)]

# Median (IQR) by GLP-1 group
summarise_lab_results <- function(var, data) {
  
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

results_labs <- do.call(rbind,
                          lapply(lab_vars, summarise_lab_results, data = df))

results_labs
cat("\n-----Lab results: median (IQR)-----\n")
print(results_labs, row.names = FALSE)

write.csv(print(results_labs, smd = TRUE,
                printToggle = FALSE, noSpaces = TRUE),
          "Results/table_3_lab_findings.csv")
