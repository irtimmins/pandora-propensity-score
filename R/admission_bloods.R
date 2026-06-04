# =============================================
# 18 - ADMISSION BLOODS
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
    paste0(round(median(x, na.rm = TRUE), 1),
           " (",
           round(quantile(x, 0.25, na.rm = TRUE), 1),
           "-",
           round(quantile(x, 0.75, na.rm = TRUE), 1),
           ")")
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

cat("\n-----Admission bloods: median (IQR)-----\n")
print(results_bloods, row.names = FALSE)

# -----MISSING BLOODS TABLE-----
df_miss <- df %>%
  mutate(across(
    all_of(blood_vars),
    ~ factor(as.integer(is.na(.)),
             levels = 0:1,
             labels = c("Present", "Missing")),
    .names = "miss_{.col}"
  ))

miss_vars <- paste0("miss_", blood_vars)

tab_miss <- CreateTableOne(
  vars       = miss_vars,
  strata     = "GLP1_use",
  data       = df_miss,
  factorVars = miss_vars,
  addOverall = TRUE
)

cat("\n-----Missing bloods by GLP-1 status-----\n")
print(tab_miss, showAllLevels = FALSE)

# -----EXPORT-----
write.csv(results_bloods,
          "table_admission_bloods.csv",
          row.names = FALSE)

write.csv(
  print(tab_miss,
        showAllLevels = FALSE,
        printToggle   = FALSE,
        noSpaces      = TRUE),
  "table_missing_bloods.csv"
)

cat("\nSaved table_admission_bloods.csv\n")
cat("Saved table_missing_bloods.csv\n")