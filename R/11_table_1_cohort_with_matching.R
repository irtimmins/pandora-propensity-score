# =============================================
# 11 - Table 1, patient characteristics.
# Combined: unmatched full cohort + matched MI
# Single wide table
# (A lot of debugging to produce).
# =============================================


tab1_vars <- c("age_cat", "gender", "bmi_cat", "cci_cat",
               "smoking_cat", "alcohol_cat2",
               "prev_pancreatitis", "gallstones_imaging",
               "composite", "severity_bin", "mort90",
               "readm90", "critical_care_adm_bin",
               "local_complication")

# -----HELPER: FACTOR OUTCOMES-----
factor_outcomes <- function(d) {
  d %>% mutate(
    across(c(composite, severity_bin, mort90, readm90,
             critical_care_adm_bin, local_complication),
           ~ factor(., levels = 0:1,
                    labels = c("No", "Yes")))
  )
}

# -----UNMATCHED FULL COHORT-----
df_recode <- df %>%
  mutate(
    smoking_cat  = factor(smoking,
                          levels = c("Never smoker", "Current Smoker",
                                     "Ex-smoker")),
    alcohol_cat2 = factor(alcohol_cat,
                          levels = c("None", "1-14", "15-35", ">35")),
    smoking_cat  = fct_na_value_to_level(
      smoking_cat,  level = "Missing"),
    alcohol_cat2 = fct_na_value_to_level(
      alcohol_cat2, level = "Missing"),
    bmi_cat      = fct_na_value_to_level(
      bmi_cat,      level = "Missing")
  ) %>%
  factor_outcomes()

tab_unmatched <- CreateTableOne(
  vars       = tab1_vars,
  strata     = "GLP1_use",
  data       = df_recode,
  factorVars = tab1_vars,
  addOverall = TRUE
)

# -----MATCHED MI (dataset 1)-----
tab_matched_mi <- CreateTableOne(
  vars       = tab1_vars,
  strata     = "GLP1_use",
  data       = matched_list[[1]] %>%
    mutate(
      smoking_cat  = factor(smoking,
                            levels = c("Never smoker", "Current Smoker",
                                       "Ex-smoker")),
      alcohol_cat2 = factor(alcohol_cat,
                            levels = c("None", "1-14", "15-35", ">35"))
    ) %>%
    factor_outcomes(),
  factorVars = tab1_vars,
  addOverall = TRUE
)

# -----EXTRACT PRINT MATRICES-----
mat_unmatched <- print(tab_unmatched,
                       smd           = FALSE,
                       test          = FALSE,
                       showAllLevels = TRUE,
                       printToggle   = FALSE,
                       noSpaces      = TRUE)

mat_matched <- print(tab_matched_mi,
                     smd           = FALSE,
                     test          = FALSE,
                     showAllLevels = TRUE,
                     printToggle   = FALSE,
                     noSpaces      = TRUE)

# -----ADD UNIQUE ROWNAMES-----
# tableone uses empty strings for level rows
# reconstruct unique names from variable + level value
add_level_rownames <- function(mat) {
  rn          <- rownames(mat)
  current_var <- ""
  new_rn      <- character(length(rn))
  
  for (i in seq_along(rn)) {
    if (rn[i] != "") {
      current_var <- rn[i]
      new_rn[i]   <- rn[i]
    } else {
      level_val <- trimws(mat[i, 1])
      new_rn[i] <- paste0(current_var,
                          "__", level_val)
    }
  }
  rownames(mat) <- new_rn
  mat
}

mat_un <- add_level_rownames(mat_unmatched)
mat_mi <- add_level_rownames(mat_matched)

# -----ALIGN ROWS-----
# Unmatched has Missing rows that matched does not
# Preserve unmatched ordering, append any matched-only rows
row_order <- c(
  rownames(mat_un),
  setdiff(rownames(mat_mi), rownames(mat_un))
)

align_mat <- function(mat, row_order, suffix) {
  out <- matrix("",
                nrow     = length(row_order),
                ncol     = ncol(mat),
                dimnames = list(row_order,
                                paste0(suffix, "_",
                                       colnames(mat))))
  shared       <- intersect(row_order, rownames(mat))
  out[shared, ] <- mat[shared, ]
  out
}

mat_un_aligned <- align_mat(mat_un, row_order,
                            "Full_cohort")
mat_mi_aligned <- align_mat(mat_mi, row_order,
                            "Matched_MI")

# -----Combine both-----
combined <- cbind(mat_un_aligned, mat_mi_aligned)

# -----Further cleaning commands-----

# 1. Fill Missing rows in matched MI columns with "0 (0.0)"
missing_rows  <- grepl("Missing", rownames(combined))
matched_cols  <- grepl("^Matched_MI_", colnames(combined))

for (r in which(missing_rows)) {
  for (c in which(matched_cols)) {
    if (combined[r, c] == "") {
      combined[r, c] <- "0 (0.0)"
    }
  }
}

# 2. Remove redundant level column from matched MI
combined <- combined[,
                     !colnames(combined) %in% "Matched_MI_level",
                     drop = FALSE]

# 3. Clean rownames for display
rownames(combined) <- gsub("__", " -- ",
                           rownames(combined))
rownames(combined) <- gsub(" \\(%\\) -- ", " -- ",
                           rownames(combined))

# -----Further cleaning of row names-----

# Lookup table: 
var_labels <- c(
  "n"                         = "N",
  "age_cat (%)"               = "Age, years (%)",
  "age_cat -- 18-35"          = "  18-35",
  "age_cat -- 36-55"          = "  36-55",
  "age_cat -- 56-75"          = "  56-75",
  "age_cat -- >75"            = "  >75",
  "gender (%)"                = "Male sex (%)",
  "gender -- Male"            = "  Male",
  "gender -- Female"          = "  Female",
  "bmi_cat (%)"               = "BMI category (%)",
  "bmi_cat -- 18.5-24.9 Normal"        = "  Normal (18.5-24.9)",
  "bmi_cat -- 25-29.9 Overweight"      = "  Overweight (25-29.9)",
  "bmi_cat -- 30-34.9 Class 1 Obesity" = "  Obesity class I (30-34.9)",
  "bmi_cat -- 35-39.9 Class 2 Obesity" = "  Obesity class II (35-39.9)",
  "bmi_cat -- >40 Class 3 Obesity"     = "  Obesity class III (>40)",
  "bmi_cat -- Missing"        = "  Missing",
  "cci_cat (%)"               = "Charlson Comorbidity Index (%)",
  "cci_cat -- 0"              = "  0",
  "cci_cat -- 1"              = "  1",
  "cci_cat -- 2"              = "  2",
  "cci_cat -- >=3"            = "  >=3",
  "smoking_cat (%)"           = "Smoking status (%)",
  "smoking_cat -- Never smoker"    = "  Never smoker",
  "smoking_cat -- Current Smoker"  = "  Current smoker",
  "smoking_cat -- Ex-smoker"       = "  Ex-smoker",
  "smoking_cat -- Missing"         = "  Missing",
  "alcohol_cat2 (%)"          = "Alcohol consumption, units/week (%)",
  "alcohol_cat2 -- None"      = "  None",
  "alcohol_cat2 -- 1-14"      = "  1-14",
  "alcohol_cat2 -- 15-35"     = "  15-35",
  "alcohol_cat2 -- >35"       = "  >35",
  "alcohol_cat2 -- Missing"   = "  Missing",
  "prev_pancreatitis (%)"     = "Prior pancreatitis (%)",
  "prev_pancreatitis -- No"   = "  No",
  "prev_pancreatitis -- Yes"  = "  Yes",
  "gallstones_imaging (%)"    = "Gallstones on imaging (%)",
  "gallstones_imaging -- No"  = "  No",
  "gallstones_imaging -- Yes" = "  Yes",
  "composite (%)"             = "Composite outcome (%)",
  "composite -- No"           = "  No",
  "composite -- Yes"          = "  Yes",
  "severity_bin (%)"          = "Severe pancreatitis (%)",
  "severity_bin -- No"        = "  Mild/Moderate",
  "severity_bin -- Yes"       = "  Severe",
  "mort90 (%)"                = "90-day mortality (%)",
  "mort90 -- No"              = "  No",
  "mort90 -- Yes"             = "  Yes",
  "readm90 (%)"               = "90-day readmission (%)",
  "readm90 -- No"             = "  No",
  "readm90 -- Yes"            = "  Yes",
  "critical_care_adm_bin (%)" = "Critical care admission (%)",
  "critical_care_adm_bin -- No"  = "  No",
  "critical_care_adm_bin -- Yes" = "  Yes",
  "local_complication (%)"    = "Local complication (%)",
  "local_complication -- No"  = "  No",
  "local_complication -- Yes" = "  Yes"
)

# Apply labels -- keep original if no match found
current_rn     <- rownames(combined)
new_rn         <- var_labels[current_rn]
missing_labels <- is.na(new_rn)
new_rn[missing_labels] <- current_rn[missing_labels]
rownames(combined) <- new_rn

# -----Further cleaning-----
colnames(combined) <- gsub("Full_cohort_", "",
                           colnames(combined))
colnames(combined) <- gsub("Matched_MI_",  "Matched_",
                           colnames(combined))

# Remove the level column from full cohort
combined <- combined[,
                     !colnames(combined) %in% "level",
                     drop = FALSE]


# -----Clean with manual comma formating -----
# Applies to counts >= 1000 in cell values
# Handles formats: "3029", "3029 (xx.x)", "1924 (68.3)"

format_cell_commas <- function(cell) {
  matches <- gregexpr("[0-9]{4,}", cell)
  nums    <- regmatches(cell, matches)[[1]]
  if (length(nums) == 0) return(cell)
  for (n in nums) {
    formatted <- formatC(as.integer(n),
                         format   = "d",
                         big.mark = ",")
    cell <- sub(n, formatted, cell, fixed = TRUE)
  }
  cell
}

combined_fmt <- apply(combined, c(1, 2),
                      format_cell_commas)

rownames(combined_fmt) <- rownames(combined)
colnames(combined_fmt) <- colnames(combined)

####################################################
# Save results.

cat("\n-----Table 1 (comma formatted)-----\n")
print(combined_fmt)

write.csv(combined_fmt,
          "Results/table_1_all_cohorts.csv")

cat("\nSaved Results/table_1_all_cohorts.csv\n")
