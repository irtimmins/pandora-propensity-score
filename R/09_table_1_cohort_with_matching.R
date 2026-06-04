# =============================================
# 09 - TABLE 1
# Combined: unmatched full cohort + matched MI
# Single wide table
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

# -----COMBINE-----
combined <- cbind(mat_un_aligned, mat_mi_aligned)

# -----CLEAN UP-----

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

# -----PRINT AND SAVE-----
cat("\n-----Table 1: Full cohort and matched MI-----\n")
print(combined)

write.csv(combined,
          "Results/table1_combined.csv")

cat("\nSaved Results/table1_combined.csv\n")
