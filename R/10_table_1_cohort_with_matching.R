# =============================================
# 11 - Table 1: Patient characteristics
# Full cohort (unmatched) + propensity score
# matched cohort (MI dataset 1) side by side
# =============================================

library(tableone)
library(tidyverse)
library(openxlsx)

tab1_vars <- c("age_cat", "gender", "bmi_cat", "cci_cat",
               "smoking_cat", "alcohol_cat2","diabetes_bin",
               "prev_pancreatitis", "gallstones_imaging"
               )

factor_outcomes <- function(d) {
  d %>% mutate(
    across(c(composite, severity_bin, mort90, readm90,
             critical_care_adm_bin, local_complication),
           ~ factor(., levels = 0:1,
                    labels = c("No", "Yes")))
  )
}

# Prepare full cohort with missing shown explicitly
df_recode <-
  df %>%
   mutate(    bmi_cat = case_when(
     bmi == 5 ~ 0,
     bmi == 1 ~ 1,
     bmi == 2 ~ 2,
     bmi == 3 ~ 3,
     bmi == 4 ~ 4,
     bmi == 6 ~ 5,
     TRUE      ~ NA_real_),
     bmi_cat = factor(bmi_cat,
                     levels = 0:5,
                     labels = c("<18.5 Underweight",
                                "18.5-24.9 Normal",
                                "25-29.9 Overweight",
                                "30-34.9 Class 1 Obesity",
                                "35-39.9 Class 2 Obesity",
                                ">40 Class 3 Obesity"))) %>%
  mutate(
    composite             = as.integer(composite),
    severity_bin          = as.integer(severity_bin),
    critical_care_adm_bin = as.integer(
      critical_care_adm_bin),
    local_complication    = as.integer(
      local_complication),
    mort90                = as.integer(mort90),
    readm90               = as.integer(readm90),
    smoking_cat  = factor(smoking,
                          levels = c("Never smoker", "Current Smoker",
                                     "Ex-smoker")),
    alcohol_cat2 = factor(alcohol_cat,
                          levels = c("None", "1-14", "15-35", ">35")),
    bmi_cat      = fct_na_value_to_level(
      bmi_cat,      level = "Missing"),
    smoking_cat  = fct_na_value_to_level(
      smoking_cat,  level = "Missing"),
    alcohol_cat2 = fct_na_value_to_level(
      alcohol_cat2, level = "Missing"),
    diabetes_bin = case_when(
      diabetes == 1  ~ 0,
      diabetes %in% c(2, 3, 4) ~ 1,
      diabetes == 99 ~ NA_real_,
      TRUE           ~ NA_real_),
    diabetes_bin = factor(diabetes_bin,
                          levels = 0:1,
                          labels = c("No", "Yes")),
    diabetes_bin = fct_na_value_to_level(
      diabetes_bin, level = "Missing")) %>%
  factor_outcomes()

tab_unmatched <- CreateTableOne(
  vars       = tab1_vars,
  strata     = "GLP1_use",
  data       = df_recode,
  factorVars = tab1_vars,
  addOverall = TRUE
)

# Matched cohort (imputed dataset 1)
# Underweight excluded, no missing after imputation
tab_matched_mi <- CreateTableOne(
  vars       = tab1_vars,
  strata     = "GLP1_use",
  data       = matched_list[[1]] %>%
    haven::zap_labels() %>%
    mutate(
      composite             = as.integer(composite),
      severity_bin          = as.integer(severity_bin),
      critical_care_adm_bin = as.integer(
        critical_care_adm_bin),
      local_complication    = as.integer(
        local_complication),
      mort90                = as.integer(mort90),
      readm90               = as.integer(readm90),
      smoking_cat  = factor(smoking,
                            levels = c("Never smoker", "Current Smoker",
                                       "Ex-smoker")),
      alcohol_cat2 = factor(alcohol_cat,
                            levels = c("None", "1-14", "15-35", ">35")),
      bmi_cat      = factor(bmi_cat,
                            levels = c("18.5-24.9 Normal",
                                       "25-29.9 Overweight",
                                       "30-34.9 Class 1 Obesity",
                                       "35-39.9 Class 2 Obesity",
                                       ">40 Class 3 Obesity")),
      # Diabetes: no missing after imputation
      # so no fct_na_value_to_level needed
      diabetes_bin = factor(
        case_when(
          diabetes == 1            ~ 0,
          diabetes %in% c(2, 3, 4) ~ 1,
          TRUE                     ~ NA_real_),
        levels = 0:1,
        labels = c("No", "Yes"))
    ) %>%
    factor_outcomes(),
  factorVars = tab1_vars,
  addOverall = TRUE
)

# Extract character matrices
mat_unmatched <- print(tab_unmatched,
                       smd           = FALSE,
                       test          = TRUE,
                       showAllLevels = TRUE,
                       printToggle   = FALSE,
                       noSpaces      = TRUE)

mat_matched <- print(tab_matched_mi,
                     smd           = FALSE,
                     test          = TRUE,
                     showAllLevels = TRUE,
                     printToggle   = FALSE,
                     noSpaces      = TRUE)

# Tag variable headers so we can split them into
# a separate blank header row and a level row
add_level_rownames <- function(mat) {
  rn          <- rownames(mat)
  current_var <- ""
  new_rn      <- character(length(rn))
  for (i in seq_along(rn)) {
    if (rn[i] != "") {
      current_var <- rn[i]
      new_rn[i]   <- paste0("HEADER__", rn[i])
    } else {
      level_val <- trimws(mat[i, 1])
      new_rn[i] <- paste0(current_var,
                          "__", level_val)
    }
  }
  rownames(mat) <- new_rn
  mat
}

insert_first_levels <- function(mat) {
  out_rows <- list()
  i <- 1
  while (i <= nrow(mat)) {
    rn <- rownames(mat)[i]
    if (startsWith(rn, "HEADER__")) {
      var_name  <- sub("HEADER__", "", rn)
      first_lev <- trimws(mat[i, 1])
      header_row           <- mat[i, , drop = FALSE]
      header_row[1, ]      <- ""
      rownames(header_row) <- var_name
      level_row            <- mat[i, , drop = FALSE]
      rownames(level_row)  <- paste0(var_name,
                                     "__", first_lev)
      out_rows[[length(out_rows) + 1]] <- header_row
      out_rows[[length(out_rows) + 1]] <- level_row
    } else {
      out_rows[[length(out_rows) + 1]] <-
        mat[i, , drop = FALSE]
    }
    i <- i + 1
  }
  do.call(rbind, out_rows)
}

mat_un <- insert_first_levels(
  add_level_rownames(mat_unmatched))
mat_mi <- insert_first_levels(
  add_level_rownames(mat_matched))

# Align rows -- full cohort has extra rows for
# missing and underweight not in matched cohort
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
  shared        <- intersect(row_order, rownames(mat))
  out[shared, ] <- mat[shared, ]
  out
}

mat_un_aligned <- align_mat(mat_un, row_order,
                            "Full_cohort")
mat_mi_aligned <- align_mat(mat_mi, row_order,
                            "Matched_MI")

combined <- base::cbind(mat_un_aligned, mat_mi_aligned)

# Remove test columns -- keep p columns only
combined <- combined[,
                     !grepl("_test$", colnames(combined)),
                     drop = FALSE]

# Fill missing and underweight rows in matched cohort
# with zeros -- but NOT in p-value columns
missing_rows     <- grepl("Missing",
                          rownames(combined))
underweight_rows <- grepl("Underweight",
                          rownames(combined))

# Exclude p columns from zero-filling
fill_cols <- grepl("^Matched_MI_", colnames(combined)) &
  !grepl("_p$", colnames(combined))

for (r in which(missing_rows | underweight_rows)) {
  for (c in which(fill_cols)) {
    if (combined[r, c] == "") {
      combined[r, c] <- "0 (0.0)"
    }
  }
}

# Tidy column names
combined <- combined[,
                     !colnames(combined) %in% "Matched_MI_level",
                     drop = FALSE]

colnames(combined) <- gsub("Full_cohort_", "",
                           colnames(combined))
colnames(combined) <- gsub("Matched_MI_",  "Matched_",
                           colnames(combined))
combined <- combined[,
                     !colnames(combined) %in% "level",
                     drop = FALSE]

# Clean rownames before display labels
rownames(combined) <- gsub("__", " -- ",
                           rownames(combined))
rownames(combined) <- gsub(" \\(%\\) -- ", " -- ",
                           rownames(combined))

var_labels <- c(
  "n"                                          = "N",
  "age_cat (%)"                                = "Age (years)",
  "age_cat -- 18-35"                           = "18-35",
  "age_cat -- 36-55"                           = "36-55",
  "age_cat -- 56-75"                           = "56-75",
  "age_cat -- >75"                             = ">75",
  "gender (%)"                                 = "Sex",
  "gender -- Female"                           = "Female",
  "gender -- Male"                             = "Male",
  "bmi_cat (%)"                                = "BMI (kg/m2)",
  "bmi_cat -- <18.5 Underweight"               = "<18.5 Underweight",
  "bmi_cat -- 18.5-24.9 Normal"                = "18.5-24.9 Normal",
  "bmi_cat -- 25-29.9 Overweight"              = "25-29.9 Overweight",
  "bmi_cat -- 30-34.9 Class 1 Obesity"         = "30-34.9 Class 1 Obesity",
  "bmi_cat -- 35-39.9 Class 2 Obesity"         = "35-39.9 Class 2 Obesity",
  "bmi_cat -- >40 Class 3 Obesity"             = ">40 Class 3 Obesity",
  "bmi_cat -- Missing"                         = "Missing",
  "cci_cat (%)"                                = "Charlson Co-morbidity Index",
  "cci_cat -- 0"                               = "0",
  "cci_cat -- 1"                               = "1",
  "cci_cat -- 2"                               = "2",
  "cci_cat -- >=3"                             = ">=3",
  "smoking_cat (%)"                            = "Smoking status",
  "smoking_cat -- Never smoker"                = "Never smoker",
  "smoking_cat -- Current Smoker"              = "Current Smoker",
  "smoking_cat -- Ex-smoker"                   = "Ex-smoker",
  "smoking_cat -- Missing"                     = "Missing",
  "alcohol_cat2 (%)"                           = "Alcohol consumption (units/week)",
  "alcohol_cat2 -- None"                       = "None",
  "alcohol_cat2 -- 1-14"                       = "1-14",
  "alcohol_cat2 -- 15-35"                      = "15-35",
  "alcohol_cat2 -- >35"                        = ">35",
  "alcohol_cat2 -- Missing"                    = "Missing",
  "diabetes_bin (%)"        = "Diabetic",
  "diabetes_bin -- No"      = "No",
  "diabetes_bin -- Yes"     = "Yes",
  "diabetes_bin -- Missing" = "Missing",
  "prev_pancreatitis (%)"                      = "History of pancreatitis",
  "prev_pancreatitis -- No"                    = "No",
  "prev_pancreatitis -- Yes"                   = "Yes",
  "gallstones_imaging (%)"                     = "Gallstones on prior or index admission imaging",
  "gallstones_imaging -- No"                   = "No",
  "gallstones_imaging -- Yes"                  = "Yes"
)

is_header <- rownames(combined) %in%
  c("n", names(var_labels)[
    !grepl(" -- ", names(var_labels))])

current_rn     <- rownames(combined)
new_rn         <- var_labels[current_rn]
missing_labels <- is.na(new_rn)
new_rn[missing_labels] <- current_rn[missing_labels]
rownames(combined) <- new_rn

is_header <- rownames(combined) %in%
  var_labels[!grepl(" -- ", names(var_labels))]
is_header[rownames(combined) == "N"] <- TRUE

variable_col <- character(nrow(combined))
level_col    <- character(nrow(combined))

for (r in seq_len(nrow(combined))) {
  rn <- rownames(combined)[r]
  if (rn == "N") {
    variable_col[r] <- "N"
    level_col[r]    <- ""
  } else if (is_header[r]) {
    variable_col[r] <- rn
    level_col[r]    <- ""
  } else {
    variable_col[r] <- ""
    level_col[r]    <- rn
  }
}

combined_final <- base::cbind(
  Variable = variable_col,
  Level    = level_col,
  combined
)
# Restore N row counts
n_row <- which(combined_final[, "Variable"] == "N")

if (length(n_row) > 0) {
  un_n <- mat_un["n", ]
  mi_n <- mat_mi["n", !colnames(mat_mi) %in% "level"]
  un_n <- un_n[!names(un_n) %in% "level"]
  
  data_cols <- colnames(combined_final)[
    3:ncol(combined_final)]
  
  for (col in data_cols) {
    un_col <- gsub("^Full_cohort_", "", col)
    if (un_col %in% names(un_n)) {
      combined_final[n_row, col] <- un_n[un_col]
    }
    mi_col <- gsub("^Matched_", "", col)
    if (mi_col %in% names(mi_n)) {
      combined_final[n_row, col] <- mi_n[mi_col]
    }
  }
}

# Remove duplicate n row from tableone
n_header_row <- which(combined_final[, "Variable"] == "N")
n_data_row   <- which(combined_final[, "Level"] == "n -- ")

if (length(n_header_row) > 0 &
    length(n_data_row)   > 0) {
  combined_final[n_header_row, 3:ncol(combined_final)] <-
    combined_final[n_data_row,  3:ncol(combined_final)]
  combined_final <- combined_final[-n_data_row, ,
                                   drop = FALSE]
}

# Comma formatting for counts >= 1000
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

# Add % symbol to percentage values
add_percent_symbol <- function(cell) {
  gsub("\\(([0-9]+\\.[0-9]+)\\)",
       "(\\1%)", cell)
}

combined_fmt <- apply(combined_final, c(1, 2),
                      format_cell_commas)
combined_fmt <- apply(combined_fmt,   c(1, 2),
                      add_percent_symbol)

# Format p-value columns
# Skip cells containing brackets (count data not p-values)
fmt_p_cell <- function(cell) {
  if (grepl("\\(", cell)) return(cell)
  if (cell == "" | is.na(cell)) return(cell)
  p <- suppressWarnings(as.numeric(cell))
  if (is.na(p))  return(cell)
  if (p < 0.001) return("<0.001")
  if (p >= 1)    return("1.00")
  formatC(p, format = "g", digits = 2, flag = "#")
}

p_col_names <- colnames(combined_fmt)[
  grepl("^p$|^Full_cohort_p$|^Matched_p$|_p$",
        colnames(combined_fmt)) &
    !grepl("Overall|No|Yes|level|Variable|Level",
           colnames(combined_fmt))]

cat("P-value columns:\n")
print(p_col_names)

for (col in p_col_names) {
  combined_fmt[, col] <- sapply(
    combined_fmt[, col], fmt_p_cell)
}

rownames(combined_fmt) <- NULL

cat("\nTable 1\n")
print(combined_fmt)

# -----EXPORT TO EXCEL-----
df_out <- as.data.frame(combined_fmt,
                        stringsAsFactors = FALSE)

wb <- createWorkbook()
addWorksheet(wb, "Table 1")

# Column positions
# 1-2:   Variable, Level
# 3-6:   Full cohort (Overall, No, Yes, p)
# 7-10:  Matched cohort (Overall, No, Yes, p)
n_data_cols    <- ncol(df_out) - 2
full_coh_start <- 3
full_coh_end   <- full_coh_start +
  (n_data_cols / 2) - 1
matched_start  <- full_coh_end + 1
matched_end    <- ncol(df_out)

style_span <- createStyle(
  halign         = "center",
  valign         = "center",
  textDecoration = "bold",
  border         = "Bottom"
)
style_subheader <- createStyle(
  halign         = "center",
  textDecoration = "bold"
)
style_text <- createStyle(numFmt = "TEXT")

# Spanning header row
writeData(wb, "Table 1",
          x = "Full cohort",
          startRow = 1, startCol = full_coh_start)
mergeCells(wb, "Table 1",
           cols = full_coh_start:full_coh_end,
           rows = 1)

writeData(wb, "Table 1",
          x = "Propensity-score matched cohort",
          startRow = 1, startCol = matched_start)
mergeCells(wb, "Table 1",
           cols = matched_start:matched_end,
           rows = 1)

addStyle(wb, "Table 1",
         style      = style_span,
         rows       = 1,
         cols       = full_coh_start:matched_end,
         gridExpand = TRUE)

# Subheader row
subheaders <- colnames(df_out)
subheaders <- gsub("Full_cohort_|Matched_", "",
                   subheaders)
subheaders[1] <- "Variable"
subheaders[2] <- "Level"

for (j in seq_along(subheaders)) {
  writeData(wb, "Table 1",
            x        = subheaders[j],
            startRow = 2,
            startCol = j)
}
addStyle(wb, "Table 1",
         style      = style_subheader,
         rows       = 2,
         cols       = 1:ncol(df_out),
         gridExpand = TRUE)

# Data
writeData(wb, "Table 1",
          x        = df_out,
          startRow = 3,
          startCol = 1,
          colNames = FALSE)

addStyle(wb, "Table 1",
         style      = style_text,
         rows       = 3:(nrow(df_out) + 3),
         cols       = 1:ncol(df_out),
         gridExpand = TRUE)

setColWidths(wb, "Table 1",
             cols   = 1:ncol(df_out),
             widths = "auto")

freezePane(wb, "Table 1",
           firstActiveRow = 3)

saveWorkbook(wb,
             "Results/table_1_all_cohorts.xlsx",
             overwrite = TRUE)

cat("\nSaved Results/table_1_all_cohorts.xlsx\n")