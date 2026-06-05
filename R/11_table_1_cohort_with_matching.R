# =============================================
# 11 - Table 1: Patient characteristics
# Full cohort (unmatched) + propensity score
# matched cohort (MI dataset 1) side by side
# =============================================

# Variables to include in table
tab1_vars <- c("age_cat", "gender", "bmi_cat", "cci_cat",
               "smoking_cat", "alcohol_cat2",
               "prev_pancreatitis", "gallstones_imaging")

# Helper: convert binary outcome variables to labelled factors
factor_outcomes <- function(d) {
  d %>% mutate(
    across(c(composite, severity_bin, mort90, readm90,
             critical_care_adm_bin, local_complication),
           ~ factor(., levels = 0:1,
                    labels = c("No", "Yes")))
  )
}

# Prepare full cohort with missing shown explicitly
df_recode <- df %>%
  haven::zap_labels() %>%
  mutate(
    composite             = as.integer(composite),
    severity_bin          = as.integer(severity_bin),
    critical_care_adm_bin = as.integer(
      critical_care_adm_bin),
    local_complication    = as.integer(local_complication),
    mort90                = as.integer(mort90),
    readm90               = as.integer(readm90),
    smoking_cat  = factor(smoking,
                          levels = c("Never smoker", "Current Smoker",
                                     "Ex-smoker")),
    alcohol_cat2 = factor(alcohol_cat,
                          levels = c("None", "1-14", "15-35", ">35")),
    bmi_cat      = fct_na_value_to_level(
      bmi_cat, level = "Missing"),
    smoking_cat  = fct_na_value_to_level(
      smoking_cat, level = "Missing"),
    alcohol_cat2 = fct_na_value_to_level(
      alcohol_cat2, level = "Missing")
  ) %>%
  factor_outcomes()

tab_unmatched <- CreateTableOne(
  vars       = tab1_vars,
  strata     = "GLP1_use",
  data       = df_recode,
  factorVars = tab1_vars,
  addOverall = TRUE
)

# Prepare matched cohort (imputed dataset 1)
# Underweight excluded, no missing after imputation
tab_matched_mi <- CreateTableOne(
  vars       = tab1_vars,
  strata     = "GLP1_use",
  data       = matched_list[[1]] %>%
    haven::zap_labels() %>%
    mutate(
      # Explicitly convert to integer first
      # to ensure factor() works correctly
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
                                       ">40 Class 3 Obesity"))
    ) %>%
    factor_outcomes(),
  factorVars = tab1_vars,
  addOverall = TRUE
)


# Extract character matrices from tableone objects
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

# tableone puts variable names only on the first level row
# and leaves subsequent rows blank. Tag each variable header
# so we can split them into a separate blank header row later.
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

mat_un <- add_level_rownames(mat_unmatched)
mat_mi <- add_level_rownames(mat_matched)

# Split each header+first-level row into two separate rows:
# one blank header row and one populated first-level row.
# This ensures all categories appear on their own line.
insert_first_levels <- function(mat) {
  out_rows <- list()
  i <- 1
  while (i <= nrow(mat)) {
    rn <- rownames(mat)[i]
    if (startsWith(rn, "HEADER__")) {
      var_name  <- sub("HEADER__", "", rn)
      first_lev <- trimws(mat[i, 1])
      
      # Blank header row
      header_row           <- mat[i, , drop = FALSE]
      header_row[1, ]      <- ""
      rownames(header_row) <- var_name
      
      # First level row with data intact
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

mat_un <- insert_first_levels(mat_un)
mat_mi <- insert_first_levels(mat_mi)

# Align rows across both tables. The full cohort has extra
# rows for missing categories and underweight that the
# matched cohort does not -- these are preserved in order.
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

combined <- cbind(mat_un_aligned, mat_mi_aligned)

# Fill missing and underweight rows in the matched cohort
# with zeros -- missing is resolved by imputation, and
# underweight patients are excluded from the PS analysis
missing_rows     <- grepl("Missing",     rownames(combined))
underweight_rows <- grepl("Underweight", rownames(combined))
matched_cols     <- grepl("^Matched_MI_", colnames(combined))

for (r in which(missing_rows | underweight_rows)) {
  for (c in which(matched_cols)) {
    if (combined[r, c] == "") {
      combined[r, c] <- "0 (0.0)"
    }
  }
}

# Tidy up column names
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

# Clean row names before applying display labels
rownames(combined) <- gsub("__", " -- ",
                           rownames(combined))
rownames(combined) <- gsub(" \\(%\\) -- ", " -- ",
                           rownames(combined))

# Display labels for each row
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
  "prev_pancreatitis (%)"                      = "History of pancreatitis",
  "prev_pancreatitis -- No"                    = "No",
  "prev_pancreatitis -- Yes"                   = "Yes",
  "gallstones_imaging (%)"                     = "Gallstones on prior or index admission imaging",
  "gallstones_imaging -- No"                   = "No",
  "gallstones_imaging -- Yes"                  = "Yes",
  "composite (%)"                              = "Composite outcome",
  "composite -- No"                            = "No",
  "composite -- Yes"                           = "Yes",
  "severity_bin (%)"                           = "Severe pancreatitis",
  "severity_bin -- No"                         = "Mild/Moderate",
  "severity_bin -- Yes"                        = "Severe",
  "mort90 (%)"                                 = "90-day mortality",
  "mort90 -- No"                               = "No",
  "mort90 -- Yes"                              = "Yes",
  "readm90 (%)"                                = "90-day readmission",
  "readm90 -- No"                              = "No",
  "readm90 -- Yes"                             = "Yes",
  "critical_care_adm_bin (%)"                  = "Critical care admission",
  "critical_care_adm_bin -- No"                = "No",
  "critical_care_adm_bin -- Yes"               = "Yes",
  "local_complication (%)"                     = "Local complication",
  "local_complication -- No"                   = "No",
  "local_complication -- Yes"                  = "Yes"
)

# Identify header vs level rows before relabelling
is_header <- rownames(combined) %in%
  c("n", names(var_labels)[!grepl(" -- ",
                                  names(var_labels))])
is_level  <- grepl(" -- ", rownames(combined))

# Apply display labels, keeping original name if not found
current_rn     <- rownames(combined)
new_rn         <- var_labels[current_rn]
missing_labels <- is.na(new_rn)
new_rn[missing_labels] <- current_rn[missing_labels]
rownames(combined) <- new_rn

# Recompute is_header after relabelling
is_header <- rownames(combined) %in%
  var_labels[!grepl(" -- ", names(var_labels))]
is_header[rownames(combined) == "N"] <- TRUE

# Build Variable and Level columns for easy formatting
# in Word or Excel -- variable name on header rows,
# category label on level rows
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

combined_final <- cbind(
  Variable = variable_col,
  Level    = level_col,
  combined
)

# Restore sample size counts to the N row
# (header rows were blanked earlier)
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

# Remove the duplicate "n -- " row that tableone generates
# leaving only the single clean N row with counts
n_header_row <- which(combined_final[, "Variable"] == "N")
n_data_row   <- which(combined_final[, "Level"] == "n -- ")

if (length(n_header_row) > 0 &
    length(n_data_row)   > 0) {
  combined_final[n_header_row, 3:ncol(combined_final)] <-
    combined_final[n_data_row,  3:ncol(combined_final)]
  combined_final <- combined_final[-n_data_row, ,
                                   drop = FALSE]
}

# Add comma separators to counts >= 1000
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

# Add % symbol to all percentage values
# Matches pattern: number (space) number.number
# e.g. "600 (19.8)" becomes "600 (19.8%)"

add_percent_symbol <- function(cell) {
  # Match opening bracket, number, closing bracket
  # but not cells that are already "0 (0.0)" with %
  # or plain counts like "3029"
  gsub(
    "\\(([0-9]+\\.[0-9]+)\\)",
    "(\\1%)",
    cell
  )
}

combined_fmt <- apply(combined_final, c(1, 2),
                      format_cell_commas)

# Add % symbol to percentage values
combined_fmt <- apply(combined_fmt, c(1, 2),
                      add_percent_symbol)

rownames(combined_fmt) <- NULL

cat("\nTable 1\n")
print(combined_fmt)

# library(openxlsx)

# Add spanning header rows above the data
# Row 1: group labels spanning the columns
# Row 2: subgroup labels (Overall, No, Yes)

library(openxlsx)

df_out <- as.data.frame(combined_fmt,
                        stringsAsFactors = FALSE)

wb <- createWorkbook()
addWorksheet(wb, "Table 1")

# Work out column positions
# Columns 1-2 are Variable and Level
# Then Full cohort: Overall, No, Yes (3 cols)
# Then Matched:     Overall, No, Yes (3 cols)
n_data_cols    <- ncol(df_out) - 2
full_coh_start <- 3
full_coh_end   <- full_coh_start +
  (n_data_cols / 2) - 1
matched_start  <- full_coh_end + 1
matched_end    <- ncol(df_out)

# Spanning header styles
style_span <- createStyle(
  halign      = "center",
  valign      = "center",
  textDecoration = "bold",
  border      = "Bottom"
)
style_subheader <- createStyle(
  halign      = "center",
  textDecoration = "bold"
)
style_text <- createStyle(numFmt = "TEXT")

# Write spanning header row 1 (row 1 in sheet)
writeData(wb, "Table 1",
          x         = "",
          startRow  = 1,
          startCol  = 1)

writeData(wb, "Table 1",
          x         = "Full cohort",
          startRow  = 1,
          startCol  = full_coh_start)

writeData(wb, "Table 1",
          x         = "Propensity-score matched cohort",
          startRow  = 1,
          startCol  = matched_start)

# Merge cells for spanning headers
mergeCells(wb, "Table 1",
           cols = full_coh_start:full_coh_end,
           rows = 1)
mergeCells(wb, "Table 1",
           cols = matched_start:matched_end,
           rows = 1)

# Apply spanning header style
addStyle(wb, "Table 1",
         style      = style_span,
         rows       = 1,
         cols       = full_coh_start:matched_end,
         gridExpand = TRUE)

# Write subheader row 2 (Overall, No, Yes labels)
subheaders <- colnames(df_out)
# Clean up prefixes for display
subheaders <- gsub("Full_cohort_|Matched_", "",
                   subheaders)
# Variable and Level columns
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

# Write data starting at row 3
writeData(wb, "Table 1",
          x         = df_out,
          startRow  = 3,
          startCol  = 1,
          colNames  = FALSE)

# Force all data cells to text
addStyle(wb, "Table 1",
         style      = style_text,
         rows       = 3:(nrow(df_out) + 3),
         cols       = 1:ncol(df_out),
         gridExpand = TRUE)

# Auto width columns
setColWidths(wb, "Table 1",
             cols   = 1:ncol(df_out),
             widths = "auto")

# Freeze panes below headers
freezePane(wb, "Table 1",
           firstActiveRow = 3)

saveWorkbook(wb,
             "Results/table_1_all_cohorts.xlsx",
             overwrite = TRUE)

cat("\nSaved Results/table_1_all_cohorts.xlsx\n")
cat("Spanning headers: Full cohort | ",
    "Propensity-score matched cohort\n")


