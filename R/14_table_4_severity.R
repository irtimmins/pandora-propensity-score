# =============================================
# 14 - Table 4, pancreatitus severity
# Individual resp/cvs/renal failure + critical care
# =============================================

# Read raw numeric values directly from saved clean data
# before script 14 converted them to factors
df_raw_sev <- readRDS("Data/pandora_clean_r.rds") %>%
  haven::zap_labels() %>%
  dplyr::select(id,
                organ_failure_severe,
                critical_care_adm_bin,
                local_complication,
                severity)

cat("Raw check:\n")
print(table(df_raw_sev$organ_failure_severe,
            useNA = "always"))
print(table(df_raw_sev$local_complication,
            useNA = "always"))

sev_data <- df_raw_sev %>%
  dplyr::left_join(
    df %>%
      haven::zap_labels() %>%
      dplyr::select(id, GLP1_use),
    by = "id"
  ) %>%
  mutate(
    organ_failure_severe = factor(
      as.integer(organ_failure_severe),
      levels = 0:1,
      labels = c("No", "Yes")),
    critical_care_adm_bin = factor(
      as.integer(critical_care_adm_bin),
      levels = 0:1,
      labels = c("No", "Yes")),
    local_complication = factor(
      as.integer(local_complication),
      levels = 0:1,
      labels = c("No", "Yes")),
    severity_3 = factor(
      as.integer(severity),
      levels = 0:2,
      labels = c("Mild", "Moderate", "Severe")),
    GLP1_use = factor(GLP1_use,
                      levels = c("No", "Yes"))
  )

matched_sev <- matched_list[[1]] %>%
  haven::zap_labels() %>%
  dplyr::select(-any_of(c("organ_failure_severe",
                          "critical_care_adm_bin",
                          "local_complication",
                          "severity"))) %>%
  dplyr::left_join(
    df_raw_sev,
    by = "id"
  ) %>%
  mutate(
    organ_failure_severe = factor(
      as.integer(organ_failure_severe),
      levels = 0:1,
      labels = c("No", "Yes")),
    critical_care_adm_bin = factor(
      as.integer(critical_care_adm_bin),
      levels = 0:1,
      labels = c("No", "Yes")),
    local_complication = factor(
      as.integer(local_complication),
      levels = 0:1,
      labels = c("No", "Yes")),
    severity_3 = factor(
      as.integer(severity),
      levels = 0:2,
      labels = c("Mild", "Moderate", "Severe")),
    GLP1_use = factor(GLP1_use,
                      levels = c("No", "Yes"))
  )

# Verify
cat("\nFull cohort organ failure:\n")
print(table(sev_data$organ_failure_severe,
            sev_data$GLP1_use, useNA = "always"))

cat("\nMatched organ failure:\n")
print(table(matched_sev$organ_failure_severe,
            matched_sev$GLP1_use, useNA = "always"))


sev_vars <- c("organ_failure_severe",
              "critical_care_adm_bin",
              "local_complication",
              "severity_3")

tab_sev_full <- CreateTableOne(
  vars       = sev_vars,
  strata     = "GLP1_use",
  data       = sev_data,
  factorVars = sev_vars,
  addOverall = TRUE
)

tab_sev_matched <- CreateTableOne(
  vars       = sev_vars,
  strata     = "GLP1_use",
  data       = matched_sev,
  factorVars = sev_vars,
  addOverall = TRUE
)

cat("Full cohort:\n")
print(tab_sev_full, showAllLevels = TRUE)

cat("\nMatched cohort:\n")
print(tab_sev_matched, showAllLevels = TRUE)


mat_full <- print(tab_sev_full,
                  showAllLevels = TRUE,
                  test          = TRUE,
                  smd           = FALSE,
                  printToggle   = FALSE,
                  noSpaces      = TRUE)

mat_matched <- print(tab_sev_matched,
                     showAllLevels = TRUE,
                     test          = TRUE,
                     smd           = FALSE,
                     printToggle   = FALSE,
                     noSpaces      = TRUE)

# Add unique rownames and split header/level rows
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

mat_full    <- insert_first_levels(
  add_level_rownames(mat_full))
mat_matched <- insert_first_levels(
  add_level_rownames(mat_matched))



# Directly restore N row from original matrices
n_row <- which(combined_final[, "Variable"] == "N")

if (length(n_row) > 0) {
  # Get n row from each matrix
  # mat_full and mat_matched still have "n" as rowname
  # before insert_first_levels was applied
  # Use the tableone print output directly
  
  full_n    <- as.character(mat_full["n", ])
  matched_n <- as.character(mat_matched["n", ])
  
  # Match to correct columns
  data_cols <- colnames(combined_final)[3:ncol(combined_final)]
  
  for (col in data_cols) {
    # Full cohort columns
    clean_col <- gsub("^Full_", "", col)
    if (clean_col %in% names(full_n)) {
      combined_final[n_row, col] <- full_n[clean_col]
    }
    # Matched columns
    clean_col <- gsub("^Matched_", "", col)
    if (clean_col %in% names(matched_n)) {
      combined_final[n_row, col] <- matched_n[clean_col]
    }
  }
}

cat("N row after fix:\n")
print(combined_final[n_row, ])








# Align rows
row_order <- c(
  rownames(mat_full),
  setdiff(rownames(mat_matched), rownames(mat_full))
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

mat_full_al    <- align_mat(mat_full,    row_order, "Full")
mat_matched_al <- align_mat(mat_matched, row_order, "Matched")

combined <- cbind(mat_full_al, mat_matched_al)

# Remove redundant level columns
combined <- combined[,
                     !colnames(combined) %in%
                       c("Full_level", "Matched_level"),
                     drop = FALSE]
# Remove test columns which shift the data right
combined <- combined[,
    !grepl("_test$", colnames(combined)),
    drop = FALSE]

cat("Columns after removing test:\n")
print(colnames(combined))

# Clean rownames
rownames(combined) <- gsub("__", " -- ",
                           rownames(combined))
rownames(combined) <- gsub(" \\(%\\) -- ", " -- ",
                           rownames(combined))


# Remove test columns which shift the data right
combined <- combined[,
                     !grepl("_test$", colnames(combined)),
                     drop = FALSE]

cat("Columns after removing test:\n")
print(colnames(combined))
# Display labels
var_labels <- c(
  "n"                                    = "N",
  "organ_failure_severe (%)"             = "Organ failure >48 hours",
  "organ_failure_severe -- No"           = "No",
  "organ_failure_severe -- Yes"          = "Yes",
  "critical_care_adm_bin (%)"            = "Admission to critical care",
  "critical_care_adm_bin -- No"          = "No",
  "critical_care_adm_bin -- Yes"         = "Yes",
  "local_complication (%)"               = "Local complication",
  "local_complication -- No"             = "No",
  "local_complication -- Yes"            = "Yes",
  "severity_3 (%)"                       = "Severity",
  "severity_3 -- Mild"                   = "Mild",
  "severity_3 -- Moderate"               = "Moderate",
  "severity_3 -- Severe"                 = "Severe"
)

is_header <- rownames(combined) %in%
  c("n", names(var_labels)[!grepl(" -- ",
                                  names(var_labels))])

current_rn     <- rownames(combined)
new_rn         <- var_labels[current_rn]
missing_labels <- is.na(new_rn)
new_rn[missing_labels] <- current_rn[missing_labels]
rownames(combined) <- new_rn

is_header <- rownames(combined) %in%
  var_labels[!grepl(" -- ", names(var_labels))]
is_header[rownames(combined) == "N"] <- TRUE

# Build Variable and Level columns
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

# Fix N row
n_row      <- which(combined_final[, "Variable"] == "N")
n_data_row <- which(grepl("^n -- $",
                          combined_final[, "Level"]))

if (length(n_row) > 0 & length(n_data_row) > 0) {
  combined_final[n_row, 3:ncol(combined_final)] <-
    combined_final[n_data_row, 3:ncol(combined_final)]
  combined_final <- combined_final[-n_data_row, ,
                                   drop = FALSE]
}

# Format
add_percent <- function(cell) {
  gsub("\\(([0-9]+\\.[0-9]+)\\)", "(\\1%)", cell)
}

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

# Format p-values
fmt_p <- function(cell) {
  if (cell == "" | is.na(cell)) return(cell)
  p <- suppressWarnings(as.numeric(cell))
  if (is.na(p))   return(cell)
  if (p < 0.001)  return("<0.001")
  formatC(p, format = "g", digits = 2, flag = "#")
}

combined_fmt <- apply(combined_final, c(1, 2),
                      format_cell_commas)
combined_fmt <- apply(combined_fmt,   c(1, 2),
                      add_percent)

# Apply p-value formatting to p columns only
p_cols <- grepl("_p$", colnames(combined_fmt))
for (col in which(p_cols)) {
  combined_fmt[, col] <- sapply(combined_fmt[, col],
                                fmt_p)
}

rownames(combined_fmt) <- NULL

cat("\nTable 4\n")
print(combined_fmt)

# Export to Excel
library(openxlsx)

df_out <- as.data.frame(combined_fmt,
                        stringsAsFactors = FALSE)

wb <- createWorkbook()
addWorksheet(wb, "Table 4")

style_span <- createStyle(
  halign         = "center",
  textDecoration = "bold",
  border         = "Bottom"
)
style_header <- createStyle(
  halign         = "center",
  textDecoration = "bold"
)
style_text <- createStyle(numFmt = "TEXT")

# Column layout:
# 1-2   = Variable, Level
# 3-6   = Full cohort (Overall, No, Yes, p)
# 7-10  = Matched cohort (Overall, No, Yes, p)
writeData(wb, "Table 4",
          x = "Full cohort",
          startRow = 1, startCol = 3)
mergeCells(wb, "Table 4",
           cols = 3:6, rows = 1)

writeData(wb, "Table 4",
          x = "Propensity-score matched cohort",
          startRow = 1, startCol = 7)
mergeCells(wb, "Table 4",
           cols = 7:10, rows = 1)

addStyle(wb, "Table 4",
         style = style_span, rows = 1,
         cols = 3:10, gridExpand = TRUE)

sub_headers <- c("Variable", "Level",
                 "Overall", "No", "Yes", "p",
                 "Overall", "No", "Yes", "p")

for (j in seq_along(sub_headers)) {
  writeData(wb, "Table 4",
            x = sub_headers[j],
            startRow = 2, startCol = j)
}

addStyle(wb, "Table 4",
         style = style_header, rows = 2,
         cols = 1:10, gridExpand = TRUE)

writeData(wb, "Table 4",
          x        = df_out,
          startRow = 3,
          startCol = 1,
          colNames = FALSE)

addStyle(wb, "Table 4",
         style      = style_text,
         rows       = 3:(nrow(df_out) + 3),
         cols       = 1:10,
         gridExpand = TRUE)

setColWidths(wb, "Table 4",
             cols = 1:10, widths = "auto")

freezePane(wb, "Table 4", firstActiveRow = 3)

saveWorkbook(wb,
             "Results/table_4_severity.xlsx",
             overwrite = TRUE)

cat("\nSaved Results/table_4_severity.xlsx\n")
