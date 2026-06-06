# =============================================
# 14 - Table 4: Pancreatitis severity
# Organ failure (respiratory, cardiovascular,
# renal), critical care admission, local
# complication, and severity classification.
# Full cohort and matched cohort side by side.
# =============================================

df_raw_sev <- readRDS("Data/pandora_clean_r.rds") 

# Full cohort -- join GLP1_use from df since
# df_raw_sev comes from the saved clean file
sev_data <- df_raw_sev %>%
  mutate(
    resp_failure_bin  = factor(
      as.integer(resp_failure_bin),
      levels = 0:1, labels = c("No", "Yes")),
    cvs_failure_bin   = factor(
      as.integer(cvs_failure_bin),
      levels = 0:1, labels = c("No", "Yes")),
    renal_failure_bin = factor(
      as.integer(renal_failure_bin),
      levels = 0:1, labels = c("No", "Yes")),
    organ_failure_severe = factor(
      as.integer(organ_failure_severe),
      levels = 0:1, labels = c("No", "Yes")),
    critical_care_adm_bin = factor(
      as.integer(critical_care_adm_bin),
      levels = 0:1, labels = c("No", "Yes")),
    local_complication = factor(
      as.integer(local_complication),
      levels = 0:1, labels = c("No", "Yes")),
    severity_3 = factor(
      as.integer(severity),
      levels = 0:2,
      labels = c("Mild", "Moderate", "Severe")),
    GLP1_use = factor(as.character(GLP1_use),
                      levels = c("No", "Yes")))

# Matched cohort -- join all severity variables
# from the raw file since they are not all carried
# through the imputation and matching
matched_sev <- matched_list[[1]] %>%
  haven::zap_labels() %>%
  dplyr::select(-any_of(c("resp_failure_bin",
                          "cvs_failure_bin",
                          "renal_failure_bin",
                          "organ_failure_severe",
                          "critical_care_adm_bin",
                          "local_complication",
                          "severity"))) %>%
  dplyr::left_join(
    df_raw_sev %>%
      dplyr::select(id, resp_failure_bin,
                    cvs_failure_bin,
                    renal_failure_bin,
                    organ_failure_severe,
                    critical_care_adm_bin,
                    local_complication,
                    severity),
    by = "id") %>%
  mutate(
    resp_failure_bin  = factor(
      as.integer(resp_failure_bin),
      levels = 0:1, labels = c("No", "Yes")),
    cvs_failure_bin   = factor(
      as.integer(cvs_failure_bin),
      levels = 0:1, labels = c("No", "Yes")),
    renal_failure_bin = factor(
      as.integer(renal_failure_bin),
      levels = 0:1, labels = c("No", "Yes")),
    organ_failure_severe = factor(
      as.integer(organ_failure_severe),
      levels = 0:1, labels = c("No", "Yes")),
    critical_care_adm_bin = factor(
      as.integer(critical_care_adm_bin),
      levels = 0:1, labels = c("No", "Yes")),
    local_complication = factor(
      as.integer(local_complication),
      levels = 0:1, labels = c("No", "Yes")),
    severity_3 = factor(
      as.integer(severity),
      levels = 0:2,
      labels = c("Mild", "Moderate", "Severe")),
    GLP1_use = factor(GLP1_use,
                      levels = c("No", "Yes")))

cat("Full cohort resp failure:\n")
print(table(sev_data$resp_failure_bin,
            sev_data$GLP1_use, useNA = "always"))
cat("\nMatched organ failure:\n")
print(table(matched_sev$organ_failure_severe,
            matched_sev$GLP1_use, useNA = "always"))

# All severity variables in clinical order
sev_vars <- c("resp_failure_bin",
              "cvs_failure_bin",
              "renal_failure_bin",
              "organ_failure_severe",
              "critical_care_adm_bin",
              "local_complication",
              "severity_3")

tab_sev_full <- CreateTableOne(
  vars       = sev_vars,
  strata     = "GLP1_use",
  data       = sev_data,
  factorVars = sev_vars,
  addOverall = TRUE)

tab_sev_matched <- CreateTableOne(
  vars       = sev_vars,
  strata     = "GLP1_use",
  data       = matched_sev,
  factorVars = sev_vars,
  addOverall = TRUE)

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

row_order <- c(
  rownames(mat_full),
  setdiff(rownames(mat_matched), rownames(mat_full)))

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

combined <- base::cbind(mat_full_al, mat_matched_al)

# Remove test and level columns
combined <- combined[,
                     !grepl("_test$", colnames(combined)) &
                       !colnames(combined) %in%
                       c("Full_level", "Matched_level"),
                     drop = FALSE]

rownames(combined) <- gsub("__", " -- ",
                           rownames(combined))
rownames(combined) <- gsub(" \\(%\\) -- ", " -- ",
                           rownames(combined))

var_labels <- c(
  "n"                                  = "N",
  "resp_failure_bin (%)"               = "Respiratory failure",
  "resp_failure_bin -- No"             = "No",
  "resp_failure_bin -- Yes"            = "Yes",
  "cvs_failure_bin (%)"                = "Cardiovascular failure",
  "cvs_failure_bin -- No"              = "No",
  "cvs_failure_bin -- Yes"             = "Yes",
  "renal_failure_bin (%)"              = "Renal failure",
  "renal_failure_bin -- No"            = "No",
  "renal_failure_bin -- Yes"           = "Yes",
  "organ_failure_severe (%)"           = "Organ failure >48 hours",
  "organ_failure_severe -- No"         = "No",
  "organ_failure_severe -- Yes"        = "Yes",
  "critical_care_adm_bin (%)"          = "Admission to critical care (HDU/ITU)",
  "critical_care_adm_bin -- No"        = "No",
  "critical_care_adm_bin -- Yes"       = "Yes",
  "local_complication (%)"             = "Local complication",
  "local_complication -- No"           = "No",
  "local_complication -- Yes"          = "Yes",
  "severity_3 (%)"                     = "Severity",
  "severity_3 -- Mild"                 = "Mild",
  "severity_3 -- Moderate"             = "Moderate",
  "severity_3 -- Severe"               = "Severe"
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
  combined)

n_row      <- which(combined_final[, "Variable"] == "N")
n_data_row <- which(grepl("^n -- $",
                          combined_final[, "Level"]))

if (length(n_row) > 0 & length(n_data_row) > 0) {
  combined_final[n_row, 3:ncol(combined_final)] <-
    combined_final[n_data_row, 3:ncol(combined_final)]
  combined_final <- combined_final[-n_data_row, ,
                                   drop = FALSE]
}

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

fmt_p_cell <- function(cell) {
  if (grepl("\\(", cell)) return(cell)
  if (cell == "" | is.na(cell)) return(cell)
  p <- suppressWarnings(as.numeric(cell))
  if (is.na(p))  return(cell)
  if (p < 0.001) return("<0.001")
  if (p >= 1)    return("1.00")
  formatC(p, format = "g", digits = 2, flag = "#")
}

combined_fmt <- apply(combined_final, c(1, 2),
                      format_cell_commas)
combined_fmt <- apply(combined_fmt,   c(1, 2),
                      add_percent)

p_col_names <- colnames(combined_fmt)[
  grepl("_p$", colnames(combined_fmt)) &
    !grepl("Overall|No|Yes|level|Variable|Level",
           colnames(combined_fmt))]

for (col in p_col_names) {
  combined_fmt[, col] <- sapply(
    combined_fmt[, col], fmt_p_cell)
}

rownames(combined_fmt) <- NULL

cat("\nTable 4\n")
print(combined_fmt)

df_out <- as.data.frame(combined_fmt,
                        stringsAsFactors = FALSE)

wb <- createWorkbook()
addWorksheet(wb, "Table 4")

style_span <- createStyle(
  halign         = "center",
  textDecoration = "bold",
  border         = "Bottom")
style_subheader <- createStyle(
  halign         = "center",
  textDecoration = "bold")
style_text <- createStyle(numFmt = "TEXT")

n_data_cols    <- ncol(df_out) - 2
full_coh_start <- 3
full_coh_end   <- full_coh_start + (n_data_cols / 2) - 1
matched_start  <- full_coh_end + 1
matched_end    <- ncol(df_out)

writeData(wb, "Table 4",
          x = "Full cohort",
          startRow = 1, startCol = full_coh_start)
mergeCells(wb, "Table 4",
           cols = full_coh_start:full_coh_end, rows = 1)

writeData(wb, "Table 4",
          x = "Propensity-score matched cohort",
          startRow = 1, startCol = matched_start)
mergeCells(wb, "Table 4",
           cols = matched_start:matched_end, rows = 1)

addStyle(wb, "Table 4",
         style = style_span, rows = 1,
         cols  = full_coh_start:matched_end,
         gridExpand = TRUE)

subheaders <- colnames(df_out)
subheaders <- gsub("Full_|Matched_", "", subheaders)
subheaders[1] <- "Variable"
subheaders[2] <- "Level"

for (j in seq_along(subheaders)) {
  writeData(wb, "Table 4",
            x = subheaders[j],
            startRow = 2, startCol = j)
}
addStyle(wb, "Table 4",
         style = style_subheader, rows = 2,
         cols  = 1:ncol(df_out), gridExpand = TRUE)

writeData(wb, "Table 4",
          x        = df_out,
          startRow = 3, startCol = 1,
          colNames = FALSE)
addStyle(wb, "Table 4",
         style      = style_text,
         rows       = 3:(nrow(df_out) + 3),
         cols       = 1:ncol(df_out),
         gridExpand = TRUE)

setColWidths(wb, "Table 4",
             cols = 1:ncol(df_out), widths = "auto")
freezePane(wb, "Table 4", firstActiveRow = 3)

saveWorkbook(wb,
             "Results/table_4_severity.xlsx",
             overwrite = TRUE)

cat("Saved Results/table_4_severity.xlsx\n")