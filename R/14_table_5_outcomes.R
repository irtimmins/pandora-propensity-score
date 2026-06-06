# =============================================
# 15 - Table 5: Primary outcomes
# Propensity score matched cohort
# Binary outcomes + LOS in two panels
# =============================================

library(openxlsx)

# -----BINARY OUTCOMES-----
# Read from saved MI PS matching results
results_mi <- readRDS("Results/propensity_score_OR_mi.rds")

# Event counts from matched dataset 1 as representative
get_events <- function(outcome_var) {
    mdf     <- matched_list[[1]] %>%
        haven::zap_labels()
    out     <- as.integer(mdf[[outcome_var]])
    glp1    <- mdf$GLP1_use
    n_glp1  <- sum(glp1 == "Yes")
    n_ctrl  <- sum(glp1 == "No")
    ev_glp1 <- sum(out == 1 & glp1 == "Yes",
                   na.rm = TRUE)
    ev_ctrl <- sum(out == 1 & glp1 == "No",
                   na.rm = TRUE)
    data.frame(
        n_glp1  = n_glp1,
        n_ctrl  = n_ctrl,
        ev_glp1 = ev_glp1,
        ev_ctrl = ev_ctrl,
        pct_glp1 = round(100 * ev_glp1 / n_glp1, 1),
        pct_ctrl = round(100 * ev_ctrl / n_ctrl, 1)
    )
}

binary_outcomes <- c("composite", "severity_bin",
                     "mort90", "local_complication",
                     "critical_care_adm_bin",
                     "readm90")

binary_labels <- c(
  composite             = "Composite outcome",
  severity_bin          = "Severe pancreatitis",
  mort90                = "90-day mortality",
  local_complication    = "Local complication",
  critical_care_adm_bin = "Critical care admission",
  readm90               = "90-day readmission"
)

# Format OR and CI to 2 decimal places
fmt_or <- function(or, lower, upper) {
  paste0(
    formatC(as.numeric(or),    format = "f", digits = 2),
    " (",
    formatC(as.numeric(lower), format = "f", digits = 2),
    " to ",
    formatC(as.numeric(upper), format = "f", digits = 2),
    ")"
  )
}

# Build binary outcomes panel
binary_rows <- do.call(rbind, lapply(
    binary_outcomes, function(oc) {

    ev   <- get_events(oc)
    res  <- results_mi[results_mi$outcome == oc, ]

    data.frame(
        Outcome  = binary_labels[oc],
        GLP1_n   = paste0(ev$ev_glp1, "/", ev$n_glp1,
                          " (", ev$pct_glp1, "%)"),
        Ctrl_n   = paste0(ev$ev_ctrl, "/", ev$n_ctrl,
                          " (", ev$pct_ctrl, "%)"),
        Statistic = fmt_or(res$OR, res$lower, res$upper),
        p        = fmt_p(res$p),
        stringsAsFactors = FALSE
    )
}))

# -----LENGTH OF STAY-----
# Ran from propensity score script.

# Ensure all data frames have identical column names
col_names <- c("Outcome", "GLP1_n", "Ctrl_n",
               "Statistic", "p")

# Reformat binary_rows to ensure correct names
names(binary_rows) <- col_names

# Separator with matching names
separator <- data.frame(
  Outcome   = "",
  GLP1_n    = "",
  Ctrl_n    = "",
  Statistic = "",
  p         = "",
  stringsAsFactors = FALSE
)
names(separator) <- col_names

# LOS row with matching names
names(los_row) <- col_names

# Trim los_row to first 5 columns only
los_row <- los_row[, 1:5]
names(los_row) <- col_names

# Now combine
combined_table <- rbind(binary_rows,
                        separator,
                        los_row)

cat("\n-----Table 5-----\n")
print(combined_table, row.names = FALSE)



# -----EXPORT TO EXCEL-----
wb <- createWorkbook()
addWorksheet(wb, "Table 5")

style_title <- createStyle(
  textDecoration = "bold",
  fontSize       = 11
)
style_subheader <- createStyle(
  textDecoration = "bold",
  halign         = "center"
)
style_text   <- createStyle(numFmt = "TEXT")
style_italic <- createStyle(
  textDecoration = "italic"
)

# Sample sizes from matched dataset 1
n_glp1 <- sum(matched_list[[1]]$GLP1_use == "Yes")
n_ctrl  <- sum(matched_list[[1]]$GLP1_use == "No")

# -----ROW LAYOUT-----
# Row 1:  Table title
# Row 2:  GLP-1 / Control sample size headers
# Row 3:  "Binary outcomes" panel label
# Row 4:  Binary column headers
# Row 5+: Binary data rows
# Then:   "Continuous outcomes" panel label
#         Continuous column headers
#         LOS data row
#         Blank row
#         Footnote

# Row 1: title
writeData(wb, "Table 5",
          x        = paste0(
            "Outcomes in propensity score matched ",
            "cohort, n=",
            n_glp1 + n_ctrl),
          startRow = 1, startCol = 1)
addStyle(wb, "Table 5",
         style = style_title,
         rows = 1, cols = 1)

# Row 2: sample size subheaders over data columns
writeData(wb, "Table 5",
          x        = paste0("GLP-1 (n=", n_glp1, ")"),
          startRow = 2, startCol = 2)
writeData(wb, "Table 5",
          x        = paste0("Control (n=", n_ctrl, ")"),
          startRow = 2, startCol = 3)
addStyle(wb, "Table 5",
         style      = style_subheader,
         rows       = 2, cols       = 2:3,
         gridExpand = TRUE)

# Row 3: binary panel label
writeData(wb, "Table 5",
          x = "Binary outcomes",
          startRow = 3, startCol = 1)
addStyle(wb, "Table 5",
         style = style_title,
         rows = 3, cols = 1)

# Row 4: binary column headers
binary_headers <- c("Outcome",
                    "Events, n/N (%)",
                    "Events, n/N (%)",
                    "OR (95% CI)",
                    "p")
for (j in seq_along(binary_headers)) {
  writeData(wb, "Table 5",
            x        = binary_headers[j],
            startRow = 4, startCol = j)
}
addStyle(wb, "Table 5",
         style      = style_subheader,
         rows       = 4, cols       = 1:5,
         gridExpand = TRUE)

# Rows 5+: binary data
binary_data_start <- 5
writeData(wb, "Table 5",
          x        = binary_rows,
          startRow = binary_data_start,
          startCol = 1,
          colNames = FALSE)
addStyle(wb, "Table 5",
         style      = style_text,
         rows       = binary_data_start:(
           binary_data_start +
             nrow(binary_rows) - 1),
         cols       = 1:5,
         gridExpand = TRUE)

# Continuous panel
cont_label_row  <- binary_data_start +
  nrow(binary_rows) + 1
cont_header_row <- cont_label_row + 1
cont_data_row   <- cont_header_row + 1
footnote_row    <- cont_data_row + 2

writeData(wb, "Table 5",
          x = "Continuous outcomes",
          startRow = cont_label_row,
          startCol = 1)
addStyle(wb, "Table 5",
         style = style_title,
         rows  = cont_label_row, cols = 1)

# Continuous column headers
cont_headers <- c("Outcome",
                  "Mean (SD), days",
                  "Mean (SD), days",
                  "Mean difference (95% CI), days",
                  "p")
for (j in seq_along(cont_headers)) {
  writeData(wb, "Table 5",
            x        = cont_headers[j],
            startRow = cont_header_row,
            startCol = j)
}
addStyle(wb, "Table 5",
         style      = style_subheader,
         rows       = cont_header_row,
         cols       = 1:5,
         gridExpand = TRUE)

# LOS data row
writeData(wb, "Table 5",
          x        = los_row,
          startRow = cont_data_row,
          startCol = 1,
          colNames = FALSE)
addStyle(wb, "Table 5",
         style      = style_text,
         rows       = cont_data_row,
         cols       = 1:5,
         gridExpand = TRUE)

# Remove these lines:
# writeData(wb, "Table 5", x = paste0("* 90-day ...
# addStyle(wb, "Table 5", style = style_italic ...

# Replace footnote_row calculation with just spacing
cont_label_row  <- binary_data_start +
  nrow(binary_rows) + 1
cont_header_row <- cont_label_row + 1
cont_data_row   <- cont_header_row + 1
# footnote_row removed

# Column widths
setColWidths(wb, "Table 5",
             cols   = 1:5,
             widths = c(32, 18, 18, 32, 8))

# Freeze top rows
freezePane(wb, "Table 5",
           firstActiveRow = 5)

saveWorkbook(wb,
             "Results/table_5_outcomes.xlsx",
             overwrite = TRUE)

cat("\nSaved Results/table_5_outcomes.xlsx\n")