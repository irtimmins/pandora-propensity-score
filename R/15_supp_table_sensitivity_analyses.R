# =============================================
# 16 - Supplementary table: Sensitivity analyses
# Full cohort multilevel regression and doubly
# robust matched analysis side by side.
# Binary outcomes and length of stay.
# =============================================

library(openxlsx)

results_reg     <- readRDS("Results/standard_regression_OR_mi.rds")
results_dr      <- readRDS("Results/doubly_robust_OR_mi.rds")
results_los_reg <- readRDS("Results/los_regression_mi.rds")
results_los_dr  <- readRDS("Results/los_doubly_robust_mi.rds")


fmt_p <- function(p) {
  if (is.na(p))  return(NA_character_)
  if (p < 0.001) return("<0.001")
  if (p >= 1)    return("1.00")
  formatC(p, format = "g", digits = 2, flag = "#")
}

fmt_or <- function(or, lower, upper) {
  paste0(
    formatC(as.numeric(or),    format = "f", digits = 2),
    " (",
    formatC(as.numeric(lower), format = "f", digits = 2),
    " to ",
    formatC(as.numeric(upper), format = "f", digits = 2),
    ")")
}

fmt_md <- function(est, lower, upper) {
  paste0(
    formatC(as.numeric(est),   format = "f", digits = 1),
    " (",
    formatC(as.numeric(lower), format = "f", digits = 1),
    " to ",
    formatC(as.numeric(upper), format = "f", digits = 1),
    ")")
}

# Event counts from full cohort -- control before
# GLP-1 to match column order in Table 5
get_events_full <- function(outcome_var) {
  out     <- as.integer(df[[outcome_var]])
  glp1    <- df$GLP1_use
  n_glp1  <- sum(glp1 == "Yes", na.rm = TRUE)
  n_ctrl  <- sum(glp1 == "No",  na.rm = TRUE)
  ev_glp1 <- sum(out == 1 & glp1 == "Yes", na.rm = TRUE)
  ev_ctrl <- sum(out == 1 & glp1 == "No",  na.rm = TRUE)
  fmt_n   <- function(n) formatC(n, format = "d",
                                 big.mark = ",")
  data.frame(
    ctrl_str = paste0(fmt_n(ev_ctrl), "/", fmt_n(n_ctrl),
                      " (", round(100 * ev_ctrl / n_ctrl, 1), "%)"),
    glp1_str = paste0(fmt_n(ev_glp1), "/", fmt_n(n_glp1),
                      " (", round(100 * ev_glp1 / n_glp1, 1), "%)"),
    stringsAsFactors = FALSE)
}

binary_outcomes <- c("composite", "severity_bin",
                     "mort90", "local_complication",
                     "critical_care_adm_bin", "readm90")

binary_labels <- c(
  composite             = "Composite outcome",
  severity_bin          = "Severe pancreatitis",
  mort90                = "90-day mortality",
  local_complication    = "Local complication",
  critical_care_adm_bin = "Critical care admission",
  readm90               = "90-day readmission")

# Column order: Outcome | Control | GLP-1 | Reg OR | Reg p | DR OR | DR p
binary_rows <- do.call(rbind, lapply(
  binary_outcomes, function(oc) {
    ev  <- get_events_full(oc)
    reg <- results_reg[results_reg$outcome == oc, ]
    dr  <- results_dr[results_dr$outcome   == oc, ]
    data.frame(
      Outcome = binary_labels[oc],
      Ctrl_n  = ev$ctrl_str,
      GLP1_n  = ev$glp1_str,
      Reg_OR  = fmt_or(reg$estimate, reg$conf.low, reg$conf.high),
      Reg_p   = fmt_p(reg$p.value),
      DR_OR   = fmt_or(dr$OR,     dr$lower,   dr$upper),
      DR_p    = fmt_p(dr$p),
      stringsAsFactors = FALSE)
  }))



# LOS descriptive from full cohort
los_full <- df %>%
  dplyr::select(id, los, GLP1_use)

los_ctrl_full <- as.numeric(
  los_full$los[los_full$GLP1_use == "No"])
los_glp1_full <- as.numeric(
  los_full$los[los_full$GLP1_use == "Yes"])

los_ctrl_desc <- paste0(
  formatC(mean(los_ctrl_full, na.rm = TRUE),
          format = "f", digits = 1),
  " (",
  formatC(sd(los_ctrl_full, na.rm = TRUE),
          format = "f", digits = 1), ")")

los_glp1_desc <- paste0(
  formatC(mean(los_glp1_full, na.rm = TRUE),
          format = "f", digits = 1),
  " (",
  formatC(sd(los_glp1_full, na.rm = TRUE),
          format = "f", digits = 1), ")")

los_row <- data.frame(
  Outcome = "Total length of stay (days)",
  Ctrl_n  = los_ctrl_desc,
  GLP1_n  = los_glp1_desc,
  Reg_OR  = fmt_md(results_los_reg$estimate,
                   results_los_reg$conf.low,
                   results_los_reg$conf.high),
  Reg_p   = fmt_p(results_los_reg$p.value),
  DR_OR   = fmt_md(results_los_dr$estimate,
                   results_los_dr$lower,
                   results_los_dr$upper),
  DR_p    = fmt_p(results_los_dr$p),
  stringsAsFactors = FALSE)

cat("Binary rows:\n")
print(binary_rows, row.names = FALSE)
cat("\nLOS row:\n")
print(los_row, row.names = FALSE)

# Excel export
n_ctrl_full <- formatC(sum(df$GLP1_use == "No",
                           na.rm = TRUE),
                       format = "d", big.mark = ",")
n_glp1_full <- formatC(sum(df$GLP1_use == "Yes",
                           na.rm = TRUE),
                       format = "d", big.mark = ",")
n_total     <- formatC(sum(df$GLP1_use %in% c("No", "Yes"),
                           na.rm = TRUE),
                       format = "d", big.mark = ",")

wb <- createWorkbook()
addWorksheet(wb, "Supplementary")

style_title     <- createStyle(textDecoration = "bold",
                               fontSize       = 11)
style_subheader <- createStyle(textDecoration = "bold",
                               halign         = "center")
style_span      <- createStyle(halign         = "center",
                               textDecoration = "bold",
                               border         = "Bottom")
style_text      <- createStyle(numFmt = "TEXT")

# Row 1: title
writeData(wb, "Supplementary",
          x = paste0("Supplementary Table: Sensitivity analyses",
                     " -- full cohort (n=", n_total, ")"),
          startRow = 1, startCol = 1)
addStyle(wb, "Supplementary",
         style = style_title, rows = 1, cols = 1)

# Row 2: sample size subheaders -- control then GLP-1
writeData(wb, "Supplementary",
          x        = paste0("Control (n=", n_ctrl_full, ")"),
          startRow = 2, startCol = 2)
writeData(wb, "Supplementary",
          x        = paste0("GLP-1 (n=", n_glp1_full, ")"),
          startRow = 2, startCol = 3)
addStyle(wb, "Supplementary",
         style      = style_subheader,
         rows       = 2, cols = 2:3,
         gridExpand = TRUE)

# Row 3: method spanning headers
writeData(wb, "Supplementary",
          x        = "Multilevel logistic regression",
          startRow = 3, startCol = 4)
mergeCells(wb, "Supplementary", cols = 4:5, rows = 3)

writeData(wb, "Supplementary",
          x        = "Doubly robust (matched + adjusted)",
          startRow = 3, startCol = 6)
mergeCells(wb, "Supplementary", cols = 6:7, rows = 3)

addStyle(wb, "Supplementary",
         style      = style_span,
         rows       = 3, cols = 4:7,
         gridExpand = TRUE)

# Binary panel
writeData(wb, "Supplementary",
          x = "Binary outcomes",
          startRow = 4, startCol = 1)
addStyle(wb, "Supplementary",
         style = style_title, rows = 4, cols = 1)

binary_headers <- c("Outcome",
                    "Events, n/N (%)",
                    "Events, n/N (%)",
                    "OR (95% CI)", "p",
                    "OR (95% CI)", "p")
for (j in seq_along(binary_headers)) {
  writeData(wb, "Supplementary",
            x = binary_headers[j],
            startRow = 5, startCol = j)
}
addStyle(wb, "Supplementary",
         style      = style_subheader,
         rows       = 5, cols = 1:7,
         gridExpand = TRUE)

binary_start <- 6
writeData(wb, "Supplementary",
          x        = binary_rows,
          startRow = binary_start,
          startCol = 1,
          colNames = FALSE)
addStyle(wb, "Supplementary",
         style      = style_text,
         rows       = binary_start:(binary_start +
                                      nrow(binary_rows) - 1),
         cols       = 1:7,
         gridExpand = TRUE)

# Continuous panel
cont_label_row  <- binary_start + nrow(binary_rows) + 1
cont_header_row <- cont_label_row + 1
cont_data_row   <- cont_header_row + 1

writeData(wb, "Supplementary",
          x = "Continuous outcomes",
          startRow = cont_label_row, startCol = 1)
addStyle(wb, "Supplementary",
         style = style_title,
         rows  = cont_label_row, cols = 1)

cont_headers <- c("Outcome",
                  "Mean (SD), days",
                  "Mean (SD), days",
                  "Mean difference (95% CI)", "p",
                  "Mean difference (95% CI)", "p")
for (j in seq_along(cont_headers)) {
  writeData(wb, "Supplementary",
            x = cont_headers[j],
            startRow = cont_header_row, startCol = j)
}
addStyle(wb, "Supplementary",
         style      = style_subheader,
         rows       = cont_header_row, cols = 1:7,
         gridExpand = TRUE)

writeData(wb, "Supplementary",
          x        = los_row,
          startRow = cont_data_row,
          startCol = 1,
          colNames = FALSE)
addStyle(wb, "Supplementary",
         style      = style_text,
         rows       = cont_data_row, cols = 1:7,
         gridExpand = TRUE)

setColWidths(wb, "Supplementary",
             cols   = 1:7,
             widths = c(32, 18, 18, 28, 8, 28, 8))

freezePane(wb, "Supplementary", firstActiveRow = 6)

saveWorkbook(wb,
             "Results/supp_table_sensitivity.xlsx",
             overwrite = TRUE)

cat("Saved Results/supp_table_sensitivity.xlsx\n")