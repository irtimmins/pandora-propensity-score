# =============================================
# 16 - Supplementary Table: Sensitivity analyses
# Full cohort regression (MI melogit) +
# Doubly robust (balancer) side by side columns
# Binary outcomes + LOS in two panels
# =============================================

library(openxlsx)

# Load saved results
results_reg <- readRDS(
  "Results/standard_regression_OR_mi.rds")
results_bw  <- readRDS(
  "Results/doubly_robust_OR_mi.rds")
results_los_reg <- readRDS(
  "Results/los_regression_mi.rds")
results_los_dr  <- readRDS(
  "Results/los_doubly_robust_mi.rds")

# Format p-values consistently:
# <0.001 for very small values
# 2 significant figures otherwise
fmt_p <- function(p) {
  if (is.na(p))  return(NA_character_)
  if (p < 0.001) return("<0.001")
  # 2 significant figures, trailing zeros kept
  # e.g. 0.0013, 0.076, 0.080
  formatC(p, format = "g", digits = 2, flag = "#")
}

# Event counts from full cohort
get_events_full <- function(outcome_var) {
  out     <- as.integer(df[[outcome_var]])
  glp1    <- df$GLP1_use
  n_glp1  <- sum(glp1 == "Yes",  na.rm = TRUE)
  n_ctrl  <- sum(glp1 == "No",   na.rm = TRUE)
  ev_glp1 <- sum(out == 1 & glp1 == "Yes",
                 na.rm = TRUE)
  ev_ctrl <- sum(out == 1 & glp1 == "No",
                 na.rm = TRUE)
  pct_glp1 <- round(100 * ev_glp1 / n_glp1, 1)
  pct_ctrl <- round(100 * ev_ctrl / n_ctrl, 1)
  
  # Format with commas for counts >= 1000
  fmt_count <- function(n) {
    formatC(n, format = "d", big.mark = ",")
  }
  
  data.frame(
    n_glp1   = n_glp1,
    n_ctrl   = n_ctrl,
    glp1_str = paste0(fmt_count(ev_glp1), "/",
                      fmt_count(n_glp1),
                      " (", pct_glp1, "%)"),
    ctrl_str = paste0(fmt_count(ev_ctrl), "/",
                      fmt_count(n_ctrl),
                      " (", pct_ctrl, "%)"),
    stringsAsFactors = FALSE
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

# Build binary rows
binary_rows <- do.call(rbind, lapply(
  binary_outcomes, function(oc) {
    
    ev  <- get_events_full(oc)
    reg <- results_reg[results_reg$outcome == oc, ]
    dr  <- results_bw[results_bw$outcome  == oc, ]
    
    data.frame(
      Outcome  = binary_labels[oc],
      GLP1_n   = ev$glp1_str,
      Ctrl_n   = ev$ctrl_str,
      Reg_OR   = fmt_or(reg$OR,
                        reg$lower,
                        reg$upper),
      Reg_p    = fmt_p(reg$p),
      DR_OR    = fmt_or(dr$OR,
                        dr$lower,
                        dr$upper),
      DR_p     = fmt_p(dr$p),
      stringsAsFactors = FALSE
    )
  }))

# LOS descriptive from full cohort
los_full <- df %>%
  haven::zap_labels() %>%
  dplyr::select(id, los, GLP1_use)

los_glp1_full <- as.numeric(
  los_full$los[los_full$GLP1_use == "Yes"])
los_ctrl_full <- as.numeric(
  los_full$los[los_full$GLP1_use == "No"])

n_los_glp1 <- sum(!is.na(los_glp1_full))
n_los_ctrl <- sum(!is.na(los_ctrl_full))

los_glp1_desc <- paste0(
  formatC(mean(los_glp1_full, na.rm = TRUE),
          format = "f", digits = 1),
  " (",
  formatC(sd(los_glp1_full, na.rm = TRUE),
          format = "f", digits = 1), ")")

los_ctrl_desc <- paste0(
  formatC(mean(los_ctrl_full, na.rm = TRUE),
          format = "f", digits = 1),
  " (",
  formatC(sd(los_ctrl_full, na.rm = TRUE),
          format = "f", digits = 1), ")")

fmt_md <- function(est, lower, upper) {
  paste0(
    formatC(as.numeric(est),
            format = "f", digits = 1),
    " (",
    formatC(as.numeric(lower),
            format = "f", digits = 1),
    " to ",
    formatC(as.numeric(upper),
            format = "f", digits = 1),
    ")"
  )
}

los_row <- data.frame(
  Outcome  = paste0("Total length of stay (days)",
                    " (n=",
                    formatC(n_los_glp1 + n_los_ctrl,
                            format   = "d",
                            big.mark = ","),
                    ")"),
  GLP1_n   = los_glp1_desc,
  Ctrl_n   = los_ctrl_desc,
  Reg_OR   = fmt_md(results_los_reg$estimate,
                    results_los_reg$lower,
                    results_los_reg$upper),
  Reg_p    = fmt_p(results_los_reg$p),
  DR_OR    = fmt_md(results_los_dr$estimate,
                    results_los_dr$lower,
                    results_los_dr$upper),
  DR_p     = fmt_p(results_los_dr$p),
  stringsAsFactors = FALSE
)

cat("\n-----Binary rows-----\n")
print(binary_rows, row.names = FALSE)

cat("\n-----LOS row-----\n")
print(los_row, row.names = FALSE)

# -----EXPORT TO EXCEL-----
wb <- createWorkbook()
addWorksheet(wb, "Supplementary")

style_title <- createStyle(
  textDecoration = "bold",
  fontSize       = 11
)
style_subheader <- createStyle(
  textDecoration = "bold",
  halign         = "center"
)
style_span <- createStyle(
  halign         = "center",
  textDecoration = "bold",
  border         = "Bottom"
)
style_text <- createStyle(numFmt = "TEXT")

n_glp1_full <- formatC(
  sum(df$GLP1_use == "Yes", na.rm = TRUE),
  format = "d", big.mark = ",")
n_ctrl_full <- formatC(
  sum(df$GLP1_use == "No", na.rm = TRUE),
  format = "d", big.mark = ",")
n_total <- formatC(
  sum(df$GLP1_use %in% c("Yes", "No"),
      na.rm = TRUE),
  format = "d", big.mark = ",")

# Row 1: title
writeData(wb, "Supplementary",
          x = paste0(
            "Supplementary Table: Sensitivity ",
            "analyses -- full cohort (n=",
            n_total, ")"),
          startRow = 1, startCol = 1)
addStyle(wb, "Supplementary",
         style = style_title, rows = 1, cols = 1)

# Row 2: sample size subheaders
writeData(wb, "Supplementary",
          x        = paste0("GLP-1 (n=",
                            n_glp1_full, ")"),
          startRow = 2, startCol = 2)
writeData(wb, "Supplementary",
          x        = paste0("Control (n=",
                            n_ctrl_full, ")"),
          startRow = 2, startCol = 3)
addStyle(wb, "Supplementary",
         style      = style_subheader,
         rows       = 2, cols = 2:3,
         gridExpand = TRUE)

# Row 3: method spanning headers
writeData(wb, "Supplementary",
          x        = "Mixed model regression",
          startRow = 3, startCol = 4)
mergeCells(wb, "Supplementary",
           cols = 4:5, rows = 3)

writeData(wb, "Supplementary",
          x        = "Doubly robust estimator (Balancer weights)",
          startRow = 3, startCol = 6)
mergeCells(wb, "Supplementary",
           cols = 6:7, rows = 3)

addStyle(wb, "Supplementary",
         style      = style_span,
         rows       = 3, cols = 4:7,
         gridExpand = TRUE)

# -----BINARY PANEL-----
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
            x        = binary_headers[j],
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

# -----CONTINUOUS PANEL-----
cont_label_row  <- binary_start +
  nrow(binary_rows) + 1
cont_header_row <- cont_label_row + 1
cont_data_row   <- cont_header_row + 1

writeData(wb, "Supplementary",
          x = "Continuous outcomes",
          startRow = cont_label_row,
          startCol = 1)
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
            x        = cont_headers[j],
            startRow = cont_header_row,
            startCol = j)
}
addStyle(wb, "Supplementary",
         style      = style_subheader,
         rows       = cont_header_row,
         cols       = 1:7,
         gridExpand = TRUE)

writeData(wb, "Supplementary",
          x        = los_row,
          startRow = cont_data_row,
          startCol = 1,
          colNames = FALSE)
addStyle(wb, "Supplementary",
         style      = style_text,
         rows       = cont_data_row,
         cols       = 1:7,
         gridExpand = TRUE)

setColWidths(wb, "Supplementary",
             cols   = 1:7,
             widths = c(32, 18, 18, 28, 8, 28, 8))

freezePane(wb, "Supplementary",
           firstActiveRow = 6)

saveWorkbook(
  wb,
  "Results/supp_table_sensitivity.xlsx",
  overwrite = TRUE)

cat("\nSaved Results/supp_table_sensitivity.xlsx\n")