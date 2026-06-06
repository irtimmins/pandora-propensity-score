# =============================================
# 15 - Table 5: Primary outcomes
# Propensity score matched cohort
# Binary outcomes and length of stay
# =============================================

library(openxlsx)

results_mi  <- readRDS("Results/propensity_score_OR_mi.rds")
results_los <- readRDS("Results/los_mi.rds")

# Event counts from matched dataset 1
get_events <- function(outcome_var) {
  mdf  <- matched_list[[1]] %>% haven::zap_labels()
  out  <- as.integer(mdf[[outcome_var]])
  glp1 <- mdf$GLP1_use
  n_glp1  <- sum(glp1 == "Yes")
  n_ctrl  <- sum(glp1 == "No")
  ev_glp1 <- sum(out == 1 & glp1 == "Yes", na.rm = TRUE)
  ev_ctrl <- sum(out == 1 & glp1 == "No",  na.rm = TRUE)
  data.frame(
    n_glp1   = n_glp1,
    n_ctrl   = n_ctrl,
    ev_glp1  = ev_glp1,
    ev_ctrl  = ev_ctrl,
    pct_glp1 = round(100 * ev_glp1 / n_glp1, 1),
    pct_ctrl = round(100 * ev_ctrl / n_ctrl,  1))
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

fmt_p <- function(p) {
  if (is.na(p))  return(NA_character_)
  if (p < 0.001) return("<0.001")
  if (p >= 1)    return("1.00")
  formatC(p, format = "g", digits = 2, flag = "#")
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

# Column order: Outcome | Control n/N (%) | GLP-1 n/N (%) | OR | p
# consistent with No before Yes in all other tables
binary_rows <- do.call(rbind, lapply(
  binary_outcomes, function(oc) {
    ev  <- get_events(oc)
    res <- results_mi[results_mi$outcome == oc, ]
    data.frame(
      Outcome   = binary_labels[oc],
      Ctrl_n    = paste0(ev$ev_ctrl,  "/", ev$n_ctrl,
                         " (", ev$pct_ctrl,  "%)"),
      GLP1_n    = paste0(ev$ev_glp1, "/", ev$n_glp1,
                         " (", ev$pct_glp1, "%)"),
      Statistic = fmt_or(res$OR, res$lower, res$upper),
      p         = fmt_p(res$p),
      stringsAsFactors = FALSE)
  }))

# LOS descriptive from matched dataset 1
los_data <- df %>%
  haven::zap_labels() %>%
  dplyr::select(id, los)

mdf1_los <- matched_list[[1]] %>%
  haven::zap_labels() %>%
  dplyr::select(-any_of("los")) %>%
  dplyr::left_join(los_data, by = "id")

los_glp1 <- as.numeric(mdf1_los$los[mdf1_los$GLP1_use == "Yes"])
los_ctrl  <- as.numeric(mdf1_los$los[mdf1_los$GLP1_use == "No"])

los_row <- data.frame(
  Outcome   = "Total length of stay (days)",
  Ctrl_n    = paste0(
    formatC(mean(los_ctrl, na.rm = TRUE),
            format = "f", digits = 1),
    " (",
    formatC(sd(los_ctrl, na.rm = TRUE),
            format = "f", digits = 1), ")"),
  GLP1_n    = paste0(
    formatC(mean(los_glp1, na.rm = TRUE),
            format = "f", digits = 1),
    " (",
    formatC(sd(los_glp1, na.rm = TRUE),
            format = "f", digits = 1), ")"),
  Statistic = fmt_md(results_los$estimate,
                     results_los$lower,
                     results_los$upper),
  p         = fmt_p(results_los$p),
  stringsAsFactors = FALSE)

cat("Binary outcomes:\n")
print(binary_rows, row.names = FALSE)
cat("\nLOS:\n")
print(los_row, row.names = FALSE)

# Export to Excel
n_glp1 <- sum(matched_list[[1]]$GLP1_use == "Yes")
n_ctrl  <- sum(matched_list[[1]]$GLP1_use == "No")

wb <- createWorkbook()
addWorksheet(wb, "Table 5")

style_title <- createStyle(textDecoration = "bold",
                           fontSize       = 11)
style_subheader <- createStyle(textDecoration = "bold",
                               halign         = "center")
style_text <- createStyle(numFmt = "TEXT")

# Row 1: title
writeData(wb, "Table 5",
          x = paste0("Outcomes in propensity score ",
                     "matched cohort (n=",
                     n_glp1 + n_ctrl, ")"),
          startRow = 1, startCol = 1)
addStyle(wb, "Table 5",
         style = style_title, rows = 1, cols = 1)

# Row 2: sample size subheaders
writeData(wb, "Table 5",
          x        = paste0("Control (n=", n_ctrl, ")"),
          startRow = 2, startCol = 2)
writeData(wb, "Table 5",
          x        = paste0("GLP-1 (n=", n_glp1, ")"),
          startRow = 2, startCol = 3)
addStyle(wb, "Table 5",
         style      = style_subheader,
         rows       = 2, cols = 2:3,
         gridExpand = TRUE)

# Row 3: binary panel label
writeData(wb, "Table 5",
          x = "Binary outcomes",
          startRow = 3, startCol = 1)
addStyle(wb, "Table 5",
         style = style_title, rows = 3, cols = 1)

# Row 4: binary column headers
binary_headers <- c("Outcome",
                    "Events, n/N (%)",
                    "Events, n/N (%)",
                    "OR (95% CI)", "p")
for (j in seq_along(binary_headers)) {
  writeData(wb, "Table 5",
            x = binary_headers[j],
            startRow = 4, startCol = j)
}
addStyle(wb, "Table 5",
         style      = style_subheader,
         rows       = 4, cols = 1:5,
         gridExpand = TRUE)

# Binary data rows
binary_data_start <- 5
writeData(wb, "Table 5",
          x        = binary_rows,
          startRow = binary_data_start,
          startCol = 1,
          colNames = FALSE)
addStyle(wb, "Table 5",
         style      = style_text,
         rows       = binary_data_start:(
           binary_data_start + nrow(binary_rows) - 1),
         cols       = 1:5,
         gridExpand = TRUE)

# Continuous panel
cont_label_row  <- binary_data_start + nrow(binary_rows) + 1
cont_header_row <- cont_label_row + 1
cont_data_row   <- cont_header_row + 1

writeData(wb, "Table 5",
          x = "Continuous outcomes",
          startRow = cont_label_row, startCol = 1)
addStyle(wb, "Table 5",
         style = style_title,
         rows  = cont_label_row, cols = 1)

cont_headers <- c("Outcome",
                  "Mean (SD), days",
                  "Mean (SD), days",
                  "Mean difference (95% CI), days",
                  "p")
for (j in seq_along(cont_headers)) {
  writeData(wb, "Table 5",
            x = cont_headers[j],
            startRow = cont_header_row, startCol = j)
}
addStyle(wb, "Table 5",
         style      = style_subheader,
         rows       = cont_header_row, cols = 1:5,
         gridExpand = TRUE)

writeData(wb, "Table 5",
          x        = los_row,
          startRow = cont_data_row,
          startCol = 1,
          colNames = FALSE)
addStyle(wb, "Table 5",
         style      = style_text,
         rows       = cont_data_row, cols = 1:5,
         gridExpand = TRUE)

setColWidths(wb, "Table 5",
             cols   = 1:5,
             widths = c(32, 18, 18, 32, 8))

freezePane(wb, "Table 5", firstActiveRow = 5)

saveWorkbook(wb,
             "Results/table_5_outcomes.xlsx",
             overwrite = TRUE)

cat("Saved Results/table_5_outcomes.xlsx\n")