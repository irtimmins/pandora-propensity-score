# =============================================
# 13 - Table 3: Lab findings
# Continuous variables: median (IQR) by GLP-1 status
# Reported in both full cohort and matched cohort
# Kruskal-Wallis tests
# =============================================

lab_vars <- c("wcc_adm", "crp", "urea_adm",
              "glucose_adm", "bili_adm", "lactate_adm")

lab_vars <- lab_vars[lab_vars %in% names(df)]

lab_labels <- c(
  wcc_adm     = "White cell count (x10\u2079/L)",
  crp         = "CRP (mg/L)",
  urea_adm    = "Urea (mmol/L)",
  glucose_adm = "Glucose (mmol/L)",
  bili_adm    = "Bilirubin (\u03bcmol/L)",
  lactate_adm = "Lactate (mmol/L)"
)

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
fmt_miqr <- function(x) {
  paste0(
    formatC(median(x, na.rm = TRUE),
            format = "f", digits = 1),
    " (",
    formatC(quantile(x, 0.25, na.rm = TRUE),
            format = "f", digits = 1),
    "-",
    formatC(quantile(x, 0.75, na.rm = TRUE),
            format = "f", digits = 1),
    ")"
  )
}

summarise_lab <- function(var, data) {
  
  overall <- data[[var]]
  glp1    <- data[[var]][data$GLP1_use == "Yes"]
  ctrl    <- data[[var]][data$GLP1_use == "No"]
  
  kw <- kruskal.test(data[[var]] ~ data$GLP1_use)
  
  data.frame(
    variable = lab_labels[var],
    overall  = fmt_miqr(overall),
    glp1     = fmt_miqr(glp1),
    control  = fmt_miqr(ctrl),
    p_kw     = fmt_p(kw$p.value),
    stringsAsFactors = FALSE
  )
}

# Full cohort
results_labs_full <- do.call(rbind,
                             lapply(lab_vars, summarise_lab, data = df))

# Matched cohort -- join lab vars from original df
# crp already in matched_list from imputation so drop first
matched_labs <- matched_list[[1]] %>%
  haven::zap_labels() %>%
  dplyr::select(-crp) %>%
  dplyr::left_join(
    df %>%
      haven::zap_labels() %>%
      dplyr::select(id, all_of(lab_vars)),
    by = "id"
  )

results_labs_matched <- do.call(rbind,
                                lapply(lab_vars, summarise_lab,
                                       data = matched_labs))

# Combined table
results_labs_combined <- data.frame(
  Variable   = results_labs_full$variable,
  FC_Overall = results_labs_full$overall,
  FC_GLP1    = results_labs_full$glp1,
  FC_Control = results_labs_full$control,
  FC_p       = results_labs_full$p_kw,
  PS_Overall = results_labs_matched$overall,
  PS_GLP1    = results_labs_matched$glp1,
  PS_Control = results_labs_matched$control,
  PS_p       = results_labs_matched$p_kw,
  stringsAsFactors = FALSE
)

cat("\n-----Full cohort lab results-----\n")
print(results_labs_full, row.names = FALSE)

cat("\n-----Matched cohort lab results-----\n")
print(results_labs_matched, row.names = FALSE)

# Export to Excel
library(openxlsx)

wb <- createWorkbook()
addWorksheet(wb, "Lab findings")

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

# Spanning header row
# Column layout:
#   1     = Variable
#   2-5   = Full cohort (Overall, GLP-1, Control, p)
#   6-9   = Matched cohort (Overall, GLP-1, Control, p)

writeData(wb, "Lab findings",
          x        = "Full cohort",
          startRow = 1,
          startCol = 2)
mergeCells(wb, "Lab findings",
           cols = 2:5, rows = 1)

writeData(wb, "Lab findings",
          x        = "Propensity-score matched cohort",
          startRow = 1,
          startCol = 6)
mergeCells(wb, "Lab findings",
           cols = 6:9, rows = 1)

addStyle(wb, "Lab findings",
         style      = style_span,
         rows       = 1,
         cols       = 2:9,
         gridExpand = TRUE)

# Sub-header row
sub_headers <- c("Variable",
                 "Overall", "GLP-1", "Control", "p",
                 "Overall", "GLP-1", "Control", "p")

for (j in seq_along(sub_headers)) {
  writeData(wb, "Lab findings",
            x        = sub_headers[j],
            startRow = 2,
            startCol = j)
}

addStyle(wb, "Lab findings",
         style      = style_header,
         rows       = 2,
         cols       = 1:9,
         gridExpand = TRUE)

# Data rows
writeData(wb, "Lab findings",
          x        = results_labs_combined,
          startRow = 3,
          startCol = 1,
          colNames = FALSE)

addStyle(wb, "Lab findings",
         style      = style_text,
         rows       = 3:(nrow(results_labs_combined) + 3),
         cols       = 1:9,
         gridExpand = TRUE)

setColWidths(wb, "Lab findings",
             cols   = 1:9,
             widths = "auto")

freezePane(wb, "Lab findings",
           firstActiveRow = 3)

saveWorkbook(wb,
             "Results/table_3_lab_findings.xlsx",
             overwrite = TRUE)

cat("\nSaved Results/table_3_lab_findings.xlsx\n")