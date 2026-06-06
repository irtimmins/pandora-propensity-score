# =============================================
# 13 - Table 3: Lab findings
# Continuous variables: median (IQR) by GLP-1 status
# Full cohort and propensity score matched cohort
# Kruskal-Wallis p-values
# =============================================

library(openxlsx)

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

fmt_p <- function(p) {
  if (is.na(p))  return(NA_character_)
  if (p < 0.001) return("<0.001")
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
    ")")
}

summarise_lab <- function(var, data) {
  overall <- data[[var]]
  ctrl    <- data[[var]][data$GLP1_use == "No"]
  glp1    <- data[[var]][data$GLP1_use == "Yes"]
  kw      <- kruskal.test(data[[var]] ~ data$GLP1_use)
  data.frame(
    variable = lab_labels[var],
    overall  = fmt_miqr(overall),
    control  = fmt_miqr(ctrl),
    glp1     = fmt_miqr(glp1),
    p_kw     = fmt_p(kw$p.value),
    stringsAsFactors = FALSE)
}

results_labs_full <- do.call(rbind,
                             lapply(lab_vars, summarise_lab, data = df))

# crp is already in the matched dataset from the
# imputation so drop it before joining from df
matched_labs <- matched_list[[1]] %>%
  dplyr::select(-crp) %>%
  dplyr::left_join(
    df %>%
      dplyr::select(id, all_of(lab_vars)),
    by = "id")

results_labs_matched <- do.call(rbind,
                                lapply(lab_vars, summarise_lab,
                                       data = matched_labs))

cat("Full cohort lab results:\n")
print(results_labs_full[, c("variable", "overall",
                            "control", "glp1",
                            "p_kw")],
      row.names = FALSE)

cat("\nMatched cohort lab results:\n")
print(results_labs_matched[, c("variable", "overall",
                               "control", "glp1",
                               "p_kw")],
      row.names = FALSE)

# Column order matches Tables 1 and 4:
# Variable | Overall | No (control) | Yes (GLP-1) | p
results_labs_combined <- data.frame(
  Variable   = results_labs_full$variable,
  FC_Overall = results_labs_full$overall,
  FC_No      = results_labs_full$control,
  FC_Yes     = results_labs_full$glp1,
  FC_p       = results_labs_full$p_kw,
  PS_Overall = results_labs_matched$overall,
  PS_No      = results_labs_matched$control,
  PS_Yes     = results_labs_matched$glp1,
  PS_p       = results_labs_matched$p_kw,
  stringsAsFactors = FALSE
)

wb <- createWorkbook()
addWorksheet(wb, "Lab findings")

style_span <- createStyle(
  halign         = "center",
  textDecoration = "bold",
  border         = "Bottom")
style_header <- createStyle(
  halign         = "center",
  textDecoration = "bold")
style_text <- createStyle(numFmt = "TEXT")

# Column layout mirrors Tables 1 and 4:
# 1       = Variable
# 2-5     = Full cohort (Overall, No, Yes, p)
# 6-9     = Matched cohort (Overall, No, Yes, p)
writeData(wb, "Lab findings",
          x = "Full cohort",
          startRow = 1, startCol = 2)
mergeCells(wb, "Lab findings",
           cols = 2:5, rows = 1)

writeData(wb, "Lab findings",
          x = "Propensity-score matched cohort",
          startRow = 1, startCol = 6)
mergeCells(wb, "Lab findings",
           cols = 6:9, rows = 1)

addStyle(wb, "Lab findings",
         style      = style_span,
         rows       = 1, cols = 2:9,
         gridExpand = TRUE)

sub_headers <- c("Variable",
                 "Overall", "No", "Yes", "p",
                 "Overall", "No", "Yes", "p")

for (j in seq_along(sub_headers)) {
  writeData(wb, "Lab findings",
            x        = sub_headers[j],
            startRow = 2, startCol = j)
}
addStyle(wb, "Lab findings",
         style      = style_header,
         rows       = 2, cols = 1:9,
         gridExpand = TRUE)

writeData(wb, "Lab findings",
          x        = results_labs_combined,
          startRow = 3, startCol = 1,
          colNames = FALSE)
addStyle(wb, "Lab findings",
         style      = style_text,
         rows       = 3:(nrow(results_labs_combined) + 3),
         cols       = 1:9,
         gridExpand = TRUE)

setColWidths(wb, "Lab findings",
             cols = 1:9, widths = "auto")

freezePane(wb, "Lab findings",
           firstActiveRow = 3)

saveWorkbook(wb,
             "Results/table_3_lab_findings.xlsx",
             overwrite = TRUE)

cat("Saved Results/table_3_lab_findings.xlsx\n")