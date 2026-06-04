# =============================================
# 12 - Table 1
# Unmatched full cohort + matched MI dataset 1
# =============================================

tab1_vars <- c("age_cat", "gender", "bmi_cat", "cci_cat",
               "smoking_cat", "alcohol_cat2",
               "prev_pancreatitis", "gallstones_imaging",
               "composite", "severity_bin", "mort90",
               "readm90", "critical_care_adm_bin",
               "local_complication")

# Helper to factor outcomes for display
factor_outcomes <- function(d) {
  d %>% mutate(
    across(c(composite, severity_bin, mort90, readm90,
             critical_care_adm_bin, local_complication),
           ~ factor(., levels = 0:1,
                    labels = c("No", "Yes")))
  )
}

# -----UNMATCHED-----
tab_unmatched <- CreateTableOne(
  vars       = tab1_vars,
  strata     = "GLP1_use",
  data       = df_ps %>% factor_outcomes(),
  factorVars = tab1_vars,
  addOverall = TRUE
)


# -----MATCHED MI (dataset 1)-----
tab_matched_mi <- CreateTableOne(
  vars       = tab1_vars,
  strata     = "GLP1_use",
  data       = matched_list[[1]] %>%
    mutate(
      smoking_cat  = factor(smoking,
                            levels = c("Never smoker", "Current Smoker",
                                       "Ex-smoker")),
      alcohol_cat2 = factor(alcohol_cat,
                            levels = c("None", "1-14", "15-35", ">35"))
    ) %>% factor_outcomes(),
  factorVars = tab1_vars,
  addOverall = TRUE
)

# -----PRINT-----
cat("\n----- Unmatched -----\n")
print(tab_unmatched, smd = TRUE, showAllLevels = FALSE)
cat("\n----- Matched MI -----\n")
print(tab_matched_mi, smd = TRUE, showAllLevels = FALSE)

# -----EXPORT-----
write.csv(print(tab_unmatched, smd = TRUE,
                printToggle = FALSE, noSpaces = TRUE),
          "table1_unmatched.csv")
write.csv(print(tab_matched_mi, smd = TRUE,
                printToggle = FALSE, noSpaces = TRUE),
          "table1_matched_mi.csv")

cat("\nTable 1 CSVs saved\n")