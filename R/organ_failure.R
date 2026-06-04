# =============================================
# 19 - ORGAN FAILURE COMPONENTS
# Individual resp/cvs/renal failure + critical care
# =============================================

df <- df %>%
  haven::zap_labels() %>%
  mutate(
    resp_failure = factor(resp_failure,
                          levels = 0:3,
                          labels = c("None", "Transient",
                                     "Persistent", "Unknown")),
    cvs_failure = factor(cvs_failure,
                         levels = 0:3,
                         labels = c("None", "Transient",
                                    "Persistent", "Unknown")),
    renal_failure = factor(renal_failure,
                           levels = 0:3,
                           labels = c("None", "Transient",
                                      "Persistent", "Unknown")),
    critical_care_adm_bin = factor(
      critical_care_adm_bin,
      levels = 0:1,
      labels = c("No", "Yes")),
    organ_failure_severe = factor(
      organ_failure_severe,
      levels = 0:1,
      labels = c("No", "Yes")),
    local_complication = factor(
      local_complication,
      levels = 0:1,
      labels = c("No", "Yes")),
    severity_bin = factor(
      severity_bin,
      levels = 0:1,
      labels = c("Mild/Moderate", "Severe")),
    composite = factor(
      composite,
      levels = 0:1,
      labels = c("No", "Yes"))
  )

# -----TABLE-----
of_vars <- c("resp_failure", "cvs_failure",
             "renal_failure", "organ_failure_severe",
             "critical_care_adm_bin",
             "local_complication",
             "severity_bin", "composite")

tab_of <- CreateTableOne(
  vars       = of_vars,
  strata     = "GLP1_use",
  data       = df,
  factorVars = of_vars,
  addOverall = TRUE
)

cat("\n-----Organ failure and severity components-----\n")
print(tab_of, showAllLevels = FALSE)

write.csv(
  print(tab_of,
        showAllLevels = FALSE,
        printToggle   = FALSE,
        noSpaces      = TRUE),
  "table_organ_failure.csv"
)

cat("\nSaved table_organ_failure.csv\n")