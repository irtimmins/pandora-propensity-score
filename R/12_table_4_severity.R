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

# -----SPLIT VARIABLES BY TYPE-----
# Binary variables -- show both levels
binary_vars <- c("organ_failure_severe",
                 "critical_care_adm_bin",
                 "local_complication",
                 "composite")

# 4-level ordinal -- show all levels
ordinal_vars <- c("resp_failure",
                  "cvs_failure",
                  "renal_failure")

# Severity -- show both levels
severity_vars <- c("severity_bin")

of_vars <- c(ordinal_vars, binary_vars, severity_vars)

# -----TABLE-----
tab_of <- CreateTableOne(
  vars       = of_vars,
  strata     = "GLP1_use",
  data       = df,
  factorVars = of_vars,
  addOverall = TRUE
)

cat("\n-----Severity components-----\n")

# showAllLevels = TRUE shows No and Yes rows
# for binary variables
print(tab_of, showAllLevels = TRUE)

# -----EXPORT-----
tab_severity <- print(tab_of,
                    showAllLevels = TRUE,
                    printToggle   = FALSE,
                    noSpaces      = TRUE)

write.csv(tab_of_mat,
          "Results/table4_severity.csv")

