# =============================================
# 03 - Derive outcomes + finalise analysis frame
# All factor wrangling happens HERE so nothing
# downstream needs re-factoring
# =============================================

df <- df %>%
  mutate(
    local_complication = as.integer(
      peripanc_collection_adm_CT    == 1 |
        peripanc_collection_adm_MRCP  == 1 |
        peripanc_collection_adm_EUS   == 1 |
        peripanc_collection_drain_90  == 1 |
        peripanc_collection_drain_ir  == 1 |
        peripanc_collection_drain_eus == 1 |
        pseudocyst_repeat_adm_CT      == 1 |
        panc_necrosis_adm_CT          == 1 |
        panc_necrosis_repeat_adm_CT   == 1 |
        panc_necrosis_adm_MRCP        == 1 |
        panc_necrosis_adm_EUS         == 1 |
        necrosectomy_90               == 1
    ),
    local_complication = if_else(
      is.na(local_complication), 0L,
      local_complication),
    
    organ_failure_severe = if_else(
      resp_failure == 3 | cvs_failure == 3 |
        renal_failure == 3,
      1L, 0L, missing = 0L),
    
    severity = case_when(
      organ_failure_severe == 1             ~ 2,
      resp_failure == 2 | cvs_failure == 2 |
        renal_failure == 2 |
        local_complication == 1               ~ 1,
      TRUE                                  ~ 0),
    
    severity_bin = if_else(severity == 2, 1L, 0L),
    
    composite = if_else(
      severity > 0 | critical_care_adm >= 2 |
        mort90 == 1,
      1L, 0L)
  )

# Finalise types
# Outcomes as plain integer 0/1 (glm-ready)
# Covariates already factored in script 02 with
# sensible level order; just enforce + zap labels
df <- df %>%
  mutate(
    across(c(composite, severity_bin, mort90,
             readm90, critical_care_adm_bin,
             local_complication,
             resp_failure_bin, cvs_failure_bin,
             renal_failure_bin),
           as.integer),
    GLP1_use = factor(as.character(GLP1_use),
                      levels = c("No", "Yes"))
  )

# Further checks
cat("Composite by GLP1:\n")
print(table(df$composite, df$GLP1_use, useNA = "always"))
cat("\nSeverity_bin by GLP1:\n")
print(table(df$severity_bin, df$GLP1_use, useNA = "always"))
cat("\nLocal complication by GLP1:\n")
print(table(df$local_complication, df$GLP1_use,
            useNA = "always"))

saveRDS(df, "Data/pandora_clean_r.rds")
cat("\nSaved pandora_clean_r.rds\n")