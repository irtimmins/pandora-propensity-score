# =============================================
# 03 - Derive outcome variables
# Composite, severity, local complication
# =============================================

df <- df %>%
  mutate(
    # Local complication
    # peripanc_collection_* = 6 vars with full prefix
    # peripanc_collec_repeat_adm_CT excluded (diff prefix)
    local_complication = if_else(
      rowSums(cbind(
        peripanc_collection_adm_CT    == 1,
        peripanc_collection_adm_MRCP  == 1,
        peripanc_collection_adm_EUS   == 1,
        peripanc_collection_drain_90  == 1,
        peripanc_collection_drain_ir  == 1,
        peripanc_collection_drain_eus == 1,
        pseudocyst_repeat_adm_CT      == 1,
        panc_necrosis_adm_CT          == 1,
        panc_necrosis_repeat_adm_CT   == 1,
        panc_necrosis_adm_MRCP        == 1,
        panc_necrosis_adm_EUS         == 1,
        necrosectomy_90               == 1
      ), na.rm = TRUE) > 0,
      1L, 0L
    ),
    
    organ_failure_severe = if_else(
      resp_failure  == 3 |
        cvs_failure   == 3 |
        renal_failure == 3,
      1L, 0L, missing = 0L
    ),
    
    severity = case_when(
      organ_failure_severe == 1 ~ 2,
      resp_failure  == 2 |
        cvs_failure   == 2 |
        renal_failure == 2 |
        local_complication == 1   ~ 1,
      TRUE                      ~ 0
    ),
    
    severity_bin = if_else(severity == 2, 1L, 0L),
    
    composite = if_else(
      severity > 0 |
        critical_care_adm >= 2 |
        mort90 == 1,
      1L, 0L
    )
  )

# ----- Verfify against Adil -----
# Expected: 0: No=1924, Yes=168 / 1: No=894, Yes=43
cat("Composite by GLP1:\n")
print(table(df$composite, df$GLP1_use, useNA = "always"))

cat("\nSeverity_bin by GLP1:\n")
print(table(df$severity_bin, df$GLP1_use, useNA = "always"))

cat("\nLocal complication by GLP1:\n")
print(table(df$local_complication, df$GLP1_use, useNA = "always"))

# Save full cleaned dataset
saveRDS(df, "Data/pandora_clean_r.rds")
cat("\nSaved pandora_clean_r.rds\n")