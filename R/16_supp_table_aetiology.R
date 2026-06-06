# =============================================
# 17 - Supplementary Aetiology table.
# Cause of pancreatitis by GLP-1 status
# If errors: rerun 02 and 03 scripts to recreate
# df tibble.
# =============================================

df_aet <- df %>%
  mutate(
    # If not on GLP-1 but GLP-1 listed as cause
    # -- recode as not GLP-1 cause
    cause_glp1 = if_else(
      GLP1_use == "No", 0L,
      as.integer(cause_glp1)
    ),
    
    # Drug causes other than GLP-1/DPP4
    cause_drugs = if_else(
      cause_dpp4 == 1 | cause_drug_other == 1,
      1L, 0L
    ),
    
    # Missing cause
    cause_missing = if_else(
      cause_gallstones   == 0 &
        cause_alcohol      == 0 &
        cause_trauma       == 0 &
        cause_steroids     == 0 &
        cause_tumour       == 0 &
        cause_ercp         == 0 &
        cause_autoimmune   == 0 &
        cause_calcium      == 0 &
        cause_triglyceride == 0 &
        cause_dpp4         == 0 &
        cause_drug_other   == 0 &
        cause_idiopathic   == 0,
      1L, 0L
    ),
    
    # If missing but GLP-1 cause ticked
    # --> recode as idiopathic
    cause_idiopathic = if_else(
      cause_missing == 1 & cause_glp1 == 1,
      1L, as.integer(cause_idiopathic)
    ),
    cause_missing = if_else(
      cause_missing == 1 & cause_glp1 == 1,
      0L, cause_missing
    )
  ) %>%
  mutate(across(
    c(cause_gallstones, cause_alcohol,
      cause_idiopathic, cause_trauma,
      cause_steroids, cause_drugs,
      cause_tumour, cause_ercp,
      cause_autoimmune, cause_calcium,
      cause_triglyceride, cause_glp1,
      cause_missing),
    ~ factor(., levels = 0:1,
             labels = c("No", "Yes"))
  ))

# -----TABLE-----
aetiology_vars <- c("cause_gallstones", "cause_alcohol",
                    "cause_idiopathic", "cause_trauma",
                    "cause_steroids", "cause_drugs",
                    "cause_tumour", "cause_ercp",
                    "cause_autoimmune", "cause_calcium",
                    "cause_triglyceride", "cause_glp1",
                    "cause_missing")

tab_aetiology <- CreateTableOne(
  vars       = aetiology_vars,
  strata     = "GLP1_use",
  data       = df_aet,
  factorVars = aetiology_vars,
  addOverall = TRUE
)

cat("\n-----Aetiology of pancreatitis-----\n")
cat("Note: patients may have >1 aetiology\n\n")
print(tab_aetiology, showAllLevels = FALSE, test = FALSE)

write.csv(
  print(tab_aetiology,
        showAllLevels = FALSE,
        printToggle   = FALSE,
        noSpaces      = TRUE),
  "Results/supp_table_aetiology.csv"
)

cat("\nSaved supp_table_aetiology.csv\n")
