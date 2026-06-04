# =============================================
# 15 - ATLANTA criterion
# Biochemical and imaging confirmation of AP
# =============================================

df <- df %>%
  mutate(
    # Biochemical: amylase >300
    biochem_amylase = if_else(
      amylase_adm >= 300 & !is.na(amylase_adm),
      1L, 0L
    ),
    
    # Biochemical: lipase >180
    biochem_lipase = if_else(
      lipase_adm >= 180 & !is.na(lipase_adm),
      1L, 0L
    ),
    
    # Either amylase or lipase criteria met
    biochem_panc = if_else(
      biochem_amylase == 1 | biochem_lipase == 1,
      1L, 0L
    ),
    
    # Imaging confirmed
    image_panc = if_else(
      pancreatitis_adm_CT        == 1 |
        pancreatitis_repeat_adm_CT == 1 |
        pancreatitis_adm_MRCP      == 1 |
        pancreatitis_adm_EUS       == 1,
      1L, 0L,
      missing = 0L
    ),
    
    # Biochemical OR imaging
    biochem_or_image_panc = if_else(
      biochem_panc == 1 | image_panc == 1,
      1L, 0L
    ),
    
    # Factor for display
    biochem_panc = factor(biochem_panc,
                          levels = 0:1,
                          labels = c("No", "Yes")),
    image_panc = factor(image_panc,
                        levels = 0:1,
                        labels = c("No", "Yes")),
    biochem_or_image_panc = factor(biochem_or_image_panc,
                                   levels = 0:1,
                                   labels = c("No", "Yes"))
  )

# -----TABLE-----
atlanta_vars <- c("biochem_panc", "image_panc",
                  "biochem_or_image_panc")

tab_atlanta <- CreateTableOne(
  vars       = atlanta_vars,
  strata     = "GLP1_use",
  data       = df,
  factorVars = atlanta_vars,
  addOverall = TRUE
)

cat("\n-----Atlanta criteria confirmation-----\n")
print(tab_atlanta, showAllLevels = FALSE)

# -----EXPORT-----
write.csv(
  print(tab_atlanta,
        showAllLevels = FALSE,
        printToggle   = FALSE,
        noSpaces      = TRUE),
  "table_atlanta.csv"
)

cat("\nSaved table_atlanta.csv\n")