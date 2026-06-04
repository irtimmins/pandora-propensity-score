# =============================================
# 12 - Table 2, GLP-1 usage
# (Among GLP-1 users only)
# =============================================

df_glp1 <- df %>%
  dplyr::filter(GLP1_use == "Yes") %>%
  haven::zap_labels() %>% # reformat from stata dataset.
  mutate(
    glp1_drug = factor(GLP1_drug,
                       levels = 1:4,
                       labels = c("Dulaglutide (Trulicity)",
                                  "Other",
                                  "Semaglutide (Ozempic/Wegovy)",
                                  "Tirzepatide (Mounjaro/Zepbound)")),
    
    glp1_duration = factor(GLP1_duration_cat,
                           levels = c(1, 2, 3, 4, 99),
                           labels = c("<3 months",
                                      "3-6 months",
                                      "6-12 months",
                                      ">12 months",
                                      "Not known")),
    
    glp1_source = case_when(
      GLP1_source == 1 ~ "Primary care",
      GLP1_source == 3 ~ "Secondary care",
      GLP1_source == 4 ~ "Other",
      TRUE             ~ "Not known"
    ),
    glp1_source = factor(glp1_source,
                         levels = c("Primary care", "Secondary care",
                                    "Other", "Not known")),
    
    glp1_indication = factor(GLP1_indication,
                             levels = 1:4,
                             labels = c("Weight loss",
                                        "Diabetes mellitus",
                                        "Weight loss and diabetes mellitus",
                                        "Not known")),
    
    glp1_stopped = factor(GLP1_stopped_post_adm,
                          levels = 0:1,
                          labels = c("No", "Yes")),
    
    yellow_card = case_when(
      yellow_card == 0  ~ "No",
      yellow_card == 1  ~ "Yes",
      yellow_card == 99 ~ "Not known",
      TRUE              ~ NA_character_
    ),
    yellow_card = factor(yellow_card,
                         levels = c("No", "Yes", "Not known"))
  )

# -----TABLE-----
glp1_vars <- c("glp1_drug", "glp1_duration",
               "glp1_source", "glp1_indication",
               "glp1_stopped", "yellow_card")

glp1_vars <- glp1_vars[glp1_vars %in% names(df_glp1)]

tab_glp1 <- CreateTableOne(
  vars       = glp1_vars,
  data       = df_glp1,
  factorVars = glp1_vars
)

cat("\n-----GLP-1 drug characteristics (users only)-----\n")
cat("N GLP-1 users:", nrow(df_glp1), "\n\n")
print(tab_glp1, showAllLevels = TRUE)

write.csv(
  print(tab_glp1,
        showAllLevels = TRUE,
        printToggle   = FALSE,
        noSpaces      = TRUE),
  "Results/table_2_glp1_characteristics.csv"
)

cat("\nSaved Results/table_2_glp1_characteristics.csv\n")
