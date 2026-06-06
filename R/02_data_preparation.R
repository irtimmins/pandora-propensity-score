# =============================================
# 02 - Prepare analysis cohort
# Read Stata file, apply exclusions, derive
# and label all covariates used in analysis.
# Run 01_setup.R first.
# =============================================

df <- read_dta("Data/pandora_clean.dta") %>%
  haven::zap_labels()


# Exclusions: missing exposure, missing gender,
# missing primary outcome or critical care variable
df <- df %>%
  mutate(GLP1_use = na_if(GLP1_use, 99)) %>%
  filter(!is.na(GLP1_use)) %>%
  filter(gender != 99) %>%
  mutate(mort90 = na_if(mort90, 99)) %>%
  filter(!is.na(mort90),
         !is.na(readm90),
         !is.na(critical_care_adm_bin)) %>%
  mutate(gender = factor(gender,
                         levels = 0:1,
                         labels = c("Male", "Female")))

cat("N after exclusions:", nrow(df), "\n")

# Age is stored as an ordinal code (1-8) in Stata
# mapping to two-year bands. We collapse to four
# clinically meaningful groups.
df <- df %>%
  mutate(
    age_cat = case_when(
      age %in% 1:2 ~ 1,
      age %in% 3:4 ~ 2,
      age %in% 5:6 ~ 3,
      age %in% 7:8 ~ 4,
      TRUE          ~ NA_real_),
    age_cat = factor(age_cat,
                     levels = 1:4,
                     labels = c("18-35", "36-55",
                                "56-75", ">75")))

# BMI is stored as a Stata code. Code 5 = underweight,
# code 7 = not recorded. Normal weight is set as the
# reference level for regression models.
df <- df %>%
  mutate(
    bmi_cat = case_when(
      bmi == 5 ~ 0,
      bmi == 1 ~ 1,
      bmi == 2 ~ 2,
      bmi == 3 ~ 3,
      bmi == 4 ~ 4,
      bmi == 6 ~ 5,
      TRUE      ~ NA_real_),
    bmi_cat = factor(bmi_cat,
                     levels = 0:5,
                     labels = c("<18.5 Underweight",
                                "18.5-24.9 Normal",
                                "25-29.9 Overweight",
                                "30-34.9 Class 1 Obesity",
                                "35-39.9 Class 2 Obesity",
                                ">40 Class 3 Obesity")),
    bmi_cat = relevel(bmi_cat,
                      ref = "18.5-24.9 Normal"))

# Smoking: Stata codes 2=never, 4=current, 1=ex,
# 3=unknown (treated as missing)
df <- df %>%
  mutate(
    smoking = case_when(
      smoking == 2 ~ 0,
      smoking == 4 ~ 1,
      smoking == 1 ~ 2,
      TRUE          ~ NA_real_),
    smoking = factor(smoking,
                     levels = 0:2,
                     labels = c("Never smoker",
                                "Current Smoker",
                                "Ex-smoker")))

# Alcohol: code 6 = unknown (missing), code 5 = none.
# Codes 3 and 4 both map to >35 units/week.
df <- df %>%
  mutate(
    alcohol = case_when(
      alcohol == 6 ~ NA_real_,
      alcohol == 5 ~ 0,
      TRUE          ~ as.numeric(alcohol)),
    alcohol_cat = case_when(
      alcohol == 0         ~ 0,
      alcohol == 1         ~ 1,
      alcohol == 2         ~ 2,
      alcohol %in% c(3, 4) ~ 3,
      TRUE                  ~ NA_real_),
    alcohol_cat = factor(alcohol_cat,
                         levels = 0:3,
                         labels = c("None", "1-14",
                                    "15-35", ">35")))

# Charlson Comorbidity Index computed from individual
# components following standard weights. Liver disease,
# DM, tumour, and AIDS have variable weights; all others
# are binary. Score capped at 3 for the categorical version.
df <- df %>%
  mutate(
    liver_disease_cci = case_when(
      liver_disease_severe == 1 ~ 3,
      liver_disease_mild   == 1 ~ 1,
      TRUE                      ~ 0),
    DM_cci = case_when(
      DM_complicated   == 1 ~ 2,
      DM_uncomplicated == 1 ~ 1,
      TRUE                  ~ 0),
    tumour_cci = case_when(
      tumour_met   == 1 ~ 6,
      tumour_local == 1 ~ 2,
      TRUE              ~ 0),
    hemiplegia_cci = if_else(hemiplegia == 1,
                             2, 0, missing = 0),
    CKD_cci        = if_else(CKD == 1,
                             2, 0, missing = 0),
    leukaemia_cci  = if_else(leukaemia == 1,
                             2, 0, missing = 0),
    lymphoma_cci   = if_else(lymphoma == 1,
                             2, 0, missing = 0),
    AIDS_cci       = if_else(AIDS == 1,
                             6, 0, missing = 0),
    cci = MI + CHF + PVD + CVA + dementia + COPD +
      connective_tissue_disease +
      peptic_ulcer_disease +
      liver_disease_cci + DM_cci +
      hemiplegia_cci + CKD_cci + tumour_cci +
      leukaemia_cci + lymphoma_cci + AIDS_cci,
    cci_cat = factor(pmin(cci, 3),
                     levels = 0:3,
                     labels = c("0", "1", "2", ">=3")))

cat("CCI distribution:\n")
print(table(df$cci, useNA = "always"))

# Gallstones on prior or index admission imaging.
# Any positive result across all imaging modalities
# counts as positive. Does not include gallstones
# as cause of admission -- imaging evidence only.
df <- df %>%
  mutate(
    gallstones_imaging = as.numeric(
      gallstones_prev_imaging),
    gallstones_imaging = if_else(
      gallstones_adm_USS == 1, 1,
      gallstones_imaging,
      missing = gallstones_imaging),
    gallstones_imaging = if_else(
      gallstones_adm_CT == 1, 1,
      gallstones_imaging,
      missing = gallstones_imaging),
    gallstones_imaging = if_else(
      gallstones_repeat_adm_CT == 1, 1,
      gallstones_imaging,
      missing = gallstones_imaging),
    gallstones_imaging = if_else(
      gallstones_adm_MRCP == 1, 1,
      gallstones_imaging,
      missing = gallstones_imaging),
    gallstones_imaging = if_else(
      gallstones_adm_EUS == 1, 1,
      gallstones_imaging,
      missing = gallstones_imaging),
    gallstones_imaging = if_else(
      cbd_stones_adm_USS == 1, 1,
      gallstones_imaging,
      missing = gallstones_imaging),
    gallstones_imaging = if_else(
      cbd_stones_adm_CT == 1, 1,
      gallstones_imaging,
      missing = gallstones_imaging),
    gallstones_imaging = if_else(
      cbd_stones_adm_MRCP == 1, 1,
      gallstones_imaging,
      missing = gallstones_imaging),
    gallstones_imaging = if_else(
      cbd_stones_adm_EUS == 1, 1,
      gallstones_imaging,
      missing = gallstones_imaging),
    gallstones_imaging = if_else(
      cbd_stones_repeat_adm_CT == 1, 1,
      gallstones_imaging,
      missing = gallstones_imaging),
    gallstones_imaging = factor(gallstones_imaging,
                                levels = 0:1,
                                labels = c("No", "Yes")))

df <- df %>%
  mutate(
    GLP1_use = factor(GLP1_use,
                      levels = 0:1,
                      labels = c("No", "Yes")),
    prev_pancreatitis = factor(prev_pancreatitis,
                               levels = 0:1,
                               labels = c("No", "Yes")),
    hospital = factor(hospital))

cat("GLP1_use:\n")
print(table(df$GLP1_use, useNA = "always"))
cat("Data prep complete\n")