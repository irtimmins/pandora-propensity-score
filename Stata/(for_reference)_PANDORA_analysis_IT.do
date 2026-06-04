clear all

cd C:\Users\AdilRashid\Documents\PANDORA

use pandora_clean, clear 

count //3,121

*Drop if missing exposure
tab GLP1_use, mi
recode GLP1_use 99=.

count if GLP1_use==.	//2
drop if GLP1_use ==.

*Drop baseline gender
tab gender 
drop if gender ==99

*Drop if missing outcome  
recode mort90 99=. 

tab mort90, mi
tab readm90, mi
tab critical_care_adm_bin, mi
lab var critical_care_adm_bin "Admission to critical care"


count if mort90==. | readm90==. | critical_care_adm_bin==.	//89

drop if mort90==. | readm90==. | critical_care_adm_bin==.

count	//3,029

*Volume
codebook hospital 

bys hospital: gen volume = _N 
bys hospital: gen n = _n
summ volume if n==1, detail

browse if n==1

*Validation
foreach V of varlist age gender bmi diabetes USS_adm_validation {
	recode `V' 99=.
}

tab age age_validation 

kap age age_validation if !missing(age, age_validation)

tab gender gender_validation

kap gender gender_validation if !missing(gender, gender_validation)

tab bmi bmi_validation
kap bmi bmi_validation if !missing(bmi, bmi_validation) 
*Exclude patients if unavailable 
tab bmi bmi_validation if bmi!=7 & bmi_validation !=7
kap bmi bmi_validation if !missing(bmi, bmi_validation) & bmi!=7 & bmi_validation !=7

tab diabetes diabetes_validation
kap diabetes diabetes_validation if !missing(diabetes, diabetes_validation) 

tab USS_adm USS_adm_validation
kap USS_adm USS_adm_validation if !missing(USS_adm, USS_adm_validation) 

tab critical_care_adm critical_care_adm_validation
kap critical_care_adm critical_care_adm_validation if !missing(critical_care_adm, critical_care_adm_validation) 

*Prevlance of GLP1_use

tab GLP1_use , mi

*Atlanta criteria for AP
*Biochemical pancreatitis 

*Amylase >300 
gen biochem_amylase = amylase_adm
replace biochem_amylase =0 if biochem_amylase<300
replace biochem_amylase =1 if biochem_amylase>=300 & biochem_amylase!=.
tab biochem_amylase, mi

*Lipase >180
gen biochem_lipase = lipase_adm
replace biochem_lipase =0 if biochem_lipase<180
replace biochem_lipase =1 if biochem_lipase>=180 & biochem_lipase!=.
tab biochem_lipase, mi

gen biochem_panc = biochem_amylase
replace biochem_panc = biochem_lipase if biochem_panc !=1
lab val biochem_panc comorb
lab var biochem_panc "Serum amylase or lipase level greater than three mes the upper normal limit"
tab biochem_panc, mi

*Image comfirmed pancreatitis 
gen image_panc = 0 
replace image_panc =. if pancreatitis_adm_CT==. & pancreatitis_repeat_adm_CT==. & pancreatitis_adm_MRCP==. & pancreatitis_adm_EUS ==.
replace image_panc =1 if pancreatitis_adm_CT==1 | pancreatitis_repeat_adm_CT==1 | pancreatitis_adm_MRCP==1 | pancreatitis_adm_EUS ==1

lab var image_panc "Radiological findings of pancreatitis on CT, MR, or EUS imaging"
lab val image_panc comorb 

tab image_panc, mi

*Biochem or image pancreatitis 
gen biochem_or_image_panc = biochem_panc
replace biochem_or_image_panc = image_panc if image_panc==1

lab var biochem_or_image_panc "Radiological or biochemical confirmation of pancreatitis"
lab val biochem_or_image_panc comorb


dtable i.biochem_panc i.image_panc i.biochem_or_image_panc, by(GLP1_use) export(atlanta.docx, replace)

tab GLP1_use biochem_panc , mi row

tab GLP1_use image_panc , mi row
 
tab GLP1_use biochem_or_image_panc , mi row chi

	***********
	***CAUSE***
	***********

codebook cause_scorpion cause_mumps	//no events

drop cause_scorpion cause_mumps

foreach V of varlist cause_gallstones cause_alcohol cause_idiopathic cause_trauma cause_steroids cause_tumour cause_ercp cause_autoimmune  cause_calcium cause_triglyceride cause_glp1 cause_dpp4 cause_drug_other  {
	tab `V', mi 
}

*Cause of pancreatitis 

*If no GLP1 use but cuase listed as GLP1 assume cause incorrect and recode GLP1 cause to no
replace cause_glp1 = 0 if GLP1_use==0 




*†Patients could have >1 aetiology so each aetiology considered as a separate variable.
lab var cause_gallstones "Gallstones"
lab var cause_alcohol "Alcohol"
lab var cause_trauma "Trauma"
lab var cause_steroids "Steroid use"
lab var cause_tumour "Tumour"
lab var cause_ercp "Post-ERCP"
lab var cause_autoimmune "Autoimmune"
lab var cause_calcium "Hypercalcaemia"
lab var cause_triglyceride "Hypertriglyceridaemia"
lab var cause_idiopathic "Idiopathic"

*Create cause due to drugs 
gen cause_drugs = 0
replace cause_drugs = 1 if cause_dpp4==1 | cause_drug_other==1
lab var cause_drugs "Drugs - Other"

*Missing 
gen cause_missing = 0 
replace cause_missing =1 if cause_gallstones==0 & cause_alcohol==0 & cause_trauma==0 & cause_steroids==0 & cause_tumour==0 & cause_ercp==0 & cause_autoimmune ==0 &  cause_calcium ==0 & cause_triglyceride==0 & cause_dpp4 ==0 & cause_drug_other==0 & cause_idiopathic==0 
lab var cause_missing "Missing"

*If missing but GLP1 cause ticked recode as idiopathic and no longer missing
replace cause_idiopathic =1 if cause_missing ==1 & cause_glp1==1

replace cause_missing =0 if cause_missing ==1 & cause_glp1==1 


lab values cause_missing cause_drugs  comorb 


dtable i.cause_gallstones i.cause_alcohol i.cause_idiopathic i.cause_trauma i.cause_steroids i.cause_drugs i.cause_tumour i.cause_ercp  i.cause_autoimmune i.cause_calcium i.cause_triglyceride i.cause_missing, by(GLP1_use) factor(, statistics(fvfrequency fvpercent)) nformat(%9.0f fvfrequency)     sformat("(%s%%)" fvpercent) export(aetiology_table.docx, replace)



*Table 1
tab age, mi 
tab gender, mi

tab bmi, mi 
recode bmi 5=0
lab define bmi 0 "<18.5", modify 
tab  bmi GLP1_use, mi col

gen bmi_cat = bmi 
recode bmi_cat 6=5
lab define bmi_cat 0 "<18.5 Underweight" 1 "18.5-24.9 Normal" 2 "25-29.9 Overweight" 3 "30-34.9 Class 1 Obesity" 4 "35-39.9 Class 2 Obesity" 5 ">40 Class 3 Obesity" 7 "Not available"
lab values bmi_cat bmi_cat
recode bmi_cat 7=.

tab smoking, mi 
recode smoking 3=. 2=0 4=1 1=2
lab drop smoking
lab define smoking 0 "Never smoker" 1 "Smoker" 2 "Ex-smoker"
lab values smoking smoking
tab smoking

tab alcohol 
tab alcohol , nol
recode alcohol 6=. 5=0
lab define alcohol 0 "None", modify
tab alcohol 

*Gallstones on prior or index admission imaging or cause is gallsotnes 
gen gallstones_imaging =gallstones_prev_imaging

foreach V of varlist gallstones_adm_* gallstones_repeat_adm_CT cbd_stones_adm* {
	recode `V' 99=.
}

foreach V of varlist gallstones_adm_* gallstones_repeat_adm_CT cbd_stones_adm* {
	replace gallstones_imaging = `V' if `V'==1 & gallstones_imaging==0
}

replace gallstones_imaging = 1 if cause_gallstones==1

gen gallstones_adm=0 
foreach V of varlist gallstones_adm_* {
	replace gallstones_adm=1 if `V'==1
}
tab gallstones_adm



*Diabates 
tab diabetes, mi
tab  diabetes GLP1_use, mi col

gen diabetes_bin = diabetes
recode diabetes_bin 1=0 2/4=1
lab val diabetes_bin comorb
lab variable diabetes_bin "Diabetic"
tab diabetes_bin
tab  diabetes_bin GLP1_use,  col chi

*Chalrson index 

*Liver disease
tab liver_disease_mild

tab liver_disease_severe

gen liver_disease_cci=sign(liver_disease_mild)
replace liver_disease_cci=3 if liver_disease_severe==1
lab define liver_disease_cci 0 "None" 1 "Mild" 3 "Moderate to severe"
lab values liver_disease_cci liver_disease_cci
tab liver_disease_cci

*DM 
tab DM_uncomplicated
tab DM_uncomplicated, nol

tab DM_complicated 
tab DM_complicated , nol

gen DM_cci = sign(DM_uncomplicated)
replace DM_cci =2 if DM_complicated==1
lab define DM_cci 0 "None or diet controlled" 1 "Uncomplicated" 2 "End-organ damage"
lab values DM_cci DM_cci 
tab DM_cci 
tab DM_uncomplicated DM_cci 
tab DM_complicated DM_cci

*Hemiplegia 
tab hemiplegia
tab hemiplegia, nol
recode hemiplegia 1=2 
lab define hemiplegia 0 "No" 2 "Yes"
lab values hemiplegia hemiplegia
tab hemiplegia

*CKD 
tab CKD
tab CKD, nol
recode CKD 1=2 
lab define CKD 0 "No" 2 "Yes"
lab values CKD CKD
tab CKD

*Tumour 
tab tumour_local
tab tumour_local, nol

tab tumour_met 
tab tumour_met , nol

gen tumour_cci = 0
replace tumour_cci=2 if tumour_local==1

replace tumour_cci =6 if tumour_met==1
lab define tumour_cci 0 "None" 2 "Localised" 6 "Metastatic"
lab var tumour_cci "Solid tumour"
lab values tumour_cci tumour_cci  
tab tumour_cci 
tab tumour_local tumour_cci
tab tumour_met tumour_cci

*Leukamiea/lmphoma 
foreach V of varlist leukaemia lymphoma {
	tab `V', nolab
	recode `V' 1=2
	lab val `V' CKD 
	tab `V'
	tab `V', nol
}

*AIDS 
recode AIDS 1=6 
lab define AIDS 0 "No" 6 "Yes"
lab values AIDS AIDS
tab AIDS
tab AIDS, nol

*Check correct score for each CCI variable 
foreach V of varlist MI CHF PVD CVA dementia COPD connective_tissue_disease peptic liver_disease_cci DM_cci hemiplegia CKD tumour_cci leukaemia lymphoma AIDS {
	tab `V', mi
	tab `V', mi nol
}

gen cci = MI + CHF + PVD + CVA + dementia + COPD + connective_tissue_disease + peptic + liver_disease_cci + DM_cci + hemiplegia + CKD + tumour_cci  + leukaemia + lymphoma + AIDS

tab cci, mi

gen cci_cat = cci 
recode cci_cat 3/max=3

tab cci_cat GLP1_use , col chi
lab var cci_cat "Charlson Co-morbidity Index"
lab define cci_cat 3 "≥3"
lab val cci_cat cci_cat 

*Table 1 
graph bar, over(age) by(GLP1_use)

gen age_cat = age
recode age_cat 1/2=1 3/4=2 5/6=3 7/8=4
lab define age_cat 1 "18-35" 2 "36-55" 3 "56-75" 4 ">75"
lab values age_cat age_cat
lab var age_cat "Age"
tab age age_cat 
lab var age_cat "Age (years)"
lab var gender "Sex"
lab var bmi_cat "BMI (kg/m2)"
lab var smoking "Smoking status"
lab define smoking 1 "Current Smoker", modify 

tab alcohol
graph bar, over(alcohol) by(GLP1_use)
gen alcohol_cat = alcohol
recode alcohol_cat 4=3
lab define alcohol_cat 0 "None" 1 "1-14" 2 "15-35" 3 ">35"
lab val alcohol_cat alcohol_cat
lab var alcohol_cat "Alcohol consumption (units/week)"
tab alcohol alcohol_cat

graph bar, over(diabetes) by(GLP1_use)
tab  diabetes GLP1_use, col chi
tab diabetes

lab var prev_pancreatitis "History of pancreatitis"
lab values gallstones_imaging comorb
lab var gallstones_imaging "Gallstones on prior or index admission imaging"

dtable i.age_cat i.gender i.bmi_cat i.smoking i.alcohol_cat i.diabetes_bin i.cci_cat i.prev_pancreatitis i.gallstones_imaging, by(GLP1_use, tests) export(myfile.docx, replace)

*Add missing data 
foreach V of varlist age_cat gender bmi_cat smoking alcohol_cat diabetes_bin cci_cat prev_pancreatitis gallstones_imaging {
	tab `V' GLP1_use, mi
}

frame copy default glp1 
frame change glp1 

keep if GLP1_use==1

rename GLP1_drug glp1_drug 
rename GLP1_source glp1_source
rename GLP1_indication glp1_indication
rename GLP1_stopped_post_adm glp1_stopped

tab glp1_source, mi nol
recode glp1_source 2=99
lab define GLP1_source 99 "Not known", modify

lab var glp1_drug "GLP-1RA drug"
lab var glp1_source "Source of GLP-1RA prescription" 
lab var glp1_indication "Primary indication for GLP-1 RA prescription"
lab var glp1_stopped "GLP-1RA discontinued after admission with acute pancreatitis"
lab var yellow_card "Was acute pancreatitis reported within 90-day via Yellow Card Scheme"

lab define yellow_card 0 "No" 1 "Yes" 99 "Not known"
lab values yellow_card yellow_card

tab glp1_source 
tab glp1_source , nol
recode glp1_source 3=4
tab glp1_source

rename GLP1_duration_cat glp1_duration_cat


*GLP1_use 
dtable i.glp1_drug i.glp1_duration_cat i.glp1_source i.glp1_indication i.glp1_stopped i.yellow_card,  export(table_2.docx, replace)

frame change default
*Bloods 
codebook wcc_adm crp urea_adm glucose_adm bili_adm lactate_adm 
rename EWS_adm ews_adm

dtable wcc_adm crp urea_adm glucose_adm bili_adm lactate_adm ews_adm, by(GLP1_use, tests) 


	
dtable, ///
    continuous(wcc_adm crp urea_adm glucose_adm bili_adm lactate_adm ews_adm, ///
        statistics(q2 iqi) test(kwallis)) ///
    define(iqi = q1 q3, delimiter("-")) ///
    sformat("(%s)" iqi) ///
    nformat(%9.1f q2 q1 q3 iqi) ///
    by(GLP1_use, tests) ///
    export(table3.docx, replace)
*Missing bloods
foreach V of varlist wcc_adm crp urea_adm glucose_adm bili_adm lactate_adm {
	gen miss_`V' = sign(`V'==.)
}

gen miss_ews_adm = sign(ews_adm==.)
dtable i.miss_wcc_adm i.miss_crp i.miss_urea_adm i.miss_glucose_adm i.miss_bili_adm i.miss_lactate_adm i.miss_ews_adm, by(GLP1_use) export(missing_bloods.docx, replace)

*Severity 
gen organ_failure_severe =0
replace organ_failure_severe =1 if resp_failure==3 | cvs_failure==3 | renal_failure==3

tab  organ_failure_severe GLP1_use, row chi
lab var organ_failure_severe "Organ failure >48 hours"
lab values organ_failure_severe comorb 
tab  organ_failure_severe GLP1_use, col chi

*Local complication:  acute peripancreatic fluid collection, pancreatic pseudocyst, acute necrotic collection and walled-off necrosis

*Peripancreatitic collection on CT or drainage of peripancreatic collection 
codebook peripanc_collection_*

*Pseudocyst on repeat imaging 
codebook pseudocyst_repeat_adm_CT

*Necrosis on CT or necrosectomy 
codebook panc_necrosis_*

codebook necrosectomy_90 

gen local_complication =0 

foreach V of varlist peripanc_collection_* pseudocyst_repeat_adm_CT panc_necrosis_* necrosectomy_90 {
	replace local_complication =1 if `V' ==1
}

tab local_complication

*
lab var local_complication "Local complication"
lab values local_complication comorb 
tab  local_complication GLP1_use, col chi

*Severity 
gen severity = 0 
replace severity = 1 if resp_failure==2 | cvs_failure==2 | renal_failure==2 | local_complication==1 
replace severity = 2 if organ_failure_severe==1

lab var severity "Severity"
lab define severity 0 "Mild" 1 "Moderate" 2 "Severe"
lab values severity severity
tab severity GLP1_use, col chi 

*Make serity binary 
gen severity_bin = severity
recode severity_bin 1=0 2=1 
tab severity severity_bin 
lab var severity_bin "Severity"
lab define severity_bin 0 "Mild/ Moderate" 1 "Severe"
lab values severity_bin severity_bin

*Composite outcome 
gen composite = mort90
replace composite=1 if severity >0 | critical_care_adm >=2 |   mort90==1
lab values composite comorb 
lab var composite "Composite outcome (organ failure of any duration, local complication, critical care admission, or 90-day mortality)"

dtable i.resp_failure i.cvs_failure i.renal_failure i.critical_care_adm , by(GLP1_use, tests) 

dtable i.resp_failure_bin i.cvs_failure_bin i.renal_failure_bin  i.critical_care_adm_bin, by(GLP1_use, tests) export(critical_care.docx, replace)

dtable i.organ_failure_severe  i.critical_care_adm_bin i.local_complication i.severity_bin i.severity i.composite, by(GLP1_use, tests) export(critical_care.docx, replace)

*Duration - critical care LOS 
bys GLP1_use: summ critical_care_los , detail
kwallis critical_care_los, by(GLP1_use)

bys GLP1_use : sum symptom_duration, detail
kwallis symptom_duration, by(GLP1_use)

codebook resp_failure cvs_failure renal_failure critical_care_adm

*OUtcomes 
tab mort90
tab readm90 
codebook los 

dtable i.mort90 i.readm90, by(GLP1_use, tests) export(outcomes.docx, replace)

dtable, ///
    continuous(los, ///
        statistics(q2 iqi) test(kwallis)) ///
    define(iqi = q1 q3, delimiter("-")) ///
    sformat("(%s)" iqi) ///
    nformat(%9.1f q2 q1 q3 iqi) ///
    by(GLP1_use, tests) 
	
	dtable los, by(GLP1_use, tests) 
	
	*Regression
	
	codebook GLP1_use gender age_cat bmi_cat cci_cat smoking alcohol_cat prev_pancreatitis gallstones_imaging
	
	foreach V of varlist GLP1_use gender age_cat bmi_cat cci_cat smoking alcohol_cat prev_pancreatitis gallstones_imaging { 
		recode `V' .=99
	}
	
	dtable i.gender i.age_cat i.bmi_cat i.cci_cat i.smoking i.alcohol_cat i.prev_pancreatitis i.gallstones_imaging, by(GLP1_use, tests) export(missing_data.docx, replace)
	
	melogit mort90 i.GLP1_use || hospital:, base or
	
	melogit mort90 i.GLP1_use i.gender i.age_cat ib1.bmi_cat i.cci_cat i.smoking i.alcohol_cat i.prev_pancreatitis i.gallstones_imaging || hospital:, base or
	

	
	melogit readm90 i.GLP1_use  || hospital:, base or
	
	melogit readm90 i.GLP1_use i.gender i.age_cat i.bmi_cat i.cci_cat i.smoking i.alcohol_cat i.prev_pancreatitis i.gallstones_imaging || hospital:, base or

*Critical care admission 
	melogit critical_care_adm_bin i.GLP1_use  || hospital:, base or
	
	melogit critical_care_adm_bin i.GLP1_use i.gender i.age_cat i.bmi_cat i.cci_cat i.smoking i.alcohol_cat i.prev_pancreatitis i.gallstones_imaging || hospital:, base or
	
*LOcal complication 
melogit local_complication i.GLP1_use  || hospital:, base or
	
	melogit local_complication i.GLP1_use i.gender i.age_cat i.bmi_cat i.cci_cat i.smoking i.alcohol_cat i.prev_pancreatitis i.gallstones_imaging || hospital:, base or
*Severity 
	melogit severity_bin i.GLP1_use  || hospital:, base or
	
	melogit severity_bin i.GLP1_use i.gender i.age_cat i.bmi_cat i.cci_cat i.smoking i.alcohol_cat i.prev_pancreatitis i.gallstones_imaging || hospital:, base or
	
	*Composite
melogit composite i.GLP1_use  || hospital:, base or
	
	melogit composite i.GLP1_use i.gender i.age_cat i.bmi_cat i.cci_cat i.smoking i.alcohol_cat i.prev_pancreatitis i.gallstones_imaging || hospital:, base or
	

	
	
	
*LOS 
bys GLP1_use: summ los
mixed los i.GLP1_use || hospital:

mixed los i.GLP1_use i.gender i.age_cat i.bmi_cat i.cci_cat ///
    i.smoking i.alcohol_cat i.prev_pancreatitis i.gallstones_imaging ///
    || hospital:
	
*CRP
bys GLP1_use: summ crp
mixed crp i.GLP1_use || hospital:

mixed crp i.GLP1_use i.gender i.age_cat i.bmi_cat i.cci_cat ///
    i.smoking i.alcohol_cat i.prev_pancreatitis i.gallstones_imaging ///
    || hospital:
	
	**********************
	***Propensity score***
	**********************
	
drop if bmi_cat==	0
	codebook gender age_cat bmi_cat cci_cat smoking alcohol_cat prev_pancreatitis gallstones_imaging
	
	ssc install psmatch2, replace

psmatch2 GLP1_use i.gender i.age_cat i.bmi_cat i.cci_cat ///
    i.smoking i.alcohol_cat i.prev_pancreatitis i.gallstones_imaging, ///
    logit neighbor(3) caliper(0.1) common 
	
	tab age_cat, gen(age_cat)
tab bmi_cat, gen(bmi_cat)
tab cci_cat, gen(cci_cat)
tab smoking, gen(smoking_cat)
tab alcohol_cat, gen(alcohol_cat)

drop age_cat1 bmi_cat1 cci_cat1 smoking_cat1 alcohol_cat1

 label var age_cat2 "36-55"
 label var age_cat3 "56-75"
 label var age_cat4 ">75"

 label var bmi_cat2 "25-29.9 Overweight"
 label var bmi_cat3 "30-34.9 Class 1 Obesity"
 label var bmi_cat4 "35-39.9 Class 2 Obesity"
 label var bmi_cat5 ">40 Class 3 Obesity"
 label var bmi_cat6 "BMI Missing"

 label var cci_cat2 "1"
 label var cci_cat3 "2"
 label var cci_cat4 "≥3"
 
 label var smoking_cat2 "Current Smoker"
 label var smoking_cat3 "Ex Smoker"
 label var smoking_cat4 "Smoking Missing"

 label var alcohol_cat2 "1-14"
 label var alcohol_cat3 "15-35"
 label var alcohol_cat4 ">35"
 label var alcohol_cat5 "Alcohol Missing"
	
	pstest gender age_cat2 age_cat3 age_cat4 bmi_cat2 bmi_cat3 bmi_cat4 bmi_cat5 bmi_cat6 cci_cat2 cci_cat3 cci_cat4 smoking_cat2 smoking_cat3 smoking_cat4 alcohol_cat2 alcohol_cat3 alcohol_cat4 alcohol_cat5 prev_pancreatitis gallstones_imaging, both graph label
	
	logit mort90 i.GLP1_use if _support==1 [pweight=_weight], ///
    vce(cluster hospital) or base
	
	logit readm90 i.GLP1_use if _support==1 [pweight=_weight], ///
    vce(cluster hospital) or base
	
	logit critical_care_adm_bin i.GLP1_use if _support==1 [pweight=_weight], ///
    vce(cluster hospital) or base
	
		logit local_complication i.GLP1_use if _support==1 [pweight=_weight], ///
    vce(cluster hospital) or base
	
	logit composite i.GLP1_use if _support==1 [pweight=_weight], ///
    vce(cluster hospital) or base
	
	

** Confounders for the propensity score model
local covars i.gender i.age_cat i.bmi_cat i.cci_cat ///
    i.smoking i.alcohol_cat i.prev_pancreatitis i.gallstones_imaging

* Estimate propensity of being on GLP1RA
logit GLP1_use `covars'

* Mark complete-case sample used in propensity model
gen byte ps_sample = e(sample)

* Predicted propensity score
predict double ps if ps_sample, pr

* Check overlap
histogram ps if GLP1_use == 1 & ps_sample==1, percent name(ps1, replace)
histogram ps if GLP1_use == 0 & ps_sample==1, percent name(ps0, replace)
graph combine ps1 ps0

*check overlap
twoway ///
    (kdensity ps if GLP1_use == 1 & ps_sample == 1) ///
    (kdensity ps if GLP1_use == 0 & ps_sample == 1), ///
    title("Propensity score overlap") ///
    xtitle("Propensity score") ///
    ytitle("Density") ///
    xscale(range(0 1)) ///
    xlabel(0(0.1)1) ///
    legend(order(1 "GLP1-RA exposure" 2 "No GLP1-RA exposure")) ///
    graphregion(color(white))
	
* Stabilized IPTW
summ GLP1_use if ps_sample==1, meanonly
local p_treated = r(mean)

gen double sw = .
replace sw = `p_treated'/ps if GLP1_use == 1 & ps_sample
replace sw = (1 - `p_treated')/(1 - ps) if GLP1_use == 0 & ps_sample

drop if bmi_cat==0

* Check weights
summ sw, detail

logit mort90 i.GLP1_use [pweight = sw] if ps_sample, vce(robust) or

*If you have clustering by hospital, use:

logit mort90 i.GLP1_use [pweight = sw] if ps_sample, ///
    vce(cluster hospital) or base
	


preserve

keep if ps_sample==1
drop if missing(sw)

local balancevars gender age_cat bmi_cat cci_cat smoking alcohol_cat ///
    prev_pancreatitis gallstones_imaging

tempfile love
postfile H str60 variable double smd_unw smd_w using `love', replace

foreach v of local balancevars {

    levelsof `v', local(levels)

    foreach l of local levels {

        gen byte tempcat = (`v'==`l') if !missing(`v')

        quietly summarize tempcat if GLP1_use==1
        scalar p1 = r(mean)

        quietly summarize tempcat if GLP1_use==0
        scalar p0 = r(mean)

        scalar smd1 = 100*(p1-p0)/sqrt((p1*(1-p1)+p0*(1-p0))/2)

        quietly summarize tempcat [aw=sw] if GLP1_use==1
        scalar wp1 = r(mean)

        quietly summarize tempcat [aw=sw] if GLP1_use==0
        scalar wp0 = r(mean)

        scalar smd2 = 100*(wp1-wp0)/sqrt((wp1*(1-wp1)+wp0*(1-wp0))/2)

        local varlabel "`v' = `l'"
        post H ("`varlabel'") (smd1) (smd2)

        drop tempcat
    }
}

postclose H
use `love', clear

gen y = _n

levelsof y, local(ys)
local ylabels
foreach i of local ys {
    local lab = variable[`i']
    local ylabels `ylabels' `i' "`lab'"
}

twoway ///
    (scatter y smd_unw, msymbol(circle) msize(medium)) ///
    (scatter y smd_w, msymbol(diamond) msize(medium)), ///
    xline(0, lcolor(black)) ///
    xline(-10 10, lpattern(dash)) ///
    xlabel(-50(10)50) ///
    ylabel(`ylabels', angle(0) labsize(small)) ///
    ytitle("") ///
    xtitle("Standardised differences (%) between treatment groups") ///
    legend(order(1 "Balance in original sample" 2 "Balance after IPTW") position(6) rows(1)) ///
    title("Love plot: covariate balance before and after IPTW") ///
    graphregion(color(white)) ///
    plotregion(color(white))

restore

save pandora_clean_mi, replace
/*

preserve

tempfile balance
tempname handle

postfile `handle' str80 covariate double before after using `balance', replace

local rawcovars gender age_cat bmi_cat cci_cat ///
    smoking alcohol_cat prev_pancreatitis gallstones_imaging

foreach v of local rawcovars {

    quietly levelsof `v' if ps_sample & !missing(`v'), local(levels)

    local nlev : word count `levels'
    local first : word 1 of `levels'

    foreach lev of local levels {

        * Omit reference category
        if "`lev'" == "`first'" continue

        tempvar d
        gen byte `d' = (`v' == `lev') if ps_sample & !missing(`v')

        * Get value label if present
        local vlab : value label `v'
        if "`vlab'" != "" {
            local levlab : label `vlab' `lev'
        }
        else {
            local levlab "`lev'"
        }

        local covlab "`v': `levlab'"

        * Unweighted proportions
        quietly summarize `d' if GLP1_use == 1 & ps_sample, meanonly
        local p1 = r(mean)

        quietly summarize `d' if GLP1_use == 0 & ps_sample, meanonly
        local p0 = r(mean)

        local den = sqrt((`p1'*(1-`p1') + `p0'*(1-`p0'))/2)

        if missing(`den') | `den' == 0 {
            local smd_before = 0
        }
        else {
            local smd_before = abs((`p1' - `p0')/`den')
        }

        * Weighted proportions
        quietly summarize `d' [aw = sw_trunc] if GLP1_use == 1 & ps_sample, meanonly
        local wp1 = r(mean)

        quietly summarize `d' [aw = sw_trunc] if GLP1_use == 0 & ps_sample, meanonly
        local wp0 = r(mean)

        local wden = sqrt((`wp1'*(1-`wp1') + `wp0'*(1-`wp0'))/2)

        if missing(`wden') | `wden' == 0 {
            local smd_after = 0
        }
        else {
            local smd_after = abs((`wp1' - `wp0')/`wden')
        }

        post `handle' ("`covlab'") (`smd_before') (`smd_after')
    }
}

postclose `handle'

use `balance', clear

gsort -before
gen y = _n

local ylabels
forvalues i = 1/`=_N' {
    local lab = covariate[`i']
    local ylabels `ylabels' `i' "`lab'"
}

twoway ///
    (scatter y before, msymbol(O)) ///
    (scatter y after, msymbol(D)), ///
    xline(0.1, lpattern(dash)) ///
    ylabel(`ylabels', angle(0) labsize(vsmall)) ///
    xlabel(0(.1).6) ///
    xtitle("Absolute standardized mean difference") ///
    ytitle("") ///
    legend(order(1 "Before IPTW" 2 "After IPTW") pos(6) rows(1)) ///
    title("Covariate balance before and after IPTW")

graph export loveplot_iptw.png, replace width(2000)

restore


histogram ps if GLP1_use == 1, percent name(ps_treated, replace)
histogram ps if GLP1_use == 0, percent name(ps_control, replace)
graph combine ps_treated ps_control

*
_pctile sw if ps_sample, p(1 99)
local p1 = r(r1)
local p99 = r(r2)

gen double sw_trunc = sw
replace sw_trunc = `p1' if sw_trunc < `p1' & ps_sample
replace sw_trunc = `p99' if sw_trunc > `p99' & ps_sample

summ sw sw_trunc if ps_sample, detail

*Then use truncated weights:

logit mort90 i.GLP1_use [pweight = sw_trunc] if ps_sample, ///
    vce(robust) or

*Or with common support:

logit mort90 i.GLP1_use [pweight = sw_trunc] if ps_sample & common_support, ///
    vce(robust) or

Then use sw_trunc in your Love plot instead of sw


	
	

save pandora_clean_mi, replace




*Keep if cause = gallstones 

keep if cause_gallstones ==1 & bmi_cat >=2 & bmi_cat !=99 
tab GLP1_use, mi 

*Propensity score
** Confounders for the propensity score model
local covars i.gender i.age_cat i.bmi_cat i.cci_cat ///
    i.smoking i.alcohol_cat i.prev_pancreatitis 

* Estimate propensity of being on GLP1RA
logit GLP1_use `covars'

* Mark complete-case sample used in propensity model
gen byte ps_sample = e(sample)

* Predicted propensity score
predict double ps if ps_sample, pr

* Check overlap
histogram ps if GLP1_use == 1 & ps_sample, percent name(ps1, replace)
histogram ps if GLP1_use == 0 & ps_sample, percent name(ps0, replace)
graph combine ps1 ps0

* Stabilized IPTW
summ GLP1_use if ps_sample, meanonly
local p_treated = r(mean)

gen double sw = .
replace sw = `p_treated'/ps if GLP1_use == 1 & ps_sample
replace sw = (1 - `p_treated')/(1 - ps) if GLP1_use == 0 & ps_sample

* Check weights
summ sw, detail

logit mort90 i.GLP1_use [pweight = sw] if ps_sample, vce(robust) or

*If you have clustering by hospital, use:

logit mort90 i.GLP1_use [pweight = sw] if ps_sample, ///
    vce(cluster hospital) or base
	
tab  GLP1_use mort90, mi row
	
	*COmposte 
	
	logit composite i.GLP1_use [pweight = sw] if ps_sample, vce(robust) or

*If you have clustering by hospital, use:

logit composite i.GLP1_use [pweight = sw] if ps_sample, ///
    vce(cluster hospital) or base
	
	
	
preserve

tempfile balance
tempname handle

postfile `handle' str80 covariate double before after using `balance', replace

local rawcovars gender age_cat bmi_cat cci_cat ///
    smoking alcohol_cat prev_pancreatitis gallstones_imaging

foreach v of local rawcovars {

    quietly levelsof `v' if ps_sample & !missing(`v'), local(levels)

    local nlev : word count `levels'
    local first : word 1 of `levels'

    foreach lev of local levels {

        * Omit reference category
        if "`lev'" == "`first'" continue

        tempvar d
        gen byte `d' = (`v' == `lev') if ps_sample & !missing(`v')

        * Get value label if present
        local vlab : value label `v'
        if "`vlab'" != "" {
            local levlab : label `vlab' `lev'
        }
        else {
            local levlab "`lev'"
        }

        local covlab "`v': `levlab'"

        * Unweighted proportions
        quietly summarize `d' if GLP1_use == 1 & ps_sample, meanonly
        local p1 = r(mean)

        quietly summarize `d' if GLP1_use == 0 & ps_sample, meanonly
        local p0 = r(mean)

        local den = sqrt((`p1'*(1-`p1') + `p0'*(1-`p0'))/2)

        if missing(`den') | `den' == 0 {
            local smd_before = 0
        }
        else {
            local smd_before = abs((`p1' - `p0')/`den')
        }

        * Weighted proportions
        quietly summarize `d' [aw = sw_trunc] if GLP1_use == 1 & ps_sample, meanonly
        local wp1 = r(mean)

        quietly summarize `d' [aw = sw_trunc] if GLP1_use == 0 & ps_sample, meanonly
        local wp0 = r(mean)

        local wden = sqrt((`wp1'*(1-`wp1') + `wp0'*(1-`wp0'))/2)

        if missing(`wden') | `wden' == 0 {
            local smd_after = 0
        }
        else {
            local smd_after = abs((`wp1' - `wp0')/`wden')
        }

        post `handle' ("`covlab'") (`smd_before') (`smd_after')
    }
}

postclose `handle'

use `balance', clear

gsort -before
gen y = _n

local ylabels
forvalues i = 1/`=_N' {
    local lab = covariate[`i']
    local ylabels `ylabels' `i' "`lab'"
}

twoway ///
    (scatter y before, msymbol(O)) ///
    (scatter y after, msymbol(D)), ///
    xline(0.1, lpattern(dash)) ///
    ylabel(`ylabels', angle(0) labsize(vsmall)) ///
    xlabel(0(.1).6) ///
    xtitle("Absolute standardized mean difference") ///
    ytitle("") ///
    legend(order(1 "Before IPTW" 2 "After IPTW") pos(6) rows(1)) ///
    title("Covariate balance before and after IPTW")

graph export loveplot_iptw.png, replace width(2000)

restore


histogram ps if GLP1_use == 1, percent name(ps_treated, replace)
histogram ps if GLP1_use == 0, percent name(ps_control, replace)
graph combine ps_treated ps_control

*
_pctile sw if ps_sample, p(1 99)
local p1 = r(r1)
local p99 = r(r2)

gen double sw_trunc = sw
replace sw_trunc = `p1' if sw_trunc < `p1' & ps_sample
replace sw_trunc = `p99' if sw_trunc > `p99' & ps_sample

summ sw sw_trunc if ps_sample, detail

*Then use truncated weights:

logit mort90 i.GLP1_use [pweight = sw_trunc] if ps_sample, ///
    vce(robust) or

*Or with common support:

logit mort90 i.GLP1_use [pweight = sw_trunc] if ps_sample & common_support, ///
    vce(robust) or

Then use sw_trunc in your Love plot instead of sw