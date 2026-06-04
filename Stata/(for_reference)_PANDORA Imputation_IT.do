clear all

cd C:\Users\AdilRashid\Documents\PANDORA

use pandora_clean_mi, clear

codebook mort90 GLP1_use gender age_cat bmi_cat cci_cat smoking alcohol_cat prev_pancreatitis gallstones_imaging

*Missing categorical data for BMI, smoking, alcohol - impute

foreach V of varlist bmi_cat  smoking alcohol_cat {
	recode `V' 99=.
}

*Check % with complete data
gen complete = 0 

replace complete =1 if (bmi_cat!=. &  smoking!=. & alcohol_cat!=.)

tab complete, mi	//36% missing

*Check individual missing data for data variables
foreach V of varlist mort90 GLP1_use gender age_cat bmi_cat cci_cat smoking alcohol_cat prev_pancreatitis gallstones_imaging {
	tab `V', mi
}

*BMI 13.0% missing, smoking 199.4% missing, alcohol 19.5% missing

gen miss_los =sign(los==.)

*Imputation

mi set flong

mi register regular mort90 GLP1_use gender age_cat  cci_cat  prev_pancreatitis gallstones_imaging readm90 critical_care_adm_bin local_complication severity_bin referral resp_failure cvs_failure renal_failure

mi register imputed bmi_cat smoking alcohol_cat crp los diabetes

mi describe 

mi impute chained  (pmm, knn(5)) bmi_cat smoking alcohol_cat crp los diabetes = mort90 GLP1_use gender age_cat  cci_cat  prev_pancreatitis gallstones_imaging readm90 critical_care_adm_bin local_complication severity_bin referral resp_failure cvs_failure renal_failure , add(20) rseed(678910) chaindots augment

save pandor_mi, replace

*90-day mortality	
mi estimate,   base or:	xtmelogit mort90 i.GLP1_use || hospital:
	
mi estimate, or base:	xtmelogit mort90 i.GLP1_use i.gender i.age_cat ib1.bmi_cat i.cci_cat i.smoking i.alcohol_cat i.prev_pancreatitis i.gallstones_imaging || hospital:
	
	

	
mi estimate,   base or cmdok:	melogit readm90 i.GLP1_use  || hospital:, base or
	
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