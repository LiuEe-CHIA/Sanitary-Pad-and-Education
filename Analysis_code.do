************************************************************************
//******************** Clean Data for Estimation **********************//
************************************************************************
***full sample
use datause, clear
*keep girls only (girls school and mixed school)
keep if caste=="TOTAL" & (gender==2 & (schtype==2 | schtype==3))
*set balanced panel from 2013-2018
forval i=1/10{
bysort school_gender: egen num_chg_enrolment`i' = count(chg_enrolment`i')
gen balancedpanel`i' = num_chg_enrolment`i' == 6
}
*generate treatment period
gen treatment_period=year-t_machine_year if !missing(t_machine_year)
gen aft=treatment_period>=0 & gender==2 & t_machine_installed==1 & !missing(treatment_period)
forval i=1/6 {
gen before`i'=(treatment_period==-`i' & gender==2 & t_machine_installed==1)
lab var before`i' "-`i'"
}
forval i=0/4 {
gen after`i'=(treatment_period==`i' & gender==2 & t_machine_installed==1)
lab var after`i' "`i'"
}
save data, replace

***subsample after merging census
use datause, clear
*merge pca (keep observations with info)
merge m:1 block_name village_name using pca_village_match_1, keepusing(village_code cd_block_code)
rename village_code village_code1
rename cd_block_code cd_block_code1
drop _m
merge m:1 block_name village_name pincode using pca_village_match_2clean, keepusing(village_code cd_block_code)
drop if _m==2
drop _m
rename village_code village_code2
rename cd_block_code cd_block_code2
gen village_code=village_code1
replace village_code=village_code2 if missing(village_code1) & !missing(village_code2)
gen cd_block_code=cd_block_code1
replace cd_block_code=cd_block_code2 if missing(cd_block_code1) & !missing(cd_block_code2)
merge m:1 village_code cd_block_code using pca_amenities, keepusing(x_is_the_area_covered_under_tota community_waste_disposal_system_ post_office__status_a_1__na_2__ telephone__landlines___status_a_ mobile_phone_coverage__status_a_ internet_cafes___common_service_ all_weather_road__status_a_1__na public_distribution_system__pds_ sports_club_recreation_centre__s cinema_video_hall__status_a_1__n public_library__status_a_1__na_2 power_supply_for_domestic_use__s power_supply_for_commercial_use_ tap_water_treated_functioning_al sub_district_head_quarter__dista district_head_quarter__distance_ nearest_statutory_town__distance within_the_state_ut__distance_in outside_the_state_ut_distance__i total_geographical_area__in_hect total__households_ total_population_of_village total_male_population_of_village total_female_population_of_villa total_scheduled_castes_populatio total_scheduled_castes_male_popu total_scheduled_castes_female_po total_scheduled_tribes_populatio total_scheduled_tribes_male_popu total_scheduled_tribes_female_po community_health_centre__numbers primary_health_centre__numbers_ primary_health_sub_centre__numbe maternity_and_child_welfare_cent family_welfare_centre__numbers_ non_government_medical_facilitie v207 v208 x_is_the_area_covered_under_tota community_toilet_complex__includ community_toilet_complex__exclud rural_production_centres_or_sani rural_production_mart_or_sanitar telephone__landlines___status_a_ public_call_office__mobile__pco_ mobile_phone_coverage__status_a_ internet_cafes___common_service_ private_courier_facility__status public_bus_service__status_a_1__ private_bus_service__status_a_1_ railway_station__status_a_1__na_ auto_modified_autos__status_a_1_ taxi__status_a_1__na_2__ vans__status_a_1__na_2__ tractors__status_a_1__na_2__ national_highway__status_a_1__na state_highway__status_a_1__na_2_ major_district_road__status_a_1_ other_district_road__status_a_1_ black_topped__pucca__road__statu gravel__kuchha__roads__status_a_ all_weather_road__status_a_1__na nutritional_centres_icds__status nutritional_centres_anganwadi_ce mandis_regular_market__status_a_ public_distribution_system__pds_ asha__status_a_1__na_2__ community_centre_with_without_tv nearest_town_distance_from_villa)
keep if _m==3
drop _m
*keep girls only (girls school and mixed school)
keep if gender==2 & (schtype==2 | schtype==3) & caste=="TOTAL"
*set balanced panel from 2013-2018
forval i=1/10{
bysort school_gender: egen num_chg_enrolment`i' = count(chg_enrolment`i')
gen balancedpanel`i' = num_chg_enrolment`i' == 6
}
gen treatment_period=year-t_machine_year if !missing(t_machine_year)
gen aft=treatment_period>=0 & gender==2 & t_machine_installed==1 & !missing(treatment_period)

save datapc, replace

*************************************************************************
//********************* Main Figures and Tables ***********************//
*************************************************************************
*year-interacted controls
global yearinteracted
global controls "total_enrolbg_2013 bookinlib_2013 toiletd_2013 toiletb_2013 toiletg_2013 clrooms_2013 clgood_2013 smc_2013 smcsdp_2013 medchk_2013 electric_2013 library_2013 playground_2013 ramps_2013 cal_2013 access_2013 water_hp_2013 water_well_2013 water_tap_2013 water_others_2013 bld_govt_2013 bld_uc_2013 bld_dilap_2013 wall_pucca_2013 wall_wire_2013 wall_hedges_2013 wall_na_2013"
foreach v in $controls {
  global yearinteracted $yearinteracted c.`v'#i.year
}

//***************************** Table 1 *****************************//
*regression
reghdfe chg_enrolment5 aft $yearinteracted if balancedpanel5==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, replace nocons bdec(3) sdec(3) ctitle(5) keep(aft) 
preserve
keep if e(sample)
duplicates drop schcd, force
tab t_machine_installed
restore 
forval i = 6/10 {
reghdfe chg_enrolment`i' aft $yearinteracted if balancedpanel`i'==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(`i') keep(aft)
preserve
keep if e(sample)
duplicates drop schcd, force
tab t_machine_installed
restore 
}

************************************************************************
//***************************** Figure 1 *****************************//
*regression
forval i=5/10 {
reghdfe chg_enrolment`i' before6 before5 before4 before3 before2 after0 after1 after2 after3 before1 $yearinteracted if balancedpanel`i'==1 , absorb(i.school_gender##c.year blockyear) vce(cluster schcd)
est store Dynamic`i'
coefplot Dynamic`i', omitted keep(before* after*) order(before6 before5 before4 before3 before2  before1 after0 after1 after2 after3) vertical recast(connect) ciopts(recast(rcap) lpattern(dash) lcolor(black%90)) lpattern(solid) mcolor(black) lcolor(black) yline(0,lcolor(gray%50)) xtitle(Treatment Period) ylabel(-1.5(.5)2,angle(90) glcolor(gs15%50)) scale(titlegap(.5) outergap(.5)) graphregion(color(white) margin(2 2 2 2)) plotregion(lcolor(black)) xline(6, lcolor(red%50)) title(`i'th Grade) ytitle(DID Estimates)
graph save figure`i',replace
}
graph combine figure5.gph figure6.gph figure7.gph figure8.gph figure9.gph figure10.gph, row(2) col(3) ysize(10) xsize(20) commonscheme graphregion(color(white))

***********************************************************************
//***************************** Table 2 *****************************//
***Bacondecomp
xtset school_gender year
*grade 5 to 10
forval i=5/10 {
preserve
keep if !missing(chg_enrolment`i')
*create balanced data
egen mminyear=min(year),by(schcd)
egen mmaxyear=max(year),by(schcd)
gen ccount=1
egen fcount=sum(ccount),by(schcd)
drop if mminyear!=2013 | mmaxyear!=2018 | fcount!=6
bacondecomp chg_enrolment`i' aft, ddetail
restore
}
***Towayfeweights
*controls
preserve
gen yearindicator=year-2012 if !missing(year)
local controls "total_enrolbg_2013 bookinlib_2013 toiletd_2013 toiletb_2013 toiletg_2013 clrooms_2013 clgood_2013 smc_2013 smcsdp_2013 medchk_2013 electric_2013 library_2013 playground_2013 ramps_2013 cal_2013 access_2013 water_hp_2013 water_well_2013 water_tap_2013 water_others_2013 bld_govt_2013 bld_uc_2013 bld_dilap_2013 wall_pucca_2013 wall_wire_2013 wall_hedges_2013 wall_na_2013"
foreach v in $controls {
forval i=1/6 {
 gen yi_`v'_`i' = `v'*(yearindicator==`i')
}
}
local yicontrols yi_*
forval i = 5/10 {
twowayfeweights chg_enrolment`i' school_gender year t_machine_installation if balancedpanel`i'==1, type(feTR) controls($yicontrols)
}
restore
***DIDM
forval i = 5/10 {
preserve
*controls
gen yearindicator=year-2012 if !missing(year)
local controls "total_enrolbg_2013 bookinlib_2013 toiletd_2013 toiletb_2013 toiletg_2013 clrooms_2013 clgood_2013 smc_2013 smcsdp_2013 medchk_2013 electric_2013 library_2013 playground_2013 ramps_2013 cal_2013 access_2013 water_hp_2013 water_well_2013 water_tap_2013 water_others_2013 bld_govt_2013 bld_uc_2013 bld_dilap_2013 wall_pucca_2013 wall_wire_2013 wall_hedges_2013 wall_na_2013"
foreach v in $controls {
forval c=1/6 {
 gen yi_`v'_`c' = `v'*(yearindicator==`c')
}
}
local yicontrols yi_*
did_multiplegt chg_enrolment`i' school_gender year t_machine_installation if balancedpanel`i'==1, cluster(schcd) breps(300) ///
placebo(2) ///
covariances jointtestplacebo ///
trends_nonparam(blockcode) trends_lin(school_gender) seed(1234) ///
controls($yicontrols) save_results(bdid`i')
ereturn list
restore
}

********************************************************************************
//***************************** Table 3 & Supp F6 *****************************//
***Alternative 1: untreated schools in same village***
reghdfe chg_enrolment7 aft $yearinteracted if balancedpanel7==1, absorb(school_gender##c.year villageyear) vce(cluster schcd)
outreg2 using 1.doc, replace nocons bdec(3) sdec(3) ctitle(1) keep(aft) 
preserve
keep if e(sample)
duplicates drop schcd, force
tab t_machine_installed
restore

reghdfe chg_enrolment7 before6 before5 before4 before3 before2 after0 after1 after2 after3 before1 $yearinteracted if balancedpanel7==1 , absorb(school_gender##c.year villageyear) vce(cluster schcd)
est store A1Dynamic7
coefplot A1Dynamic7, omitted keep(before* after*) order(before6 before5 before4 before3 before2  before1 after0 after1 after2 after3) vertical recast(connect) ciopts(recast(rcap) lpattern(dash) lcolor(black%90)) lpattern(solid) mcolor(black) lcolor(black) yline(0,lcolor(gray%50)) xtitle(Treatment Period) ylabel(,angle(90) glcolor(gs15%50)) scale(titlegap(.5) outergap(.5)) graphregion(color(white) margin(2 2 2 2)) plotregion(lcolor(black)) xline(6, lcolor(red%50)) title(Alternative Control Group 1) ytitle(DID Estimates)
graph save figure3a1,replace

***Alternative 2: same-grade boys in untreated schools***
use datause, replace
keep if caste=="TOTAL" & ((gender==2 & t_machine_installed==1) | (gender==1 & t_machine_installed==0))
*set balanced panel from 2013-2018
forval i=1/10{
bysort school_gender: egen num_chg_enrolment`i' = count(chg_enrolment`i')
gen balancedpanel`i' = num_chg_enrolment`i' == 6
}
*generate treatment period
gen treatment_period=year-t_machine_year if !missing(t_machine_year)
gen aft=treatment_period>=0 & gender==2 & t_machine_installed==1  & !missing(treatment_period)
reghdfe chg_enrolment7 aft $yearinteracted if balancedpanel7==1, absorb(school_gender##c.year villageyear) vce(cluster schcd)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(2) keep(aft) 
preserve
keep if e(sample)
duplicates drop schcd, force
tab t_machine_installed
restore
*generate period dummies
forval i=1/6 {
gen before`i'=(treatment_period==-`i' & gender==2 & t_machine_installed==1)
lab var before`i' "-`i'"
}
forval i=0/3 {
gen after`i'=(treatment_period==`i' & gender==2 & t_machine_installed==1)
lab var after`i' "`i'"
}
reghdfe chg_enrolment7 before6 before5 before4 before3 before2 after0 after1 after2 after3 before1 $yearinteracted if balancedpanel7==1, absorb(school_gender##c.year villageyear) vce(cluster schcd)
est store A2Dynamic7
coefplot A2Dynamic7, omitted keep(before* after*) order(before6 before5 before4 before3 before2  before1 after0 after1 after2 after3) vertical recast(connect) ciopts(recast(rcap) lpattern(dash) lcolor(black%90)) lpattern(solid) mcolor(black) lcolor(black) yline(0,lcolor(gray%50)) xtitle(Treatment Period) ylabel(,angle(90) glcolor(gs15%50)) scale(titlegap(.5) outergap(.5)) graphregion(color(white) margin(2 2 2 2)) plotregion(lcolor(black)) xline(6, lcolor(red%50)) title(Alternative Control Group 2) ytitle(DID Estimates)
graph save figure3b1,replace

***Alternative 3,4,5: prepubsecent boys and girls in treated school***
/*use datause, replace
gen rid=_n

foreach i in 1 2 3 4 5 6 7 8 9 10 11{
rename chg_enrolment`i' chg_enrolment_grade`i'
}
reshape long chg_enrolment_grade, i(rid) j(grade)
drop if missing(chg_enrolment_grade)

egen school_gender_grade=group(schcd gender grade)
save reshapedata, replace*/

use reshapedata, clear
*keep caste
keep if caste=="TOTAL" & t_machine_installed==1
*set balanced panel from 2013-2018
egen num_chg_enrolment = count(chg_enrolment_grade), by(school_gender_grade)
gen balancedpanel = num_chg_enrolment==6
*generate treatment period
gen treatment_period=year-t_machine_year if !missing(t_machine_year)
gen aft=treatment_period>=0 & gender==2 & t_machine_installed==1 & grade==7  & !missing(treatment_period)

*prepubescent girls in grade<=3 & blockyear FE
reghdfe chg_enrolment_grade aft $yearinteracted if (grade<=3 | grade==7) & gender==2 & balancedpanel==1, absorb(school_gender_grade##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(3) keep(aft) 
preserve
keep if e(sample)
duplicates drop schcd grade, force
tab t_machine_installed grade
restore

*prepubescent girls in grade<=5 & blockyear FE
reghdfe chg_enrolment_grade aft $yearinteracted if (grade<=5 | grade==7) & gender==2 & balancedpanel==1, absorb(school_gender_grade##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(3) keep(aft) 
preserve
keep if e(sample)
duplicates drop schcd grade, force
tab t_machine_installed grade
restore

*prepubescent girls and boys in grade<=5 & blockyear FE
reghdfe chg_enrolment_grade aft $yearinteracted if (grade<=5 | (grade==7 & gender==2)) & balancedpanel==1, absorb(school_gender_grade##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(3) keep(aft) 
preserve
keep if e(sample)
duplicates drop schcd grade gender, force
tab t_machine_installed gender if grade==7
tab t_machine_installed gender if grade<=5
restore

*prepubescent girls and boys in grade<=5 & school FE
reghdfe chg_enrolment_grade aft if (grade<=5 | (grade==7 & gender==2)) & balancedpanel==1, absorb(school_gender_grade##c.year school_year) vce(cluster schcd)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(3) keep(aft) 
preserve
keep if e(sample)
duplicates drop schcd grade gender, force
tab t_machine_installed gender if grade==7
tab t_machine_installed gender if grade<=5
restore

*generate period dummies
forval i=1/6 {
gen before`i'=(treatment_period==-`i' & gender==2 & t_machine_installed==1 & grade==7)
lab var before`i' "-`i'"
}
forval i=0/3 {
gen after`i'=(treatment_period==`i' & gender==2 & t_machine_installed==1 & grade==7)
lab var after`i' "`i'"
}
*prepubescent girls in grade<=3 & blockyear FE
reghdfe chg_enrolment_grade before6 before5 before4 before3 before2 after0 after1 after2 after3 before1 $yearinteracted if (grade<=3 | grade==7) & gender==2 & balancedpanel==1, absorb(school_gender_grade##c.year blockyear) vce(cluster schcd)
est store A3Dynamic7
coefplot A3Dynamic7, omitted keep(before* after*) order(before6 before5 before4 before3 before2  before1 after0 after1 after2 after3) vertical recast(connect) ciopts(recast(rcap) lpattern(dash) lcolor(black%90)) lpattern(solid) mcolor(black) lcolor(black) yline(0,lcolor(gray%50)) xtitle(Treatment Period) ylabel(,angle(90) glcolor(gs15%50)) scale(titlegap(.5) outergap(.5)) graphregion(color(white) margin(2 2 2 2)) plotregion(lcolor(black)) xline(6, lcolor(red%50)) title(Alternative Control Group 3) ytitle(DID Estimates)
graph save figure3c1,replace

*prepubescent girls in grade<=5 & blockyear FE
reghdfe chg_enrolment_grade before6 before5 before4 before3 before2 after0 after1 after2 after3 before1 $yearinteracted if (grade<=5 | grade==7) & gender==2 & balancedpanel==1, absorb(school_gender_grade##c.year blockyear) vce(cluster schcd)
est store A3Dynamic7
coefplot A3Dynamic7, omitted keep(before* after*) order(before6 before5 before4 before3 before2  before1 after0 after1 after2 after3) vertical recast(connect) ciopts(recast(rcap) lpattern(dash) lcolor(black%90)) lpattern(solid) mcolor(black) lcolor(black) yline(0,lcolor(gray%50)) xtitle(Treatment Period) ylabel(,angle(90) glcolor(gs15%50)) scale(titlegap(.5) outergap(.5)) graphregion(color(white) margin(2 2 2 2)) plotregion(lcolor(black)) xline(6, lcolor(red%50)) title(Alternative Control Group 4) ytitle(DID Estimates)
graph save figure3d1,replace

*prepubescent girls and boys in grade<=5 & blockyear FE
reghdfe chg_enrolment_grade before6 before5 before4 before3 before2 after0 after1 after2 after3 before1 $yearinteracted if (grade<=5 | (grade==7 & gender==2)) & balancedpanel==1, absorb(school_gender_grade##c.year blockyear) vce(cluster schcd)
est store A3Dynamic7
coefplot A3Dynamic7, omitted keep(before* after*) order(before6 before5 before4 before3 before2  before1 after0 after1 after2 after3) vertical recast(connect) ciopts(recast(rcap) lpattern(dash) lcolor(black%90)) lpattern(solid) mcolor(black) lcolor(black) yline(0,lcolor(gray%50)) xtitle(Treatment Period) ylabel(,angle(90) glcolor(gs15%50)) scale(titlegap(.5) outergap(.5)) graphregion(color(white) margin(2 2 2 2)) plotregion(lcolor(black)) xline(6, lcolor(red%50)) title(Alternative Control Group 5) ytitle(DID Estimates)
graph save figure3e1,replace

*prepubescent girls and boys in grade<=5 & school FE
reghdfe chg_enrolment_grade before6 before5 before4 before3 before2 after0 after1 after2 after3 before1 if (grade<=5 | (grade==7 & gender==2)) & balancedpanel==1, absorb(school_gender_grade##c.year school_year) vce(cluster schcd)
est store A3Dynamic7
coefplot A3Dynamic7, omitted keep(before* after*) order(before6 before5 before4 before3 before2  before1 after0 after1 after2 after3) vertical recast(connect) ciopts(recast(rcap) lpattern(dash) lcolor(black%90)) lpattern(solid) mcolor(black) lcolor(black) yline(0,lcolor(gray%50)) xtitle(Treatment Period) ylabel(,angle(90) glcolor(gs15%50)) scale(titlegap(.5) outergap(.5)) graphregion(color(white) margin(2 2 2 2)) plotregion(lcolor(black)) xline(6, lcolor(red%50)) title(Alternative Control Group 5) ytitle(DID Estimates)
graph save figure3f1,replace

graph combine figure3a1.gph figure3b1.gph figure3c1.gph figure3d1.gph figure3e1.gph figure3f1.gph, row(3) col(2) ysize(15) xsize(13) commonscheme graphregion(color(white))

**********************************************************************
//***************************** Table 4 *****************************//
***Adjusted Dropout***
use data, clear
merge m:1 gender schcd year using enrolment_adjusted2, keepusing(adjusted2chg_*) nogen
reghdfe adjusted2chg_enrolment7 aft $yearinteracted if balancedpanel7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, replace nocons bdec(3) sdec(3) ctitle(1) keep(aft) 
preserve
keep if e(sample)
duplicates drop schcd, force
tab t_machine_installed
restore

***block-schcat-year fixed effects***
reghdfe chg_enrolment7 aft $yearinteracted if balancedpanel7==1, absorb(school_gender##c.year block_schcat_year) vce(cluster schcd)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(3) keep(aft) 
preserve
keep if e(sample)
duplicates drop schcd, force
tab t_machine_installed
restore

***Sample restrict to only one schools in village***
merge m:1 villagecode using village_schoolcat2, nogen
reghdfe chg_enrolment7 aft $yearinteracted if balancedpanel7==1 & grade8==1, absorb(school_gender blockyear) vce(cluster schcd) su(mean)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(1) keep(aft)
preserve
keep if e(sample)
duplicates drop schcd, force
tab t_machine_installed
restore

***Toilets***
*SHE toilet
gen she_toilet_year=shetoiletwomenfriendlytoiletinst
gen confounded=she_toilet_year>=t_machine_year if !missing(t_machine_year) & !missing(she_toilet_year)
gen toilet_treat=toiletg if t_machine_installed==1 & year==t_machine_year
egen maxtoilet_treat=max(toilet_treat), by(schcd)
gen diff=1 if toiletg!=maxtoilet_treat & year>=t_machine_year & t_machine_installed==1
egen todrop=max(diff), by(schcd)
*drop control and treated with toilet installation
reghdfe chg_enrolment7 aft $yearinteracted if balancedpanel7==1 & (she_toilet_year==. | she_toilet_year<2013), absorb(school_gender##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(3) keep(aft) 
preserve
keep if e(sample)
duplicates drop schcd, force
tab t_machine_installed
restore
*drop treated schools with changing number of toilet after treatment
reghdfe chg_enrolment7 aft $yearinteracted if  balancedpanel7==1 & todrop!=1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(3) keep(aft) 
preserve
keep if e(sample)
duplicates drop schcd, force
tab t_machine_installed
restore
*time-varying covariates
local tvcontrols "total_enrolbg bookinlib toiletd toiletb toiletg clrooms clgood smc smcsdp medchk electric library playground ramps cal access water_hp water_well water_tap water_others bld_govt bld_uc bld_dilap wall_pucca wall_wire wall_hedges wall_na"
reghdfe chg_enrolment7 aft $tvcontrols if  balancedpanel7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(3) keep(aft) 
preserve
keep if e(sample)
duplicates drop schcd, force
tab t_machine_installed
restore

**********************************************************************
//***************************** Table 5 *****************************//
use data, clear
*generate variable
gen gtoiletratio=c7_2013/toiletg_2013
replace gtoiletratio=0 if toiletg_2013==0
summ gtoiletratio if year==2013 & balancedpanel7==1
gen gtoilet_ratio=gtoiletratio>=7.772192

gen gtoiletfratio=c7_2013/toiletg_func_2013
replace gtoiletfratio=0 if toiletg_func_2013==0
summ gtoiletfratio if year==2013 & balancedpanel7==1
gen gtoiletf_ratio=gtoiletfratio>=8.539367

*generate interactions
gen aft_gtoilet=aft*(toiletg_2013>0)
gen aft_ngtoilet=aft*(toiletg_2013==0)
gen aft_gtoiletf=aft*(toiletg_func_2013>0)
gen aft_ngtoiletf=aft*(toiletg_func_2013==0)

gen aft_btoilet=aft*(toiletb_2013>0)
gen aft_nbtoilet=aft*(toiletb_2013==0)
gen aft_btoiletf=aft*(toiletb_func_2013>0)
gen aft_nbtoiletf=aft*(toiletb_func_2013==0)

gen aft_gtoilet_ratio=aft*(gtoilet_ratio==1)
gen aft_ngtoilet_ratio=aft*(gtoilet_ratio==0)
gen aft_gtoiletf_ratio=aft*(gtoiletf_ratio==1)
gen aft_ngtoiletf_ratio=aft*(gtoiletf_ratio==0)

gen btoiletf=toiletb_func_2013>0
gen btoilet=toiletb_2013>0
gen gtoiletf=toiletg_func_2013>0
gen gtoilet=toiletg_2013>0

gen christian=strpos(school_name,"S.T")>0 | strpos(school_name,"ST")>0 | strpos(school_name,"SAINT")>0 | strpos(school_name,"HOLY")>0 | strpos(school_name,"CHURCH")>0 | strpos(school_name,"SNT")>0
gen aft_christian=aft*christian
gen aft_nchristian=aft*(christian==0)

reghdfe chg_enrolment7 aft_gtoilet aft_ngtoilet $yearinteracted if balancedpanel7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, replace nocons bdec(3) sdec(3) ctitle(5) keep(aft*) 
test aft_gtoilet=aft_ngtoilet
tab gtoilet t_machine_installed if year==2013 & e(sample)

reghdfe chg_enrolment7 aft_gtoiletf aft_ngtoiletf $yearinteracted if balancedpanel7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(5) keep(aft*) 
test aft_gtoiletf=aft_ngtoiletf
tab gtoiletf t_machine_installed if year==2013 & e(sample)

reghdfe chg_enrolment7 aft_btoilet aft_nbtoilet $yearinteracted if balancedpanel7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(5) keep(aft*) 
test aft_btoilet=aft_nbtoilet
tab btoilet t_machine_installed if year==2013 & e(sample)

reghdfe chg_enrolment7 aft_btoiletf aft_nbtoiletf $yearinteracted if balancedpanel7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(5) keep(aft*) 
test aft_btoiletf=aft_nbtoilet
tab btoiletf t_machine_installed if year==2013 & e(sample)

reghdfe chg_enrolment7 aft_gtoilet_ratio aft_ngtoilet_ratio $yearinteracted if balancedpanel7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(5) keep(aft*) 
test aft_gtoilet=aft_ngtoilet
tab gtoilet_ratio t_machine_installed if year==2013 & e(sample)

reghdfe chg_enrolment7 aft_gtoiletf_ratio aft_ngtoiletf_ratio $yearinteracted if balancedpanel7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(5) keep(aft*) 
test aft_gtoiletf_ratio=aft_ngtoiletf_ratio
tab gtoiletf_ratio t_machine_installed if year==2013 & e(sample)

reghdfe chg_enrolment7 aft_christian aft_nchristian $yearinteracted if balancedpanel7==1 , absorb(school_gender##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, replace nocons bdec(3) sdec(3) ctitle(5) keep(aft_christian aft_nchristian) 
test aft_christian=aft_nchristian if year==2013 & e(sample)

********************************************************************************
//***************************** Figure 2 & Supp F9 ***************************//
use data, clear
drop logappr* logpass* logp60*
foreach g in 4 7 {
local j=`g'+1
bysort schcd caste gender (year): gen logappr`g'=log(appr`j'+1) if !missing(appr`j') & highclass>=`g' & lowclass<`g'
bysort schcd caste gender (year): gen logpass`g'=log(pass`j'+1) if !missing(pass`j') & highclass>=`g' & lowclass<`g'
bysort schcd caste gender (year): gen logp60_`g'=log(p60_`j'+1) if !missing(p60_`j') & highclass>=`g' & lowclass<`g'
}
*set balanced panel from 2013-2016
foreach i in 4 7{
bysort school_gender: egen num_logappr`i' = count(logappr`i')
gen bp_logappr`i' = num_logappr`i' == 4
}

*log Exam 7th
reghdfe logappr7 before6 before5 before4 before3 before2 after0 after1 before1 $yearinteracted if bp_logappr7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
test (before6=0) (before5=0) (before4=0) (before3=0) (before2=0)
est store Dynamic7
coefplot Dynamic7, omitted keep(before* after*) order(before6 before5 before4 before3 before2 before1 after0 after1) vertical recast(connect) ciopts(recast(rcap) lpattern(dash) lcolor(black%90)) lpattern(solid) mcolor(black) lcolor(black) yline(0,lcolor(gray%50)) xtitle(Treatment Period) ylabel(,angle(90) glcolor(gs15%50)) scale(titlegap(.5) outergap(.5)) graphregion(color(white) margin(2 2 2 2)) plotregion(lcolor(black)) xline(6, lcolor(red%50)) title(Log Appear) ytitle(DID Estimates)
graph save figure8a,replace

reghdfe logpass7 before6 before5 before4 before3 before2 after0 after1 before1 $yearinteracted if bp_logappr7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
test (before6=0) (before5=0) (before4=0) (before3=0) (before2=0)
est store Dynamic7
coefplot Dynamic7, omitted keep(before* after*) order(before6 before5 before4 before3 before2 before1 after0 after1) vertical recast(connect) ciopts(recast(rcap) lpattern(dash) lcolor(black%90)) lpattern(solid) mcolor(black) lcolor(black) yline(0,lcolor(gray%50)) xtitle(Treatment Period) ylabel(,angle(90) glcolor(gs15%50)) scale(titlegap(.5) outergap(.5)) graphregion(color(white) margin(2 2 2 2)) plotregion(lcolor(black)) xline(6, lcolor(red%50)) title(Log Pass) ytitle(DID Estimates)
graph save figure8b,replace

reghdfe logp60_7 before6 before5 before4 before3 before2 after0 after1 before1 $yearinteracted if bp_logappr7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
test (before6=0) (before5=0) (before4=0) (before3=0) (before2=0)
est store Dynamic7
coefplot Dynamic7, omitted keep(before* after*) order(before6 before5 before4 before3 before2 before1 after0 after1) vertical recast(connect) ciopts(recast(rcap) lpattern(dash) lcolor(black%90)) lpattern(solid) mcolor(black) lcolor(black) yline(0,lcolor(gray%50)) xtitle(Treatment Period) ylabel(,angle(90) glcolor(gs15%50)) scale(titlegap(.5) outergap(.5)) graphregion(color(white) margin(2 2 2 2)) plotregion(lcolor(black)) xline(6, lcolor(red%50)) title(Log Pass with Distinction) ytitle(DID Estimates)
graph save figure8c,replace

reghdfe pcnt_appear7 before6 before5 before4 before3 before2 after0 after1 before1 $yearinteracted if bp_logappr7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd) su(mean)
est store Dynamic7
coefplot Dynamic7, omitted keep(before* after*) order(before6 before5 before4 before3 before2 before1 after0 after1) vertical recast(connect) ciopts(recast(rcap) lpattern(dash) lcolor(black%90)) lpattern(solid) mcolor(black) lcolor(black) yline(0,lcolor(gray%50)) xtitle(Treatment Period) ylabel(-.4(.2).6,angle(90) glcolor(gs15%50)) scale(titlegap(.5) outergap(.5)) graphregion(color(white) margin(2 2 2 2)) plotregion(lcolor(black)) xline(6, lcolor(red%50)) title(Rate of Appearance) ytitle(DID Estimates)
graph save figure8d,replace

reghdfe pcnt_pass7 before6 before5 before4 before3 before2 after0 after1 before1 $yearinteracted if bp_logappr7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd) su(mean)
est store Dynamic7
coefplot Dynamic7, omitted keep(before* after*) order(before6 before5 before4 before3 before2 before1 after0 after1) vertical recast(connect) ciopts(recast(rcap) lpattern(dash) lcolor(black%90)) lpattern(solid) mcolor(black) lcolor(black) yline(0,lcolor(gray%50)) xtitle(Treatment Period) ylabel(-.4(.2).6,angle(90) glcolor(gs15%50)) scale(titlegap(.5) outergap(.5)) graphregion(color(white) margin(2 2 2 2)) plotregion(lcolor(black)) xline(6, lcolor(red%50)) title(Rate of Passing) ytitle(DID Estimates)
graph save figure8e,replace

reghdfe pcnt_p60_7 before6 before5 before4 before3 before2 after0 after1 before1 $yearinteracted if bp_logappr7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd) su(mean)
est store Dynamic7
coefplot Dynamic7, omitted keep(before* after*) order(before6 before5 before4 before3 before2 before1 after0 after1) vertical recast(connect) ciopts(recast(rcap) lpattern(dash) lcolor(black%90)) lpattern(solid) mcolor(black) lcolor(black) yline(0,lcolor(gray%50)) xtitle(Treatment Period) ylabel(-.4(.2).6,angle(90) glcolor(gs15%50)) scale(titlegap(.5) outergap(.5)) graphregion(color(white) margin(2 2 2 2)) plotregion(lcolor(black)) xline(6, lcolor(red%50)) title(Rate of Passing w/ Distinction) ytitle(DID Estimates)
graph save figure8f,replace

graph combine figure8a.gph  figure8b.gph figure8c.gph figure8d.gph figure8e.gph figure8f.gph, row(3) col(3) ysize(13) xsize(20) commonscheme graphregion(color(white))

*log Exam 4th
reghdfe logappr4 before6 before5 before4 before3 before2 after0 after1 before1 $yearinteracted if bp_logappr4==1, absorb(i.school_gender##c.year blockyear) vce(cluster schcd)
est store Dynamic7
coefplot Dynamic7, omitted keep(before* after*) order(before6 before5 before4 before3 before2 before1 after0 after1) vertical recast(connect) ciopts(recast(rcap) lpattern(dash) lcolor(black%90)) lpattern(solid) mcolor(black) lcolor(black) yline(0,lcolor(gray%50)) xtitle(Treatment Period) ylabel(,angle(90) glcolor(gs15%50)) scale(titlegap(.5) outergap(.5)) graphregion(color(white) margin(2 2 2 2)) plotregion(lcolor(black)) xline(6, lcolor(red%50)) title(Log Appear) ytitle(DID Estimates)
graph save figure8a2,replace

reghdfe logpass4 before6 before5 before4 before3 before2 after0 after1 before1 $yearinteracted if bp_logappr4==1, absorb(i.school_gender##c.year blockyear) vce(cluster schcd)
est store Dynamic7
coefplot Dynamic7, omitted keep(before* after*) order(before6 before5 before4 before3 before2 before1 after0 after1) vertical recast(connect) ciopts(recast(rcap) lpattern(dash) lcolor(black%90)) lpattern(solid) mcolor(black) lcolor(black) yline(0,lcolor(gray%50)) xtitle(Treatment Period) ylabel(,angle(90) glcolor(gs15%50)) scale(titlegap(.5) outergap(.5)) graphregion(color(white) margin(2 2 2 2)) plotregion(lcolor(black)) xline(6, lcolor(red%50)) title(Log Pass) ytitle(DID Estimates)
graph save figure8b2,replace

reghdfe logp60_4 before6 before5 before4 before3 before2 after0 after1 before1 $yearinteracted if bp_logappr4==1, absorb(i.school_gender##c.year blockyear) vce(cluster schcd)
est store Dynamic7
coefplot Dynamic7, omitted keep(before* after*) order(before6 before5 before4 before3 before2 before1 after0 after1) vertical recast(connect) ciopts(recast(rcap) lpattern(dash) lcolor(black%90)) lpattern(solid) mcolor(black) lcolor(black) yline(0,lcolor(gray%50)) xtitle(Treatment Period) ylabel(,angle(90) glcolor(gs15%50)) scale(titlegap(.5) outergap(.5)) graphregion(color(white) margin(2 2 2 2)) plotregion(lcolor(black)) xline(6, lcolor(red%50)) title(Log Pass with Distinction) ytitle(DID Estimates)
graph save figure8c2,replace

reghdfe pcnt_appear4 before6 before5 before4 before3 before2 after0 after1 before1 $yearinteracted if bp_logappr4==1, absorb(school_gender##c.year blockyear) vce(cluster schcd) su(mean)
est store Dynamic7
coefplot Dynamic7, omitted keep(before* after*) order(before6 before5 before4 before3 before2 before1 after0 after1) vertical recast(connect) ciopts(recast(rcap) lpattern(dash) lcolor(black%90)) lpattern(solid) mcolor(black) lcolor(black) yline(0,lcolor(gray%50)) xtitle(Treatment Period) ylabel(,angle(90) glcolor(gs15%50)) scale(titlegap(.5) outergap(.5)) graphregion(color(white) margin(2 2 2 2)) plotregion(lcolor(black)) xline(6, lcolor(red%50)) title(Rate of Appearance) ytitle(DID Estimates)
graph save figure8d2,replace

reghdfe pcnt_pass4 before6 before5 before4 before3 before2 after0 after1 before1 $yearinteracted if bp_logappr4==1, absorb(school_gender##c.year blockyear) vce(cluster schcd) su(mean)
est store Dynamic7
coefplot Dynamic7, omitted keep(before* after*) order(before6 before5 before4 before3 before2 before1 after0 after1) vertical recast(connect) ciopts(recast(rcap) lpattern(dash) lcolor(black%90)) lpattern(solid) mcolor(black) lcolor(black) yline(0,lcolor(gray%50)) xtitle(Treatment Period) ylabel(,angle(90) glcolor(gs15%50)) scale(titlegap(.5) outergap(.5)) graphregion(color(white) margin(2 2 2 2)) plotregion(lcolor(black)) xline(6, lcolor(red%50)) title(Rate of Passing) ytitle(DID Estimates)
graph save figure8e2,replace

reghdfe pcnt_p60_4 before6 before5 before4 before3 before2 after0 after1 before1 $yearinteracted if bp_logappr4==1, absorb(school_gender##c.year blockyear) vce(cluster schcd) su(mean)
est store Dynamic7
coefplot Dynamic7, omitted keep(before* after*) order(before6 before5 before4 before3 before2 before1 after0 after1) vertical recast(connect) ciopts(recast(rcap) lpattern(dash) lcolor(black%90)) lpattern(solid) mcolor(black) lcolor(black) yline(0,lcolor(gray%50)) xtitle(Treatment Period) ylabel(,angle(90) glcolor(gs15%50)) scale(titlegap(.5) outergap(.5)) graphregion(color(white) margin(2 2 2 2)) plotregion(lcolor(black)) xline(6, lcolor(red%50)) title(Rate of Passing w/ Distinction) ytitle(DID Estimates)
graph save figure8f2,replace

graph combine figure8a2.gph  figure8b2.gph figure8c2.gph figure8d2.gph figure8e2.gph figure8f2.gph, row(3) col(3) ysize(13) xsize(20) commonscheme graphregion(color(white))

**********************************************************************
//***************************** Table 6 *****************************//
use data, clear
**rural or urban
preserve
gen aft_rural=aft*rural
gen aft_urban=aft*(rural==0)
reghdfe chg_enrolment7 aft_rural aft_urban $yearinteracted if balancedpanel7==1 , absorb(school_gender##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, replace nocons bdec(3) sdec(3) ctitle(urban or rural) keep(aft*) 
test aft_rural=aft_urban
tab rural t_machine_installed if year==2013 & e(sample)
restore

**public or private
preserve
gen public=schmgt==1 | schmgt==2 | schmgt==3 | schmgt==7
gen aft_public=aft*public
gen aft_private=aft*(public==0)
reghdfe chg_enrolment7 aft_public aft_private $yearinteracted if balancedpanel7==1 , absorb(school_gender##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(private or public) keep(aft*) 
test aft_public=aft_private
tab public t_machine_installed if year==2013 & e(sample)
restore

**backward caste or forward caste
preserve
use datause, clear
*caste SC + ST + OBC & forward caste (total-obc-sc-st)
merge m:1 gender schcd year using enrolment_caste, keepusing(castec*) nogen
merge m:1 gender schcd year using enrolment_ncaste, keepusing(ncastec*) nogen
keep if caste=="TOTAL"
*enrolment ratios
egen total_caste_enrol2013=rowtotal(castec1 castec2 castec3 castec4 castec5 castec6 castec7 castec8 castec9 castec10 castec11 castec12) if year==2013
egen total_caste_enrolbg2013=total(total_caste_enrol2013) if year==2013, by(schcd year)
egen total_caste_enrolbg_2013=max(total_caste_enrolbg2013), by(schcd)
replace total_caste_enrolbg_2013=total_caste_enrolbg_2013/total_enrolbg_2013
egen total_caste_enrolg_2013=max(total_caste_enrol2013), by(schcd)
replace total_caste_enrolg_2013=total_caste_enrolg_2013/total_enrol_2013
*keep girls only (girls school and mixed school)
keep if gender==2 & (schtype==2 | schtype==3)
*set balanced panel from 2013-2018
forval i=1/10{
bysort school_gender: egen num_chg_enrolment`i' = count(chg_enrolment`i')
gen balancedpanel`i' = num_chg_enrolment`i' == 6
}
*generate treatment period
gen treatment_period=year-t_machine_year if !missing(t_machine_year)
gen aft=treatment_period>=0 & gender==2 & t_machine_installed==1 & !missing(treatment_period)
summ total_caste_enrolg_2013 if balancedpanel7==1 & year==2013, detail
gen lowcaste=total_caste_enrolbg_2013>=.6869307 & !missing(total_caste_enrolg_2013)
gen aft_lowc=aft*lowcaste
gen aft_genc=aft*(lowcaste==0)
reghdfe chg_enrolment7 aft_lowc aft_genc $yearinteracted if balancedpanel7==1 , absorb(school_gender##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(low or general caste) keep(aft*) 
test aft_lowc=aft_genc
tab lowcaste t_machine_installed if year==2013 & e(sample)
restore

***girls only school
gen mix=schtype==3
gen aft_mix=aft*mix
gen aft_gonly=aft*(mix==0)
reghdfe chg_enrolment7 aft_mix aft_gonly $yearinteracted if balancedpanel7==1 , absorb(school_gender##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, replace nocons bdec(3) sdec(3) keep(aft*) 
test aft_mix=aft_gonly
tab mix t_machine_installed if year==2013 & e(sample)

***schools with large fraction of male students
preserve
gen bratio_2013=(total_enrolbg_2013-total_enrol_2013)/total_enrolbg_2013
*large male ratio if >=.4315406 above mean
summ bratio_2013 if year==2013 & balancedpanel7==1
gen malestudent=bratio_2013>=.4204045 & !missing(bratio_2013)
gen aft_maleratioh=aft*malestudent
gen aft_maleratiol=aft*(malestudent==0)
reghdfe chg_enrolment7 aft_maleratioh aft_maleratiol $yearinteracted if balancedpanel7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) keep(aft*) 
test aft_maleratioh=aft_maleratiol
tab malestudent t_machine_installed if year==2013 & e(sample)
restore

***schools with large initial total enrollment
preserve
summ total_enrolbg_2013 if year==2013 & balancedpanel7==1
gen largeschool=total_enrolbg_2013>=918.6251
gen aft_large=aft*largeschool
gen aft_small=aft*(largeschool==0)
reghdfe chg_enrolment7 aft_large aft_small $yearinteracted if balancedpanel7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) keep(aft*) 
test aft_large=aft_small
tab largeschool t_machine_installed if year==2013 & e(sample)
restore

***schools with large enrollment of girls in 7th grade
preserve
summ c7_2013 if year==2013 & balancedpanel7==1
gen largeschoolc7=c7_2013>=52.93585
gen aft_large7=aft*largeschoolc7
gen aft_small7=aft*(largeschoolc7==0)
reghdfe chg_enrolment7 aft_large7 aft_small7 $yearinteracted if balancedpanel7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) keep(aft*) 
test aft_large7=aft_small7
tab largeschoolc7 t_machine_installed if year==2013 & e(sample)
restore

***schools with large share of female teacher
preserve
gen ftchshare=tch_female/(tch_female+tch_male+tch_nr) if year==2013
egen ftchshare2013=max(ftchshare), by(schcd)
summ ftchshare2013 if year==2013 & balancedpanel7==1
gen largeftch=ftchshare2013>=.790306
gen aft_largeftch=aft*largeftch
gen aft_smallftch=aft*(largeftch==0)
reghdfe chg_enrolment7 aft_largeftch aft_smallftch $yearinteracted if balancedpanel7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) keep(aft*) 
test aft_largeftch=aft_smallftch
tab largeftch t_machine_installed if year==2013 & e(sample)
restore

*************************************************************************
//***************** Supplementary Figures and Tables ******************//
*************************************************************************

//***************************** Supp Fig 2 *****************************//
preserve
forval c=1/11 {
egen totalc`c'=total(c`c'), missing by(blockcode year)
egen totalc`c'_next=total(c`c'_next), missing by(blockcode year)
}
egen totalc=rowtotal(totalc1 totalc2 totalc3 totalc4 totalc5 totalc6 totalc7 totalc8 totalc9 totalc10 totalc11)
replace totalc=. if totalc==0
egen totalc_next=rowtotal(totalc1_next totalc2_next totalc3_next totalc4_next totalc5_next totalc6_next totalc7_next totalc8_next totalc9_next totalc10_next totalc11_next)
replace totalc_next=. if totalc_next==0
gen diff = ((totalc-totalc_next)/totalc)*100 if !missing(totalc) & !missing(totalc_next)
egen mint_machine_year=min(t_machine_year), by(blockcode year)
egen mint_machine_installed=max(t_machine_installed), by(blockcode year)

keep year blockcode year totalc totalc_next mint_machine_year mint_machine_installed diff
duplicates drop blockcode year, force
gen treatment_period=year-mint_machine_year if !missing(mint_machine_year)
keep if mint_machine_year!=.
gen after=treatment_period>=0

graph bar (mean) diff, over(after, relabel(1 "before treatment" 2 "after treatment")) graphregion(color(white)) bar(1, color(black%80) lcolor(black%80)) ytitle(Average Dropout Rate) 
restore

//***************************** Supp Fig 3 *****************************//
forval i=1/11 {
gen log_c`i'=log(c`i'+1) if !missing(c`i')
gen log_c`i'_2013=log(c`i'_2013+1) if !missing(c`i'_2013)
}
forval i = 6/11 {
local j=`i'-1
reghdfe log_c`i' before6 before5 before4 before3 before2 after0 after1 after2 after3 after4 before1 $yearinteracted if balancedpanel`j'==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
est store Dynamic8
coefplot Dynamic8, omitted keep(before* after*) order(before7 before6 before5 before4 before3 before2  before1 after0 after1 after2 after3) vertical recast(connect) ciopts(recast(rcap) lpattern(dash) lcolor(black%90)) lpattern(solid) mcolor(black) lcolor(black) yline(0,lcolor(gray%50)) xtitle(Treatment Period) ylabel(-.5(.1).5,angle(90) glcolor(gs15%50)) scale(titlegap(.5) outergap(.5)) graphregion(color(white) margin(2 2 2 2)) plotregion(lcolor(black)) xline(6, lcolor(red%50)) title(`i'th Grade ) ytitle(DID Estimates)
preserve
keep if e(sample)
summ c`i'
restore
graph save figure`i',replace
}
graph combine figure6.gph figure7.gph figure8.gph figure9.gph figure10.gph figure11.gph, row(2) col(3) ysize(10) xsize(20) commonscheme graphregion(color(white))

*************************************************************************
//***************************** Supp Fig 5 *****************************//
reghdfe chg_enrolment6 before6 before5 before4 before3 before2 after0 after1 after2 after3 before1 $yearinteracted if rural==0 & balancedpanel6==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
est store Dynamic7a
reghdfe chg_enrolment6 before6 before5 before4 before3 before2 after0 after1 after2 after3 before1 $yearinteracted if rural==1 & balancedpanel6==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
est store Dynamic7b
coefplot Dynamic7a, bylabel(Urban Girls - 6th Grade) || Dynamic7b, bylabel(Rural Girls - 6th Grade)  ||, omitted keep(before* after*) order(before6 before5 before4 before3 before2  before1 after0 after1 after2 after3) vertical recast(connect) ciopts(recast(rcap) lpattern(dash) lcolor(black%90)) lpattern(solid) mcolor(black) lcolor(black) yline(0,lcolor(gray%50)) xtitle(Treatment Period) ylabel(-1.5(.5).5,angle(90) glcolor(gs15%50)) xline(6, lcolor(red%50)) ytitle(DID Estimates) byopts(plotregion(fcolor(white)) graphregion(fcolor(white) margin(2 2 2 2))) p1(plotregion(lcolor(black) lwidth(medthin))) p2(plotregion(lcolor(black))) subtitle(, bcolor(white)) ysize(5) xsize(10)

*************************************************************************
//***************************** Supp Fig 8 *****************************//
/*generate treated sample only
use datause, replace
keep if t_machine_year!=.
duplicates drop schcd, force
keep schcd t_machine_year
sort schcd
save treat_sample, replace*/
statsby _b[aft] _se[aft], saving(coef_placebo7,replace): reghdfe chg_enrolment7 aft $yearinteracted if balancedpanel7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
set seed 1234
forval i=1/1000{
**Step 1: generate random sample year for treated schools
preserve
use treat_sample, clear
sort schcd
gen t_year=runiformint(2013,2020)
tempfile newtreat
save `newtreat'
restore
**Step 2: merge to original dataset
preserve
merge m:1 schcd using `newtreat', keepusing(t_year)
*generate new treatment period
drop treatment_period aft
gen treatment_period=year-t_year if !missing(t_year)
gen aft=treatment_period>=0 & gender==2 & t_machine_installed==1
statsby _b[aft] _se[aft], saving(placebo7,replace): reghdfe chg_enrolment7 aft $yearinteracted if balancedpanel7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
*append coefficient data
use coef_placebo7,clear
append using placebo7
save coef_placebo7,replace
restore
}
preserve
use coef_placebo7, clear
drop in 1/1
kdensity _stat_1, gen(epoints densitye) xline(0,lcolor(red%50)) graphregion(color(white) margin(2 2 2 2)) plotregion(lcolor(black)) ylabel(,angle(90) glcolor(gs15%50)) lcolor(black) xlabel(-0.4(0.1)0.4) xtitle("Placebo Estimates")
summ densitye
kdensity _stat_1, xline(0,lcolor(red%50)) xline(.01088683,lcolor(navy)) graphregion(color(white) margin(2 2 2 2)) plotregion(lcolor(black)) ylabel(,angle(90) glcolor(gs15%50)) lcolor(black) xlabel(-0.4(0.1)0.4) xtitle("Placebo Estimates") ytitle("Kernel Density") title("Distribution of Placebo Treatment Effects")
graph save placebo1,replace
restore

*************************************************************************
//***************************** Supp Fig 9 *****************************//
statsby _b[aft] _se[aft], saving(coef_placebo7_2,replace): reghdfe chg_enrolment7 aft $yearinteracted if balancedpanel7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
set seed 1234
forval i=1/1000{
**Step 1: generate random sample of treated schools among untreated (494) & generate random sample year for them
preserve
reghdfe chg_enrolment7 aft $yearinteracted if balancedpanel7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
keep if e(sample) & t_machine_year==.
duplicates drop schcd, force
keep schcd
sample 494, count
tempfile t_sample
save `t_sample'
sort schcd
gen t_year=runiformint(2013,2020)
tempfile newtreat
save `newtreat'
restore
**Step 3: merge to original dataset
preserve
merge m:1 schcd using `newtreat', keepusing(t_year)
*generate treatment period
drop treatment_period aft
drop if t_machine_installed==1
gen treatment_period=year-t_year if !missing(t_year)
gen aft=treatment_period>=0 & gender==2 & !missing(t_year)
statsby _b[aft] _se[aft], saving(placebo7_2,replace): reghdfe chg_enrolment7 aft $yearinteracted if balancedpanel7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
*append coefficient data
use coef_placebo7_2,clear
append using placebo7_2
save coef_placebo7_2,replace
restore
}
preserve
use coef_placebo7_2, clear
drop in 1/1
kdensity _stat_1, gen(epoints densitye) xline(0,lcolor(red%50)) graphregion(color(white) margin(2 2 2 2)) plotregion(lcolor(black)) ylabel(,angle(90) glcolor(gs15%50)) lcolor(black) xlabel(-0.4(0.1)0.4) xtitle("Placebo Estimates")
summ densitye
kdensity _stat_1, xline(0,lcolor(red%50)) xline(-.00072453,lcolor(navy)) graphregion(color(white) margin(2 2 2 2)) plotregion(lcolor(black)) ylabel(,angle(90) glcolor(gs15%50)) lcolor(black) xlabel(-0.5(0.1)0.5) xtitle("Placebo Estimates") ytitle("Kernel Density") title("Distribution of Placebo Treatment Effects")
graph save placebo2,replace
restore

***************************************************************************
//***************************** Supp Fig 11 *****************************//
preserve
egen totaltch=rowtotal(tch_female tch_male tch_nr) if caste=="TOTAL"
gen femaleshare=tch_female/totaltch
gen maleshare=tch_male/totaltch
gen profshare=tchwithprof/totaltch
gen gradshare=gradabove/totaltch
reghdfe femaleshare before6 before5 before4 before3 before2 after0 after1 after2 after3 after4 before1  $yearinteracted if balancedpanel7==1 & c7!=., absorb(school_gender##c.year blockyear) vce(cluster schcd)
est store Dynamic7
coefplot Dynamic7, omitted keep(before* after*) order(before6 before5 before4 before3 before2  before1 after0 after1 after2 after3 after4) vertical recast(connect) ciopts(recast(rcap) lpattern(dash) lcolor(black%90)) lpattern(solid) mcolor(black) lcolor(black) yline(0,lcolor(gray%50)) xtitle(Treatment Period) ylabel(,angle(90) glcolor(gs15%50)) scale(titlegap(.5) outergap(.5)) graphregion(color(white) margin(2 2 2 2)) plotregion(lcolor(black)) xline(6, lcolor(red%50)) title(Share of Female Teacher) ytitle(DID Estimates)
graph save channel3as,replace
reghdfe maleshare before6 before5 before4 before3 before2 after0 after1 after2 after3 after4 before1  $yearinteracted if balancedpanel7==1 & c7!=., absorb(school_gender##c.year blockyear) vce(cluster schcd)
est store Dynamic7
coefplot Dynamic7, omitted keep(before* after*) order(before6 before5 before4 before3 before2  before1 after0 after1 after2 after3 after4) vertical recast(connect) ciopts(recast(rcap) lpattern(dash) lcolor(black%90)) lpattern(solid) mcolor(black) lcolor(black) yline(0,lcolor(gray%50)) xtitle(Treatment Period) ylabel(,angle(90) glcolor(gs15%50)) scale(titlegap(.5) outergap(.5)) graphregion(color(white) margin(2 2 2 2)) plotregion(lcolor(black)) xline(6, lcolor(red%50)) title(Share of Male Teacher) ytitle(DID Estimates)
graph save channel3bs,replace
reghdfe profshare before6 before5 before4 before3 before2 after0 after1 after2 after3 after4 before1  $yearinteracted if balancedpanel7==1 & c7!=., absorb(school_gender##c.year blockyear) vce(cluster schcd)
est store Dynamic7
coefplot Dynamic7, omitted keep(before* after*) order(before6 before5 before4 before3 before2  before1 after0 after1 after2 after3 after4) vertical recast(connect) ciopts(recast(rcap) lpattern(dash) lcolor(black%90)) lpattern(solid) mcolor(black) lcolor(black) yline(0,lcolor(gray%50)) xtitle(Treatment Period) ylabel(,angle(90) glcolor(gs15%50)) scale(titlegap(.5) outergap(.5)) graphregion(color(white) margin(2 2 2 2)) plotregion(lcolor(black)) xline(6, lcolor(red%50)) title(Share of Professional Teacher) ytitle(DID Estimates)
graph save channel3cs,replace
reghdfe gradshare before6 before5 before4 before3 before2 after0 after1 after2 after3 after4 before1  $yearinteracted if balancedpanel7==1 & c7!=., absorb(school_gender##c.year blockyear) vce(cluster schcd)
est store Dynamic7
coefplot Dynamic7, omitted keep(before* after*) order(before6 before5 before4 before3 before2  before1 after0 after1 after2 after3 after4) vertical recast(connect) ciopts(recast(rcap) lpattern(dash) lcolor(black%90)) lpattern(solid) mcolor(black) lcolor(black) yline(0,lcolor(gray%50)) xtitle(Treatment Period) ylabel(,angle(90) glcolor(gs15%50)) scale(titlegap(.5) outergap(.5)) graphregion(color(white) margin(2 2 2 2)) plotregion(lcolor(black)) xline(6, lcolor(red%50)) title(Share of Graduate Teacher) ytitle(DID Estimates)
graph save channel3ds,replace
restore

***************************************************************************
//***************************** Supp Table 1 *****************************//
reghdfe chg_enrolment5 aft $yearinteracted, absorb(school_gender##c.year blockyear) vce(cluster schcd) su(mean)
outreg2 using 1.doc, replace nocons bdec(3) sdec(3) ctitle(5) keep(aft) 
preserve
keep if e(sample)
duplicates drop schcd, force
tab t_machine_installed
restore 
forval i = 6/10 {
reghdfe chg_enrolment`i' aft $yearinteracted, absorb(school_gender##c.year blockyear) vce(cluster schcd) su(mean)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(`i') keep(aft) 
preserve
keep if e(sample)
duplicates drop schcd, force
tab t_machine_installed
restore 
}

***************************************************************************
//***************************** Supp Table 2 *****************************//
global controls "total_enrolbg_2013 bookinlib_2013 toiletd_2013 toiletb_2013 toiletg_2013 clrooms_2013 clgood_2013 smc_2013 smcsdp_2013 medchk_2013 electric_2013 library_2013 playground_2013 ramps_2013 cal_2013 access_2013 water_hp_2013 water_well_2013 water_tap_2013 water_others_2013 bld_govt_2013 bld_uc_2013 bld_dilap_2013 wall_pucca_2013 wall_wire_2013 wall_hedges_2013 wall_na_2013"

set seed 1234
preserve
keep if balancedpanel7==1 & year==2013
duplicates drop schcd, force
***psmatch 1: n1 trim 2
psmatch2 t_machine_installed $controls if balancedpanel7==1, qui outcome(chg_enrolment7) n(1) trim(2)
***psmatch 2: n10 trim 2 ties
*psmatch2 t_machine_installed $controls if balancedpanel7==1, qui outcome(chg_enrolment7) n(10) trim(2) ties
***psmatch 3: trim2 kernel kerneltype(epan)
*psmatch2 t_machine_installed $controls if balancedpanel7==1, qui outcome(chg_enrolment7) trim(2) kernel kerneltype(epan)
***psmatch 4: radius trim2 caliper0.1
*psmatch2 t_machine_installed $controls if balancedpanel7==1, qui outcome(chg_enrolment7) trim(2) radius caliper(0.10)
***psmatch 5: mahalanobis
*psmatch2 t_machine_installed if balancedpanel7==1, mahalanobis($controls) outcome(chg_enrolment7)
keep schcd _weight
keep if _weight>0 & !missing(_weight)
save psmatch,replace
restore
preserve
merge m:1 schcd using psmatch, keepusing(_weight)
statsby _b[aft], saving(coef_psm7,replace): reghdfe chg_enrolment7 aft if _merge==3 & balancedpanel7==1, absorb(i.school_gender##c.year blockyear)
restore
*bootstrap to get standard error
forval i=1/1050{
preserve
keep if balancedpanel7==1
*generate random sample by schcd cluster
bsample, cluster(schcd) idcluster(schcd_gender)
save pssample,replace
keep if year==2013
duplicates drop schcd, force
***psmatch 1: n1 trim 2
capture nois psmatch2 t_machine_installed $controls if balancedpanel7==1, qui outcome(chg_enrolment7) n(1) trim(2)
***psmatch 2: n10 trim 2 ties
*capture nois psmatch2 t_machine_installed $controls if balancedpanel7==1, qui outcome(chg_enrolment7) n(10) trim(2) ties
***psmatch 3: trim2 kernel kerneltype(epan)
*capture nois psmatch2 t_machine_installed $controls if balancedpanel7==1, qui outcome(chg_enrolment7) trim(2) kernel kerneltype(epan)
***psmatch 4: radius trim2 caliper0.1
*capture nois psmatch2 t_machine_installed $controls if balancedpanel7==1, qui outcome(chg_enrolment7) trim(2) radius caliper(0.10)
***psmatch 5: mahalanobis
*capture nois psmatch2 t_machine_installed if balancedpanel7==1, mahalanobis($controls) outcome(chg_enrolment7)

if _rc ==0 {
keep schcd _weight
keep if _weight>0 & !missing(_weight)
duplicates drop schcd, force
save psmatch,replace
use pssample,clear
*run regression using psmatch sample
merge m:1 schcd using psmatch
statsby _b[aft], saving(psm7,replace): reghdfe chg_enrolment7 aft if _merge==3 & balancedpanel7==1, absorb(i.schcd_gender##c.year blockyear)
*append coefficient data
use coef_psm7,clear
append using psm7
save coef_psm7,replace
}
else continue
restore
}
*calculate bootstrap standard error
preserve
use coef_psm7, clear
keep in 2/1001
egen meanb=mean(_stat_1)
gen diff2=(_stat_1-meanb)^2
collapse (sum) diff2
gen s=((1/(1000-1))*diff2)^1/2
list s
restore

***psmatch 6: cem
set seed 1234
preserve
keep if balancedpanel7==1 & year==2013
duplicates drop schcd, force
cem $controls if balancedpanel7==1, treatment(t_machine_installed)
keep schcd cem_matched
keep if cem_matched==1
save psmatch,replace
restore
preserve
merge m:1 schcd using psmatch
statsby _b[aft], saving(coef_psm7,replace): reghdfe chg_enrolment7 aft if _merge==3 & balancedpanel7==1, absorb(i.school_gender##c.year blockyear)
restore
forval i=1/1050{
preserve
*generate random sample by schcd cluster
bsample, cluster(schcd) idcluster(schcd_gender)
save pssample,replace
keep if year==2013
duplicates drop schcd, force
capture nois cem $controls if balancedpanel7==1, treatment(t_machine_installed)
if _rc ==0 {
keep schcd cem_matched
keep if cem_matched==1
duplicates drop schcd, force
save psmatch,replace
use pssample,clear
*run regression using psmatch sample
merge m:1 schcd using psmatch
statsby _b[aft], saving(psm7,replace): reghdfe chg_enrolment7 aft if _merge==3 & balancedpanel7==1, absorb(i.schcd_gender##c.year blockyear)
*append coefficient data
use coef_psm7,clear
append using psm7
save coef_psm7,replace
}
else continue
restore
}

*calculate bootstrap standard error*
use coef_psm7, clear
keep in 2/1001
egen meanb=mean(_stat_1)
gen diff2=(_stat_1-meanb)^2
collapse (sum) diff2
gen s=((1/(1000-1))*diff2)^1/2
list s

***************************************************************************
//***************************** Supp Table 3 *****************************//
preserve
use datapc, clear
keep if year==2013

gen publicbus=public_bus_service__status_a_1__==1
gen highway=national_highway__status_a_1__na==1
gen chc=community_health_centre__numbers==1
gen phc=primary_health_centre__numbers_>=1 | primary_health_sub_centre__numbe>=1
gen fwc=family_welfare_centre__numbers_>=1
gen mcw=maternity_and_child_welfare_cent>=1
gen rpm=rural_production_mart_or_sanitar==1
gen courier=private_courier_facility__status==1
gen waste=community_waste_disposal_system_==1
gen postoffice=post_office__status_a_1__na_2__==1
gen internet=internet_cafes___common_service_==1
gen sports=sports_club_recreation_centre__s==1
gen cinema=cinema_video_hall__status_a_1__n==1
gen plibrary=public_library__status_a_1__na_2==1
gen powercom=power_supply_for_commercial_use_==1
gen tap=tap_water_treated_functioning_al==1

*baseline village attributes
reghdfe t_machine_installed total_geographical_area__in_hect total__households_ total_population_of_village total_scheduled_castes_populatio total_scheduled_tribes_populatio publicbus highway chc phc fwc mcw rpm courier waste postoffice internet sports cinema plibrary powercom tap, noabsorb vce(cluster villagecode)
outreg2 using 1.doc, replace nocons bdec(4) sdec(4)
preserve
keep if e(sample)
tab t_machine_installed
restore

probit t_machine_installed total_geographical_area__in_hect total__households_ total_population_of_village total_scheduled_castes_populatio total_scheduled_tribes_populatio publicbus highway chc phc fwc mcw rpm courier waste postoffice internet sports cinema plibrary powercom tap, vce(cluster villagecode)
margins, atmeans dydx(_all) post
outreg2 using 1.doc, append nocons bdec(4) sdec(4)

logit t_machine_installed total_geographical_area__in_hect total__households_ total_population_of_village total_scheduled_castes_populatio total_scheduled_tribes_populatio publicbus highway chc phc fwc mcw rpm courier waste postoffice internet sports cinema plibrary powercom tap, vce(cluster villagecode)
margins, atmeans dydx(_all) post
outreg2 using 1.doc, append nocons bdec(4) sdec(4)
restore

***************************************************************************
//***************************** Supp Table 4 *****************************//
*full sample
preserve
keep if year==2013
outreg2 using summarystats, word replace sum(log) dec(3) keep(total_enrol_2013 total_enrolbg_2013 appr5 appr8 pass5 pass8 p60_5 p60_8 bookinlib_2013 toiletd_2013 toiletb_2013 toiletg_2013 clrooms_2013 clgood_2013 smc_2013 smcsdp_2013 medchk_2013 electric_2013 library_2013 playground_2013 ramps_2013 cal_2013 access_2013 water_hp_2013 water_well_2013 water_tap_2013 water_others_2013 bld_govt_2013 bld_uc_2013 bld_dilap_2013 wall_pucca_2013 wall_wire_2013 wall_hedges_2013 wall_na_2013)
*treatment vs main control
asdoc ttest total_enrol_2013, by(t_machine_installed) dec(3) replace stat(obs mean se p dif) stars save(summarystats.doc)
foreach var in total_enrolbg_2013 appr5 appr8 pass5 pass8 p60_5 p60_8 bookinlib_2013 toiletd_2013 toiletb_2013 toiletg_2013 clrooms_2013 clgood_2013 smc_2013 smcsdp_2013 medchk_2013 electric_2013 library_2013 playground_2013 ramps_2013 cal_2013 access_2013 water_hp_2013 water_well_2013 water_tap_2013 water_others_2013 bld_govt_2013 bld_uc_2013 bld_dilap_2013 wall_pucca_2013 wall_wire_2013 wall_hedges_2013 wall_na_2013 {
asdoc ttest `var', by(t_machine_installed) rowappend dec(3) stat(obs mean se p dif) stars  save(summarystats.doc)
}
restore

***************************************************************************
//***************************** Supp Table 5 *****************************//
preserve
reghdfe chg_enrolment7 aft $yearinteracted if balancedpanel7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd) su(mean)
keep if e(sample) & year==2013
asdoc ttest total_enrol_2013, by(t_machine_installed) dec(3) replace stat(obs mean se p dif) stars save(summarystats.doc)
foreach var in total_enrolbg_2013 appr5 appr8 pass5 pass8 p60_5 p60_8 bookinlib_2013 toiletd_2013 toiletb_2013 toiletg_2013 clrooms_2013 clgood_2013 smc_2013 smcsdp_2013 medchk_2013 electric_2013 library_2013 playground_2013 ramps_2013 cal_2013 access_2013 water_hp_2013 water_well_2013 water_tap_2013 water_others_2013 bld_govt_2013 bld_uc_2013 bld_dilap_2013 wall_pucca_2013 wall_wire_2013 wall_hedges_2013 wall_na_2013 {
asdoc ttest `var', by(t_machine_installed) rowappend dec(3) stat(obs mean se p dif) stars  save(summarystats.doc)
}
restore

***************************************************************************
//***************************** Supp Table 6 *****************************//
/***psmatch: n10 trim2 ties with replacement
set seed 1234
preserve
keep if balancedpanel7==1 & year==2013
duplicates drop schcd, force
psmatch2 t_machine_installed $controls if balancedpanel7==1, qui outcome(chg_enrolment7) n(10) trim(2) ties
keep schcd _weight
keep if _weight>0 & !missing(_weight)
save psmatch,replace
restore
preserve
merge m:1 schcd using psmatch, keepusing(_weight)
reghdfe chg_enrolment7 aft if _merge==3 & balancedpanel7==1, absorb(i.school_gender##c.year blockyear)
*/

/***psmatch: n(1) trim(2)
set seed 1234
preserve 
keep if balancedpanel7==1 & year==2013
duplicates drop schcd, force
psmatch2 t_machine_installed $controls if balancedpanel7==1, qui outcome(chg_enrolment7) n(1) trim(2)
keep schcd _weight
keep if _weight>0 & !missing(_weight)
save psmatch,replace
restore
preserve
merge m:1 schcd using psmatch, keepusing(_weight)
reghdfe chg_enrolment7 aft if _merge==3 & balancedpanel7==1, absorb(i.school_gender##c.year blockyear)
*/

/***psmatch: trim2 kernel kerneltype(epan)
set seed 1234
preserve
keep if balancedpanel7==1 & year==2013
duplicates drop schcd, force
psmatch2 t_machine_installed $controls if balancedpanel7==1, qui outcome(chg_enrolment7) trim(2) kernel kerneltype(epan)
keep schcd _weight
keep if _weight>0 & !missing(_weight)
save psmatch,replace
restore
preserve
merge m:1 schcd using psmatch, keepusing(_weight)
reghdfe chg_enrolment7 aft if _merge==3 & balancedpanel7==1, absorb(i.school_gender##c.year blockyear)
*/

/***psmatch: mahalanobis
set seed 1234
preserve
keep if balancedpanel7==1 & year==2013
duplicates drop schcd, force
psmatch2 t_machine_installed if balancedpanel7==1, mahalanobis($controls) outcome(chg_enrolment7)
keep schcd _weight
keep if _weight>0 & !missing(_weight)
save psmatchb,replace
restore
preserve
merge m:1 schcd using psmatchb, keepusing(_weight)
reghdfe chg_enrolment7 aft if _merge==3 & balancedpanel7==1, absorb(i.school_gender##c.year blockyear)
*/

/***psmatch: cem
set seed 1234
preserve
keep if balancedpanel7==1 & year==2013
duplicates drop schcd, force
cem $controls if balancedpanel7==1, treatment(t_machine_installed)
keep schcd cem_matched
keep if cem_matched==1
save psmatch,replace
restore
preserve
merge m:1 schcd using psmatch
reghdfe chg_enrolment7 aft if _merge==3 & balancedpanel7==1, absorb(i.school_gender##c.year blockyear)
*/

*estimation sample only
keep if e(sample) & year==2013
*treatment vs main control
asdoc ttest total_enrol_2013, by(t_machine_installed) dec(3) replace stat(obs mean se p dif) stars save(summarystats.doc)
foreach var in total_enrolbg_2013 chg_enrolment7 appr8 pass8 p60_8 bookinlib_2013 toiletd_2013 toiletb_2013 toiletg_2013 clrooms_2013 clgood_2013 smc_2013 smcsdp_2013 medchk_2013 electric_2013 library_2013 playground_2013 ramps_2013 cal_2013 access_2013 water_hp_2013 water_well_2013 water_tap_2013 water_others_2013 bld_govt_2013 bld_uc_2013 bld_dilap_2013 wall_pucca_2013 wall_wire_2013 wall_hedges_2013 wall_na_2013 {
asdoc ttest `var', by(t_machine_installed) rowappend dec(3) stat(obs mean se p dif) stars  save(summarystats.doc)
}
restore

***************************************************************************
//***************************** Supp Table 7 *****************************//
use data, clear
forval i=1/4 {
reghdfe chg_enrolment`i' aft $yearinteracted if balancedpanel`i'==1, absorb(school_gender##c.year blockyear) vce(cluster schcd) su(mean)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(`i') keep(aft)
preserve
keep if e(sample)
duplicates drop schcd, force
tab t_machine_installed
restore
}
preserve 
use datause, clear
keep if caste=="TOTAL"
gen treatment_period=year-t_machine_year if !missing(t_machine_year)
keep if gender==1 & (schtype==2 | schtype==3)
forval i=1/10{
bysort school_gender: egen num_chg_enrolment`i' = count(chg_enrolment`i')
gen balancedpanel`i' = num_chg_enrolment`i' == 6
}
gen aft=treatment_period>=0 & gender==1 & t_machine_installed==1 & !missing(treatment_period)
reghdfe chg_enrolment7 aft $yearinteracted if balancedpanel7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd) su(mean)
outreg2 using 1.doc, replace nocons bdec(3) sdec(3) ctitle(5) keep(aft) 
keep if e(sample)
duplicates drop schcd, force
tab t_machine_installed
restore

***************************************************************************
//***************************** Supp Table 8 *****************************//
reghdfe chg_enrolment7 aft $yearinteracted if balancedpanel7==1, absorb(school_gender##c.year blockyear) vce(cluster villagecode)
outreg2 using 1.doc, replace nocons bdec(3) sdec(3) ctitle(4) keep(aft) 
*pad distribution but no machine
gen drop1=t_machine_installed==0 & t_distribution==1
*machine but no reported collection
gen drop2=t_machine_installed==1 & t_machine_nostudent==0
reghdfe chg_enrolment7 aft $yearinteracted if drop1!=1 & balancedpanel7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd) su(mean)
outreg2 using 1.doc, replace nocons bdec(3) sdec(3) ctitle(5) keep(aft) 
preserve
keep if e(sample)
duplicates drop schcd, force
tab t_machine_installed
restore 
reghdfe chg_enrolment7 aft $yearinteracted if drop2!=1 & balancedpanel7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd) su(mean)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(5) keep(aft) 
preserve
keep if e(sample)
duplicates drop schcd, force
tab t_machine_installed
restore 
reghdfe chg_enrolment7 aft $yearinteracted if drop2!=1 & drop1!=1 & balancedpanel7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd) su(mean)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(5) keep(aft) 
preserve
keep if e(sample)
duplicates drop schcd, force
tab t_machine_installed
restore 

***************************************************************************
//***************************** Supp Table 9 *****************************//
*grant&visits reported is lagged by one year
foreach v in conti_e funds_e schmntcgrant_e conti_r funds_r schmntcgrant_r visitscrc visitsbrc{
bysort schcd (year): replace `v'=`v'[_n+1]
}
gen log_conti_e=log(conti_e +1) if !missing(conti_e)
gen log_funds_e=log(funds_e +1) if !missing(funds_e)
gen log_schmntcgrant_e=log(schmntcgrant_e +1) if !missing(schmntcgrant_e)
gen log_conti_r=log(conti_r +1) if !missing(conti_r)
gen log_funds_r=log(funds_r +1) if !missing(funds_r)
gen log_schmntcgrant_r=log(schmntcgrant_r +1) if !missing(schmntcgrant_r)
gen logvisitscrc=log(visitscrc+1) if !missing(visitscrc)
gen logvisitsbrc=log(visitsbrc+1) if !missing(visitsbrc)

reghdfe log_conti_e aft $yearinteracted if balancedpanel7==1 & c7!=., absorb(school_gender##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, replace nocons bdec(3) sdec(3) ctitle(5) keep(aft) 
preserve
keep if e(sample)
duplicates drop schcd, force
tab t_machine_installed
restore
reghdfe log_funds_e aft $yearinteracted if balancedpanel7==1 & c7!=., absorb(school_gender##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(5) keep(aft) 
preserve
keep if e(sample)
duplicates drop schcd, force
tab t_machine_installed
restore
reghdfe log_schmntcgrant_e aft $yearinteracted if balancedpanel7==1 & c7!=., absorb(school_gender##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(5) keep(aft) 
preserve
keep if e(sample)
duplicates drop schcd, force
tab t_machine_installed
restore
reghdfe log_conti_r aft $yearinteracted if balancedpanel7==1 & c7!=., absorb(school_gender##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(5) keep(aft) 
preserve
keep if e(sample)
duplicates drop schcd, force
tab t_machine_installed
restore
reghdfe log_funds_r aft $yearinteracted if balancedpanel7==1 & c7!=., absorb(school_gender##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(5) keep(aft) 
preserve
keep if e(sample)
duplicates drop schcd, force
tab t_machine_installed
restore
reghdfe log_schmntcgrant_r aft $yearinteracted if balancedpanel7==1 & c7!=., absorb(school_gender##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(5) keep(aft) 
preserve
keep if e(sample)
duplicates drop schcd, force
tab t_machine_installed
restore
reghdfe logvisitscrc aft $yearinteracted if balancedpanel7==1 & c7!=., absorb(school_gender##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(5) keep(aft) 
preserve
keep if e(sample)
duplicates drop schcd, force
tab t_machine_installed
restore
reghdfe logvisitsbrc aft $yearinteracted if balancedpanel7==1 & c7!=., absorb(school_gender##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(5) keep(aft) 
preserve
keep if e(sample)
duplicates drop schcd, force
tab t_machine_installed
restore

***************************************************************************
//***************************** Supp Table 10 *****************************//
use datapc, clear
gen publicbus=public_bus_service__status_a_1__==1
gen aft_publicbus=aft*publicbus
gen aft_npublicbus=aft*(publicbus==0)

gen highway=national_highway__status_a_1__na==1
gen aft_highway=aft*highway
gen aft_nhighway=aft*(highway==0)

gen chc=community_health_centre__numbers==1
gen aft_chc=aft*chc
gen aft_nchc=aft*(chc==0)

gen phc=primary_health_centre__numbers_>=1 | primary_health_sub_centre__numbe>=1
gen aft_phc=aft*phc
gen aft_nphc=aft*(phc==0)

gen fwc=family_welfare_centre__numbers_>=1
gen aft_fwc=aft*fwc
gen aft_nfwc=aft*(fwc==0)

gen mcw=maternity_and_child_welfare_cent>=1
gen aft_mcw=aft*mcw
gen aft_nmcw=aft*(mcw==0)

gen rpm=rural_production_mart_or_sanitar==1
gen aft_rpm=aft*rpm
gen aft_nrpm=aft*(rpm==0)

gen internet=internet_cafes___common_service_==1
gen aft_internet=aft*internet
gen aft_ninternet=aft*(internet==0)

gen courier=private_courier_facility__status==1
gen aft_courier=aft*courier
gen aft_ncourier=aft*(courier==0)

reghdfe chg_enrolment7 aft_publicbus  aft_npublicbus $yearinteracted if balancedpanel7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, replace nocons bdec(3) sdec(3) ctitle(publicbus) keep(aft_publicbus aft_npublicbus) 
test _b[aft_publicbus] = _b[aft_npublicbus] 

reghdfe chg_enrolment7 aft_highway aft_nhighway $yearinteracted if balancedpanel7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(highway) keep(aft_highway aft_nhighway) 
test _b[aft_highway] = _b[aft_nhighway] 

reghdfe chg_enrolment7 aft_chc aft_nchc $yearinteracted if balancedpanel7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
test _b[aft_chc] = _b[aft_nchc]
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(chc) keep(aft_*) 

reghdfe chg_enrolment7 aft_phc aft_nphc $yearinteracted if balancedpanel7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
test _b[aft_phc] = _b[aft_nphc]
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(phc) keep(aft_*) 

reghdfe chg_enrolment7 aft_fwc aft_nfwc $yearinteracted if balancedpanel7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
test _b[aft_fwc] = _b[aft_nfwc]
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(fwc) keep(aft_*) 

reghdfe chg_enrolment7 aft_mcw aft_nmcw $yearinteracted if balancedpanel7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
test _b[aft_mcw] = _b[aft_nmcw]
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(mcw) keep(aft_*) 

reghdfe chg_enrolment7 aft_rpm aft_nrpm $yearinteracted if balancedpanel7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
test _b[aft_rpm] = _b[aft_nrpm]
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(rpm) keep(aft_*) 

reghdfe chg_enrolment7 aft_internet aft_ninternet $yearinteracted if balancedpanel7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
test _b[aft_internet] = _b[aft_ninternet]
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(internet) keep(aft_*) 

reghdfe chg_enrolment7 aft_courier aft_ncourier $yearinteracted if balancedpanel7==1, absorb(school_gender##c.year blockyear) vce(cluster schcd)
test _b[aft_courier] = _b[aft_ncourier]
outreg2 using 1.doc, append nocons bdec(3) sdec(3) ctitle(courier) keep(aft_*) 

***************************************************************************
//***************************** Supp Table 11 *****************************//
preserve
keep if caste=="TOTAL" & t_machine_installed==1 & year==2013

duplicates drop schcd, force

tab lowhigh
restore
