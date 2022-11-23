/*
This .do file reproduce the empirical analysis of the paper, i.e. the
Heckman estimates and likelihood ratio test.
*/

clear

* Laptop - lenovo
cd "G:\Il mio Drive\PhD Padova\3rd year\ELSA data\stata13_se\"
* Surface
*cd "C:\Users\39334\Google Drive\PhD Padova\3rd year\ELSA data\stata13_se"
* Surface
*cd "your path here"

clear

*use "wave_8_FM.dta"
*use "wave_8_FM_v1.dta"
use "wave_8_FM_v2.dta"

drop fuid_x_y coupid_x_y hhresp_y hhtot_y hoh_x savings_bu_s_y invests_bu_s_y /*
*/ grossfw_bu_s_y netfw_bu_s_y netpw_bu_s_y nettotw_bu_s_y numhhldrsinhh_y /*
*/ empinc_bu_s_y seinc_bu_s_y ppinc_bu_s_y spinc_bu_s_y beninc_bu_s_y /*
*/ nettotnhw_bu_s_y grosstotnhw_bu_s_y dhiasep_x dhiasep_y

rename *_x *_m
rename *_y *_f

summarize H_index_m H_index_f

gen savings_hh = savings_bu_s_m
gen invests_hh = invests_bu_s_m
gen gross_finw_hh = grossfw_bu_s_m
gen net_finw_hh = netfw_bu_s_m
gen net_physw_hh = netpw_bu_s_m
gen net_totw_hh = nettotw_bu_s_m
gen employ_inc_hh = empinc_bu_s_m
gen selfempl_inc_hh = seinc_bu_s_m
gen annuity_inc_hh = ppinc_bu_s_m
gen state_pens_inc_hh = spinc_bu_s_m
gen state_ben_inc_hh = beninc_bu_s_m
gen net_tot_nohw_hh = nettotnhw_bu_s_m
gen gross_tot_nohw_hh = grosstotnhw_bu_s_m

gen gross_nofinw_hh = gross_tot_nohw_hh - gross_finw_hh
summarize gross_tot_nohw_hh gross_finw_hh gross_nofinw_hh 
/* Financial debt (benefit unit level) (credit cards, overdrafts, other private debt but not mortgages)
netfw_bu_s Net financial wealth (benefit unit level). Gross financial wealth with financial debt subtracted.*/ 

* RESPONDENT 
* if the IA section respondent is the male, then "deci"=1, otherwise "deci"=2
gen dec_m = iapid_m==perid_m
tab dec_m
gen dec_f = (iapid_m==perid_f)*2
tab dec_f
gen deci = dec_m+dec_f
tab deci
summarize iapid_m perid_m perid_f if deci==0
*drop if deci == 0

* INCOME
gen job_inc = employ_inc_hh + selfempl_inc_hh
gen pens_inc = annuity_inc_hh + state_pens_inc_hh + state_ben_inc_hh

gen job_inc_m = empinc_r_s_m + seinc_r_s_m
gen pens_inc_m = ppinc_r_s_m + spinc_r_s_m + beninc_r_s_m
gen income_m = job_inc_m + pens_inc_m
gen job_inc_f = empinc_r_s_f + seinc_r_s_f
gen pens_inc_f = ppinc_r_s_f + spinc_r_s_f + beninc_r_s_f
gen income_f = job_inc_f + pens_inc_f

summarize ppinc_r_s_m  spinc_r_s_m  beninc_r_s_m /*
*/ ppinc_r_s_f  spinc_r_s_f  beninc_r_s_f if pens_inc == .

gen hh_income = job_inc + pens_inc
summarize hh_income 
* check hh income == 0
summarize employ_inc_hh selfempl_inc_hh /*
*/ annuity_inc_hh  state_pens_inc_hh  state_ben_inc_hh /*
*/ job_inc pens_inc  if hh_income == 0
drop if hh_income == 0 /* 4 observation deleted */
* check hh income == . (missing)
summarize employ_inc_hh selfempl_inc_hh /*
*/ annuity_inc_hh  state_pens_inc_hh  state_ben_inc_hh /*
*/ job_inc pens_inc  if hh_income == .
summarize employ_inc_hh selfempl_inc_hh /*
*/ annuity_inc_hh state_pens_inc_hh state_ben_inc_hh /*
*/ job_inc pens_inc  /* 
*/ job_inc_m pens_inc_m job_inc_f pens_inc_f if pens_inc == .
* pension income is missing and while job income is positive
replace hh_income = job_inc if hh_income == . 



summarize job_inc pens_inc selfempl_inc_hh income_m income_f 
summarize income_m job_inc_f pens_inc_f empinc_r_s_f seinc_r_s_f /*
*/ 		   ppinc_r_s_f spinc_r_s_f beninc_r_s_f /*if income_f < 0*/
summarize hh_income job_inc pens_inc selfempl_inc_hh /*
*/ 		  income_m job_inc_m pens_inc_m income_f job_inc_f pens_inc_f if hh_income < 0

* Respondent income
gen income_dec = income_m if deci==1
replace income_dec = income_f if deci==2
gen income_ndec = income_f if deci==1
replace income_ndec = income_m if deci==2

* Gamma - bargaining power proxy
gen gamma_m = income_m/hh_income /* 4 missing because hh_income = 0*/
summarize gamma_m
replace gamma_m = 1 if gamma_m > 1

gen gamma_dec = income_dec/hh_income
summarize gamma_dec
replace gamma_dec = 1 if gamma_dec > 1

* Income quartile
xtile quart_income = hh_income, nq(4)
* Total wealth quartile
xtile quart_finw_hh = net_finw_hh, nq(4)
xtile quart_totw_hh = net_totw_hh, nq(4)

* RISK AND PATIENCE
* respondent
gen scgrisk_dec = scgrisk_1_m if deci == 1
replace scgrisk_dec = scgrisk_1_f if deci == 2
gen scfrisk_dec = scfrisk_1_m if deci == 1
replace scfrisk_dec = scfrisk_1_f if deci == 2
gen scgpati_dec = scgpati_1_m if deci == 1
replace scgpati_dec = scgpati_1_f if deci == 2
gen scfpati_dec = scfpati_1_m if deci == 1
replace scfpati_dec = scfpati_1_f if deci == 2
* non respondent
gen scgrisk_ndec = scgrisk_1_f if deci == 1
replace scgrisk_ndec = scgrisk_1_m if deci == 2
gen scfrisk_ndec = scfrisk_1_f if deci == 1
replace scfrisk_ndec = scfrisk_1_m if deci == 2
gen scgpati_ndec = scgpati_1_f if deci == 1
replace scgpati_ndec = scgpati_1_m if deci == 2
gen scfpati_ndec = scfpati_1_f if deci == 1
replace scfpati_ndec = scfpati_1_m if deci == 2

* Weighted risk aversion
* standard - general
gen scgrisk_hh_mf = gamma_m*scgrisk_1_m + (1-gamma_m)*scgrisk_1_f
gen scgrisk_hh_dec = gamma_dec*scgrisk_dec + (1-gamma_dec)*scgrisk_ndec
* standard - financial
gen scfrisk_hh_mf = gamma_m*scfrisk_1_m + (1-gamma_m)*scfrisk_1_f
gen scfrisk_hh_dec = gamma_dec*scfrisk_dec + (1-gamma_dec)*scfrisk_ndec

summarize scgrisk_1_m scfrisk_1_m scgrisk_dec scfrisk_dec /*
*/ 		  scgrisk_hh_mf scfrisk_hh_mf scgrisk_hh_dec scfrisk_hh_dec

* Weighted patience
* standard - general
gen scgpati_hh_mf = gamma_m*scgpati_1_m + (1-gamma_m)*scgpati_1_f
gen scgpati_hh_dec = gamma_dec*scgpati_dec + (1-gamma_dec)*scgpati_ndec
* standard - financial
gen scfpati_hh_mf = gamma_m*scfpati_1_m + (1-gamma_m)*scfpati_1_f
gen scfpati_hh_dec = gamma_dec*scfpati_dec + (1-gamma_dec)*scfpati_ndec

summarize scgpati_1_m scfpati_1_m scgpati_dec scfpati_dec /*
*/ 		  scgpati_hh_mf scfpati_hh_mf scgpati_hh_dec scfpati_hh_dec


* weighted utilities solution - general
gen scgrisk_hh_den_mf = gamma_m*scgrisk_1_m^2 + (1-gamma_m)*scgrisk_1_f^2
gen scgrisk_hh_tot_mf = scgrisk_hh_mf/scgrisk_hh_den_mf

gen scgrisk_hh_den_dec = gamma_dec*scgrisk_dec^2 + (1-gamma_dec)*scgrisk_ndec^2
gen scgrisk_hh_tot_dec = scgrisk_hh_dec/scgrisk_hh_den_dec

* weighted utilities solution - financial
gen scfrisk_hh_den_mf = gamma_m*scfrisk_1_m^2 + (1-gamma_m)*scfrisk_1_f^2
gen scfrisk_hh_tot_mf = scfrisk_hh_mf/scfrisk_hh_den_mf

gen scfrisk_hh_den_dec = gamma_dec*scfrisk_dec^2 + (1-gamma_dec)*scfrisk_ndec^2
gen scfrisk_hh_tot_dec = scfrisk_hh_dec/scfrisk_hh_den_dec

summarize gamma_m gamma_dec
/*hist gamma_m
hist gamma_dec*/

* NUMERACY
/* right answers
a: 100-7=93   b: 93-7= 86   c: 86-7=79   d: 79-7=72  e: 72-7=65 */
gen n1 = cfsva_m == 93
gen n2 = cfsvb_m == 86
gen n3 = cfsvc_m == 79
gen n4 = cfsvd_m == 72
gen n5 = cfsve_m == 65
gen numeracy_m = n1 + n2 + n3 + n4 + n5
drop n1 n2 n3 n4 n5

gen n1 = cfsva_f == 93
gen n2 = cfsvb_f == 86
gen n3 = cfsvc_f == 79
gen n4 = cfsvd_f == 72
gen n5 = cfsve_f == 65
gen numeracy_f = n1 + n2 + n3 + n4 + n5
drop n1 n2 n3 n4 n5
* respondent and non respondent
gen numeracy_dec = numeracy_m if deci == 1
replace numeracy_dec = numeracy_f if deci == 2
gen numeracy_ndec = numeracy_f if deci == 1
replace numeracy_ndec = numeracy_m if deci == 2
* Female respondent dummy
gen female_dec = 1 if deci == 2
replace female_dec = 0 if deci <2
* RESPONDENT SEX & AGE
gen indager_dec = indager_f if deci == 2
replace indager_dec = indager_m if deci <2
* age non respondent
gen indager_ndec = indager_m if deci == 2
replace indager_ndec = indager_f if deci <2
* Dummy age difference
gen mfmf = indager_m - indager_f
gen delta_10 = 1 if abs(mfmf)>10
recode delta_10 .=0
tab mfmf if delta_10 == 1

* Square age
gen indager_m2 = indager_m^2
gen indager_f2 = indager_f^2
gen indager_dec2 = indager_dec^2
gen indager_ndec2 = indager_ndec^2


* NUMERACY CATEGORIES
gen nn_m = numeracy_m
replace nn_m = 2 if numeracy_m == 2 | numeracy_m == 3 |numeracy_m == 4 
replace nn_m = 3 if numeracy_m == 5
gen nn_f = numeracy_f
replace nn_f = 2 if numeracy_f == 2 | numeracy_f == 3 |numeracy_f == 4 
replace nn_f = 3 if numeracy_f == 5
gen nn_dec = numeracy_dec
replace nn_dec = 2 if numeracy_dec == 2 | numeracy_dec == 3 |numeracy_dec == 4 
replace nn_dec = 3 if numeracy_dec == 5
gen nn_ndec = numeracy_ndec
replace nn_ndec = 2 if numeracy_ndec == 2 | numeracy_ndec == 3 |numeracy_ndec == 4 
replace nn_ndec = 3 if numeracy_ndec == 5

tab numeracy_m nn_m
tab numeracy_f nn_f
tab numeracy_dec nn_dec
tab numeracy_ndec nn_ndec
* HH higher numeracy and weighted numeracy
gen high_numeracy = nn_m if nn_m >= nn_f
replace high_numeracy = nn_f if high_numeracy==.
gen w_numeracy = gamma_m*nn_m+(1-gamma_m)*nn_f
summarize w_numeracy

* WORK / JOB
tab everwork_m everwork_f
tab wpactive_m wpactive_f
tab wpactive_m wpactive_f, nolabel
* respondent
gen wpactive_dec = wpactive_m if deci == 1
replace wpactive_dec = wpactive_f if deci == 2
tab wpactive_dec
*non respondent
gen wpactive_ndec = wpactive_f if deci == 1
replace wpactive_ndec = wpactive_m if deci == 2
tab wpactive_ndec

* EDUCATION
tab edend_m edend_f
tab edend_m, nolabel
tab edend_f
tab edqual_m edqual_f
* male
gen edu_m = 1 if edend_m <9
replace edu_m = 2 if edend_m == 9 | edend_m == 10 | edend_m == 11
replace edu_m = 3 if edend_m > 11
tab edu_m
* female
gen edu_f = 1 if edend_f<9
replace edu_f = 2 if edend_f == 9 | edend_f == 10 | edend_f == 11
replace edu_f = 3 if edend_f > 11
tab edu_f
* respondent
gen edu_dec = edu_m if deci == 1
replace edu_dec = edu_f if deci == 2
tab edu_dec
* non respondent
gen edu_ndec = edu_f if deci == 1
replace edu_ndec = edu_m if deci == 2
tab edu_ndec

tab edu_m edu_f
tab edu_dec edu_ndec

******* Stock market share - NET FINANCIAL WEALTH *******

summarize invests_hh net_finw_hh
summarize net_finw_hh if net_finw_hh <0
summarize net_finw_hh if invests_hh ==0
summarize invests_hh if net_finw_hh ==0 /* 13 obs, no investments. Later, substitute these alpha = 0 */

/*gen invests = invests_hh
replace invests = 0 if invests_hh < 100*/

gen stock_share = invests_hh/net_finw_hh
drop if net_finw_hh == .
replace stock_share = 0 if net_finw_hh == 0 /* invests_hh is 0 in all 13 cases*/ 
summarize stock_share if net_finw_hh < 0 /* 112 cases */
summarize stock_share if invests_hh ==0  /* 446 cases */
summarize stock_share if stock_share > 1 /* 20 cases  */ 
drop if net_finw_hh < 0
drop if stock_share == .
drop if stock_share > 1

* PARTICIPATION IN THE STOCK MARKET
gen participation = invests_hh>0
tab participation

summarize stock_share if participation == 1
summarize stock_share if invests_hh < 11 & participation == 1

* needed, otherwise the heckman selection does not work
gen stock_share_0 = stock_share
replace stock_share =. if stock_share==0

gen age_count = 1 if indager_m > 70 | indager_f > 70
recode age_count .=0

cor scgpati_1_m scfpati_1_m scgpati_1_f scfpati_1_f
cor scgrisk_1_m scfrisk_1_m scgrisk_1_f scfrisk_1_f

gen H_index_dec = H_index_m 
replace H_index_dec = H_index_f if deci == 2

gen H_index_ndec = H_index_m 
replace H_index_ndec = H_index_f if deci == 1

drop if H_index_m == .
drop if H_index_f == .

save wave_8_couple_v2, replace
/*
switch to R file "Sample_selection_and_descriptive_statistics.R" for 
descriptive tables (Table 1,2,3,4) and graphs (Figure 1,2,3)
*/

 xxxxxxxxx
 
/*************************************************************/
/*********************** MAIN RESULTS ************************/
/*************************************************************/

/******************************************************************/
* MALE & FEMALE PARTNERS
/******************************************************************/

* Table 11: BASELINE MODEL SELECTION - general vs financial risk aversion *

* only hh fin risk
heckman stock_share /*
*/  indager_m indager_m2 delta_10 i.quart_income/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_hh_mf scfpati_hh_mf, /*
*/ select(i.nn_m i.nn_f /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_hh_mf scfpati_hh_mf) 
eststo p1
estat ic

* only hh gen risk
heckman stock_share /*
*/  indager_m indager_m2 delta_10 i.quart_income/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scgrisk_hh_mf scgpati_hh_mf, /*
*/ select(i.nn_m i.nn_f /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scgrisk_hh_mf scgpati_hh_mf)
eststo p2 
estat ic

esttab p1 p2 /*
*/ using heckman_baseline_model_mf_c1.tex , /*
*/ mtitles("Financial risk" "General risk") star(* 0.10 ** 0.05 *** 0.01) /*
*/ collabels(none) label stats(aic bic N, fmt(%9.4f %9.0f %9.0fc) /* fmt(%9.4f %9.0f %9.0fc)
*/ labels("AIC" "BIC" "Number of observations")) plain b(%9.4f) /*
*/ se(%9.4f) noabbrev se nonumbers lines parentheses replace fragment

/*********************************************************/
* Tables 12 and 13: HECKMAN TWOSTAGE

	* individual risk
heckman stock_share /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfrisk_1_f scfpati_1_m scfpati_1_f, /*
*/ select(i.nn_m i.nn_f /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfrisk_1_f scfpati_1_m scfpati_1_f) twostep
eststo p1

	* weighted risk 
heckman stock_share /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_hh_mf scfpati_hh_mf, /*
*/ select(i.nn_m i.nn_f /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_hh_mf scfpati_hh_mf) twostep
eststo p2

esttab p1 p2 /* p3 p4
*/ using heckman_twostep_mf_c1.tex , /*
*/ mtitles("Individual" "Baseline") star(* 0.10 ** 0.05 *** 0.01) /*
*/ collabels(none) label stats(N, fmt(%9.4f %9.0f %9.0fc) /*
*/ labels("Number of individuals" "Number of observations")) plain b(%9.4f) /*
*/ se(%9.4f) noabbrev se nonumbers lines parentheses replace fragment

/*********************************************************************/

* Table 14: LIKELIHOOD RATIO TESTS 
* financial risk only - decisor risk preferences

* collective individual
heckman stock_share /*
*/  indager_m indager_m2 delta_10 i.quart_income /*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfrisk_1_f scfpati_1_m scfpati_1_f, /*
*/ select(i.nn_m i.nn_f /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfrisk_1_f scfpati_1_m scfpati_1_f)
eststo p1
estat ic
* collective weighted + male
heckman stock_share /*
*/  indager_m indager_m2 delta_10 i.quart_income /*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_hh_mf scfrisk_1_m scfpati_hh_mf scfpati_1_m, /*
*/ select(i.nn_m i.nn_f /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfrisk_1_f scfpati_1_m scfpati_1_f) 
eststo p2
estat ic
* unitary
heckman stock_share /*
*/  indager_m indager_m2 delta_10 i.quart_income /*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfpati_1_m, /*
*/ select(i.nn_m i.nn_f /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfrisk_1_f scfpati_1_m scfpati_1_f) 
eststo p3
estat ic

* Likelihood ratio test *
lrtest p1 p3
lrtest p2 p3


esttab p1 p2 p3/*
*/ using heckman_likelihood_test_mf_c1.tex , /*
*/ mtitles( "Individual" "HH risk + male" "Unitary") star(* 0.10 ** 0.05 *** 0.01) /*
*/ collabels(none) label stats(aic bic N, fmt(%9.4f %9.0f %9.0fc) /* fmt(%9.4f %9.0f %9.0fc)
*/ labels("AIC" "BIC" "Number of observations")) plain b(%9.4f) /*
*/ se(%9.4f) noabbrev se nonumbers lines parentheses replace fragment

/*********************************************************************/

* Table 15: COLLECTIVE BASELINE vs UNITARY - information criteria

*	* weighted risk 
heckman stock_share /*
*/  indager_m indager_m2 delta_10 i.quart_income /*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_hh_mf scfpati_hh_mf, /*
*/ select(i.nn_m i.nn_f /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_hh_mf scfpati_hh_mf) 
eststo p1
estat ic

	* unitary male
heckman stock_share /*
*/  indager_m indager_m2 delta_10 i.quart_income /*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfpati_1_m, /*
*/ select(i.nn_m i.nn_f /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfpati_1_m) 
eststo p2
estat ic

esttab p1 p2 /*
*/ using heckman_collective_unitary_AICBIC_mf_c1.tex , /*
*/ mtitles( "Baseline" "Unitary") star(* 0.10 ** 0.05 *** 0.01) /*
*/ collabels(none) label stats(aic bic N, fmt(%9.4f %9.0f %9.0fc) /* fmt(%9.4f %9.0f %9.0fc)
*/ labels("AIC" "BIC" "Number of observations")) plain b(%9.4f) /*
*/ se(%9.4f) noabbrev se nonumbers lines parentheses replace fragment

/**************************************************************/
/******************** Male - Female u65 ***********************/
/**************************************************************/

* Select only hh with at least one members younger than 65yo *
gen young = 1 if indager_m < 65 | indager_f < 65
replace young = 0 if young ==.
count if indager_m < 65 | indager_f < 65

sum participation if young==1
count if participation==1 & young==1

* Table 16: COLLECTIVE VS UNITARY - LIKELIHOOD RATIO TESTS u65 
* financial risk only - decisor risk preferences

* collective individual
heckman stock_share /*
*/  indager_m indager_m2 delta_10 i.quart_income /*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfrisk_1_f scfpati_1_m scfpati_1_f if young==1, /*
*/ select(i.nn_m i.nn_f /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfrisk_1_f scfpati_1_m scfpati_1_f)
eststo p1
estat ic
* collective weighted + decisor
heckman stock_share /*
*/  indager_m indager_m2 delta_10 i.quart_income /*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfrisk_hh_mf scfpati_1_m scfpati_hh_mf if young==1, /*
*/ select(i.nn_m i.nn_f /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfrisk_1_f scfpati_1_m scfpati_1_f)
eststo p2
estat ic
* unitary - dec
heckman stock_share /*
*/  indager_m indager_m2 delta_10 i.quart_income /*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfpati_1_m if young==1, /*
*/ select(i.nn_m i.nn_f /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfrisk_1_f scfpati_1_m scfpati_1_f)
eststo p3
estat ic

* Likelihood ratio test *
lrtest p1 p3
lrtest p2 p3

esttab p1 p2 p3 /*
*/ using heckman_likelihood_test_dec_u65.tex , /*
*/ mtitles( "Individual" "Collective" "Unitary dec") star(* 0.10 ** 0.05 *** 0.01) /*
*/ collabels(none) label stats(aic bic N, fmt(%9.4f %9.0f %9.0fc) /* fmt(%9.4f %9.0f %9.0fc)
*/ labels("AIC" "BIC" "Number of observations")) plain b(%9.4f) /*
*/ se(%9.4f) noabbrev se nonumbers lines parentheses replace fragment

/***************************************************/
* Table 17: OLLECTIVE VS UNITARY - information criteria - u65

	* weighted risk 
heckman stock_share /*
*/  indager_m indager_m2 delta_10 i.quart_income /*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_hh_mf scfpati_hh_mf if young==1, /*
*/ select(i.nn_m i.nn_f /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_hh_mf scfpati_hh_mf) 
eststo p1
estat ic

	* unitary dec
heckman stock_share /*
*/  indager_m indager_m2 delta_10 i.quart_income /*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfpati_1_m if young==1, /*
*/ select(i.nn_m i.nn_f /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfpati_1_m) 
eststo p2
estat ic

esttab p1 p2 /*
*/ using heckman_infcrit_mf_u65_c1.tex , /*
*/ mtitles("Collective" "Unitary") star(* 0.10 ** 0.05 *** 0.01) /*
*/ collabels(none) label stats(aic bic N, fmt(%9.4f %9.0f %9.0fc) /* fmt(%9.4f %9.0f %9.0fc)
*/ labels("AIC" "BIC" "Number of observations")) plain b(%9.4f) /*
*/ se(%9.4f) noabbrev se nonumbers lines parentheses replace fragment

/*******************************************************/

/******************************************************/
/********************** APPENDIX **********************/
/****************** ROBUSTNESS CHECK ******************/
/******************************************************/

/*****************************************************/
* Appendix Table 2 & 3: HECKMAN TWOSTAGE - u65 - male/female

* collective individual - u65
heckman stock_share /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfpati_1_m scfrisk_1_f scfpati_1_f if young==1, /*
*/ select(i.nn_m i.nn_f /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfpati_1_m scfrisk_1_f scfpati_1_f) twostep
eststo p1

* collective weighted - u65
heckman stock_share /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_hh_mf scfpati_hh_mf if young==1, /*
*/ select(i.nn_m i.nn_f /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_hh_mf scfpati_hh_mf) twostep
eststo p2

esttab p1 p2/*
*/ using heckman_2stage_mf_u65_c1.tex , /*
*/ mtitles( "Individual" "Weighted") star(* 0.10 ** 0.05 *** 0.01) /*
*/ collabels(none) label stats(aic bic N, fmt(%9.4f %9.0f %9.0fc) /* fmt(%9.4f %9.0f %9.0fc)
*/ labels("AIC" "BIC" "Number of observations")) plain b(%9.4f) /*
*/ se(%9.4f) noabbrev se nonumbers lines parentheses replace fragment

/*****************************************************/
* Appendix Table 4 & 5: HECKMAN TWOSTAGE - male numeracy only

	* collective individual
heckman stock_share /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfpati_1_m scfrisk_1_f scfpati_1_f if young==1, /*
*/ select(i.nn_m /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfpati_1_m scfrisk_1_f scfpati_1_f) twostep
eststo p1
	* collective hh
heckman stock_share /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_hh_mf scfpati_hh_mf, /*
*/ select(i.nn_m /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_hh_mf scfpati_hh_mf) twostep
eststo p2

esttab p1 p2/*
*/ using heckman_2stage_mf_numeracy_c1.tex , /*
*/ mtitles( "Individual" "Weighted") star(* 0.10 ** 0.05 *** 0.01) /*
*/ collabels(none) label stats(aic bic N, fmt(%9.4f %9.0f %9.0fc) /* fmt(%9.4f %9.0f %9.0fc)
*/ labels("AIC" "BIC" "Number of observations")) plain b(%9.4f) /*
*/ se(%9.4f) noabbrev se nonumbers lines parentheses replace fragment

/*****************************************************/
* Appendix Table 6 & 7: HECKMAN TWOSTAGE - general risk preferences

* collective individual
heckman stock_share /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfpati_1_m scfrisk_1_f scfpati_1_f, /*
*/ select(i.nn_m i.nn_f /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfpati_1_m scfrisk_1_f scfpati_1_f) twostep
eststo p1

	* unitary dec
heckman stock_share /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_hh_mf scfpati_hh_mf, /*
*/ select(i.nn_m i.nn_f /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_hh_mf scfpati_hh_mf) twostep
eststo p2


esttab p1 p2/*
*/ using heckman_2stage_mf_general_risk_c1.tex , /*
*/ mtitles( "Individual" "Weighted") star(* 0.10 ** 0.05 *** 0.01) /*
*/ collabels(none) label stats(aic bic N, fmt(%9.4f %9.0f %9.0fc) /* fmt(%9.4f %9.0f %9.0fc)
*/ labels("AIC" "BIC" "Number of observations")) plain b(%9.4f) /*
*/ se(%9.4f) noabbrev se nonumbers lines parentheses replace fragment

/*****************************************************/


/**********************************************************/
/********** Financial Respondent DISTINCTION **************/
/**********************************************************/

* Appendix Table 8 & 9: TWO-STAGE HECKMAN ESTIMATES - male female partners

* Male-female financial risk tolerance
heckman stock_share /*
*/  indager_dec indager_dec2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_dec i.wpactive_ndec i.edu_dec i.edu_ndec H_index_dec H_index_ndec/*
*/  scfrisk_1_dec scfrisk_1_ndec scfpati_1_dec scfpati_1_ndec, /*
*/ select(i.nn_dec i.nn_ndec /*
*/  indager_dec indager_dec2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_dec i.wpactive_ndec i.edu_dec i.edu_ndec H_index_dec H_index_ndec/*
*/  scfrisk_1_dec scfrisk_1_ndec scfpati_1_dec scfpati_1_ndec) twostep
eststo p1

* hh financial risk tolerance
heckman stock_share /*
*/  indager_dec indager_dec2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_dec i.wpactive_ndec i.edu_dec i.edu_ndec H_index_dec H_index_ndec/*
*/  scfrisk_hh_dec scfpati_hh_dec, /*
*/ select(i.nn_m i.nn_f  /*
*/  indager_dec indager_dec2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_dec i.wpactive_ndec i.edu_dec i.edu_ndec H_index_dec H_index_ndec/*
*/  scfrisk_hh_dec scfpati_hh_dec) twostep
eststo p2

esttab p1 p2 /*
*/ using heckman_twostep_dec_c1.tex , /*
*/ mtitles("Individual" "Collective" ) star(* 0.10 ** 0.05 *** 0.01) /*
*/ collabels(none) label stats(r2 N, fmt(%9.4f %9.0f %9.0fc) /*
*/ labels("R-squared" "Number of observations")) plain b(%9.4f) /*
*/ se(%9.4f) noabbrev se nonumbers lines parentheses replace fragment


/*********************************************************************/
* Appendix Table 10: COLLECTIVE vs UNITARY - LIKELIHOOD RATIO TEST

* individual preferences
heckman stock_share /*
*/  indager_dec indager_dec2 delta_10 i.quart_income /*
*/  i.wpactive_dec i.wpactive_ndec i.edu_dec i.edu_ndec H_index_dec H_index_ndec/*
*/  scfrisk_1_dec scfrisk_1_ndec scfpati_1_dec scfpati_1_ndec, /*
*/ select(i.nn_dec i.nn_ndec /*
*/  indager_dec indager_dec2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_dec i.wpactive_ndec i.edu_dec i.edu_ndec H_index_dec H_index_ndec/*
*/  scfrisk_1_dec scfrisk_1_ndec scfpati_1_dec scfpati_1_ndec) 
eststo p1

* hh financial risk tolerance + decisor
heckman stock_share /*
*/  indager_dec indager_dec2 delta_10 i.quart_income /*
*/  i.wpactive_dec i.wpactive_ndec i.edu_dec i.edu_ndec H_index_dec H_index_ndec/*
*/  scfrisk_hh_dec scfpati_hh_dec, /*
*/ select(i.nn_m i.nn_f  /*
*/  indager_dec indager_dec2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_dec i.wpactive_ndec i.edu_dec i.edu_ndec H_index_dec H_index_ndec/*
*/  scfrisk_1_dec scfrisk_1_ndec scfpati_1_dec scfpati_1_ndec) 
eststo p2

* unitary decisor
heckman stock_share /*
*/  indager_dec indager_dec2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_dec i.wpactive_ndec i.edu_dec i.edu_ndec H_index_dec H_index_ndec/*
*/  scfrisk_hh_dec scfpati_hh_dec, /*
*/ select(i.nn_m i.nn_f  /*
*/  indager_dec indager_dec2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_dec i.wpactive_ndec i.edu_dec i.edu_ndec H_index_dec H_index_ndec/*
*/  scfrisk_hh_dec scfpati_hh_dec) twostep
eststo p3

* Likelihood ratio test
lrtest p1 p3
lrtest p2 p3

esttab p1 p2 p3 /*
*/ using heckman_likelihood_test.tex , /*
*/ mtitles("Individual" "Weighted male" "Unitary") star(* 0.10 ** 0.05 *** 0.01) /*
*/ collabels(none) label stats(aic bic N, fmt(%9.4f %9.0f %9.0fc) /* fmt(%9.4f %9.0f %9.0fc)
*/ labels("AIC" "BIC" "Number of observations")) plain b(%9.4f) /*
*/ se(%9.4f) noabbrev se nonumbers lines parentheses replace fragment

/*************************************************************/
* Appendix Table 11: Baseline Collective vs Unitary - AIC and BIC *

* Baseline Collective
heckman stock_share /*
*/  indager_m indager_m2 delta_10 i.quart_income /*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_hh_mf scfpati_hh_mf, /*
*/ select(i.nn_m i.nn_f /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_hh_mf scfpati_hh_mf) 
eststo p1
estat ic

* Unitary - male
heckman stock_share /*
*/  indager_m indager_m2 delta_10 i.quart_income /*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfpati_1_m , /*
*/ select(i.nn_m i.nn_f /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfpati_1_m)
eststo p2
estat ic

* Unitary - female
heckman stock_share /*
*/  indager_m indager_m2 delta_10 i.quart_income /*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_f scfpati_1_f , /*
*/ select(i.nn_m i.nn_f /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_f scfpati_1_f)
eststo p3
estat ic

esttab p1 p2 p3/*
*/ using heckman_AIC_BIC_mf.tex , /*
*/ mtitles("Collective" "Unitary - Male" "Unitary - Female") star(* 0.10 ** 0.05 *** 0.01) /*
*/ collabels(none) label stats(aic bic N, fmt(%9.4f %9.0f %9.0fc) /* fmt(%9.4f %9.0f %9.0fc)
*/ labels("AIC" "BIC" "Number of observations")) plain b(%9.4f) /*
*/ se(%9.4f) noabbrev se nonumbers lines parentheses replace fragment

/*****************************************************************/

/*************************************************************/
* U65 yo ANALYSIS *
/*************************************************************/

* Appendix Table 12 & 13: TWO-STAGE HECKMAN ESTIMATES - male female partners u65

* Male-female financial risk tolerance
heckman stock_share /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfrisk_1_f scfpati_1_m scfpati_1_f if young == 1, /*
*/ select(i.nn_m i.nn_f /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfrisk_1_f scfpati_1_m scfpati_1_f) twostep
eststo p1

* hh financial risk tolerance
heckman stock_share /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_hh_mf scfpati_hh_mf if young == 1, /*
*/ select(i.nn_m i.nn_f  /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_hh_mf scfpati_hh_mf) twostep
eststo p2

esttab p1 p2 /*
*/ using heckman_twostep_mf_u65.tex , /*
*/ mtitles("Individual" "Collective" ) star(* 0.10 ** 0.05 *** 0.01) /*
*/ collabels(none) label stats(r2 N, fmt(%9.4f %9.0f %9.0fc) /*
*/ labels("R-squared" "Number of observations")) plain b(%9.4f) /*
*/ se(%9.4f) noabbrev se nonumbers lines parentheses replace fragment


/*********************************************************************/
* Appendix Table 14: COLLECTIVE vs UNITARY - LIKELIHOOD RATIO TEST

* male and female
heckman stock_share /*
*/  indager_m indager_m2 delta_10 i.quart_income /*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfrisk_1_f scfpati_1_m scfpati_1_f if young == 1, /*
*/ select(i.nn_m i.nn_f /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfrisk_1_f scfpati_1_m scfpati_1_f) 
eststo p1
estat ic

* male and hh fin risk
heckman stock_share /*
*/  indager_m indager_m2 delta_10 i.quart_income/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfrisk_hh_mf scfpati_1_m scfpati_hh_mf if young == 1, /*
*/ select(i.nn_m i.nn_f /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfrisk_1_f scfpati_1_m scfpati_1_f) 
eststo p2
estat ic

* only male fin risk
heckman stock_share /*
*/  indager_m indager_m2 delta_10 i.quart_income/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfpati_1_m if young == 1, /*
*/ select(i.nn_m i.nn_f /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfrisk_1_f scfpati_1_m scfpati_1_f) 
eststo p3
estat ic

* Likelihood ratio test
lrtest p1 p3
lrtest p2 p3

esttab p1 p2 p3 /*
*/ using heckman_likelihood_test_mf_u65.tex , /*
*/ mtitles("Individual" "Weighted male" "Unitary") star(* 0.10 ** 0.05 *** 0.01) /*
*/ collabels(none) label stats(aic bic N, fmt(%9.4f %9.0f %9.0fc) /* fmt(%9.4f %9.0f %9.0fc)
*/ labels("AIC" "BIC" "Number of observations")) plain b(%9.4f) /*
*/ se(%9.4f) noabbrev se nonumbers lines parentheses replace fragment

/*************************************************************/
* Appendix Table 15: Baseline Collective vs Unitary - AIC and BIC *

* Baseline Collective
heckman stock_share /*
*/  indager_m indager_m2 delta_10 i.quart_income /*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_hh_mf scfpati_hh_mf if young == 1, /*
*/ select(i.nn_m i.nn_f /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_hh_mf scfpati_hh_mf) 
eststo p1
estat ic

* Unitary - male
heckman stock_share /*
*/  indager_m indager_m2 delta_10 i.quart_income /*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfpati_1_m  if young == 1, /*
*/ select(i.nn_m i.nn_f /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_m scfpati_1_m)
eststo p2
estat ic

* Unitary - female
heckman stock_share /*
*/  indager_m indager_m2 delta_10 i.quart_income /*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_f scfpati_1_f if young == 1, /*
*/ select(i.nn_m i.nn_f /*
*/  indager_m indager_m2 delta_10 i.quart_income i.quart_totw_hh/*
*/  i.wpactive_m i.wpactive_f i.edu_m i.edu_f H_index_m H_index_f/*
*/  scfrisk_1_f scfpati_1_f)
eststo p3
estat ic

esttab p1 p2 p3/*
*/ using heckman_AIC_BIC_mf_u65.tex , /*
*/ mtitles("Collective" "Unitary - Male" "Unitary - Female") star(* 0.10 ** 0.05 *** 0.01) /*
*/ collabels(none) label stats(aic bic N, fmt(%9.4f %9.0f %9.0fc) /* fmt(%9.4f %9.0f %9.0fc)
*/ labels("AIC" "BIC" "Number of observations")) plain b(%9.4f) /*
*/ se(%9.4f) noabbrev se nonumbers lines parentheses replace fragment

/*****************************************************************/