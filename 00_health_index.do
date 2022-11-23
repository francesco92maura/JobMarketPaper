/*
This .do file generates the sickness/health index used in the paper.
I use the code provided by Chiara Dal Bianco in 
"Disability insurance and the effects of return-to-work policies"
*/

clear

* Laptop - lenovo
cd "G:\Il mio Drive\PhD Padova\3rd year\ELSA data\stata13_se\"
* Surface
*cd "C:\Users\39334\Google Drive\PhD Padova\3rd year\ELSA data\stata13_se"
* Surface
*cd "your path here"

/***************************************************************/
* Health index *
/***************************************************************/

clear
set maxvar 10000

use "wave_8_elsa_ifs_dvs_eul_v1.dta"
merge 1:1 idauniq using "wave_8_elsa_data_eul_v2.dta"
drop _merge
merge 1:1 idauniq using "wave_8_elsa_financial_dvs_eul_v1.dta"
drop _merge

rename *, lower

* adl/iadl
	egen adl=rowtotal(headldr headlwa headlba headlea headlbe headlwc)
	replace adl=. if headldr==.
	gen adl1=(adl>0) if adl!=.
	
	egen iadl=rowtotal(headlma headlpr headlsh headlph headlme headlho headlmo)
	replace iadl=. if headldr==.
	gen iadl1=(iadl>0) if iadl!=.
		
* Incontinence	
	gen incont = (schelncta==1 | schebowa==1)
	recode incont min/-1=.

** eye problems
	gen glaucoma = (heoptgl==1 |  heopcgl==1)
	replace glaucoma = . if heoptgl<0 | heopcgl<-1
	gen cataracts = (heoptca==1 | heopcca==1)
	replace cataracts = . if heoptca<0 | heopcca<-1
	gen diabeye = (heoptdi==1 |  heopcdi==1)
	replace diabeye = . if heoptdi<0 | heopcdi<-1
	gen maculardeg = (heoptmd==1 | heopcmd==1)
	replace maculardeg = . if heoptmd<0 | heopcmd<-1
	
recode cesd_sc 9=8 0/1=1
egen othereyepr=rowmax(diabeye maculardeg)
egen demalzpark=rowmax(hedibde hedibad hedibpd)
	
* the different number of valid answers to self reported health is due to proxy interviews
# delimit ;
	global health "hemobwa hemobsi hemobch hemobcs hemobcl hemobst hemobre hemobpu hemobli hemobpi 
		hedimbp hedimdi	hedimst hediblu hedibas hedibar hedibos	hedibca hedibps demalzpark
		incont heeye hefunc glaucoma othereyepr cataracts hehear 
		hediman	hedimmi	hedimhf	hedimhm	hedimar	adl iadl cesd_sc" ;
# delimit cr

* Principal component analysis
	pca $health, components(1) means
	predict H_index, score 

keep idauniq idahhw8 H_index
save wave8_Hindex, replace