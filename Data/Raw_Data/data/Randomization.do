*qui {

loc n = 3 

clear
capture log close
cd "~/Data-transfers"
*log using nccsCorePC_2014_logbunching_by_nogrow.log, replace
*set mem 10G
set more off
*set matsize 3000
set seed 12345`n'


*import excel "~/Data-transfers/Lumina Export 050115 reformat.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 051115.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 051215.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 051315.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 051415.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 051815.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 051915.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 052015.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 052115.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 052215.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 052615.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 052715.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 052815.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 053115.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 060315.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 060415.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 060715.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 060915.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 061115.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 061215.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 061515.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 061815.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 062115.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 062315.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 062515.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 062815.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 063015.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 070215.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 070715.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 070915.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 071215.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 071415.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 071615.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 072015.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 072115.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 072215.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 072615.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 072615_2.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 072915.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 073115.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 080315.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 080415.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 080515.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 080515_2.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 080715.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 080915.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 081015.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 081115.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 081315.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 081415.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 081715.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 081915.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 082015.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 082115.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 082415.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 082415_2.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 082515.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 082615.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Exports 082815.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 083015.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 083115.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 090215.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 090315.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 090815.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 090815_2.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 091015.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 091015_2.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 091415.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 091515.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 091615.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 091715.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 092115.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 092115_2.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 092215.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 092315.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 092415.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 092715.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 092815.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 092915.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 093015.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 100115.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 100415.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 100515.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 100615.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 100715.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 100815.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 101215.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 101215_2.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 101315.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 101415.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 101515.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 101915.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 102115.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 102115_2.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 102215.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 102512.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 102715.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 102715_2.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 102815.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 110115.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 110315.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 110415.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 110515.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 110815.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 111015.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 111015_2.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 111215.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 111515.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 111615.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 111715.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 111815.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 111915.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 112315.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 112315_2.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 112515.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 112915.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 113015.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 120115.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 120215.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 120415.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 120615.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 120715.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 120815.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Exports 120815.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 121015.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 121415.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 121415_2.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 121515.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 121615.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 121715.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 122115.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 123115.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 010416.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 010616.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 010716.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 011016.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 011116.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 011216.xlsx", firstrow clear
*import excel "~/Data-transfers/Lumina Export 011316.xlsx", firstrow clear
local m = 8
local d = 29
 
if `d'<10 & `m'<10 {
	import excel "/home/uiuc_CCA/Data-transfers/Lumina Export 0`m'0`d'16.xlsx", first clear	
	gen date_packaged = "0`m'0`d'16"
}

if `d'<10 & `m'>9 {
	import excel "/home/uiuc_CCA/Data-transfers/Lumina Export `m'0`d'16.xlsx", first clear	
	gen date_packaged = "`m'0`d'16"
}

if `d'>9 & `m'<10 {
	import excel "/home/uiuc_CCA/Data-transfers/Lumina Export 0`m'`d'16.xlsx", first clear	
	gen date_packaged = "0`m'`d'16"
}

if `d'>9 & `m'>9 {
	import excel "/home/uiuc_CCA/Data-transfers/Lumina Export `m'`d'16.xlsx", first clear	
	gen date_packaged = "`m'`d'16"
}

drop if ResearchID==.
*gen test = .
destring DependencyStatus, replace
foreach var of varlist TotalSubsidizedLoans TotalUnsubsidizedLoans TotalPLUSLoans {
	replace `var'=0 if `var'==.
}
gen totalloans = TotalSubsidizedLoans + TotalUnsubsidizedLoans + TotalPLUSLoans
*gen totalloans = TotalSubsidizedLoans + TotalUnsubsidizedLoans  /* no PLUS loans observed in previous year */
gen freshman = AcademicLevel=="First Year"
gen new = FirstTimeatTriC=="YES" | FirstTimeatTriC=="Yes"
gen indep = 1-DependencyStatus
rename EFC Prmry_EFC
global pellEFC = 5198	/*	https://webmail.illinois.edu/owa/redir.aspx?C=I7q6EQTU8kyWNAI5pK_FKiYsxM1QKdIICsXzGEtQSVlYW_M-Hg7bR8_-jFyoMpehWF41iJdpLx0.&URL=http%3a%2f%2fwww.ifap.ed.gov%2fdpcletters%2fattachments%2fGEN1502Attach.pdf 	*/



* Assign strata



gen stratum = ""

replace stratum = "New, 1st, dep, debt, EFC   0" if new==1 & freshman==1 & indep==0 & Prmry_EFC==0 & totalloans>0
replace stratum = "New, 1st, dep, debt, EFC 0-$pellEFC" if new==1 & freshman==1 & indep==0 & Prmry_EFC>0 & Prmry_EFC<=$pellEFC & totalloans>0
replace stratum = "New, 1st, dep, debt, EFC >$pellEFC" if new==1 & freshman==1 & indep==0 & Prmry_EFC>$pellEFC & totalloans>0
replace stratum = "New, 1st, dep, nodebt, EFC 0" if new==1 & freshman==1 & indep==0 & Prmry_EFC==0 & totalloans==0
replace stratum = "New, 1st, dep, nodebt, EFC   0-750" if new==1 & freshman==1 & indep==0 & Prmry_EFC>0 & Prmry_EFC<=750 & totalloans==0
replace stratum = "New, 1st, dep, nodebt, EFC   750-2000" if new==1 & freshman==1 & indep==0 & Prmry_EFC>750 & Prmry_EFC<=2000 & totalloans==0
replace stratum = "New, 1st, dep, nodebt, EFC  2000-3000" if new==1 & freshman==1 & indep==0 & Prmry_EFC>2000 & Prmry_EFC<=3000 & totalloans==0
replace stratum = "New, 1st, dep, nodebt, EFC  3000-$pellEFC" if new==1 & freshman==1 & indep==0 & Prmry_EFC>3000 & Prmry_EFC<=$pellEFC & totalloans==0
replace stratum = "New, 1st, dep, nodebt, EFC  $pellEFC-7500" if new==1 & freshman==1 & indep==0 & Prmry_EFC>$pellEFC & Prmry_EFC<=7500 & totalloans==0
replace stratum = "New, 1st, dep, nodebt, EFC  7500-10K" if new==1 & freshman==1 & indep==0 & Prmry_EFC>7500 & Prmry_EFC<=10000 & totalloans==0
replace stratum = "New, 1st, dep, nodebt, EFC 10K-17K" if new==1 & freshman==1 & indep==0 & Prmry_EFC>10000 & Prmry_EFC<=17000 & totalloans==0
replace stratum = "New, 1st, dep, nodebt, EFC >17K" if new==1 & freshman==1 & indep==0 & Prmry_EFC>17000 & totalloans==0
replace stratum = "New, 1st, ind, debt, EFC 0" if new==1 & freshman==1 & indep==1 & Prmry_EFC==0 & totalloans>0
replace stratum = "New, 1st, ind, debt, EFC 0-$pellEFC" if new==1 & freshman==1 & indep==1 & Prmry_EFC>0 & Prmry_EFC<=$pellEFC & totalloans>0
replace stratum = "New, 1st, ind, debt, EFC >$pellEFC" if new==1 & freshman==1 & indep==1 & Prmry_EFC>$pellEFC & totalloans>0
replace stratum = "New, 1st, ind, nodebt, EFC 0" if new==1 & freshman==1 & indep==1 & Prmry_EFC==0 & totalloans==0
replace stratum = "New, 1st, ind, nodebt, EFC 0-$pellEFC" if new==1 & freshman==1 & indep==1 & Prmry_EFC>0 & Prmry_EFC<=$pellEFC & totalloans==0
replace stratum = "New, 1st, ind, nodebt, EFC >$pellEFC" if new==1 & freshman==1 & indep==1 & Prmry_EFC>$pellEFC & totalloans==0

replace stratum = "New, 2nd, dep, debt, EFC 0" if new==1 & freshman==0 & indep==0 & Prmry_EFC==0 & totalloans>0
replace stratum = "New, 2nd, dep, debt, EFC 0-$pellEFC" if new==1 & freshman==0 & indep==0 & Prmry_EFC>0 & Prmry_EFC<=$pellEFC & totalloans>0
replace stratum = "New, 2nd, dep, debt, EFC >$pellEFC" if new==1 & freshman==0 & indep==0 & Prmry_EFC>$pellEFC & totalloans>0
replace stratum = "New, 2nd, dep, nodebt, EFC 0" if new==1 & freshman==0 & indep==0 & Prmry_EFC==0 & totalloans==0
replace stratum = "New, 2nd, dep, nodebt, EFC 0-$pellEFC" if new==1 & freshman==0 & indep==0 & Prmry_EFC>0 & Prmry_EFC<=$pellEFC & totalloans==0
replace stratum = "New, 2nd, dep, nodebt, EFC >$pellEFC" if new==1 & freshman==0 & indep==0 & Prmry_EFC>$pellEFC & totalloans==0
replace stratum = "New, 2nd, ind, debt, EFC 0" if new==1 & freshman==0 & indep==1 & Prmry_EFC==0 & totalloans>0
replace stratum = "New, 2nd, ind, debt, EFC 0-$pellEFC" if new==1 & freshman==0 & indep==1 & Prmry_EFC>0 & Prmry_EFC<=$pellEFC & totalloans>0
replace stratum = "New, 2nd, ind, debt, EFC >$pellEFC" if new==1 & freshman==0 & indep==1 & Prmry_EFC>$pellEFC & totalloans>0
replace stratum = "New, 2nd, ind, nodebt, EFC 0" if new==1 & freshman==0 & indep==1 & Prmry_EFC==0 & totalloans==0
replace stratum = "New, 2nd, ind, nodebt, EFC 0-$pellEFC" if new==1 & freshman==0 & indep==1 & Prmry_EFC>0 & Prmry_EFC<=$pellEFC & totalloans==0
replace stratum = "New, 2nd, ind, nodebt, EFC >$pellEFC" if new==1 & freshman==0 & indep==1 & Prmry_EFC>$pellEFC & totalloans==0

replace stratum = "Old, 1st, dep, debt, EFC 0" if new==0 & freshman==1 & indep==0 & Prmry_EFC==0 & totalloans>0
replace stratum = "Old, 1st, dep, debt, EFC 0-2000" if new==0 & freshman==1 & indep==0 & Prmry_EFC>0 & Prmry_EFC<=2000 & totalloans>0
replace stratum = "Old, 1st, dep, debt, EFC 2000-$pellEFC" if new==0 & freshman==1 & indep==0 & Prmry_EFC>2000 & Prmry_EFC<=$pellEFC & totalloans>0
replace stratum = "Old, 1st, dep, debt, EFC $pellEFC-10K" if new==0 & freshman==1 & indep==0 & Prmry_EFC>$pellEFC & Prmry_EFC<=10000 & totalloans>0
replace stratum = "Old, 1st, dep, debt, EFC >10K" if new==0 & freshman==1 & indep==0 & Prmry_EFC>10000 & totalloans>0
replace stratum = "Old, 1st, dep, nodebt, EFC 0" if new==0 & freshman==1 & indep==0 & Prmry_EFC==0 & totalloans==0
replace stratum = "Old, 1st, dep, nodebt, EFC 0-2000" if new==0 & freshman==1 & indep==0 & Prmry_EFC>0 & Prmry_EFC<=2000 & totalloans==0
replace stratum = "Old, 1st, dep, nodebt, EFC 2000-$pellEFC" if new==0 & freshman==1 & indep==0 & Prmry_EFC>2000 & Prmry_EFC<=$pellEFC & totalloans==0
replace stratum = "Old, 1st, dep, nodebt, EFC $pellEFC-10K" if new==0 & freshman==1 & indep==0 & Prmry_EFC>$pellEFC & Prmry_EFC<=10000 & totalloans==0
replace stratum = "Old, 1st, dep, nodebt, EFC >10K" if new==0 & freshman==1 & indep==0 & Prmry_EFC>10000 & totalloans==0
replace stratum = "Old, 1st, ind, debt, EFC 0" if new==0 & freshman==1 & indep==1 & Prmry_EFC==0 & totalloans>0
replace stratum = "Old, 1st, ind, debt, EFC 0-2000" if new==0 & freshman==1 & indep==1 & Prmry_EFC>0 & Prmry_EFC<=2000 & totalloans>0
replace stratum = "Old, 1st, ind, debt, EFC 2000-$pellEFC" if new==0 & freshman==1 & indep==1 & Prmry_EFC>2000 & Prmry_EFC<=$pellEFC & totalloans>0
replace stratum = "Old, 1st, ind, debt, EFC $pellEFC-10K" if new==0 & freshman==1 & indep==1 & Prmry_EFC>$pellEFC & Prmry_EFC<=10000 & totalloans>0
replace stratum = "Old, 1st, ind, debt, EFC >10K" if new==0 & freshman==1 & indep==1 & Prmry_EFC>10000 & totalloans>0
replace stratum = "Old, 1st, ind, nodebt, EFC 0" if new==0 & freshman==1 & indep==1 & Prmry_EFC==0 & totalloans==0
replace stratum = "Old, 1st, ind, nodebt, EFC 0-2000" if new==0 & freshman==1 & indep==1 & Prmry_EFC>0 & Prmry_EFC<=2000 & totalloans==0
replace stratum = "Old, 1st, ind, nodebt, EFC 2000-$pellEFC" if new==0 & freshman==1 & indep==1 & Prmry_EFC>2000 & Prmry_EFC<=$pellEFC & totalloans==0
replace stratum = "Old, 1st, ind, nodebt, EFC $pellEFC-10K" if new==0 & freshman==1 & indep==1 & Prmry_EFC>$pellEFC & Prmry_EFC<=10000 & totalloans==0
replace stratum = "Old, 1st, ind, nodebt, EFC >10K" if new==0 & freshman==1 & indep==1 & Prmry_EFC>10000 & totalloans==0

replace stratum = "Old, 2nd, dep, debt, EFC   0" if new==0 & freshman==0 & indep==0 & Prmry_EFC==0 & totalloans>0
replace stratum = "Old, 2nd, dep, debt, EFC   0-750" if new==0 & freshman==0 & indep==0 & Prmry_EFC>0 & Prmry_EFC<=750 & totalloans>0
replace stratum = "Old, 2nd, dep, debt, EFC   750-2000" if new==0 & freshman==0 & indep==0 & Prmry_EFC>750 & Prmry_EFC<=2000 & totalloans>0
replace stratum = "Old, 2nd, dep, debt, EFC  2000-3000" if new==0 & freshman==0 & indep==0 & Prmry_EFC>2000 & Prmry_EFC<=3000 & totalloans>0
replace stratum = "Old, 2nd, dep, debt, EFC  3000-$pellEFC" if new==0 & freshman==0 & indep==0 & Prmry_EFC>3000 & Prmry_EFC<=$pellEFC & totalloans>0
replace stratum = "Old, 2nd, dep, debt, EFC  $pellEFC-7500" if new==0 & freshman==0 & indep==0 & Prmry_EFC>$pellEFC & Prmry_EFC<=7500 & totalloans>0
replace stratum = "Old, 2nd, dep, debt, EFC  7500-10K" if new==0 & freshman==0 & indep==0 & Prmry_EFC>7500 & Prmry_EFC<=10000 & totalloans>0
replace stratum = "Old, 2nd, dep, debt, EFC 10K-17K" if new==0 & freshman==0 & indep==0 & Prmry_EFC>10000 & Prmry_EFC<=17000 & totalloans>0
replace stratum = "Old, 2nd, dep, debt, EFC >17K" if new==0 & freshman==0 & indep==0 & Prmry_EFC>17000 & totalloans>0
replace stratum = "Old, 2nd, dep, nodebt, EFC   0" if new==0 & freshman==0 & indep==0 & Prmry_EFC==0 & totalloans==0
replace stratum = "Old, 2nd, dep, nodebt, EFC   0-750" if new==0 & freshman==0 & indep==0 & Prmry_EFC>0 & Prmry_EFC<=750 & totalloans==0
replace stratum = "Old, 2nd, dep, nodebt, EFC   750-2000" if new==0 & freshman==0 & indep==0 & Prmry_EFC>750 & Prmry_EFC<=2000 & totalloans==0
replace stratum = "Old, 2nd, dep, nodebt, EFC  2000-3000" if new==0 & freshman==0 & indep==0 & Prmry_EFC>2000 & Prmry_EFC<=3000 & totalloans==0
replace stratum = "Old, 2nd, dep, nodebt, EFC  3000-$pellEFC" if new==0 & freshman==0 & indep==0 & Prmry_EFC>3000 & Prmry_EFC<=$pellEFC & totalloans==0
replace stratum = "Old, 2nd, dep, nodebt, EFC  $pellEFC-7500" if new==0 & freshman==0 & indep==0 & Prmry_EFC>$pellEFC & Prmry_EFC<=7500 & totalloans==0
replace stratum = "Old, 2nd, dep, nodebt, EFC  7500-10K" if new==0 & freshman==0 & indep==0 & Prmry_EFC>7500 & Prmry_EFC<=10000 & totalloans==0
replace stratum = "Old, 2nd, dep, nodebt, EFC 10K-17K" if new==0 & freshman==0 & indep==0 & Prmry_EFC>10000 & Prmry_EFC<=17000 & totalloans==0
replace stratum = "Old, 2nd, dep, nodebt, EFC >17K" if new==0 & freshman==0 & indep==0 & Prmry_EFC>17000 & totalloans==0
replace stratum = "Old, 2nd, ind, debt, EFC   0" if new==0 & freshman==0 & indep==1 & Prmry_EFC==0 & totalloans>0
replace stratum = "Old, 2nd, ind, debt, EFC   0-750" if new==0 & freshman==0 & indep==1 & Prmry_EFC>0 & Prmry_EFC<=750 & totalloans>0
replace stratum = "Old, 2nd, ind, debt, EFC   750-2000" if new==0 & freshman==0 & indep==1 & Prmry_EFC>750 & Prmry_EFC<=2000 & totalloans>0
replace stratum = "Old, 2nd, ind, debt, EFC  2000-3000" if new==0 & freshman==0 & indep==1 & Prmry_EFC>2000 & Prmry_EFC<=3000 & totalloans>0
replace stratum = "Old, 2nd, ind, debt, EFC  3000-$pellEFC" if new==0 & freshman==0 & indep==1 & Prmry_EFC>3000 & Prmry_EFC<=$pellEFC & totalloans>0
replace stratum = "Old, 2nd, ind, debt, EFC  $pellEFC-7500" if new==0 & freshman==0 & indep==1 & Prmry_EFC>$pellEFC & Prmry_EFC<=7500 & totalloans>0
replace stratum = "Old, 2nd, ind, debt, EFC  7500-10K" if new==0 & freshman==0 & indep==1 & Prmry_EFC>7500 & Prmry_EFC<=10000 & totalloans>0
replace stratum = "Old, 2nd, ind, debt, EFC 10K-17K" if new==0 & freshman==0 & indep==1 & Prmry_EFC>10000 & Prmry_EFC<=17000 & totalloans>0
replace stratum = "Old, 2nd, ind, debt, EFC >17K" if new==0 & freshman==0 & indep==1 & Prmry_EFC>17000 & totalloans>0
replace stratum = "Old, 2nd, ind, nodebt, EFC   0" if new==0 & freshman==0 & indep==1 & Prmry_EFC==0 & totalloans==0
replace stratum = "Old, 2nd, ind, nodebt, EFC   0-750" if new==0 & freshman==0 & indep==1 & Prmry_EFC>0 & Prmry_EFC<=750 & totalloans==0
replace stratum = "Old, 2nd, ind, nodebt, EFC   750-2000" if new==0 & freshman==0 & indep==1 & Prmry_EFC>750 & Prmry_EFC<=2000 & totalloans==0
replace stratum = "Old, 2nd, ind, nodebt, EFC  2000-3000" if new==0 & freshman==0 & indep==1 & Prmry_EFC>2000 & Prmry_EFC<=3000 & totalloans==0
replace stratum = "Old, 2nd, ind, nodebt, EFC  3000-$pellEFC" if new==0 & freshman==0 & indep==1 & Prmry_EFC>3000 & Prmry_EFC<=$pellEFC & totalloans==0
replace stratum = "Old, 2nd, ind, nodebt, EFC  $pellEFC-7500" if new==0 & freshman==0 & indep==1 & Prmry_EFC>$pellEFC & Prmry_EFC<=7500 & totalloans==0
replace stratum = "Old, 2nd, ind, nodebt, EFC  7500-10K" if new==0 & freshman==0 & indep==1 & Prmry_EFC>7500 & Prmry_EFC<=10000 & totalloans==0
replace stratum = "Old, 2nd, ind, nodebt, EFC 10K-17K" if new==0 & freshman==0 & indep==1 & Prmry_EFC>10000 & Prmry_EFC<=17000 & totalloans==0
replace stratum = "Old, 2nd, ind, nodebt, EFC >17K" if new==0 & freshman==0 & indep==1 & Prmry_EFC>17000 & totalloans==0






local type : type stratum 
local type : subinstr local type "str" "" 
format stratum %-`type's


** Check whether treatment is exactly balanced

sort stratum
merge m:1 stratum using ~/Data-UIUC/samplecount
tab _merge
drop if _merge==2
gen addpackage = count_noloans>count_package
replace addpackage = -1 if count_noloans<count_package
summ count_noloans
local sum_noloans = r(sum)
summ count_package
local sum_package = r(sum)
drop _merge count_noloans count_package


** Assign treatment

* Strata that will be balanced or are already unbalanced
bysort stratum: egen count_stratum = sum(1)
gen newobs_even = 1-mod(count_stratum,2)
gen random = uniform()
bysort stratum: egen cutoff = pctile(random)
gen package = (addpackage==-1 & newobs_even==0)*(random<cutoff) + (addpackage==1 & newobs_even==0)*(random<=cutoff) + (newobs_even==1)*(random<cutoff) if (addpackage!=0 | newobs_even!=0)

* Strata that will be newly unbalanced (randomly choosing direction of imbalance, minimizing total sample imbalance)
count if package==0
local sum_noloans = `sum_noloans'+r(N)
count if package==1
local sum_package = `sum_package'+r(N)
count if package==.
local epsilon = 0 + .0001*(`sum_noloans'<`sum_package') - .0001*(`sum_noloans'>`sum_package')
bysort stratum: gen unevens = uniform() if _n==1 & package==.
bysort stratum: gen obs1 = _n==1
bysort obs1 package: egen unevenscutoff = pctile(unevens) if package==. & obs1==1
gen package_extra = (unevens<=unevenscutoff+`epsilon') if addpackage==0 & newobs_even==0 & obs1==1
bysort stratum: egen package_extra_stratum = max(package_extra)
replace package = (package_extra_stratum==0)*(random<cutoff) + (package_extra_stratum==1)*(random<=cutoff) if addpackage==0 & newobs_even==0

* Assignment file for comm college
sort ResearchID
local date = subinstr(c(current_date)," ","",.)
local time = c(current_time)
export excel ResearchID package using "Packaging_CCA_`date'_`time'.xlsx", firstrow(variables) replace


** Update sample counts

bysort stratum package: egen stratum_package_count_temp = sum(1) if package==1
bysort stratum: egen stratum_package_count = max(stratum_package_count_temp)
replace stratum_package_count = 0 if stratum_package_count==.
bysort stratum: egen stratum_count = sum(1)
gen stratum_noloans_count = stratum_count - stratum_package_count
preserve
keep if obs1==1
sort stratum
merge m:1 stratum using ~/Data-UIUC/samplecount
replace count_noloans = stratum_noloans_count if _merge==1		/* Code only added after "Lumina Export 051315" was processed, at which point samplecount.dta was replaced by collapsing fullsample_CCA.dta */
replace count_package = stratum_package_count if _merge==1		/* Code only added after "Lumina Export 051315" was processed, at which point samplecount.dta was replaced by collapsing fullsample_CCA.dta */
replace count_noloans = count_noloans + stratum_noloans_count if _merge==3
replace count_package = count_package + stratum_package_count if _merge==3
keep stratum count_noloans count_package
save ~/Data-UIUC/samplecount, replace


** Maintain data on full sample

restore
append using ~/Data-UIUC/fullsample_CCA
saveold ~/Data-UIUC/fullsample_CCA, replace

log using "~/Data-UIUC/sample_CCA_`date'_`time'.log", replace
table stratum package, left
tab package
log close

* End
