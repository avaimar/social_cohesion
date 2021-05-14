/** Table 1: Characteristics of Community Colleges by Loan Packaging Procedures **/
#d;

clear all;
set more off; 
set scrollbufsize 200000;

gl dir = " ";
gl original = "${dir}\data\original";
gl intermediate = "${dir}\data\intermediate";
gl output = "${dir}\output";
gl programs = "${dir}\programs\callable_programs";

cd "${dir}\programs";

use "${intermediate}\T1_data_packaging_practices", clear; 

** OUTPUT FOR TABLE 1;
* NOTE: CAT = 1 IF OFFERS LOANS, CAT = 2 IF DOES NOT OFFER LOANS; 
tab cat [aw=enrollment]; 

preserve;

	collapse (mean) ba upgrntp pell_per_recip ufloanp  fedloan_per_recip (rawsum) unweighted_n = num [aw = enrollment], by(cat);
	outsheet using ${tables}\T1A.txt, replace; 

restore;

preserve;

	collapse (mean) drate [aw = cohort], by(cat);
	outsheet using ${tables}\T1B.txt, replace; 

restore;




