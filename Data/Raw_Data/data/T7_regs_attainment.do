/** Table 7: OLS and IV Estimates of the Impact of Nonzero Loan Offers on Attainment: 2015-16 Academic Year **/

#d;

set more off; 
set matsize 4000;

gl dir = " ";
gl original = "${dir}\data\original";
gl intermediate = "${dir}\data\intermediate";
gl output = "${dir}\output";
gl programs = "${dir}\programs\callable_programs";

cd "${dir}\programs";
 
use "${intermediate}/formatted_analysis_data",clear; 

* ATTAINMENT SAMPLE - ENROLLED IN F15; 
keep if enrolled_fall ==1; 

gen amount = AcceptedAmount/1000; 
lab var amount "Amount borrowed ($1k)"; 

** REGRESSION COEFFICIENTS AND CONTROL MEANS;
loc j = 0;

foreach v of var crdattm_total credits_total gpa_total anydeg  {;

loc dec = 3; 
loc j = `j'+1; 
loc ap = "ap";

if `j' == 1 {; loc ap = "replace"; }; 

** PANELS A AND B - CONTROL MEANS AND OLS/ITT/RF ESTIMATES; 
	xi: areg `v'  package i.month_packaged Prmry_EFC CumulativeGPA  CumulativeEarnedHours pell_elig indep has_outstanding, cluster(stratum_code) a(stratum_code);
	sum `v' if package==0 & e(sample) == 1;
	local mean=r(mean);
	outreg2 using "${output}/T6", `ap'  excel keep(package) addstat("Mean control", `mean') ctitle("`title_`v''") dec(`dec') aster(se) sym(**,*,+) nocons nor label;

** PANEL C - IV ESTIMATES (ENDOG = OFFERED);
	xi: xtivreg2 `v' (offered = package) i.month_packaged Prmry_EFC CumulativeGPA  CumulativeEarnedHours pell_elig indep has_outstanding, 
		partial(i.month_packaged Prmry_EFC CumulativeGPA  CumulativeEarnedHours pell_elig indep has_outstanding) cluster(stratum_code) fe i(stratum_code);
	sum `v' if package==0 & e(sample) == 1;
	local mean=r(mean);
	outreg2 using "${output}/T6", ap excel keep(offered) addstat("Mean control", `mean') ctitle("`title_`v''") dec(`dec') aster(se) sym(**,*,+) nocons nor label;

** PANEL D - IV ESTIMATES (ENDOG = BORROWED);
	xi: xtivreg2 `v' (borrowed = package) i.month_packaged Prmry_EFC CumulativeGPA  CumulativeEarnedHours pell_elig indep has_outstanding, 
		partial(i.month_packaged Prmry_EFC CumulativeGPA  CumulativeEarnedHours pell_elig indep has_outstanding)  cluster(stratum_code) fe i(stratum_code);
	sum `v' if package==0 & e(sample) == 1;
	local mean=r(mean);
	outreg2  using "${output}/T6", ap excel keep(amount) addstat("Mean control", `mean') ctitle("`title_`v''") dec(`dec') aster(se) sym(**,*,+) nocons nor label;

** PANEL E - IV ESTIMATES (ENDOG = AMOUNT BORROWED $1k);
	xi: xtivreg2 `v' (amount = package) i.month_packaged Prmry_EFC CumulativeGPA  CumulativeEarnedHours pell_elig indep has_outstanding, 
		partial(i.month_packaged Prmry_EFC CumulativeGPA  CumulativeEarnedHours pell_elig indep has_outstanding)  cluster(stratum_code) fe i(stratum_code);
	sum `v' if package==0 & e(sample) == 1;
	local mean=r(mean);
	outreg2  using "${output}/T6", ap excel keep(amount) addstat("Mean control", `mean') ctitle("`title_`v''") dec(`dec') aster(se) sym(**,*,+) nocons nor label;

* CLOSE LOOP OVER DEP VARS;
};


** LEE BOUNDS (PANELS C - E, CONTINUOUS DEP VARS;
* ESTIMATED EFFECT OF PACKAGING ON ENROLLMENT IS  -0.0059294;
* GENERATE BOUNDS BY TRIMMING THE TOP/BOTTOM  0.0059294 OF CONTROL GROUP MEMBERS (IN TERMS OF THE OUTCOME); 

loc j = 0; 

foreach v of var crdattm_total credits_total gpa_total {;

foreach endog of var offered borrowed amount {;

loc j = `j'+1; 

bysort package: cumul `v', gen(c_`v') ;

* LOWER BOUND; 
	xi: xtivreg2 `v' (`endog' = package) i.month_packaged Prmry_EFC CumulativeGPA  CumulativeEarnedHours pell_elig indep has_outstanding 
		if package ==1 | (package == 0 & c_`v' <=(1-  0.0059294 )), 
		partial(i.month_packaged Prmry_EFC pell_elig indep has_outstanding) cluster(stratum_code) fe i(stratum_code);
	loc ap = "ap"; if `j' == 1 {; loc ap = "replace"; }; 
	outreg2 using "${output}/TX_ref_regs_bounds", `ap'  excel keep(amount) ctitle("`title_`v''") dec(`dec') aster(se) sym(**,*,+) nocons nor label;

* UPPER BOUND; 
	xi: xtivreg2 `v' (`endog'  = package) i.month_packaged Prmry_EFC CumulativeGPA  CumulativeEarnedHours pell_elig indep has_outstanding 
		if package ==1 | (package == 0 & c_`v' >= 0.0059294), 
		partial(i.month_packaged Prmry_EFC pell_elig indep has_outstanding) cluster(stratum_code) fe i(stratum_code);
	outreg2 using "${output}/TX_ref_regs_bounds", ap excel keep(amount) ctitle("`title_`v''") dec(`dec') aster(se) sym(**,*,+) nocons nor label;

* CLOSE LOOP OVER VARS;
};


*** CODE TO CALCULATE FAMILYWISE PVALS FOR INDIVIDUAL ATTAINMENT OUTCOMES STARTS HERE;
** FOLLOWS HECKMAN ET AL. 2010 QE APPENDIX D.3.1;
* K = 4 (NUMBER OF OUTCOMES);
* N = 1000 (NUMBER OF BOOTSTRAP SAMPLE DRAWS) ;
loc N = 1000; 
  
* RENAME OUTCOME VARS AS var1 - var4, ORDERED BY SIGNIFICANCE OF TEST OF NULL ;
for any gpa_total credits_total crdattm_total anydeg
	\ any var1 var2 var3 var4 
	: rename X Y;	

* STORE PVALS FROM TEST OF NULL;
forv k = 1/4 {;

	xi: qui reg var`k' package i.month_packaged Prmry_EFC CumulativeGPA  CumulativeEarnedHours pell_elig indep has_outstanding i.stratum_code; 
	est store est`k';	
		
	outreg2 package using reps`k', st(pval) addstat("rep",0) replace noaster noparen dta dec(5) ti("") ctitle(" ") nonot nocon nor noobs noni;
	
};

suest est1 est2 est3 est4, cl(stratum_code);

* TEST FULL SET OF NULL HYPOTHESES;
test package; 
loc t1 = r(chi2); /* p = 0.021 */

test [est2_mean]package = [est3_mean]package = [est4_mean]package = 0;
loc t2 = r(chi2); /* p = 0.139 */

test [est3_mean]package = [est4_mean]package = 0;
loc t3 = r(chi2); /* p = 0.160 */

test [est4_mean]package = 0;
loc t4 = r(chi2); /* p = 0.613*/

* BOOTSTRAP TO GENERATE DISTRIBUTION OF TEST STAT (CHI2) UNDER NULL;
forv i = 1/`N' {;

preserve;

* DRAW N SAMPLES WITH REPLACEMENT;
	bsample, strata(stratum_code);

* DRAW BINARY TREATMENT ASSIGMENTS FROM EMPIRICAL DISTRIBUTION OF ORIGINAL TREATMENT ASSIGNMENT (P=0.5|STRATUM) WITHOUT REPLACEMENT;
	drop package;
	bysort stratum: gen pr= runiform(0,1);
	gen package =0;

foreach v of num 1	2	3	4	5	6	7	8	9	10	11	12	13	16	17	19	31	34	36	37	39	41	42	44	46	47	48	49	51	52	56	60	61	63	65	69	70	71	72	73	74	76	78	79	81	83 {;
	 
	replace package = 1 if pr <= `prpack`v'' & stratum == `v'; 
};

forv k = 1/4 {;

	xi: qui reg var`k' package i.month_packaged Prmry_EFC CumulativeGPA  CumulativeEarnedHours pell_elig indep has_outstanding i.stratum_code;
	est store est`k';			 
		
* CLOSE LOOP OVER OUTCOMES;
};

	suest est1 est2 est3 est4, cl(stratum_code); 

* TEST FULL SET OF NULL HYPOTHESES;
	test package; 
	loc t1_`i' = r(chi2); 
	
	test [est2_mean]package = [est3_mean]package = [est4_mean]package = 0;
	loc t2_`i' = r(chi2); 
	
	test [est3_mean]package = [est4_mean]package = 0;
	loc t3_`i' = r(chi2); 
	
	test [est4_mean]package = 0;
	loc t4_`i' = r(chi2); 


restore;

* CLOSE LOOP OVER BOOTSTRAP REPS;
};

* LOAD EACH OUTPUT FILE CORRESPONDING TO EACH HYPOTHESIS;
clear;

loc num = `N'+1; 
set obs `num'; 
gen rep = _n-1; 

forv k = 1/4 {;

	gen t`k' = `t`k'' if rep ==0; 
	
forv i = 1/`N' {;
	
	di "`k'" "`i'"; 
	di `t`k'_`i''; 
	replace t`k' = `t`k'_`i'' if rep == `i'; 

};
};


* STEP 1: FOR EACH DRAW, COMPUTE SUCCESSIVE MINIMA;
egen q_1 = rowmin(p1 p2 p3 p4);
egen q_2 = rowmin(p2 p3 p4);
egen q_3 = rowmin(p3 p4);
gen q_4 = p4; 

* STEP 2: FOR EACH HYPOTHESIS, COMPUTE pbar_k => % OF TIMES THE ADJUSTED DRAWS ARE =< p_k;
forv k = 1/4 {;

	sum p`k' if _n == 1; loc p_`k' = r(mean); 
	gen dum`k' = (q_`k' <= `p_`k''); 
	
};

collapse (mean) pbar_1 = dum1 pbar_2 = dum2 pbar_3 = dum3 pbar_4 = dum4;

* STEP 3: FOR EACH HYPOTHESIS, ENFORCE SUCCESSIVE MAXIMA;
gen ptilda_1 = pbar_1;
egen ptilda_2 = rowmax(pbar_1 pbar_2); 
egen ptilda_3 = rowmax(pbar_1 pbar_2 pbar_3); 
egen ptilda_4 = rowmax(pbar_1 pbar_2 pbar_3 pbar_4); 

li;
