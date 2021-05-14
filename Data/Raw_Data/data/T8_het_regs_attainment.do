/** Table 8: Heterogeneity in the Impact of Amount Borrowed on Attainment: 2015-16 Academic Year **/

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

** RESTRICT TO ATTAINMENT SAMPLE (ENROLLED F15); 
keep if enrolled_fall == 1; 

gen amount = AcceptedAmount/1000; 
lab var amount "Amount borrowed ($1k)"; 

*** T8;
** ESTIMATE SEPARATE EQUATIONS FOR EACH SUBGROUP, JOINTLY ESTIMATE ALL 8 VIA SUR, CALCULATE INDEX FOR EACH SUBGROUP;
** SUBGROUPS ARE ({0,1}) OUTSTANDING DEBT, PELL ELIGIBLE, NEW, INDEPENDENT, FRESHMAN;

** SOME DATA CLEANING;
* RENAME SUBGROUP VARS TO HAVE FEWER CHARS;
for any has_outstanding pell_elig new indep freshman 
	\ any ho pe n i f 
	: rename X Y; 
	
** FS - AMOUNT BORROWED;
foreach c of var ho pe n i f  {;

forv t = 0/1 {;

	xi: qui reg amount package i.month_packaged Prmry_EFC CumulativeGPA  CumulativeEarnedHours pe i ho i.stratum_code if `c' == `t'; 
	est store fs_`c'`t';

};

};

** CALCULATE AND STORE STDEV OF EACH DEP VAR FOR CONTROL GROUP FOR CALCULATION OF STANDARDIZED TREATMENT EFFECT;
foreach c of var ho pe n i f  {;

forv t = 0/1 {;

foreach v of var crdattm_total credits_total gpa_total anydeg {;

	sum `v' if package == 0 & `c' == `t'; 
	loc sd_`v'_`c'`t' = r(sd); 

* CLOSE LOOP OVER DEPVARS;	
};

* CLOSE LOOP OVER GROUP VALS;
};

* CLOSE LOOP OVER GROUPS;
};

** RF/ITT ESTIMATES;
foreach v of var crdattm_total credits_total gpa_total anydeg {;

foreach c of var ho pe n i f  {;

forv t = 0/1 {;

	xi: qui reg `v' package i.month_packaged Prmry_EFC CumulativeGPA  CumulativeEarnedHours pe i ho i.stratum_code if `c' == `t'; 
	est store rf`v'_`c'`t';

* CLOSE LOOP OVER HAS CHAR;
};

* CLOSE LOOP OVER CHAR;
};

** SUR: SINGLE OUTCOME, ACROSS SUBGROUPS;
	qui suest rf`v'_ho0 rf`v'_ho1 rf`v'_pe0 rf`v'_pe1 rf`v'_n0 rf`v'_n1 rf`v'_i0 rf`v'_i1 rf`v'_f0 rf`v'_f1, cl(stratum_code);   

* JOINT SIGNIFICANCE OF PACKAGE COEFFS;
	test package;

* CLOSE LOOP OVER DEPVAR;
};

** IV ESTIMATES
* SUR - EACH OUTCOME SEPARATELY;
foreach v of var crdattm_total credits_total gpa_total anydeg {;

	qui suest fs_ho0 fs_ho1 fs_pe0 fs_pe1 fs_n0 fs_n1 fs_i0 fs_i1 fs_f0 fs_f1 
	rf`v'_ho0 rf`v'_ho1 rf`v'_pe0 rf`v'_pe1 rf`v'_n0 rf`v'_n1 rf`v'_i0 rf`v'_i1 rf`v'_f0 rf`v'_f1, cl(stratum_code);   

foreach t in ho pe n i f {;
	
	nlcom [rf`v'_`t'0_mean]package/[fs_`t'0_mean]package;
	nlcom [rf`v'_`t'1_mean]package/[fs_`t'1_mean]package;
	testnl ([rf`v'_`t'0_mean]package/[fs_`t'0_mean]package) = ([rf`v'_`t'1_mean]package/[fs_`t'1_mean]package);
	
* CLOSE LOOP OVER CHARS;
};

* TEST EQUALITY OF ALL IV ESTIMATES;

	testnl ([rf`v'_ho0_mean]package/[fs_ho0_mean]package) = ([rf`v'_ho1_mean]package/[fs_ho1_mean]package) 
	= ([rf`v'_pe0_mean]package/[fs_pe0_mean]package) = ([rf`v'_pe1_mean]package/[fs_pe1_mean]package)
	= ([rf`v'_n0_mean]package/[fs_n0_mean]package) = ([rf`v'_n1_mean]package/[fs_n1_mean]package) 
	= ([rf`v'_i0_mean]package/[fs_i0_mean]package) = ([rf`v'_i1_mean]package/[fs_i1_mean]package) 
	= ([rf`v'_pe0_mean]package/[fs_f0_mean]package) = ([rf`v'_f1_mean]package/[fs_f1_mean]package);

* TEST JOINT SIGNIFICANCE OF ALL IV ESTIMATES;
	testnl ([rf`v'_ho0_mean]package/[fs_ho0_mean]package) = ([rf`v'_ho1_mean]package/[fs_ho1_mean]package) 
	= ([rf`v'_pe0_mean]package/[fs_pe0_mean]package) = ([rf`v'_pe1_mean]package/[fs_pe1_mean]package)
	= ([rf`v'_n0_mean]package/[fs_n0_mean]package) = ([rf`v'_n1_mean]package/[fs_n1_mean]package) 
	= ([rf`v'_i0_mean]package/[fs_i0_mean]package) = ([rf`v'_i1_mean]package/[fs_i1_mean]package) 
	= ([rf`v'_pe0_mean]package/[fs_f0_mean]package) = ([rf`v'_f1_mean]package/[fs_f1_mean]package) = 0; 

* CLOSE LOOP OVER DEPVARS;
};

** STANDARDIZED TREATMENT INDEX;
* SUR - ALL OUTCOMES ALL SUBGROUPS;

suest 	fs_ho0 fs_ho1 fs_pe0 fs_pe1 fs_n0 fs_n1 fs_i0 fs_i1 fs_f0 fs_f1 
		rfcrdattm_total_ho0 rfcrdattm_total_ho1 rfcrdattm_total_pe0 rfcrdattm_total_pe1 rfcrdattm_total_n0 rfcrdattm_total_n1 rfcrdattm_total_i0 rfcrdattm_total_i1 rfcrdattm_total_f0 rfcrdattm_total_f1
		rfcredits_total_ho0 rfcredits_total_ho1 rfcredits_total_pe0 rfcredits_total_pe1 rfcredits_total_n0 rfcredits_total_n1 rfcredits_total_i0 rfcredits_total_i1 rfcredits_total_f0 rfcredits_total_f1
		rfgpa_total_ho0 rfgpa_total_ho1 rfgpa_total_pe0 rfgpa_total_pe1 rfgpa_total_n0 rfgpa_total_n1 rfgpa_total_i0 rfgpa_total_i1 rfgpa_total_f0 rfgpa_total_f1
		rfanydeg_ho0 rfanydeg_ho1 rfanydeg_pe0 rfanydeg_pe1 rfanydeg_n0 rfanydeg_n1 rfanydeg_i0 rfanydeg_i1 rfanydeg_f0 rfanydeg_f1, cl(stratum_code);   

		
foreach c of var ho pe n i f  {;

forv t = 0/1 {;

	nlcom (1/4)*((([rfcrdattm_total_`c'`t'_mean]package/[fs_`c'`t'_mean]package)/`sd_crdattm_total_`c'`t' ')
		+(([rfcredits_total_`c'`t'_mean]package/[fs_`c'`t'_mean]package)/`sd_credits_total_`c'`t'')
		+(([rfgpa_total_`c'`t'_mean]package/[fs_`c'`t'_mean]package)/`sd_gpa_total_`c'`t'')
		+(([rfanydeg_`c'`t'_mean]package/[fs_`c'`t'_mean]package)/`sd_anydeg_`c'`t''));

* CLOSE LOOP OVER GROUP VALS;
};

testnl ((1/4)*((([rfcrdattm_total_`c'0_mean]package/[fs_`c'0_mean]package)/`sd_crdattm_total_`c'0 ')
		+(([rfcredits_total_`c'0_mean]package/[fs_`c'0_mean]package)/`sd_credits_total_`c'0')
		+(([rfgpa_total_`c'0_mean]package/[fs_`c'0_mean]package)/`sd_gpa_total_`c'0')
		+(([rfanydeg_`c'0_mean]package/[fs_`c'0_mean]package)/`sd_anydeg_`c'0')))
		=
		((1/4)*((([rfcrdattm_total_`c'1_mean]package/[fs_`c'1_mean]package)/`sd_crdattm_total_`c'1 ')
		+(([rfcredits_total_`c'1_mean]package/[fs_`c'1_mean]package)/`sd_credits_total_`c'1')
		+(([rfgpa_total_`c'1_mean]package/[fs_`c'1_mean]package)/`sd_gpa_total_`c'1')
		+(([rfanydeg_`c'1_mean]package/[fs_`c'1_mean]package)/`sd_anydeg_`c'1')))
	;

* CLOSE LOOP OVER GROUPS;
};


* TEST OF EQUALITY ACROSS STANDARDIZED TE;
testnl ((1/4)*((([rfcrdattm_total_ho0_mean]package/[fs_ho0_mean]package)/`sd_crdattm_total_ho0 ')
		+(([rfcredits_total_ho0_mean]package/[fs_ho0_mean]package)/`sd_credits_total_ho0')
		+(([rfgpa_total_ho0_mean]package/[fs_ho0_mean]package)/`sd_gpa_total_ho0')
		+(([rfanydeg_ho0_mean]package/[fs_ho0_mean]package)/`sd_anydeg_ho0')))
		=
		((1/4)*((([rfcrdattm_total_ho1_mean]package/[fs_ho1_mean]package)/`sd_crdattm_total_ho1 ')
		+(([rfcredits_total_ho1_mean]package/[fs_ho1_mean]package)/`sd_credits_total_ho1')
		+(([rfgpa_total_ho1_mean]package/[fs_ho1_mean]package)/`sd_gpa_total_ho1')
		+(([rfanydeg_ho1_mean]package/[fs_ho1_mean]package)/`sd_anydeg_ho1')))
		=
		((1/4)*((([rfcrdattm_total_pe0_mean]package/[fs_pe0_mean]package)/`sd_crdattm_total_pe0 ')
		+(([rfcredits_total_pe0_mean]package/[fs_pe0_mean]package)/`sd_credits_total_pe0')
		+(([rfgpa_total_pe0_mean]package/[fs_pe0_mean]package)/`sd_gpa_total_pe0')
		+(([rfanydeg_pe0_mean]package/[fs_pe0_mean]package)/`sd_anydeg_pe0')))
		=
		((1/4)*((([rfcrdattm_total_pe1_mean]package/[fs_pe1_mean]package)/`sd_crdattm_total_pe1 ')
		+(([rfcredits_total_pe1_mean]package/[fs_pe1_mean]package)/`sd_credits_total_pe1')
		+(([rfgpa_total_pe1_mean]package/[fs_pe1_mean]package)/`sd_gpa_total_pe1')
		+(([rfanydeg_pe1_mean]package/[fs_pe1_mean]package)/`sd_anydeg_pe1')))
		=
		((1/4)*((([rfcrdattm_total_n0_mean]package/[fs_n0_mean]package)/`sd_crdattm_total_n0 ')
		+(([rfcredits_total_n0_mean]package/[fs_n0_mean]package)/`sd_credits_total_n0')
		+(([rfgpa_total_n0_mean]package/[fs_n0_mean]package)/`sd_gpa_total_n0')
		+(([rfanydeg_n0_mean]package/[fs_n0_mean]package)/`sd_anydeg_n0')))
		=
		((1/4)*((([rfcrdattm_total_n1_mean]package/[fs_n1_mean]package)/`sd_crdattm_total_n1 ')
		+(([rfcredits_total_n1_mean]package/[fs_n1_mean]package)/`sd_credits_total_n1')
		+(([rfgpa_total_n1_mean]package/[fs_n1_mean]package)/`sd_gpa_total_n1')
		+(([rfanydeg_n1_mean]package/[fs_n1_mean]package)/`sd_anydeg_n1')))
		=
		((1/4)*((([rfcrdattm_total_i0_mean]package/[fs_i0_mean]package)/`sd_crdattm_total_i0 ')
		+(([rfcredits_total_i0_mean]package/[fs_i0_mean]package)/`sd_credits_total_i0')
		+(([rfgpa_total_i0_mean]package/[fs_i0_mean]package)/`sd_gpa_total_i0')
		+(([rfanydeg_i0_mean]package/[fs_i0_mean]package)/`sd_anydeg_i0')))
		=
		((1/4)*((([rfcrdattm_total_i1_mean]package/[fs_i1_mean]package)/`sd_crdattm_total_i1 ')
		+(([rfcredits_total_i1_mean]package/[fs_i1_mean]package)/`sd_credits_total_i1')
		+(([rfgpa_total_i1_mean]package/[fs_i1_mean]package)/`sd_gpa_total_i1')
		+(([rfanydeg_i1_mean]package/[fs_i1_mean]package)/`sd_anydeg_i1')))
		=
		((1/4)*((([rfcrdattm_total_f0_mean]package/[fs_f0_mean]package)/`sd_crdattm_total_f0 ')
		+(([rfcredits_total_f0_mean]package/[fs_f0_mean]package)/`sd_credits_total_f0')
		+(([rfgpa_total_f0_mean]package/[fs_f0_mean]package)/`sd_gpa_total_f0')
		+(([rfanydeg_f0_mean]package/[fs_f0_mean]package)/`sd_anydeg_f0')))
		=
		((1/4)*((([rfcrdattm_total_f1_mean]package/[fs_f1_mean]package)/`sd_crdattm_total_f1 ')
		+(([rfcredits_total_f1_mean]package/[fs_f1_mean]package)/`sd_credits_total_f1')
		+(([rfgpa_total_f1_mean]package/[fs_f1_mean]package)/`sd_gpa_total_f1')
		+(([rfanydeg_f1_mean]package/[fs_f1_mean]package)/`sd_anydeg_f1')))
		;

* TEST OF JOINT SIGN OF STANDARDIZED TE;
testnl ((1/4)*((([rfcrdattm_total_ho0_mean]package/[fs_ho0_mean]package)/`sd_crdattm_total_ho0 ')
		+(([rfcredits_total_ho0_mean]package/[fs_ho0_mean]package)/`sd_credits_total_ho0')
		+(([rfgpa_total_ho0_mean]package/[fs_ho0_mean]package)/`sd_gpa_total_ho0')
		+(([rfanydeg_ho0_mean]package/[fs_ho0_mean]package)/`sd_anydeg_ho0')))
		=
		((1/4)*((([rfcrdattm_total_ho1_mean]package/[fs_ho1_mean]package)/`sd_crdattm_total_ho1 ')
		+(([rfcredits_total_ho1_mean]package/[fs_ho1_mean]package)/`sd_credits_total_ho1')
		+(([rfgpa_total_ho1_mean]package/[fs_ho1_mean]package)/`sd_gpa_total_ho1')
		+(([rfanydeg_ho1_mean]package/[fs_ho1_mean]package)/`sd_anydeg_ho1')))
		=
		((1/4)*((([rfcrdattm_total_pe0_mean]package/[fs_pe0_mean]package)/`sd_crdattm_total_pe0 ')
		+(([rfcredits_total_pe0_mean]package/[fs_pe0_mean]package)/`sd_credits_total_pe0')
		+(([rfgpa_total_pe0_mean]package/[fs_pe0_mean]package)/`sd_gpa_total_pe0')
		+(([rfanydeg_pe0_mean]package/[fs_pe0_mean]package)/`sd_anydeg_pe0')))
		=
		((1/4)*((([rfcrdattm_total_pe1_mean]package/[fs_pe1_mean]package)/`sd_crdattm_total_pe1 ')
		+(([rfcredits_total_pe1_mean]package/[fs_pe1_mean]package)/`sd_credits_total_pe1')
		+(([rfgpa_total_pe1_mean]package/[fs_pe1_mean]package)/`sd_gpa_total_pe1')
		+(([rfanydeg_pe1_mean]package/[fs_pe1_mean]package)/`sd_anydeg_pe1')))
		=
		((1/4)*((([rfcrdattm_total_n0_mean]package/[fs_n0_mean]package)/`sd_crdattm_total_n0 ')
		+(([rfcredits_total_n0_mean]package/[fs_n0_mean]package)/`sd_credits_total_n0')
		+(([rfgpa_total_n0_mean]package/[fs_n0_mean]package)/`sd_gpa_total_n0')
		+(([rfanydeg_n0_mean]package/[fs_n0_mean]package)/`sd_anydeg_n0')))
		=
		((1/4)*((([rfcrdattm_total_n1_mean]package/[fs_n1_mean]package)/`sd_crdattm_total_n1 ')
		+(([rfcredits_total_n1_mean]package/[fs_n1_mean]package)/`sd_credits_total_n1')
		+(([rfgpa_total_n1_mean]package/[fs_n1_mean]package)/`sd_gpa_total_n1')
		+(([rfanydeg_n1_mean]package/[fs_n1_mean]package)/`sd_anydeg_n1')))
		=
		((1/4)*((([rfcrdattm_total_i0_mean]package/[fs_i0_mean]package)/`sd_crdattm_total_i0 ')
		+(([rfcredits_total_i0_mean]package/[fs_i0_mean]package)/`sd_credits_total_i0')
		+(([rfgpa_total_i0_mean]package/[fs_i0_mean]package)/`sd_gpa_total_i0')
		+(([rfanydeg_i0_mean]package/[fs_i0_mean]package)/`sd_anydeg_i0')))
		=
		((1/4)*((([rfcrdattm_total_i1_mean]package/[fs_i1_mean]package)/`sd_crdattm_total_i1 ')
		+(([rfcredits_total_i1_mean]package/[fs_i1_mean]package)/`sd_credits_total_i1')
		+(([rfgpa_total_i1_mean]package/[fs_i1_mean]package)/`sd_gpa_total_i1')
		+(([rfanydeg_i1_mean]package/[fs_i1_mean]package)/`sd_anydeg_i1')))
		=
		((1/4)*((([rfcrdattm_total_f0_mean]package/[fs_f0_mean]package)/`sd_crdattm_total_f0 ')
		+(([rfcredits_total_f0_mean]package/[fs_f0_mean]package)/`sd_credits_total_f0')
		+(([rfgpa_total_f0_mean]package/[fs_f0_mean]package)/`sd_gpa_total_f0')
		+(([rfanydeg_f0_mean]package/[fs_f0_mean]package)/`sd_anydeg_f0')))
		=
		((1/4)*((([rfcrdattm_total_f1_mean]package/[fs_f1_mean]package)/`sd_crdattm_total_f1 ')
		+(([rfcredits_total_f1_mean]package/[fs_f1_mean]package)/`sd_credits_total_f1')
		+(([rfgpa_total_f1_mean]package/[fs_f1_mean]package)/`sd_gpa_total_f1')
		+(([rfanydeg_f1_mean]package/[fs_f1_mean]package)/`sd_anydeg_f1')))
		=0;
		