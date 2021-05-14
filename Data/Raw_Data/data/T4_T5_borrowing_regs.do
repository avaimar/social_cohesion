/** Table 4: The Impact of Nonzero Loan Offers on Borrowing: 2015-16 Academic Year **/
/** Table 5: Heterogeneity in the Impact of Loan Offers on Borrowing **/

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

*** T4;
** "FIRST STAGE" REGS;
xi: areg offered package i.month_packaged Prmry_EFC CumulativeGPA  CumulativeEarnedHours  pell_elig indep has_outstanding, cl(stratum_code) a(stratum_code);
local mean= 0;
outreg2 using "${output}/colA_T4", replace excel keep(package) addstat("Mean control", `mean') ctitle("Offered any loan") dec(3) aster(se) sym(**,*,+) nocons nor label;

** IV REGS;
foreach v of var borrowed AcceptedAmount  {;

loc dec = 3; 
if "`v'" == "AcceptedAmount" {; loc dec = 0; };

	xi: xtivreg2 `v' (offered = package) i.month_packaged Prmry_EFC CumulativeGPA  CumulativeEarnedHours  pell_elig indep has_outstanding, 
		partial(i.month_packaged  Prmry_EFC CumulativeGPA CumulativeEarnedHours pell_elig indep has_outstanding)  cluster(stratum_code) fe i(stratum_code);
	sum `v' if package==0 & e(sample) == 1;
	local mean=r(mean);
	outreg2 using "${output}/colA_T4", excel keep(offered) addstat("Mean control", `mean') ctitle("`title_`v''") dec(`dec') aster(se) sym(**,*,+) nocons nor label;

* CLOSE LOOP OVER DEP VARS;
};


*** T5;
** ESTIMATE SEPARATE EQUATIONS FOR EACH SUBGROUP, JOINTLY ESTIMATE ALL 8 VIA SUR, CALCULATE INDEX FOR EACH SUBGROUP;
** SUBGROUPS ARE ({0,1}) OUTSTANDING DEBT, PELL ELIGIBLE, NEW, INDEPENDENT, FRESHMAN;

** SOME DATA CLEANING;
* RENAME SUBGROUP VARS TO HAVE FEWER CHARS;
for any has_outstanding pell_elig new indep freshman 
	\ any ho pe n i f 
	: rename X Y; 

** FS;
foreach c of var ho pe n i f  {;

forv t = 0/1 {;

	xi: qui reg offered package i.month_packaged Prmry_EFC CumulativeGPA  CumulativeEarnedHours pe i ho i.stratum_code if `c' == `t'; 
	est store fs_`c'`t';

};

};

** RF/ITT ESTIMATES;
foreach v of var borrowed AcceptedAmount {;

foreach c of var ho pe n i f  {;

forv t = 0/1 {;

	xi: qui reg `v' package i.month_packaged Prmry_EFC CumulativeGPA  CumulativeEarnedHours pe i ho i.stratum_code if `c' == `t'; 
	est store rf`v'_`c'`t';


* CLOSE LOOP OVER HAS CHAR;
};

* CLOSE LOOP OVER CHAR;
};

* SUR: SINGLE OUTCOME, ACROSS SUBGROUPS;
	suest rf`v'_ho0 rf`v'_ho1 rf`v'_pe0 rf`v'_pe1 rf`v'_n0 rf`v'_n1 rf`v'_i0 rf`v'_i1 rf`v'_f0 rf`v'_f1, cl(stratum_code);   

* JOINT SIGNIFICANCE OF PACKAGE COEFFS;
	test package;

* CLOSE LOOP OVER DEPVAR;
};

** IV ESTIMATES
* SUR - EACH OUTCOME SEPARATELY;
foreach v of var borrowed AcceptedAmount AcceptedAmount_cond took_default2  {;

	suest fs_ho0 fs_ho1 fs_pe0 fs_pe1 fs_n0 fs_n1 fs_i0 fs_i1 fs_f0 fs_f1 
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
