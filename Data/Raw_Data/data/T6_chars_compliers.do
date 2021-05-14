`/** Table 6: Characteristics of CCA Students by Response to Treatment **/
/** COMPLIERS ARE STUDENTS INDUCED TO BORROW (ANY LOANS) BY INSTRUMENT (PACKAGED) **/  

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

gen amount = AcceptedAmount/1000; 
lab var amount "Amount borrowed ($1k)"; 

* RECODE CUMULATIVE GPA AS MISSING IF THE STUDENT IS NEW;
replace CumulativeGPA = . if new == 1; 
recode CumulativeGPA (. = 0) if new ==0 & CumulativeEarnedHours ~=. ;

* RECODE CUMULATIVE CREDITS AS MISSING IF STUDENT IS NEW;
replace CumulativeEarnedHours = . if new == 1;  

** ALWAYS TAKERS ALWAYS BORROW REGARDLESS OF TREATMENT ASSIGNMENT;
* Pr(Always taker) = Pr(D = 1 | Z = 0) ;
qui reg borrowed if package ==0; est store pr_at;

** NEVER TAKERS NEVER BORROW REGARDLESS OF TREATMENT ASSIGNMENT; 
* Pr(Never taker) = Pr(D = 0 | Z = 1) ; 
gen noloan = 1-borrowed; 
qui reg noloan if package == 1; est stor pr_nt;

** LOOP OVER BASELINE CHARACTERISTICS; 
loc j = 0;

foreach v of var female white par_coll age Prmry_EFC CostofAttendance pell_elig indep has_outstanding new freshman CumulativeEarnedHours 	CumulativeGPA  {;

loc j = `j'+1; 

* E[X|D=1,Z=1] ;
	qui reg `v' if borrowed == 1 & package == 1;  est store e_1;

* E[X|D=1,Z=0];
	qui reg `v' if borrowed ==1 & package ==0; est store e_2;

* E[X];
	qui reg `v' ; est store e_3;
	qui suest pr_at pr_nt e_1 e_2 e_3, robust

	
* E[X|COMPLIER]= (1/(1-PR(AT)-PR(NT))*{(1-PR(NT)*E[X|D=1,Z=1])-(PR(AT)*E[X|D=1,Z=0]));
	nlcom (1/((1-[pr_at_mean]_cons - [pr_nt_mean]_cons) )  * (((1-[pr_nt_mean]_cons)*[e_1_mean]_cons)-([pr_at_mean]_cons*[e_2_mean]_cons)));
	loc est_comp = el(r(b),1,1); 
	loc se_comp = el(r(V),1,1); 

* TEST OF H0: E[X|COMPLIER]= E[X];
	testnl (1/((1-[pr_at_mean]_cons - [pr_nt_mean]_cons) )  * (((1-[pr_nt_mean]_cons)*[e_1_mean]_cons)-([pr_at_mean]_cons*[e_2_mean]_cons))) = ([e_3_mean]_cons);
	loc p = r(p); 
	
* E[X|AT] = E[X|D = 1, Z = 0];
	qui reg `v' if	borrowed ==1 & package ==0; est store e_at;

* E[X|NT] = E[X|D = 0, Z = 1];
	qui reg `v' if	borrowed ==0 & package ==1; est store e_nt;
	
* E[X|AT U NT] = E[X|(D = 0, Z = 1) U (D = 1, Z = 0)];
	qui reg `v' if	(borrowed ==0 & package ==1) | (borrowed ==1 & package ==0); est store e_atnt;
	
	qui suest pr_at pr_nt e_1 e_2 e_3 e_at e_nt e_atnt, robust;
	
** TEST OF H0: E[X|COMPLIER] = E[X|AT];
	testnl (1/((1-[pr_at_mean]_cons - [pr_nt_mean]_cons) )  * (((1-[pr_nt_mean]_cons)*[e_1_mean]_cons)-([pr_at_mean]_cons*[e_2_mean]_cons))) = ([e_at_mean]_cons);
	loc p_c_at = r(p); 

** TEST OF H0: E[X|COMPLIER] = E[X|NT];
	testnl (1/((1-[pr_at_mean]_cons - [pr_nt_mean]_cons) )  * (((1-[pr_nt_mean]_cons)*[e_1_mean]_cons)-([pr_at_mean]_cons*[e_2_mean]_cons))) = ([e_nt_mean]_cons);
	loc p_c_nt = r(p); 

** TEST OF H0: E[X|COMPLIER] = E[X|AT U NT];
	testnl (1/((1-[pr_at_mean]_cons - [pr_nt_mean]_cons) )  * (((1-[pr_nt_mean]_cons)*[e_1_mean]_cons)-([pr_at_mean]_cons*[e_2_mean]_cons))) = ([e_atnt_mean]_cons);
	loc p_c_atnt = r(p); 
	
** TEST OF H0: E[X|AT] = E[X|AT];
	test  ([e_nt_mean]_cons) = ([e_at_mean]_cons);
	loc p_at_nt = r(p); 
	
** E[X|AT], E[X|NT];
	lincom 	[e_nt_mean]_cons-0; loc mean_nt = r(estimate); 
	lincom 	[e_at_mean]_cons-0; loc mean_at = r(estimate); 
	
* CONSTANT THAT IS OUTREGGED IS E[X];	
	qui reg `v', robust; 
	
	loc ap = "ap"; if `j' ==1 {; loc ap = "replace"; };
	outreg2 using "complier_chars.txt", `ap' dec(3) se noaster 	addstat("AT mean",`mean_at', "comp mean",`est_comp', "NT mean", `mean_nt', 
		"pval C = full sample",`p',"pval C = AT",`p_c_at', "pval C = NT",`p_c_nt', "pval AT = NT",`p_at_nt',"pval C = AT U NT", `p_c_atnt');
	
};
