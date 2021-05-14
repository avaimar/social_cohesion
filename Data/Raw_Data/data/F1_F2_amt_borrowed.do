/** Figure 1: Distribution of (Recentered) Amount Borrowed **/
/** Figure 2: Distribution of Amount Borrowed by Level **/

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

*** F1;
gen damount = AcceptedAmount-3500 if freshman ==1;
gen damount = AcceptedAmount-4500 if freshman ==0;

** PANEL A: TREATMENT AND CONTROL GROUP BORROWERS;
tw (histogram damount if package == 1, w(500) bc(midblue) fi(80) start(-4500) freq) 
	(histogram damount if package == 0, w(500) barw(250) bc(dknavy) start(-4500) freq) 
	if AcceptedAmount>0 & damount>=-4500 & damount <=6000,
	graphr(fc(white)) legend(lab(1 Treatment) lab(2 Control) size(small)) ti("A. Treatment and Control Group Borrowers")
	xlab(-4000(1000)6000, labs(small)) xti("(Recentered) Amount Borrowed") 
	ylab(0(100)400, gmin gmax labs(small)) yti(Number of Students); 

graph export "${output}\F1A.pdf", as(pdf) replace; 

** PANEL B: EFFECT OF NONZERO OFFERS ON THE PROBABILITY OF BORROWING SPECIFIC AMOUNTS;
* GENERATE RANGES OF AMOUNTS BORROWED;
gen p_3500 = damount > -3500 & damount < -3000;
gen p_3000 = damount >= -3000 & damount < -2500; 
gen p_2500 = damount >= -2500 & damount < -2000;
gen p_2000 = damount >= -2000 & damount < -1500;
gen p_1500 = damount >= -1500 & damount < -1000;
gen p_1000 = damount >= -1000 & damount < -500;
gen p_500 = damount >= -500 & damount < 0;
gen p = damount ==0; 
gen p500 = damount > 0 & damount <= 500;
gen p1000 = damount > 500  & damount <= 1000;
gen p1500 = damount > 1000 & damount <= 1500;
gen p2000 = damount > 1500 & damount <= 2000;

* LOOP OVER DEP VARS TO GENERATE POINT ESTIMATES;
loc j = 0;

foreach v of var p_3500 - p2000 {;
	
loc j = `j'+1;

	xi: xtivreg2 `v' (offered = package) i.month_packaged Prmry_EFC CumulativeGPA  CumulativeEarnedHours  pell_elig indep has_outstanding, 
		partial(i.month_packaged  Prmry_EFC CumulativeGPA CumulativeEarnedHours pell_elig indep has_outstanding)  cluster(stratum_code) fe i(stratum_code);

	sum `v' if package==0 & e(sample) == 1;
	local mean=r(mean);

	loc ap = "ap"; 
	if `j' == 1 {; loc ap = "replace"; };	
 	outreg2 using "${output}/F1B_output", `ap' keep(offered) addstat("Mean control", `mean') dec(3) noaster nocons nor noparen nonot noni noobs st(coef ci_low ci_high);

* CLOSE LOOP OVER DEP VARS;
};

* INPUT POINT ESTS, CIS, AND CONTROL MEANS;
preserve;

	insheet using "${output}\F1B_output.txt", names clear;
	drop if _n == 2 | _n == 6;
	keep in 2/5; 
	gen type = _n; 
	reshape long v, i(type) j(ob);
	reshape wide v, i(ob) j(type);
	drop if _n == 1; 
	foreach v of var v* {; destring `v' , replace; };
	for any v1 v2 v3 v4 \ any coeff ci_l ci_h cont_mean : rename X Y; 
	foreach v of var coeff ci_l ci_h {; replace `v' = `v'+cont_mean; };
	
	lab define cat 2 "(P-3500,P-3000)" 3 "[P-3000,P-2500)" 4 "[P-2500,P-2000)" 5 "[P-2000,P-1500)" 6 "[P-1500,P-1000)" 7 "[P-1000,P-500)"
		8 "[P-500,P)" 9 "P" 10 "(P,P+500]" 11 "(P+500,P+1000]" 12 "(P+1000,P+1500]" 13 "(P+1500,P+2000]";
	lab val ob cat; 
	
	
	tw (bar cont_mean ob , barw(.75)) (sc coeff ob, mc(black)) (rcap ci_l ci_h ob , lc(black)) 
		, graphr(fc(white)) legend(lab(1 Control mean) lab(2 "Control mean + treatment effect") lab(3 "95% CI") size(small) col(3)) 
		ti("B. Effect of Nonzero Offers on the Probability of Borrowing Specific Amounts")
		xlab(2(1)13, labs(small) val angle(45)) xti("(Recentered) Amount Borrowed") 
		ylab(0(.01).05, labs(small) gmin gmax) yti(Share borrowing within specified range);
		
	graph export using  "${output}\F1B.pdf", as(pdf) replace;
	
restore;

*** F2;
** PANEL A: FRESHMEN;
tw (histogram AcceptedAmount if package == 1, w(500) bc(midblue) fi(80) start(0) freq) 
	(histogram AcceptedAmount if package == 0, w(500) barw(250) bc(dknavy) start(0) freq) 
	if freshman == 1 & AcceptedAmount>0 & AcceptedAmount <=9500,
	graphr(fc(white)) legend(lab(1 Treatment) lab(2 Control) size(small)) ti("A. Freshmen") xli(1750 3500 )
	xlab(0(1000)9000, labs(small)) xti(" Amount Borrowed") 
	ylab(0(50)200, gmin gmax labs(small)) yti(Number of Students); 

graph export "${output}\F2A.pdf", as(pdf) replace; 

** PANEL B: SOPHOMORES;
tw (histogram AcceptedAmount if package == 1, w(500) bc(midblue) fi(80) start(0) freq) 
	(histogram AcceptedAmount if package == 0, w(500) barw(250) bc(dknavy) start(0) freq) 
	if freshman == 0 & AcceptedAmount>0 & AcceptedAmount <=10500,
	graphr(fc(white)) legend(lab(1 Treatment) lab(2 Control) size(small)) ti("B. Sophomores") xli(2250 4500 )
	xlab(0(1000)9000, labs(small)) xti(" Amount Borrowed") 
	ylab(0(50)200, gmin gmax labs(small)) yti(Number of Students); 

graph export "${output}\F2B.pdf", as(pdf) replace; 

 