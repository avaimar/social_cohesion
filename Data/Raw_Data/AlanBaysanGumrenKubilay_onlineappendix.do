
*******************************************************************************************************
*******************************************************************************************************
*** Title: Building Social Cohesion in Ethnically Mixed Schools: An Intervention on Perspective Taking
*** Authors: Sule Alan, Ceren Baysan, Elif Kubilay, Mert Gumren

*** Notes: This do-file replicates the results in the Online Appendix. 
*******************************************************************************************************
*******************************************************************************************************


do "~/Dropbox/Empathy Project/Empathy Revision/Final QJE Files/Do-Files/AlanBaysanGumrenKubilay_measure_construction.do"

*****************************************
*****************************************
*SECTION E: Additional Tables and Figures
*****************************************
*****************************************

*****************************************************************
*Table E2: Balance with the Restricted Sample
*****************************************************************

preserve

*Keep students who are present both in the baseline and endline 
keep if fv1_student_absent==0 & b_student_absent==0 & studentstatus!="NEW"


*Student Level Charactheristics

global studentvars male ageinm refugee workingmother workingfather braven_sd beyes_sd bmath_sd bturk_sd bsbully_c bsbully_s bdonation_sd bdonate bcoord bempathy_pt_sd bempathy_ec_sd bethnicbias_sd bimpulsivity_sd bfriend bsupportself bstudyself bnode_in_friend bnode_in_supportself bnode_in_studyself

   
local count: word count $studentvars
mat eb=J(1,`count',0)
mat p=J(1,`count',0)
mat se=J(1,`count',0)
loc k=1
foreach i in $studentvars { 
	xi: reg `i' treatment i.bstrata, vce(cl b_schoolid) 
mat eb[1,`k']=_b[treatment]
mat p[1,`k']=(2 * ttail(e(df_r), abs(_b[treatment]/_se[treatment])))
loc k = `k'+1
}
matrix colnames eb = $studentvars

  
matrix colnames p = $studentvars

 
estpost ttest $studentvars , by(treatment)


estadd matrix eb
estadd matrix p, replace
estimates store balance


esttab balance using "$path2/tableE2.tex", cells("count(fmt(0) label(N)) mu_1(fmt(3) label(Control Mean)) mu_2(fmt(3) label(Treatment Mean)) eb(fmt(3) label(Difference (T-C)) star) se(fmt(3) label(SE))  p(fmt(3) label(p-value)) ") ///
coeflabels(male "\addlinespace[1mm] Male" ageinm "Age in Months" refugee "Refugee" workingmother "Working Mother" ///
workingfather "Working Father" braven_sd "\addlinespace[1mm] Raven Score" beyes_sd "Eyes Test Score" bmath_sd "Math Score" ///
bturk_sd "Turkish Score"  bsbully_c "\addlinespace[1mm] Proportion Bullied by Peers in Classroom"  ///
bsbully_s "Proportion Bullied by Peers in School" bdonation_sd "Fraction Donated" bdonate "Willing to Donate" bcoord "Proportion Cooperate" bempathy_pt_sd "Perspective Taking" ///
bempathy_ec_sd "Empathetic Concern" bethnicbias_sd  "Ethnic Bias" bimpulsivity_sd "Impulsivity" bfriend "Having a Friend" bsupportself "Having Emotional Support" bstudyself "Having Academic Support" bnode_in_friend "Friendship Ties (in-degree)" bnode_in_supportself "Emotional Support Ties (in-degree)" bnode_in_studyself "Academic Support Ties (in-degree)") ///
refcat(male "\textbf{Student Demographics:}" braven_sd "\addlinespace[1.5mm] \textbf{Cognitive Tests:}" ///
bsbully_c "\addlinespace[1.5mm] \textbf{Cohesion Indicators:}", nolabel) noobs  star(* 0.10 ** 0.05 *** 0.01)  replace nonumbers
eststo clear

restore

*************************************************
*Table E3: Missing Observation Balance at Endline
*************************************************

*Generate missing dummies for each outcome variable:

foreach var in fsbully_c fsbully_s tbully_f ffriend fsupportself fstudyself fs_decision_in fs_decision_out ///
fr_decision_in_perc fr_decision_out_perc fcoordin fcoordout fdonate fdonation_perc fturk_sd fmath_sd {
	
gen `var'_mis=1 if `var'==. & studentstatus!="GONE"
replace `var'_mis=0 if `var'!=.
replace `var'_mis=. if studentstatus=="GONE"

}


global misvars fsbully_c_mis fsbully_s_mis tbully_f_mis ffriend_mis fsupportself_mis fstudyself_mis fs_decision_in_mis fs_decision_out_mis fr_decision_in_perc_mis fr_decision_out_perc_mis fcoordin_mis fcoordout_mis fdonate_mis fturk_sd_mis fmath_sd_mis



local count: word count $misvars
mat eb=J(1,`count',0)
mat p=J(1,`count',0)
mat se=J(1,`count',0)
loc k=1
foreach i in $misvars { 
	xi: reg `i' treatment i.bstrata, vce(cl b_schoolid) 
mat eb[1,`k']=_b[treatment]
mat p[1,`k']=(2 * ttail(e(df_r), abs(_b[treatment]/_se[treatment])))
loc k = `k'+1
}
matrix colnames eb = $misvars

  
matrix colnames p = $misvars

 
estpost ttest $misvars, by(treatment)


estadd matrix eb
estadd matrix p, replace
estimates store balance


esttab balance using "$path2/tableE3.tex", cells("count(fmt(0) label(N)) mu_1(fmt(3) label(Control Mean)) mu_2(fmt(3) label(Treatment Mean)) eb(fmt(3) label(Difference (T-C)) star)  se(fmt(3) label(SE)) p(fmt(3) label(p-value))") coeflabels(fsbully_c_mis "Bullying In-Class" fsbully_s_mis "Bullying Out-Class" tbully_f_mis "Teacher Behavioral Grade" ffriend_mis "Friendship Ties" fsupportself_mis "Emotional Support Ties" fstudyself_mis "Academic Support Ties" fs_decision_in_mis "In-Class Trust" fs_decision_out_mis "Out-School Trust" fr_decision_in_perc_mis "In-Class Reciprocity"fr_decision_out_perc_mis "Out-School Reciprocity" fcoordin_mis "In-Class Cooperation" fcoordout_mis "Out-School Cooperation" fdonate_mis "Willing to Donate"  fturk_sd_mis "Turkish Score" fmath_sd_mis "Math Score") noobs  star(* 0.10 ** 0.05 *** 0.01) replace nonumbers
eststo clear


*****************************************************************
*Figure E3: Teacher-Reported Program Implementation Intensity
*****************************************************************

preserve
collapse ftline treatment, by(b_classid)
hist ftline if treatment==1, xtitle("Implementation Intensity") percent graphregion(color(white)) lcolor(white) bgcolor(white) addlabopts(yvarformat(%4.1f)) fcolor(navy) name(ftline, replace) 
graph export "$path1/figureE3.png", replace 
restore


******************************
******************************
*Baseline Imputation
******************************
******************************


foreach i in age bdonation_sd braven_sd beyes_sd bimpulsivity_sd bethnicbias_sd bempathy_pt_sd bempathy_ec_sd bmath_sd bturk_sd bcpayoff bnode_in_friend bnode_in_support bnode_in_study bnode_in_supportself bnode_in_studyself bstudyself_total_host bfriend_total_host bsupportself_total_host {
egen  m`i'=median(`i')
replace `i'=m`i' if `i'==. 

}


foreach var in bfriend bsupportself bstudyself bhostfriend bhostsupportself bhoststudyself bsbully_c bsbully_s bcoord{
replace `var'=2 if `var'==.
}


foreach i in bhomophily_eth_rh_friend bhomophily_eth_rh_supportself bhomophily_eth_rh_studyself {
preserve 
collapse `i', by(b_classid)
sum `i', d
local  med=r(p50)
restore
replace `i'=`med' if `i'==. 
}


*If student is absent, replace the observation with missing

foreach i in fnode_in_friend fnode_in_supportself fnode_in_studyself {
replace `i'=. if  fv1_student_absent==1
}


*Teacher Raven
bysort b_classid: egen maxbtraven=max(btraven_sd)
replace btraven_sd=maxbtraven if studentstatus=="NEW"

*Teacher Eyes
bysort b_classid: egen maxbteyes=max(bteyes_sd)
replace bteyes_sd=maxbteyes if studentstatus=="NEW"



******************************************************
******************************************************
*SECTION A: Correction for Multiple Hypothesis Testing
******************************************************
******************************************************

*****************************************************************
*Table A1: Original and Romano Wolf P-Values
*****************************************************************


*****************************************************************
*Table A1, Panel 1: Experimental Outcomes
*****************************************************************
/*
*Original p-values of model % you need to change this model if you make any changes in the main models!!!
global expvars fs_decision_in fs_decision_out fr_decision_in_perc fr_decision_out_perc fcoordin fcoordout tpayoff otpayoff cpayoff_in cpayoff_out fdonation_perc fdonate


local count: word count $expvars
mat eb1=J(1,`count',0)  //create empty matrix with 2 (as many variables as we are looping over) rows, 1 column

matrix list eb1  //look at the matrix

treatment

reg fs_decision_in treatment $controls i.bstrata, cluster(b_schoolid)
test empathy //this does an F-test, but for one variable it's equivalent to a t-test (check: -help test- there is lots this can do
matrix eb1[1, 1] = `r(p)'  //save the p-value in the matrix 
  
reg fs_decision_out treatment $controls i.bstrata, cluster(b_schoolid)
test empathy //this does an F-test, but for one variable it's equivalent to a t-test (check: -help test- there is lots this can do
matrix eb1[1, 2] = `r(p)'  //save the p-value in the matrix 

reg fr_decision_in_perc treatment $controls i.bstrata, cluster(b_schoolid)
test empathy //this does an F-test, but for one variable it's equivalent to a t-test (check: -help test- there is lots this can do
matrix eb1[1, 3] = `r(p)'  //save the p-value in the matrix 

reg fr_decision_out_perc treatment $controls i.bstrata, cluster(b_schoolid)
test empathy //this does an F-test, but for one variable it's equivalent to a t-test (check: -help test- there is lots this can do
matrix eb1[1, 4] = `r(p)'  //save the p-value in the matrix 

reg fcoordin treatment $controls i.bstrata, cluster(b_schoolid)
test empathy //this does an F-test, but for one variable it's equivalent to a t-test (check: -help test- there is lots this can do
matrix eb1[1, 5] = `r(p)'  //save the p-value in the matrix 

reg fcoordout treatment $controls i.bstrata, cluster(b_schoolid)
test empathy //this does an F-test, but for one variable it's equivalent to a t-test (check: -help test- there is lots this can do
matrix eb1[1, 6] = `r(p)'  //save the p-value in the matrix 

reg tpayoff treatment $controls i.bstrata, cluster(b_schoolid)
test empathy //this does an F-test, but for one variable it's equivalent to a t-test (check: -help test- there is lots this can do
matrix eb1[1, 7] = `r(p)'  //save the p-value in the matrix 

reg otpayoff treatment $controls i.bstrata, cluster(b_schoolid)
test empathy //this does an F-test, but for one variable it's equivalent to a t-test (check: -help test- there is lots this can do
matrix eb1[1, 8] = `r(p)'  //save the p-value in the matrix 

reg cpayoff_in treatment $controls i.bstrata, cluster(b_schoolid)
test empathy //this does an F-test, but for one variable it's equivalent to a t-test (check: -help test- there is lots this can do
matrix eb1[1, 9] = `r(p)'  //save the p-value in the matrix 

reg cpayoff_out treatment $controls i.bstrata, cluster(b_schoolid)
test empathy //this does an F-test, but for one variable it's equivalent to a t-test (check: -help test- there is lots this can do
matrix eb1[1, 10] = `r(p)'  //save the p-value in the matrix 

reg fdonation_perc treatment bdonation_sd $controls i.bstrata, cluster(b_schoolid)
test empathy //this does an F-test, but for one variable it's equivalent to a t-test (check: -help test- there is lots this can do
matrix eb1[1, 11] = `r(p)'  //save the p-value in the matrix 

reg fdonate treatment bdonation_sd $controls i.bstrata, cluster(b_schoolid)
test empathy //this does an F-test, but for one variable it's equivalent to a t-test (check: -help test- there is lots this can do
matrix eb1[1, 12] = `r(p)'  //save the p-value in the matrix 
matrix list eb1  //look at your p-values 



rwolf $expvars, ///
indepvar(treatment) controls($controls i.bstrata) reps(500) cluster(b_schoolid) seed(2021)

local count: word count $expvars
mat eb2=J(1,`count',0)


loc n = 1 //count the iterations


foreach variableName in $expvars {

matrix eb2[1, `n'] =  e(rw_`variableName' ) 

loc n = `n' + 1  //each iteration, adjust the count
}



matrix list eb1  //look at your p-values 
matrix list eb2

matrix colnames eb1 = $expvars
matrix colnames eb2 = $expvars

estpost ttest $expvars, by(treatment)
estadd matrix eb1, replace
estadd matrix eb2, replace

estimates store pvalues

esttab pvalues using "$path2/tableA1a.tex", cells("eb1(fmt(3) label(Original)) eb2(fmt(3) label(Romano Wolf))") coeflabels(fs_decision_in "In-Class Trust" fs_decision_out "Out-School Trust" fr_decision_in_perc "In-Class Reciprocity" ///
fr_decision_out_perc "Out-School Reciprocity" fcoordin "In-Class Cooperation" fcoordout "Out-School Cooperation"  tpayoff "In-Class Trust Payoff" otpayoff "Out-School Trust Payoff" cpayoff_in "In-Class Cooperation Payoff" ///
cpayoff_out "Out-School Cooperation Payoff" fdonate "Willing to Donate" fdonation_perc "Fraction Donated") noobs replace nonumbers nogaps

eststo clear

*****************************************************************
*Table A1, Panel 2:Network Outcomes
*****************************************************************


clear matrix

global networkvars ffriend fsupportself fstudyself  fnode_in_friend fnode_in_supportself fnode_in_studyself

local count: word count $networkvars
mat eb1=J(1,`count',0)  //create empty matrix with 2 (as many variables as we are looping over) rows, 1 column

matrix list eb1  //look at the matrix



reg ffriend treatment $controls bfriend i.bstrata, cluster(b_schoolid)
test empathy //this does an F-test, but for one variable it's equivalent to a t-test (check: -help test- there is lots this can do
matrix eb1[1, 1] = `r(p)'  //save the p-value in the matrix 

reg fsupportself treatment $controls bsupportself i.bstrata, cluster(b_schoolid)
test empathy //this does an F-test, but for one variable it's equivalent to a t-test (check: -help test- there is lots this can do
matrix eb1[1, 2] = `r(p)'  //save the p-value in the matrix 
matrix list eb1  //look at your p-values 

reg fstudyself treatment $controls bstudyself i.bstrata, cluster(b_schoolid)
test empathy //this does an F-test, but for one variable it's equivalent to a t-test (check: -help test- there is lots this can do
matrix eb1[1, 3] = `r(p)'  //save the p-value in the matrix 
matrix list eb1  //look at your p-values 

reg fnode_in_friend treatment $controls bnode_in_friend i.bstrata, cluster(b_schoolid)
test empathy //this does an F-test, but for one variable it's equivalent to a t-test (check: -help test- there is lots this can do
matrix eb1[1, 4] = `r(p)'  //save the p-value in the matrix 
matrix list eb1  //look at your p-values 

reg fnode_in_supportself treatment $controls bnode_in_supportself i.bstrata, cluster(b_schoolid)
test empathy //this does an F-test, but for one variable it's equivalent to a t-test (check: -help test- there is lots this can do
matrix eb1[1, 5] = `r(p)'  //save the p-value in the matrix 
matrix list eb1  //look at your p-values 

reg fnode_in_studyself treatment $controls bnode_in_studyself i.bstrata, cluster(b_schoolid)
test empathy //this does an F-test, but for one variable it's equivalent to a t-test (check: -help test- there is lots this can do
matrix eb1[1, 6] = `r(p)'  //save the p-value in the matrix 
matrix list eb1  //look at your p-values 


rwolf $networkvars, ///
indepvar(treatment) controls($controls i.bstrata) reps(500) cluster(b_schoolid) seed(2021)

local count: word count $networkvars
mat eb2=J(1,`count',0)

loc n = 1 //count the iterations


foreach variableName in $networkvars {

matrix eb2[1, `n'] =  e(rw_`variableName' ) 

loc n = `n' + 1  //each iteration, adjust the count
}


matrix list eb1  //look at your p-values 
matrix list eb2

matrix colnames eb1 = $networkvars
matrix colnames eb2 = $networkvars

estpost ttest $networkvars, by(treatment)
estadd matrix eb1, replace
estadd matrix eb2, replace

estimates store pvalues

esttab pvalues using "$path2/tableA1b.tex", cells("eb1(fmt(3) label(Original)) eb2(fmt(3) label(Romano Wolf))") coeflabels(ffriend "Having A Friend" fsupportself "Having Emotional Support" fstudyself "Having Academic Support" fnode_in_friend "Friendship Ties (in-degree)" fnode_in_supportself "Emotional Support Ties (in-degree)" fnode_in_studyself "Academic Support Ties (in-degree)") noobs replace nonumbers nogaps

eststo clear

*****************************************************************
*Table A1, Panel 3: Survey Outcomes
*****************************************************************

clear matrix

global surveyvars1 fsbully_c fsbully_s tbully_f desnorms_sd fethnicbias_sd fempathy_pt_sd fimpulsivity_sd fempathy_ec_sd feyes_sd


local count: word count $surveyvars1
mat eb1=J(1,`count',0)  //create empty matrix with 2 (as many variables as we are looping over) rows, 1 column

matrix list eb1  //look at the matrix


reg fsbully_c treatment  bsbully_c $controls i.bstrata, cluster(b_schoolid)
test empathy //this does an F-test, but for one variable it's equivalent to a t-test (check: -help test- there is lots this can do
matrix eb1[1, 1] = `r(p)'  //save the p-value in the matrix 

reg fsbully_s treatment  bsbully_s $controls i.bstrata, cluster(b_schoolid)
test empathy //this does an F-test, but for one variable it's equivalent to a t-test (check: -help test- there is lots this can do
matrix eb1[1, 2] = `r(p)'  //save the p-value in the matrix 
  
reg tbully_f treatment bsbully_c $controls i.bstrata, cluster(b_schoolid)
test empathy //this does an F-test, but for one variable it's equivalent to a t-test (check: -help test- there is lots this can do
matrix eb1[1, 3] = `r(p)'  //save the p-value in the matrix 

reg desnorms_sd treatment $controls i.bstrata, cluster(b_schoolid)
test empathy //this does an F-test, but for one variable it's equivalent to a t-test (check: -help test- there is lots this can do
matrix eb1[1, 4] = `r(p)'  //save the p-value in the matrix 

reg fethnicbias_sd treatment bethnicbias_sd $controls i.bstrata, cluster(b_schoolid)
test empathy //this does an F-test, but for one variable it's equivalent to a t-test (check: -help test- there is lots this can do
matrix eb1[1, 5] = `r(p)'  //save the p-value in the matrix 

reg fempathy_pt_sd treatment bempathy_pt_sd  $controls i.bstrata, cluster(b_schoolid)
test empathy //this does an F-test, but for one variable it's equivalent to a t-test (check: -help test- there is lots this can do
matrix eb1[1, 6] = `r(p)'  //save the p-value in the matrix 

reg fimpulsivity_sd treatment bimpulsivity_sd $controls i.bstrata, cluster(b_schoolid)
test empathy //this does an F-test, but for one variable it's equivalent to a t-test (check: -help test- there is lots this can do
matrix eb1[1, 7] = `r(p)'  //save the p-value in the matrix 

reg fempathy_ec_sd treatment bempathy_ec_sd  $controls i.bstrata, cluster(b_schoolid)
test empathy //this does an F-test, but for one variable it's equivalent to a t-test (check: -help test- there is lots this can do
matrix eb1[1, 8] = `r(p)'  //save the p-value in the matrix 

reg feyes_sd treatment  $controls i.bstrata, cluster(b_schoolid)
test empathy //this does an F-test, but for one variable it's equivalent to a t-test (check: -help test- there is lots this can do
matrix eb1[1, 9] = `r(p)'  //save the p-value in the matrix 

rwolf $surveyvars1, ///
indepvar(treatment) controls($controls i.bstrata) reps(500) cluster(b_schoolid) seed(2021)

local count: word count $surveyvars1
mat eb2=J(1,`count',0)


loc n = 1 //count the iterations


foreach variableName in $surveyvars1 {

matrix eb2[1, `n'] =  e(rw_`variableName' ) 

loc n = `n' + 1  //each iteration, adjust the count
}


matrix list eb1  //look at your p-values 
matrix list eb2

matrix colnames eb1 = $surveyvars1
matrix colnames eb2 = $surveyvars1

estpost ttest $surveyvars1, by(treatment)
estadd matrix eb1, replace
estadd matrix eb2, replace

estimates store pvalues

esttab pvalues using "$path2/tableA1c.tex", cells("eb1(fmt(3) label(Original)) eb2(fmt(3) label(Romano Wolf))") coeflabels(fsbully_c "Bullying In-Class" fsbully_s "Bullying Out-Class" tbully_f "Teacher Behavioral Grade" desnorms_sd "Behavioral Norms" ///
fethnicbias_sd "Ethnic Bias" fempathy_pt_sd "Perspective Taking"  fimpulsivity_sd "Impulsivity" fempathy_ec_sd "Empathetic Concern" feyes_sd "Eyes Test") noobs replace nonumbers nogaps

eststo clear

restore
*/

*******************************************
*******************************************
*SECTION B: Main Results without Covariates
*******************************************
*******************************************

*****************************************************************
*Table B1: Treatment Effects on Violence and Anti-Social Behavior
*****************************************************************


*****************************************************************
*Table B1, Panel 1: Peer Violence and Victimization-Diary Records
*****************************************************************
preserve

collapse events perpetrator victim treatment bstrata n_class, by(b_schoolid)

gen spill=events-perpetrator

reg perpetrator treatment n_class i.bstrata,r
estimates store spec1
sum perpetrator if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

reg victim treatment n_class i.bstrata,r
estimates store spec2
sum victim if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

reg events treatment n_class i.bstrata,r
estimates store spec3
sum events if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

reg spill treatment n_class i.bstrata,r
estimates store spec4
sum spill if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1



#delimit ;
esttab spec1 spec2 spec3 spec4 using "$path2/tableB1a.tex",  replace label compress b(%8.3f) se(3) nonumbers keep(treatment) 
order(treatment) 
coeflabels(treatment "Treatment")
star(* 0.10 ** 0.05 *** 0.01) nonotes s(Cmean N, fmt(%9.3g %9.0g) 
labels("Control Mean" "Observations")) nogaps
mtitles ( "Perpetrated" "Victimized" "Total Events" "Spillover")
;
#delimit cr

restore

***********************************************************************************
*Table B1, Panel 2: Student and Teacher Reports of Violence and Antisocial Behavior
***********************************************************************************


xi: reg fsbully_c treatment i.bstrata, cluster(b_schoolid)
est store spec1
sum fsbully_c if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

xi: reg fsbully_s treatment i.bstrata, cluster(b_schoolid)
est store spec2
sum fsbully_s if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

*Teacher Reports
xi: reg tbully_f treatment i.bstrata, cluster(b_schoolid)
estimates store spec3
sum tbully_f if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1


#delimit ;
esttab spec1 spec2 spec3 using "$path2/tableB1b.tex",  replace label compress b(%8.3f) se(3) nonumbers keep(treatment) 
coeflabels(treatment "Treatment")
star(* 0.10 ** 0.05 *** 0.01) nonotes s(Cmean N, fmt(%9.2f %9.0g) 
labels("Control Mean" "Observations")) nogaps
mtitles ("Bullying In-Class" "Bullying Out-Class" "Teacher Behavioral Grade")
;
#delimit cr




******************************************************************************************
*Table B2: Treatment Effects on Social Inclusion and Ethnic Segregation in the Classroom
******************************************************************************************


*******************************************
*Table B2, Panel 1: Social Exclusion-Binary
*******************************************

xi: reg ffriend treatment i.bstrata, cluster(b_schoolid)
est store spec1
sum ffriend if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

xi: reg fsupportself treatment i.bstrata, cluster(b_schoolid)
est store spec2
sum fsupportself if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

xi: reg fstudyself treatment i.bstrata, cluster(b_schoolid)
est store spec3
sum fstudyself if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

#delimit ;
esttab spec1 spec2 spec3 using "$path2/tableB2a.tex",  replace label compress b(%8.3f) se(3) nonumbers keep(treatment) 
order(treatment) 
coeflabels(treatment "Treatment")
star(* 0.10 ** 0.05 *** 0.01) nonotes s(Cmean N, fmt(%9.2f %9.0g) 
labels("Control Mean" "Observations"))  
mtitles ("Friendship" "Emotional Support"  "Academic Support") nogaps
;
#delimit cr

***************************************************
*Table B2, Panel 2: Social Exclusion-In-degree Ties
***************************************************

xi: reg fnode_in_friend treatment i.bstrata, cluster(b_schoolid)
est store spec1
sum fnode_in_friend if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

xi: reg fnode_in_supportself treatment i.bstrata, cluster(b_schoolid)
est store spec2
sum fnode_in_supportself if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

xi: reg fnode_in_studyself treatment i.bstrata, cluster(b_schoolid)
est store spec3
sum fnode_in_studyself if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1


#delimit ;
esttab spec1 spec2 spec3 using "$path2/tableB2b.tex",  replace label compress b(%8.3f) se(3) nonumbers keep(treatment) 
order(treatment) 
coeflabels(treatment "Treatment")
star(* 0.10 ** 0.05 *** 0.01) nonotes s( Cmean N, fmt( %9.2f %9.0g) 
labels( "Control Mean" "Observations")) nogaps
mtitles ("Friendship" "Emotional Support" "Academic Support")
;
#delimit cr

**************************************
*Table B1, Panel 3: Ethnic Segregation
**************************************

preserve

drop if studentstatus=="GONE" | studentstatus=="NEW"

collapse fhomophily_eth_rh_friend fhomophily_eth_rh_supportself  fhomophily_eth_rh_studyself treatment bstrata b_schoolid, by(f_classid)



reg fhomophily_eth_rh_friend treatment i.bstrata, cluster(b_schoolid)
est store spec1
sum fhomophily_eth_rh_friend if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1


reg fhomophily_eth_rh_supportself treatment i.bstrata, cluster(b_schoolid)
est store spec2
sum fhomophily_eth_rh_supportself if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1


reg fhomophily_eth_rh_studyself treatment i.bstrata, cluster(b_schoolid)
est store spec3
sum fhomophily_eth_rh_studyself if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1


#delimit ;
esttab spec1 spec2 spec3 using "$path2/tableB2c.tex",  replace label compress b(%8.3f) se(3) nonumbers keep(treatment) 
order(treatment) 
coeflabels(treatment "Treatment")
star(* 0.10 ** 0.05 *** 0.01) nonotes s(Cmean N, fmt(%9.2f %9.0g) 
labels("Control Mean" "Observations"))  
mtitles ("Friendship" "Emotional Support" "Academic Support") nogaps
;
#delimit cr

restore


****************************************************************************************
*Table B3: Treatment Effects on Prosocial Behavior and Expected Payoffs
****************************************************************************************



******************************************************
*Table B3, Panel 1: Trust, Reciprocity and Cooperation
******************************************************

*In-Class Trust
reg fs_decision_in treatment i.bstrata, cluster(b_schoolid)
estimates store spec1a
sum fs_decision_in if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

*Out-School Trust
reg fs_decision_out treatment i.bstrata, cluster(b_schoolid)
estimates store spec1b
sum fs_decision_out if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

quietly reg fs_decision_in treatment i.bstrata 
est store s1
quietly reg fs_decision_out treatment i.bstrata 
est store s2
suest s1 s2, cluster(b_schoolid)
test [s1_mean]treatment=[s2_mean]treatment 
estadd scalar p1=r(p),  : spec1b

*In-Class Reciprocity
reg fr_decision_in_perc treatment i.bstrata, cluster(b_schoolid)
estimates store spec2a
sum fr_decision_in_perc if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1

*Out-School Reciprocity
reg fr_decision_out_perc treatment i.bstrata, cluster(b_schoolid)
estimates store spec2b
sum fr_decision_out_perc if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1


quietly reg fr_decision_in_perc treatment i.bstrata 
est store s1
quietly reg fr_decision_out_perc treatment  i.bstrata 
est store s2
suest s1 s2, cluster(b_schoolid)
test [s1_mean]treatment=[s2_mean]treatment 
estadd scalar p1=r(p),  : spec2b

*In-Class Cooperation
reg fcoordin treatment i.bstrata , cluster(b_schoolid)
estimates store spec3a
sum fcoordin if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1

*Out-School Cooperation
reg fcoordout treatment i.bstrata, cluster(b_schoolid)
estimates store spec3b
sum fcoordout if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1


quietly reg fcoordin treatment  i.bstrata 
est store s1
quietly reg fcoordout treatment i.bstrata
est store s2
suest s1 s2, cluster(b_schoolid)
test [s1_mean]treatment=[s2_mean]treatment 
estadd scalar p1=r(p),  : spec3b



#delimit ;
esttab spec1a spec1b spec2a spec2b  spec3a spec3b using "$path2/tableB3a.tex",  replace label compress b(%8.3f) se(3) nonumbers keep(treatment) 
order(treatment) 
coeflabels(treatment "Treatment")
star(* 0.10 ** 0.05 *** 0.01) nonotes s(p1 Cmean N, fmt(%9.3f %9.2f %9.0g) 
labels("p-val (In-Class=Out-School)" "Control Mean" "Observations") 
layout( "\multicolumn{2}{S}{@}" @ @ )) nogaps
mtitles ("In-Class" "Out-School" "In-Class" "Out-School" "In-Class" "Out-School")
mgroups("Trust" "Reciprocity" "Cooperation", pattern(1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) 
substitute("&\multicolumn{2}{S}{" "\multicolumn{2}{c}{")
;
#delimit cr

******************************************************
*Table B3, Panel 2: Trust, Reciprocity and Cooperation
******************************************************

*Trust In-Class Payoffs

*Sender
reg mpayoff treatment i.bstrata, cluster(b_schoolid)
estimates store spec1
sum mpayoff if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1

*Receiver
reg rpayoff treatment i.bstrata, cluster(b_schoolid)
estimates store spec2
sum rpayoff if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1

*Overall
reg tpayoff treatment i.bstrata, cluster(b_schoolid)
estimates store spec3
sum tpayoff if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1


*Trust Out-School Payoffs

*Sender
reg ompayoff treatment i.bstrata, cluster(b_schoolid)
estimates store spec4
sum ompayoff if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1

*Receiver
reg orpayoff treatment i.bstrata, cluster(b_schoolid)
estimates store spec5
sum orpayoff if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1

*Overall
reg otpayoff treatment i.bstrata, cluster(b_schoolid)
estimates store spec6
sum otpayoff if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1

*Cooperation In-Class Payoffs
reg cpayoff_in treatment i.bstrata, cluster(b_schoolid)
estimates store spec7
sum cpayoff_in if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1

*Cooperation Out-School Payoffs
reg cpayoff_out treatment i.bstrata, cluster(b_schoolid)
estimates store spec8
sum cpayoff_out if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1



#delimit ;
esttab spec1 spec2 spec3 spec4 spec5 spec6 spec7 spec8 using "$path2/tableB3b.tex",  replace label compress b(%8.3f) se(3) nonumbers keep(treatment) 
order(treatment) 
coeflabels(treatment "Treatment")
star(* 0.10 ** 0.05 *** 0.01) nonotes s(Cmean N, fmt(%9.2f %9.0g) 
labels("Control Mean" "Observations")) nogaps
mtitles("Sender" "Receiver" "Overall" "Sender" "Receiver" "Overall" "In-Class" "Out-School")  
mgroups("Trust In-Class" "Trust Out-School" "Cooperation", pattern(1 0 0 1 0 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ;
#delimit cr



****************************************
*Table B4: Treatment Effects on Altruism
****************************************

gen inter1=a2*treatment

reg fdonate treatment a2 inter1 i.bstrata, cluster(b_schoolid)
estimates store spec1
sum fdonate if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

reg fdonation_perc treatment a2 inter1 i.bstrata, cluster(b_schoolid)
estimates store spec2
sum fdonation_perc if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

#delimit;
esttab spec1 spec2  using "$path2/tableB4.tex",  replace label compress b(%8.3f) se(3) nonumbers keep(treatment a2 inter1) 
order(treatment a2 inter1) 
coeflabels(treatment "Treatment" a2 "Ethnic Reference" inter1 "Treatment*Ethnic Reference")
star(* 0.10 ** 0.05 *** 0.01) nonotes s(Cmean N, fmt(%9.2f %9.0g) 
labels("Control Mean" "Observations")) nogaps eqlabels("")
mtitles ("Willing to Donate" "Fraction Donated")
;
#delimit cr


***************************************************************************************************************
***************************************************************************************************************
*SECTION C: Heterogeneous Treatment Effects by Teacher Gender, Experience, Fluid IQ and Emotional Intelligence
***************************************************************************************************************
***************************************************************************************************************

gen intertmale=treatment*baseline_tmale

gen interexper=treatment*baseline_texperience

gen interraven=treatment*btraven_sd

gen intereyes=treatment*bteyes_sd

*********************************************************************************************************************
*Table C1: Heterogeneous Treatment Effects on Student Reports of Violence and Teacher Reports of Antisocial Behavior
*********************************************************************************************************************


**********************************
*Table C1, Panel 1: Teacher Gender
**********************************

*In-Class Bullying
reg fsbully_c treatment intertmale baseline_tmale bsbully_c $controls i.bstrata, cluster(b_schoolid)
estimates store spec1

*Out-School Bullying
reg fsbully_s treatment intertmale baseline_tmale bsbully_s $controls i.bstrata, cluster(b_schoolid)
estimates store spec2

*Teacher Behavoral Grade
reg tbully_f treatment intertmale baseline_tmale bsbully_c $controls i.bstrata, cluster(b_schoolid)
estimates store spec3

#delimit ;
esttab spec1 spec2 spec3 using "$path2/tableC1a.tex", replace label compress b(%8.3f) se(3) nonumbers
keep(treatment baseline_tmale intertmale)
order(treatment baseline_tmale intertmale)
coeflabels(treatment "Treatment" baseline_tmale "Male Teacher" intertmale "Treatment*Male Teacher")
star(* 0.1 ** 0.05 *** 0.01)
nonotes 
s( N, fmt(%9.0g) labels("Observations")) nogaps
mtitles ("Bullying In-Class" "Bullying Out-Class" "Teacher Behavioral Grade")
;
#delimit cr

**************************************
*Table C1, Panel 2: Teacher Experience
**************************************

*In-Class Bullying
reg fsbully_c treatment interexper baseline_texperience bsbully_c $controls i.bstrata , cluster(b_schoolid)
estimates store spec1

*Out-School Bullying
reg fsbully_s treatment interexper baseline_texperience bsbully_s $controls i.bstrata , cluster(b_schoolid)
estimates store spec2

*Teacher Behavoral Grade
reg tbully_f treatment interexper baseline_texperience bsbully_c $controls i.bstrata, cluster(b_schoolid)
estimates store spec3


#delimit ;
esttab spec1 spec2 spec3 using "$path2/tableC1b.tex", replace label compress b(%8.3f) se(3) nonumbers
keep(treatment baseline_texperience interexper)
order(treatment baseline_texperience interexper)
coeflabels(treatment "Treatment" baseline_texperience "Experience in Years" interexper "Treatment*Experience in Years")
star(* 0.1 ** 0.05 *** 0.01)
nonotes 
s(N, fmt(%9.0g) labels( "Observations")) nogaps
mtitles ("Bullying In-Class" "Bullying Out-Class" "Teacher Behavioral Grade")
;
#delimit cr


********************************************************************
*Table C1, Panel 3: Teacher's Fluid Cognitive Ability (Raven's Test)
********************************************************************

*In-Class Bullying
reg fsbully_c treatment interraven btraven_sd bsbully_c $controls i.bstrata , cluster(b_schoolid)
estimates store spec1

*Out-School Bullying
reg fsbully_s treatment interraven btraven_sd bsbully_s $controls i.bstrata , cluster(b_schoolid)
estimates store spec2

*Teacher Behavoral Grade
reg tbully_f treatment interraven btraven_sd bsbully_c $controls i.bstrata, cluster(b_schoolid)
estimates store spec3



#delimit ;
esttab spec1 spec2 spec3 using "$path2/tableC1c.tex", replace label compress b(%8.3f) se(3) nonumbers
keep(treatment btraven_sd interraven )
order(treatment btraven_sd interraven )
coeflabels(treatment "Treatment" btraven_sd "Raven Score" interraven "Treatment*Raven Score")
star(* 0.1 ** 0.05 *** 0.01)
nonotes 
s(N, fmt(%9.0g) labels( "Observations")) nogaps
mtitles ("Bullying In-Class" "Bullying Out-Class" "Teacher Behavioral Grade")
;
#delimit cr



****************************************************************
*Table C1, Panel 4: Teacher's Emotional Intelligence (Eyes Test)
****************************************************************

*In-Class Bullying
reg fsbully_c treatment intereyes bteyes_sd bsbully_c $controls i.bstrata , cluster(b_schoolid)
estimates store spec1

*Out-School Bullying
reg fsbully_s treatment intereyes bteyes_sd bsbully_s $controls i.bstrata , cluster(b_schoolid)
estimates store spec2

*Teacher Reports
reg tbully_f treatment intereyes bteyes_sd bsbully_c $controls i.bstrata, cluster(b_schoolid)
estimates store spec3



#delimit ;
esttab spec1 spec2 spec3 using "$path2/tableC1d.tex", replace label compress b(%8.3f) se(3) nonumbers
keep(treatment bteyes_sd intereyes)
order(treatment bteyes_sd intereyes)
coeflabels(treatment "Treatment" bteyes_sd "Eyes Score" intereyes "Treatment*Eyes Score")
star(* 0.1 ** 0.05 *** 0.01)
nonotes 
s(N, fmt(%9.0g) labels( "Observations")) nogaps
mtitles ("Bullying In-Class" "Bullying Out-Class"  "Teacher Behavioral Grade")
;
#delimit cr



****************************************************************************
*Table C2: Heterogeneous Treatment Effects on Social Exclusion - Binary Ties
****************************************************************************

**********************************
*Table C2, Panel 1: Teacher Gender
**********************************

*Best Friend
xi: reg ffriend treatment bfriend intertmale baseline_tmale $controls i.bstrata, cluster(b_schoolid)
est store spec1


*Emotional Support
xi: reg fsupportself treatment bsupportself intertmale baseline_tmale $controls i.bstrata, cluster(b_schoolid)
est store spec2

*Academic Support
xi: reg fstudyself treatment bstudyself intertmale baseline_tmale $controls i.bstrata, cluster(b_schoolid)
est store spec3


#delimit ;
esttab spec1 spec2 spec3  using  "$path2/tableC2a.tex",  replace label compress b(%8.3f) se(3) nonumbers 
keep(treatment baseline_tmale intertmale)  order(treatment baseline_tmale intertmale)
coeflabels(treatment "Treatment" baseline_tmale "Male Teacher" intertmale "Treatment*Male Teacher")
star(* 0.10 ** 0.05 *** 0.01) nonotes stats( N, fmt(%9.0g) 
labels("Observations")) nogaps
mtitles ("Friendship" "Emotional Support" "Academic Support")  ;
#delimit cr
eststo clear

**************************************
*Table C2, Panel 2: Teacher Experience
**************************************

*Best Friend
xi: reg ffriend treatment bfriend interexper baseline_texperience $controls i.bstrata, cluster(b_schoolid)
est store spec1


*Emotional Support
xi: reg fsupportself treatment bsupportself interexper baseline_texperience  $controls i.bstrata, cluster(b_schoolid)
est store spec2


*Academic Support
xi: reg fstudyself treatment bstudyself interexper baseline_texperience $controls i.bstrata, cluster(b_schoolid)
est store spec3


#delimit ;
esttab spec1 spec2 spec3  using  "$path2/tableC2b.tex",  replace label compress b(%8.3f) se(3) nonumbers 
keep(treatment baseline_texperience interexper)
order(treatment baseline_texperience interexper)
coeflabels(treatment "Treatment" baseline_texperience "Experience in Years" interexper "Treatment*Experience in Years")
star(* 0.10 ** 0.05 *** 0.01) nonotes stats(N, fmt( %9.0g) 
labels("Observations")) nogaps
mtitles ("Friendship" "Emotional Support" "Academic Support")  ;
#delimit cr
eststo clear


********************************************************************
*Table C2, Panel 3: Teacher's Fluid Cognitive Ability (Raven's Test)
********************************************************************

*Best Friend
xi: reg ffriend treatment bfriend  interraven btraven_sd  $controls i.bstrata, cluster(b_schoolid)
est store spec1


*Emotional Support
xi: reg fsupportself treatment bsupportself interraven btraven_sd   $controls i.bstrata, cluster(b_schoolid)
est store spec2


*Academic Support
xi: reg fstudyself treatment bstudyself  interraven btraven_sd  $controls i.bstrata, cluster(b_schoolid)
est store spec3


#delimit ;
esttab spec1 spec2 spec3  using  "$path2/tableC2c.tex",  replace label compress b(%8.3f) se(3) nonumbers 
keep(treatment btraven_sd interraven )
order(treatment btraven_sd interraven )
coeflabels(treatment "Treatment" btraven_sd "Raven Score" interraven "Treatment*Raven Score")
star(* 0.10 ** 0.05 *** 0.01) nonotes stats(N, fmt( %9.0g) 
labels("Observations")) nogaps
mtitles ("Friendship" "Emotional Support" "Academic Support")  ;
#delimit cr
eststo clear

****************************************************************
*Table C2, Panel 4: Teacher's Emotional Intelligence (Eyes Test)
****************************************************************

*Best Friend
xi: reg ffriend treatment bfriend intereyes bteyes_sd  $controls i.bstrata, cluster(b_schoolid)
est store spec1


*Emotional Support
xi: reg fsupportself treatment bsupportself intereyes bteyes_sd  $controls i.bstrata, cluster(b_schoolid)
est store spec2


*Academic Support
xi: reg fstudyself treatment bstudyself intereyes bteyes_sd  $controls i.bstrata, cluster(b_schoolid)
est store spec3


#delimit ;
esttab spec1 spec2 spec3  using  "$path2/tableC2d.tex",  replace label compress b(%8.3f) se(3) nonumbers 
keep(treatment bteyes_sd intereyes)
order(treatment bteyes_sd intereyes)
coeflabels(treatment "Treatment" bteyes_sd "Eyes Score" intereyes "Treatment*Eyes Score")
star(* 0.10 ** 0.05 *** 0.01) nonotes stats(N, fmt( %9.0g) 
labels("Observations")) nogaps
mtitles ("Friendship" "Emotional Support" "Academic Support")  ;
#delimit cr
eststo clear

*********************************************************************************************************************
*Table C3: Heterogeneous Treatment Effects on Prosocial Behavior: Trust, Reciprocity and Cooperation
*********************************************************************************************************************

**********************************
*Table C3, Panel 1: Teacher Gender
**********************************


*Trust In-Class
xi: reg fs_decision_in treatment intertmale baseline_tmale $controls i.bstrata, cluster(b_schoolid)
est store spec1a

*Trust Out-School
xi: reg fs_decision_out treatment intertmale baseline_tmale $controls i.bstrata, cluster(b_schoolid)
est store spec1b

*Reciprocity In-Class
reg fr_decision_in_perc treatment intertmale baseline_tmale $controls i.bstrata, cluster(b_schoolid)
estimates store spec2a

*Reciprocity Out-School
reg fr_decision_out_perc treatment intertmale baseline_tmale $controls i.bstrata, cluster(b_schoolid)
estimates store spec2b

*Cooperation In-Class
reg fcoordin treatment intertmale baseline_tmale bcoord $controls i.bstrata, cluster(b_schoolid)
estimates store spec3a

*Cooperation Out-School
reg fcoordout treatment intertmale baseline_tmale $controls i.bstrata, cluster(b_schoolid)
estimates store spec3b



#delimit ;
esttab spec1a spec1b spec2a spec2b spec3a spec3b using "$path2/tableC3a.tex",  replace label compress b(%8.3f) se(3) nonumbers 
keep(treatment baseline_tmale intertmale)  order(treatment baseline_tmale intertmale)
coeflabels(treatment "Treatment" baseline_tmale "Male Teacher" intertmale "Treatment*Male Teacher")
star(* 0.10 ** 0.05 *** 0.01) nonotes s( N, fmt( %9.0g) 
labels("Observations")) nogaps
mtitles ("In-Class" "Out-School" "In-Class" "Out-School" "In-Class" "Out-School")
mgroups("Trust" "Reciprocity" "Cooperation", pattern(1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))  ;
#delimit cr
eststo clear


**************************************
*Table C3, Panel 2: Teacher Experience
**************************************

*Trust In-Class
reg fs_decision_in treatment interexper baseline_texperience $controls i.bstrata, cluster(b_schoolid)
est store spec1a

*Trust Out-School
reg fs_decision_out treatment interexper baseline_texperience $controls  i.bstrata, cluster(b_schoolid)
est store spec1b

*Reciprocity In-Class
reg fr_decision_in_perc treatment interexper baseline_texperience $controls i.bstrata, cluster(b_schoolid)
estimates store spec2a

*Reciprocity Out-School
reg fr_decision_out_perc treatment interexper baseline_texperience $controls i.bstrata, cluster(b_schoolid)
estimates store spec2b

*Cooperation In-Class
reg fcoordin  treatment interexper  baseline_texperience bcoord $controls i.bstrata, cluster(b_schoolid)
estimates store spec3a

*Cooperation Out-School
reg fcoordout  treatment interexper baseline_texperience $controls  i.bstrata, cluster(b_schoolid)
estimates store spec3b



#delimit ;
esttab spec1a spec1b spec2a spec2b spec3a spec3b using "$path2/tableC3b.tex",  replace label compress b(%8.3f) se(3) nonumbers 
keep(treatment baseline_texperience interexper) order(treatment baseline_texperience interexper)
coeflabels(treatment "Treatment" baseline_texperience "Experience in Years" interexper "Treatment*Experience in Years")
star(* 0.10 ** 0.05 *** 0.01) nonotes s(N, fmt( %9.0g) 
labels("Observations")) nogaps
mtitles ("In-Class" "Out-School" "In-Class" "Out-School" "In-Class" "Out-School")
mgroups("Trust" "Reciprocity" "Cooperation", pattern(1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))  ;
#delimit cr
eststo clear


********************************************************************
*Table C3, Panel 3: Teacher's Fluid Cognitive Ability (Raven's Test)
********************************************************************

*Trust In-Class
reg fs_decision_in treatment interraven btraven_sd $controls i.bstrata, cluster(b_schoolid)
est store spec1a

*Trust Out-School
reg fs_decision_out treatment interraven btraven_sd $controls  i.bstrata, cluster(b_schoolid)
est store spec1b

*Reciprocity In-Class
reg fr_decision_in_perc treatment interraven btraven_sd $controls i.bstrata, cluster(b_schoolid)
estimates store spec2a

*Reciprocity Out-School
reg fr_decision_out_perc treatment interraven btraven_sd $controls i.bstrata, cluster(b_schoolid)
estimates store spec2b

*Cooperation In-Class
reg fcoordin treatment interraven btraven_sd bcoord $controls i.bstrata, cluster(b_schoolid)
estimates store spec3a

*Cooperation Out-School
reg fcoordout treatment interraven btraven_sd $controls  i.bstrata, cluster(b_schoolid)
estimates store spec3b



#delimit ;
esttab spec1a spec1b spec2a spec2b spec3a spec3b using "$path2/tableC3c.tex",  replace label compress b(%8.3f) se(3) nonumbers 
keep(treatment btraven_sd interraven )
order(treatment btraven_sd interraven )
coeflabels(treatment "Treatment" btraven_sd "Raven Score" interraven "Treatment*Raven Score")
star(* 0.10 ** 0.05 *** 0.01) nonotes s(N, fmt( %9.0g) 
labels("Observations")) nogaps
mtitles ("In-Class" "Out-School" "In-Class" "Out-School" "In-Class" "Out-School")
mgroups("Trust" "Reciprocity" "Cooperation", pattern(1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))  ;
#delimit cr
eststo clear

****************************************************************
*Table C3, Panel 4: Teacher's Emotional Intelligence (Eyes Test)
****************************************************************

*Trust In-Class
reg fs_decision_in treatment intereyes bteyes_sd  $controls i.bstrata, cluster(b_schoolid)
est store spec1a

*Trust Out-School
reg fs_decision_out treatment intereyes bteyes_sd  $controls  i.bstrata, cluster(b_schoolid)
est store spec1b

*Reciprocity In-Class
reg fr_decision_in_perc treatment intereyes bteyes_sd  $controls i.bstrata, cluster(b_schoolid)
estimates store spec2a

*Reciprocity Out-School
reg fr_decision_out_perc treatment intereyes bteyes_sd $controls i.bstrata, cluster(b_schoolid)
estimates store spec2b

*Cooperation In-Class
reg fcoordin treatment intereyes bteyes_sd bcoord  $controls i.bstrata, cluster(b_schoolid)
estimates store spec3a

*Cooperation Out-School
reg fcoordout treatment intereyes bteyes_sd  $controls  i.bstrata, cluster(b_schoolid)
estimates store spec3b



#delimit ;
esttab spec1a spec1b spec2a spec2b spec3a spec3b using "$path2/tableC3d.tex",  replace label compress b(%8.3f) se(3) nonumbers 
keep(treatment bteyes_sd intereyes)
order(treatment bteyes_sd intereyes)
coeflabels(treatment "Treatment" bteyes_sd "Eyes Score" intereyes "Treatment*Eyes Score")
star(* 0.10 ** 0.05 *** 0.01) nonotes s(N, fmt( %9.0g) 
labels("Observations")) nogaps
mtitles ("In-Class" "Out-School" "In-Class" "Out-School" "In-Class" "Out-School")
mgroups("Trust" "Reciprocity" "Cooperation", pattern(1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))  ;
#delimit cr
eststo clear


******************************************************
*Table C4: Heterogeneous Treatment Effects on Altruism
******************************************************



**********************************
*Table C4, Panel 1: Teacher Gender
**********************************

*Willingness to Donate
reg fdonate treatment a2 intertmale baseline_tmale bdonation_sd $controls i.bstrata,cluster(b_schoolid)
estimates store spec1

*Fraction Donated
reg fdonation_perc treatment a2 intertmale baseline_tmale bdonation_sd $controls i.bstrata,cluster(b_schoolid)
estimates store spec2



#delimit;
esttab spec1 spec2 using "$path2/tableC4a.tex",  replace label compress b(%8.3f) se(3) nonumbers 
keep(treatment baseline_tmale intertmale)  order(treatment baseline_tmale intertmale)
coeflabels(treatment "Treatment" baseline_tmale "Male Teacher" intertmale "Treatment*Male Teacher")
star(* 0.10 ** 0.05 *** 0.01) nonotes s(N, fmt(%9.0g) 
labels( "Observations")) nogaps eqlabels("")
mtitles ("Willing to Donate" "Fraction Donated")
;
#delimit cr
eststo clear
**************************************
*Table C4, Panel 2: Teacher Experience
**************************************

*Willingness to Donate
reg fdonate treatment a2 interexper baseline_texperience bdonation_sd $controls i.bstrata,cluster(b_schoolid)
estimates store spec1

*Fraction Donated
reg fdonation_perc treatment a2 interexper baseline_texperience bdonation_sd $controls i.bstrata,cluster(b_schoolid)
estimates store spec2


#delimit;
esttab spec1 spec2 using "$path2/tableC4b.tex",  replace label compress b(%8.3f) se(3) nonumbers 
keep(treatment baseline_texperience interexper) order(treatment baseline_texperience interexper)
coeflabels(treatment "Treatment" baseline_texperience "Experience in Years" interexper "Treatment*Experience in Years")
star(* 0.10 ** 0.05 *** 0.01) nonotes s(N, fmt(%9.0g) 
labels( "Observations")) nogaps eqlabels("")
mtitles ("Willing to Donate" "Fraction Donated")
;
#delimit cr
eststo clear

********************************************************************
*Table C4, Panel 3: Teacher's Fluid Cognitive Ability (Raven's Test)
********************************************************************

*Willingness to Donate
reg fdonate treatment a2 interraven btraven_sd bdonation_sd $controls i.bstrata, cluster(b_schoolid)
estimates store spec1

*Fraction Donated
reg fdonation_perc treatment a2 interraven btraven_sd bdonation_sd $controls i.bstrata,cluster(b_schoolid)
estimates store spec2


#delimit;
esttab spec1 spec2 using "$path2/tableC4c.tex",  replace label compress b(%8.3f) se(3) nonumbers 
keep(treatment btraven_sd interraven )
order(treatment btraven_sd interraven )
coeflabels(treatment "Treatment" btraven_sd "Raven Score" interraven "Treatment*Raven Score")
star(* 0.10 ** 0.05 *** 0.01) nonotes s(N, fmt(%9.0g) 
labels( "Observations")) nogaps eqlabels("")
mtitles ("Willing to Donate" "Fraction Donated")
;
#delimit cr
eststo clear

****************************************************************
*Table C4, Panel 4: Teacher's Emotional Intelligence (Eyes Test)
****************************************************************


*Willingness to Donate
reg fdonate treatment a2  intereyes bteyes_sd bdonation_sd $controls i.bstrata, cluster(b_schoolid)
estimates store spec1

*Fraction Donated
reg fdonation_perc treatment a2  intereyes bteyes_sd bdonation_sd $controls i.bstrata,cluster(b_schoolid)
estimates store spec2


#delimit;
esttab spec1 spec2 using "$path2/tableC4d.tex",  replace label compress b(%8.3f) se(3) nonumbers 
keep(treatment bteyes_sd intereyes)
order(treatment bteyes_sd intereyes)
coeflabels(treatment "Treatment" bteyes_sd "Eyes Score" intereyes "Treatment*Eyes Score")
star(* 0.10 ** 0.05 *** 0.01) nonotes s(N, fmt(%9.0g) 
labels( "Observations")) nogaps eqlabels("")
mtitles ("Willing to Donate" "Fraction Donated")
;
#delimit cr
eststo clear

*******************************************
*******************************************
*SECTION D: Local Average Treatment Effects
*******************************************
*******************************************
  
****************************************************************
*Table D1: Violence and Anti-Social Behavior
****************************************************************


*****************************************************************
*Table D1, Panel 1: Peer Violence and Victimization-Diary Records
*****************************************************************


preserve

drop if studentstatus=="GONE" | studentstatus=="NEW"


collapse events perpetrator victim b_schoolsize n_class srefshare treatment b_provinceid bstrata ftline dist1-dist10, by(b_schoolid)

gen spill=events-perpetrator

ivregress 2sls perpetrator b_schoolsize n_class srefshare i.b_provinceid i.bstrata dist1-dist10 (ftline=treatment), r
estimates store spec1
sum perpetrator if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

ivregress 2sls victim b_schoolsize n_class srefshare i.b_provinceid i.bstrata dist1-dist10 (ftline=treatment), r
estimates store spec2
sum victim if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

ivregress 2sls events b_schoolsize n_class srefshare i.b_provinceid i.bstrata dist1-dist10 (ftline=treatment), r
estimates store spec3
sum events if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

ivregress 2sls spill b_schoolsize n_class srefshare i.b_provinceid i.bstrata dist1-dist10 (ftline=treatment), r
estimates store spec4
sum spill if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1


#delimit ;
esttab spec1 spec2 spec3 spec4 using "$path2/tableD1a.tex",  replace label compress b(%8.3f) se(3) nonumbers keep(ftline) 
coeflabels(ftline "Teacher Coverage")
star(* 0.10 ** 0.05 *** 0.01) nonotes s(Cmean N, fmt(%9.3g %9.0g) 
labels("Control Mean" "Observations")) nogaps
mtitles ( "Perpetrated" "Victimized" "Total Events" "Spillover")
;
#delimit cr

restore


************************************************************************************
*Table D1, Panel 2: Student and Teacher Reports of Violence and Antisocial Behavior
************************************************************************************


*In-Class Bullying
ivregress 2sls fsbully_c bsbully_c $controls i.bstrata  (ftline=treatment), cluster(b_schoolid)
est store spec1
sum fsbully_c if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

*Out-School Bullying
ivregress 2sls fsbully_s bsbully_s $controls i.bstrata  (ftline=treatment), cluster(b_schoolid)
est store spec2
sum fsbully_s if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1


*Teacher Behavioral Grade
ivregress 2sls tbully_f bsbully_c $controls i.bstrata  (ftline=treatment), cluster(b_schoolid)
estimates store spec3
sum tbully_f if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1


#delimit ;
esttab spec1 spec2 spec3 using "$path2/tableD1b.tex",  replace label compress b(%8.3f) se(3) nonumbers keep(ftline) 
coeflabels(ftline "Teacher Coverage")
star(* 0.10 ** 0.05 *** 0.01) nonotes s(Cmean N, fmt(%9.2f %9.0g) 
labels("Control Mean" "Observations")) nogaps
mtitles ("Bullying In-Class" "Bullying Out-Class" "Teacher Behavioral Grade")
;
#delimit cr
****************************************************************
*Table D2: Treatment Effects on Social Exclusion and Ethnic Segregation in the Classroom
****************************************************************

****************************************************************
*Table D2, Panel 1: Social Exclusion-Binary
****************************************************************

*Best Friend
ivregress 2sls ffriend bfriend $controls i.bstrata  (ftline=treatment), cluster(b_schoolid)
est store spec1
sum ffriend if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

*Emotional Support
ivregress 2sls fsupportself bsupportself $controls i.bstrata  (ftline=treatment), cluster(b_schoolid)
est store spec2
sum fsupportself if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

*Academic Support
ivregress 2sls fstudyself bstudyself $controls i.bstrata  (ftline=treatment), cluster(b_schoolid)
est store spec3
sum fstudyself if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1


#delimit ;
esttab spec1 spec2 spec3 using "$path2/tableD2a.tex",  replace label compress b(%8.3f) se(3) nonumbers keep(ftline) 
order(ftline) 
coeflabels(ftline "Teacher Coverage")
star(* 0.10 ** 0.05 *** 0.01) nonotes s(Cmean N, fmt(%9.2f %9.0g) 
labels("Control Mean" "Observations"))  
mtitles ("Friendship" "Emotional Support"  "Academic Support") nogaps
;
#delimit cr

****************************************************************
*Table D2, Panel 2: Social Exclusion-In-degree Ties
****************************************************************


ivregress 2sls fnode_in_friend bnode_in_friend $controls i.bstrata  (ftline=treatment), cluster(b_schoolid)
est store spec1
sum fnode_in_friend if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1


ivregress 2sls fnode_in_supportself bnode_in_supportself $controls i.bstrata  (ftline=treatment), cluster(b_schoolid)
est store spec2
sum fnode_in_supportself if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1


ivregress 2sls fnode_in_studyself bnode_in_studyself $controls i.bstrata  (ftline=treatment), cluster(b_schoolid)
est store spec3
sum fnode_in_studyself if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1


#delimit ;
esttab spec1 spec2 spec3 using "$path2/tableD2b.tex",  replace label compress b(%8.3f) se(3) nonumbers keep(ftline) 
order(ftline) 
coeflabels(ftline "Teacher Coverage")
star(* 0.10 ** 0.05 *** 0.01) nonotes s( Cmean N, fmt( %9.2f %9.0g) 
labels( "Control Mean" "Observations")) nogaps
mtitles ("Friendship" "Emotional Support" "Academic Support")
;
#delimit cr

****************************************************************
*Table D2, Panel 3: Ethnic Segregation
****************************************************************

preserve

drop if studentstatus=="GONE" | studentstatus=="NEW"

collapse fhomophily_eth_rh_friend bhomophily_eth_rh_friend fhomophily_eth_rh_supportself bhomophily_eth_rh_supportself b_refshare ///
fhomophily_eth_rh_studyself bhomophily_eth_rh_studyself b_schoolsize f_csize treatment ///
bstrata dist1-dist11 b_schoolid f_experimenter1-f_experimenter9 gradetwo age male ftline, by(f_classid)

global controls2 b_schoolsize f_csize dist1-dist10 gradetwo f_experimenter1-f_experimenter9 age male

ivregress 2sls fhomophily_eth_rh_friend bhomophily_eth_rh_friend $controls2 i.bstrata  (ftline=treatment), cluster(b_schoolid)
est store spec1
sum fhomophily_eth_rh_friend if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

ivregress 2sls fhomophily_eth_rh_supportself bhomophily_eth_rh_supportself $controls2 i.bstrata  (ftline=treatment), cluster(b_schoolid)
est store spec2
sum fhomophily_eth_rh_supportself if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

ivregress 2sls fhomophily_eth_rh_studyself bhomophily_eth_rh_studyself $controls2 i.bstrata  (ftline=treatment), cluster(b_schoolid)
est store spec3
sum fhomophily_eth_rh_studyself if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1


#delimit ;
esttab spec1 spec2 spec3 using "$path2/tableD2c.tex",  replace label compress b(%8.3f) se(3) nonumbers keep(ftline) 
order(ftline) 
coeflabels(ftline "Teacher Coverage")
star(* 0.10 ** 0.05 *** 0.01) nonotes s(Cmean N, fmt(%9.2f %9.0g) 
labels("Control Mean" "Observations"))  
mtitles ("Friendship" "Emotional Support" "Academic Support") nogaps
;
#delimit cr


restore


***********************************************************************
*Table D3: Treatment Effects on Prosocial Behavior and Expected Payoffs
***********************************************************************

******************************************************
*Table D3, Panel 1: Trust, Reciprocity and Cooperation
******************************************************

*In-Class Trust
ivregress 2sls fs_decision_in $controls i.bstrata  (ftline=treatment), cluster(b_schoolid)
estimates store spec1a
sum fs_decision_in if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

*Out-School Trust
ivregress 2sls fs_decision_out $controls i.bstrata  (ftline=treatment), cluster(b_schoolid)
estimates store spec1b
sum fs_decision_out if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

*In-Class Reciprocity
ivregress 2sls fr_decision_in_perc $controls i.bstrata (ftline=treatment), cluster(b_schoolid)
estimates store spec2a
sum fr_decision_in_perc if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1

*Out-School Reciprocity
ivregress 2sls fr_decision_out_perc $controls i.bstrata (ftline=treatment), cluster(b_schoolid)
estimates store spec2b
sum fr_decision_out_perc if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1

*In-Class Cooperation
ivregress 2sls fcoordin bcoord $controls i.bstrata (ftline=treatment), cluster(b_schoolid)
estimates store spec3a
sum fcoordin if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1

*Out-School Cooperation
ivregress 2sls fcoordout $controls i.bstrata (ftline=treatment), cluster(b_schoolid)
estimates store spec3b
sum fcoordout if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1



#delimit ;
esttab spec1a spec1b spec2a spec2b spec3a spec3b using "$path2/tableD3a.tex",  replace label compress b(%8.3f) se(3) nonumbers keep(ftline) 
order(ftline) 
coeflabels(ftline "Teacher Coverage")
star(* 0.10 ** 0.05 *** 0.01) nonotes s(Cmean N, fmt(%9.2f %9.0g) 
labels("Control Mean" "Observations"))  nogaps
mtitles ("In-Class" "Out-School" "In-Class" "Out-School" "In-Class" "Out-School")
mgroups("Trust" "Reciprocity" "Cooperation", pattern(1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) 
substitute("&\multicolumn{2}{S}{" "\multicolumn{2}{c}{")
;
#delimit cr

****************************************************************
*Table D3, Panel 2: Payoffs from Trust and Cooperation Game
****************************************************************

*In-Class Trust Payoffs

*Sender
ivregress 2sls mpayoff $controls i.bstrata (ftline=treatment), cluster(b_schoolid)
estimates store spec1
sum mpayoff if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1

*Receiver
ivregress 2sls rpayoff $controls i.bstrata (ftline=treatment), cluster(b_schoolid)
estimates store spec2
sum rpayoff if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1

*Overall
ivregress 2sls tpayoff $controls i.bstrata (ftline=treatment), cluster(b_schoolid)
estimates store spec3
sum tpayoff if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1


*Out-School Trust Payoffs

*Note: District FE are taken out 

*Sender
ivregress 2sls ompayoff age male refugee astudent braven_sd beyes_sd b_schoolsize f_csize i.bstrata (ftline=treatment), cluster(b_schoolid)
estimates store spec4
sum ompayoff if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1

*Receiver
ivregress 2sls orpayoff age male refugee astudent braven_sd beyes_sd b_schoolsize f_csize i.bstrata (ftline=treatment), cluster(b_schoolid)
estimates store spec5
sum orpayoff if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1

*Overall
ivregress 2sls otpayoff age male refugee astudent braven_sd beyes_sd b_schoolsize f_csize i.bstrata (ftline=treatment), cluster(b_schoolid)
estimates store spec6
sum otpayoff if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1


*In-Class Cooperation Payoffs

ivregress 2sls cpayoff_in bcpayoff $controls i.bstrata  (ftline=treatment), cluster(b_schoolid)
estimates store spec7
sum cpayoff_in if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1

*Out-School Cooperation Payoffs
ivregress 2sls cpayoff_in age male refugee astudent braven_sd beyes_sd b_schoolsize f_csize i.bstrata  (ftline=treatment), cluster(b_schoolid)
estimates store spec8
sum cpayoff_out if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1

#delimit ;
esttab spec1 spec2 spec3 spec4 spec5 spec6 spec7 spec8 using "$path2/tableD3b.tex",  replace label compress b(%8.3f) se(3) nonumbers keep(ftline) 
order(ftline) 
coeflabels(ftline "Teacher Coverage")
star(* 0.10 ** 0.05 *** 0.01) nonotes s(Cmean N, fmt(%9.2f %9.0g) 
labels("Control Mean" "Observations")) nogaps
mtitles("Sender" "Receiver" "Overall" "Sender" "Receiver" "Overall" "In-Class" "Out-School")  
mgroups("Trust In-Class" "Trust Out-School" "Cooperation", pattern(1 0 0 1 0 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ;
#delimit cr

****************************************
*Table D4: Treatment Effects on Altruism
****************************************

ivregress 2sls fdonate a2 inter1 bdonation_sd $controls i.bstrata  (ftline=treatment), cluster(b_schoolid)
estimates store spec1
sum fdonate if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

ivregress 2sls fdonation_perc a2 inter1 bdonation_sd $controls i.bstrata  (ftline=treatment), cluster(b_schoolid)
estimates store spec2
sum fdonation_perc if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1



#delimit;
esttab spec1 spec2  using "$path2/tableD4.tex",  replace label compress b(%8.3f) se(3) nonumbers keep(ftline a2 inter1) 
order(ftline a2 inter1) 
coeflabels(ftline "Teacher Coverage" a2 "Ethnic Ref." inter1 "Teacher Coverage*Ethnic Ref.")
star(* 0.10 ** 0.05 *** 0.01) nonotes s(Cmean N, fmt(%9.2f %9.0g) 
labels("Control Mean" "Observations")) nogaps eqlabels("")
mtitles ("Willing to Donate" "Fraction Donated")
;
#delimit cr


*****************************************************
*****************************************************
*SECTION E (CONTINUED): Additional Tables and Figures
*****************************************************
*****************************************************

*Replace missing baselines
bysort b_classid: egen max_refshare=max(b_refshare)
replace b_refshare=max_refshare if b_refshare==.

*Generate high refugee share dummy
sum b_refshare,d
gen high_refshare= b_refshare > r(p50)
replace high_refshare=. if b_refshare==. 

*****************************************************************
*Figure E4: HTEs based on fraction of refugees - Social Exclusion (Binary)
*****************************************************************


gen intershare=treatment*high_refshare

*Marginsplot

*Best Friend
reg ffriend treatment##c.b_refshare bfriend  $controls  i.bstrata, cluster(b_schoolid)
testparm treatment##c.b_refshare
margins treatment,  at(b_refshare=(0(0.1)1)) vsquish
marginsplot, xtitle(Class Level Refugee Share) ytitle("Friendship")   graphregion(color(white)) bgcolor(white) ylabel(0.8(.1)1) legend(label(1 "Control") label(2 "Treatment")) legend (order(1 "Control" 2 "Treatment")) name(friend, replace) title("Friendship")
graph export "$path1/figureE4a.png", replace 

*Emotional Support
xi: reg fsupportself treatment##c.b_refshare bsupportself $controls  i.bstrata, cluster(b_schoolid)
testparm treatment##c.b_refshare
margins treatment,  at(b_refshare=(0(0.1)1)) vsquish
marginsplot, xtitle(Class Level Refugee Share) ytitle("Emotional Support")   graphregion(color(white)) bgcolor(white) ylabel(0.5(.1)1) legend(label(1 "Control") label(2 "Treatment")) legend (order(1 "Control" 2 "Treatment")) name(emotional, replace) title("Emotional Support")
graph export "$path1/figureE4b.png", replace 

*Academic Support
xi: reg fstudyself treatment##c.b_refshare bstudyself  $controls  i.bstrata, cluster(b_schoolid)
testparm treatment##c.b_refshare
margins treatment,  at(b_refshare=(0(0.1)1)) vsquish
marginsplot, xtitle(Class Level Refugee Share) ytitle("Academic Support")   graphregion(color(white)) bgcolor(white) ylabel(0.5(.1)1) legend(label(1 "Control") label(2 "Treatment")) legend (order(1 "Control" 2 "Treatment")) name(academic, replace) title("Academic Support")
graph export "$path1/figureE4c.png", replace 



*****************************************************************
*Figure E5: HTEs based on fraction of refugees - Trust
*****************************************************************

reg fs_decision_in treatment##c.b_refshare  $controls i.bstrata, cluster(b_schoolid)
testparm treatment##c.b_refshare
margins treatment,  at(b_refshare=(0(0.1)1)) vsquish
marginsplot, xtitle(Class Level Refugee Share) ytitle("In-Class Trust")   graphregion(color(white)) bgcolor(white) ylabel(0(1)4) legend(label(1 "Control") label(2 "Treatment")) legend (order(1 "Control" 2 "Treatment")) name(friend, replace) title("In-Class Trust")
graph export "$path1/figureE5a.png", replace 


reg fs_decision_out treatment##c.b_refshare $controls i.bstrata, cluster(b_schoolid)
testparm treatment##c.b_refshare
margins treatment,  at(b_refshare=(0(0.1)1)) vsquish
marginsplot, xtitle(Class Level Refugee Share) ytitle("Out-School Trust")   graphregion(color(white)) bgcolor(white) ylabel(0(1)4) legend(label(1 "Control") label(2 "Treatment")) legend (order(1 "Control" 2 "Treatment")) name(friend, replace) title("Out-School Trust")
graph export "$path1/figureE5b.png", replace 


*****************************************************************
*Figure E6: HTEs based on fraction of refugees - Reciprocity
*****************************************************************

reg fr_decision_in_perc treatment##c.b_refshare  $controls i.bstrata, cluster(b_schoolid)
testparm treatment##c.b_refshare
margins treatment,  at(b_refshare=(0(0.1)1)) vsquish
marginsplot, xtitle(Class Level Refugee Share) ytitle("In-Class Reciprocity")   graphregion(color(white)) bgcolor(white) ylabel(0(.1)1) legend(label(1 "Control") label(2 "Treatment")) legend (order(1 "Control" 2 "Treatment")) name(friend, replace) title("In-Class Reciprocity")
graph export "$path1/figureE5c.png", replace 

reg fr_decision_out_perc treatment##c.b_refshare  $controls i.bstrata, cluster(b_schoolid)
testparm treatment##c.b_refshare
margins treatment,  at(b_refshare=(0(0.1)1)) vsquish
marginsplot, xtitle(Class Level Refugee Share) ytitle("Out-School Reciprocity")   graphregion(color(white)) bgcolor(white) ylabel(0(.1)1) legend(label(1 "Control") label(2 "Treatment")) legend (order(1 "Control" 2 "Treatment")) name(friend, replace) title("Out-School Reciprocity")
graph export "$path1/figureE5d.png", replace 
