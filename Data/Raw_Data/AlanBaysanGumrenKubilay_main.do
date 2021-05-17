
*******************************************************************************************************
*******************************************************************************************************
*** Title: Building Social Cohesion in Ethnically Mixed Schools: An Intervention on Perspective Taking
*** Authors: Sule Alan, Ceren Baysan, Elif Kubilay, Mert Gumren

*** Notes: This do-file replicates the main results. 
*******************************************************************************************************
*******************************************************************************************************

do "~/Dropbox/Empathy Project/Empathy Revision/Final QJE Files/Do-Files/AlanBaysanGumrenKubilay_measure_construction.do"

*Define controls
global controls age male refugee astudent braven_sd beyes_sd b_schoolsize f_csize dist1-dist10

*****************************************************************
*Table 1a, 1b and 1c: Balance across Treatment and Control
*****************************************************************

preserve

keep if data_baseline=="1"


*Student Level Charactheristics (Table 1a)


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

 
estpost ttest $studentvars, by(treatment)

estadd matrix eb
estadd matrix p, replace
estimates store balance

esttab balance using "$path2/table1a.tex", cells("count(fmt(0) label(N)) mu_1(fmt(3) label(Control Mean)) mu_2(fmt(3) label(Treatment Mean)) eb(fmt(3) label(Difference (T-C)) star)  se(fmt(3) label(SE)) p(fmt(3) label(p-value))") ///
coeflabels(male "\addlinespace[1mm] Male" ageinm "Age in Months" refugee "Refugee" workingmother "Working Mother" ///
workingfather "Working Father" braven_sd "\addlinespace[1mm] Raven Score" beyes_sd "Eyes Test Score" bmath_sd "Math Score" ///
bturk_sd "Turkish Score"  bsbully_c "\addlinespace[1mm] Proportion Bullied by Peers in Classroom"  ///
bsbully_s "Proportion Bullied by Peers in School" bdonation_sd "Fraction Donated" bdonate "Willingness to Donate" bcoord "Proportion Cooperate" bempathy_pt_sd "Perspective Taking" ///
bempathy_ec_sd "Empathetic Concern" bethnicbias_sd  "Ethnic Bias" bimpulsivity_sd "Impulsivity" bfriend "Having a Friend" bsupportself "Having Emotional Support" bstudyself "Having Academic Support" bnode_in_friend "Friendship Ties (in-degree)" bnode_in_supportself "Emotional Support Ties (in-degree)" bnode_in_studyself "Academic Support Ties (in-degree)") ///
refcat(male "\textbf{Student Demographics:}" braven_sd "\addlinespace[1.5mm] \textbf{Cognitive Tests:}" ///
bsbully_c "\addlinespace[1.5mm] \textbf{Cohesion Indicators:}", nolabel) noobs  star(* 0.10 ** 0.05 *** 0.01)  replace nonumbers
eststo clear

restore


*Teacher and Classroom Level Charactheristics (Table 1b)
preserve

keep if data_baseline=="1"

collapse b_csize b_refshare baseline_tage baseline_tmale baseline_texperience baseline_ttenured btraven_sd bteyes_sd bstrata treatment b_schoolid bhomophily_eth_rh_friend  bhomophily_eth_rh_supportself bhomophily_eth_rh_studyself, by(b_classid)

global classvars b_csize b_refshare bhomophily_eth_rh_friend bhomophily_eth_rh_supportself bhomophily_eth_rh_studyself baseline_tage baseline_tmale baseline_texperience baseline_ttenured btraven_sd bteyes_sd 

local count: word count $classvars
mat eb=J(1,`count',0)
mat p=J(1,`count',0)
mat sd=J(1,`count',0)
loc k=1
foreach i in $classvars { 
	xi: reg `i' treatment i.bstrata, vce(cl b_schoolid) 
mat eb[1,`k']=_b[treatment]
mat p[1,`k']=(2 * ttail(e(df_r), abs(_b[treatment]/_se[treatment])))
loc k = `k'+1
}
matrix colnames eb = $classvars
matrix colnames p =  $classvars
estpost ttest $classvars , by(treatment)
estadd matrix eb
estadd matrix p, replace
estimates store balance


esttab balance using "$path2/table1b.tex", cells("count(fmt(0) label(N)) mu_1(fmt(3) label(Control Mean)) mu_2(fmt(3) label(Treatment Mean)) eb(fmt(3) label(Difference (T-C)) star) se(fmt(3) label(SE)) p(fmt(3) label(p-value))")  ///
coeflabels(b_csize "Classroom Size" b_refshare "Refugee Share" bhomophily_eth_rh_friend "Ethnic Segregation in Friendship Ties" bhomophily_eth_rh_supportself "Ethnic Segregation in Emotional Support Ties"  bhomophily_eth_rh_studyself "Ethnic Segregation in Academic Support Ties"  baseline_tage "Teacher Age in Years" baseline_tmale "Male Teacher" baseline_texperience ///
"Teacher Years of Experience" baseline_ttenured "Tenured Teacher" btraven_sd "Teacher Raven Score" bteyes_sd "Teacher Eyes Test Score") ///
noobs  star(* 0.10 ** 0.05 *** 0.01)  replace nonumbers
eststo clear

restore


*School Level Charactheristics (Table 1c)
preserve

keep if data_baseline=="1"
collapse b_schoolsize n_totalclass bstrata treatment, by(b_schoolid)

global schoolvars b_schoolsize n_totalclass  //

local count: word count $schoolvars
mat eb=J(1,`count',0)
mat p=J(1,`count',0)
mat sd=J(1,`count',0)
loc k=1
foreach i in $schoolvars { 
	xi: reg `i' treatment i.bstrata, r
mat eb[1,`k']=_b[treatment]
mat p[1,`k']=(2 * ttail(e(df_r), abs(_b[treatment]/_se[treatment])))
loc k = `k'+1
} 
matrix colnames eb =  $schoolvars
matrix colnames p =$schoolvars
estpost ttest $schoolvars , by(treatment)
estadd matrix eb
estadd matrix p, replace
estimates store balance


esttab balance using "$path2/table1c.tex", cells("count(fmt(0) label(N)) mu_1(fmt(3) label(Control Mean)) mu_2(fmt(3) label(Treatment Mean)) eb(fmt(3) label(Difference (T-C)) star) se(fmt(3) label(SE)) p(fmt(3) label(p-value))")  ///
coeflabels(b_schoolsize "School Size (3rd and 4th grades only)" n_totalclass "Total Number of 3rd and 4th-grade Classrooms") ///
noobs  star(* 0.10 ** 0.05 *** 0.01)  replace nonumbers
eststo clear

restore


************************************************************************************************
*Figure 2: Cumulative Distribution of Expected and Observed Inter-Ethnic Ties at Baseline
************************************************************************************************ 

*Friendship

preserve

keep if data_baseline=="1"

collapse bexpected_interethnic_friend bobserved_interethnic_friend b_refshare, by(b_classid)

drop if b_refshare==0 
drop if bexpected_interethnic_friend==.

ksmirnov bexpected_interethnic_friend=bobserved_interethnic_friend

local pval:  display %5.3f = r(p_2)
cumul  bexpected_interethnic_friend, gen(cumul1)
cumul  bobserved_interethnic_friend, gen(cumul2)
stack  cumul1  bexpected_interethnic_friend cumul2  bobserved_interethnic_friend  , into(c temp) wide clear
line cumul1 cumul2 temp,sort note("p-value for Kolmogorov-Smirnov Test: `pval'") legend(col(1) label(1 "Expected Inter-Ethnic Tie Proportion") label(2 "Observed Inter-Ethnic Tie Proportion")) title("Friendship") xtitle("")  graphregion(color(white)) bgcolor(white) 

graph save "$path1/baselinenetwork1.gph", replace

restore

*Emotional Support

preserve

keep if data_baseline=="1"
collapse bexpected_interethnic_supprtslf bobserved_interethnic_supprtslf b_refshare, by(b_classid)

drop if b_refshare==0
drop if bexpected_interethnic_supprtslf==.

ksmirnov bexpected_interethnic_supprtslf=bobserved_interethnic_supprtslf

local pval:  display %5.3f = r(p_2)
cumul  bexpected_interethnic_supprtslf, gen(cumul1)
cumul  bobserved_interethnic_supprtslf, gen(cumul2)
stack  cumul1  bexpected_interethnic_supprtslf cumul2  bobserved_interethnic_supprtslf  , into(c temp) wide clear
line cumul1 cumul2 temp,sort note("p-value for Kolmogorov-Smirnov Test: `pval'") legend(col(1) label(1 "Expected Inter-Ethnic Tie Proportion") label(2 "Observed Inter-Ethnic Tie Proportion")) title("Emotional Support") xtitle("") graphregion(color(white)) bgcolor(white) 

graph save "$path1/baselinenetwork2.gph", replace 

restore

*Academic Support

preserve

keep if data_baseline=="1"
collapse bexpected_interethnic_studyself bobserved_interethnic_studyself b_refshare, by(b_classid)

drop if b_refshare==0
drop if bexpected_interethnic_studyself==.

ksmirnov bexpected_interethnic_studyself=bobserved_interethnic_studyself

local pval:  display %5.3f = r(p_2)
cumul  bexpected_interethnic_studyself, gen(cumul1)
cumul  bobserved_interethnic_studyself, gen(cumul2)
stack  cumul1  bexpected_interethnic_studyself cumul2  bobserved_interethnic_studyself  , into(c temp) wide clear
line cumul1 cumul2 temp,sort note("p-value for Kolmogorov-Smirnov Test: `pval'") legend(col(1) label(1 "Expected Inter-Ethnic Tie Proportion") label(2 "Observed Inter-Ethnic Tie Proportion")) title("Academic Support") xtitle("") graphregion(color(white)) bgcolor(white) 

graph save "$path1/baselinenetwork3.gph", replace 

restore


graph combine "$path1/baselinenetwork1.gph" "$path1/baselinenetwork2.gph" "$path1/baselinenetwork3.gph", col(3) xsize(10) graphregion(color(white) lcolor(white) fcolor(white) ) 


erase "$path1/baselinenetwork1.gph" 
erase "$path1/baselinenetwork2.gph" 
erase "$path1/baselinenetwork3.gph"

graph export "$path1/figure2.png", as(png) name("Graph") replace

*****************************************************************
*Figure 3: Baseline Conditions for Refugee Children
*****************************************************************

*In-Class Bullying
reg bsbully_c refugee b_refshare b_schoolsize b_csize i.bstrata dist1-dist10, cluster(b_schoolid)
est store spec1

*Out-Class Bullying
reg bsbully_s refugee b_refshare b_schoolsize b_csize i.bstrata dist1-dist10, cluster(b_schoolid)
est store spec2

*Friendship
reg bfriend refugee b_refshare b_schoolsize b_csize i.bstrata dist1-dist10, cluster(b_schoolid)
est store spec3

*Emotional Support
reg bsupportself refugee b_refshare b_schoolsize b_csize i.bstrata dist1-dist10, cluster(b_schoolid)
est store spec4

*Academic Support
reg bstudyself refugee b_refshare b_schoolsize b_csize i.bstrata dist1-dist10, cluster(b_schoolid)
est store spec5


#delimit ;

coefplot 
(spec1, keep(refugee))
(spec2, keep(refugee))
(spec3, keep(refugee))
(spec4, keep(refugee))
(spec5, keep(refugee))
, horizontal  xline(0) xscale(range (-0.15 0.2))  
xtitle(Point Estimates with 95% CIs) mlabel(cond(@pval<.01, string(@b,"%9.3f") + "***", cond(@pval<.05, string(@b,"%9.3f") + "**", cond(@pval<.1, string(@b,"%9.3f") + "*", string(@b,"%9.3f"))))) format(%8.2f)  mlabposition(12) mlabgap(*2) mlabcolor(navy) ciopts(recast(rcap) color(navy)) color(navy) citop
graphregion(color(white)) lcolor(white) bgcolor(white) 
xline(0, lcolor(black))
legend(off)
ylabel(0.66 "Bullying In-Class" 0.83 "Bullying Out-Class" 1.00 "Friendship" 1.17 "Emotional Support" 1.34 "Academic Support" )
name(figure3,replace)
;

#delimit cr


graph export "$path1/figure3.png", replace


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

*****************************************************************
*Table 2: Treatment Effects on Violence and Anti-Social Behavior
*****************************************************************

*********************************************************
*Table 2, Panel 1:  Peer Violence and Victimization-Diary Records
*********************************************************

preserve

collapse events perpetrator victim b_schoolsize n_class srefshare treatment b_provinceid bstrata dist1-dist10, by(b_schoolid)

gen spill=events-perpetrator

*Perpetrator
reg perpetrator treatment b_schoolsize n_class srefshare i.b_provinceid i.bstrata dist1-dist10, r
estimates store spec1
sum perpetrator if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

*Victimized
reg victim treatment b_schoolsize n_class srefshare i.b_provinceid i.bstrata dist1-dist10,r
estimates store spec2
sum victim if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

*Total Events
reg events treatment b_schoolsize n_class srefshare i.b_provinceid i.bstrata dist1-dist10,r
estimates store spec3
sum events if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

*Spill
reg spill treatment b_schoolsize n_class srefshare i.b_provinceid i.bstrata dist1-dist10, r
estimates store spec4
sum spill if treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1


#delimit ;
esttab spec1 spec2 spec3 spec4 using "$path2/table2a.tex",  replace label compress b(%8.3f) se(3) nonumbers keep(treatment) 
order(empathy) 
coeflabels(treatment "Treatment")
star(* 0.10 ** 0.05 *** 0.01) nonotes s(Cmean N, fmt(%9.3g %9.0g) 
labels("Control Mean" "Observations")) nogaps
mtitles ( "Perpetrated" "Victimized" "Total Events" "Spillover")
;
#delimit cr

restore


**************************************************************************
*Table 2, Panel 2: Student and Teacher Reports of Violence and Antisocial Behavior
**************************************************************************

*In-Class Bullying
xi: reg fsbully_c treatment bsbully_c $controls i.bstrata, cluster(b_schoolid)
est store spec1
sum fsbully_c if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

*Out-Class Bullying
xi: reg fsbully_s treatment bsbully_s $controls i.bstrata, cluster(b_schoolid)
est store spec2
sum fsbully_s if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

*Teacher Behavioral Grade
xi: reg tbully_f treatment bsbully_c $controls i.bstrata, cluster(b_schoolid)
estimates store spec3
sum tbully_f if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1


#delimit ;
esttab spec1 spec2 spec3 using "$path2/table2b.tex",  replace label compress b(%8.3f) se(3) nonumbers keep(treatment) 
coeflabels(treatment "Treatment")
star(* 0.10 ** 0.05 *** 0.01) nonotes s(Cmean N, fmt(%9.2f %9.0g) 
labels("Control Mean" "Observations")) nogaps
mtitles ("Bullying In-Class" "Bullying Out-Class" "Teacher Behavioral Grade")
;
#delimit cr



************************************************************************************************************
*Table 2, Panel 3: Heterogeneous Treatment Effects on Student and Teacher Reports of Violence and Antisocial Behavior
************************************************************************************************************



*In-Class Bullying
reg fsbully_c bsbully_c i.treatment##i.refugee  $controls i.bstrata, cluster(b_schoolid)
margins, dydx(treatment) over(refugee) post
test [1.treatment]0.refugee = [1.treatment]1.refugee 
scalar b2=r(p)
est store spec1
estadd scalar pvalue=b2,  : spec1
sum fsbully_c if  treatment==0 & refugee==1
scalar b3=r(mean)
estadd scalar CRmean=b3,  : spec1
sum fsbully_c if  treatment==0 & refugee==0
scalar b4=r(mean)
estadd scalar CHmean=b4,  : spec1


*Out-School Bullying
reg fsbully_s bsbully_s i.treatment##i.refugee $controls i.bstrata, cluster(b_schoolid)
margins, dydx(treatment) over(refugee) post
test [1.treatment]0.refugee = [1.treatment]1.refugee 
scalar b2=r(p)
est store spec2
estadd scalar pvalue=b2,  : spec2
sum fsbully_s if  treatment==0 & refugee==1
scalar b3=r(mean)
estadd scalar CRmean=b3,  : spec2
sum fsbully_s if  treatment==0 & refugee==0
scalar b4=r(mean)
estadd scalar CHmean=b4,  : spec2

*Teacher Behavioral Grade 
reg tbully_f bsbully_c i.treatment##i.refugee $controls i.bstrata, cluster(b_schoolid)
margins, dydx(treatment) over(refugee) post
test [1.treatment]0.refugee = [1.treatment]1.refugee 
scalar b2=r(p)
est store spec3
estadd scalar pvalue=b2,  : spec3
sum tbully_f if  treatment==0 & refugee==1
scalar b3=r(mean)
estadd scalar CRmean=b3,  : spec3
sum tbully_f if  treatment==0 & refugee==0
scalar b4=r(mean)
estadd scalar CHmean=b4,  : spec3


#delimit ;
esttab spec1 spec2 spec3 using "$path2/table2c.tex",  replace label compress b(%8.3f) se(3) nonumbers 
keep(1.treatment:0.refugee 1.treatment:1.refugee)
order(1.treatment:0.refugee 1.treatment:1.refugee) 
coeflabels(1.treatment:0.refugee "Treatment (Hosts)" 1.treatment:1.refugee "Treatment (Refugees)")
star(* 0.10 ** 0.05 *** 0.01) nonotes stats(pvalue CHmean CRmean N, fmt(%9.3f %9.2f %9.2f %9.0g)
labels("p-value : Hosts=Refugees" "\hline Control Mean (Hosts)" "Control Mean (Refugees)" "Observations")) nogaps eqlabels(none)
mtitles ("Bullying In-Class" "Bullying Out-Class" "Teacher Behavioral Grade")  ;
#delimit cr
eststo clear



******************************************************************
*Table 3: Social Exclusion and Ethnic Segregation in the Classroom
******************************************************************

******************************************************************
*Table 3, Panel 1: Social Exclusion-Binary
******************************************************************

*Friend
xi: reg ffriend treatment bfriend  $controls i.bstrata, cluster(b_schoolid)
est store spec1
sum ffriend if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

*Emotional Support
xi: reg fsupportself treatment bsupportself  $controls i.bstrata, cluster(b_schoolid)
est store spec2
sum fsupportself if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

*Academic Support
xi: reg fstudyself treatment bstudyself $controls i.bstrata, cluster(b_schoolid)
est store spec3
sum fstudyself if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1


#delimit ;
esttab spec1 spec2 spec3 using "$path2/table3a.tex",  replace label compress b(%8.3f) se(3) nonumbers keep(treatment) 
order(treatment) 
coeflabels(treatment "Treatment")
star(* 0.10 ** 0.05 *** 0.01) nonotes s(Cmean N, fmt(%9.2f %9.0g) 
labels("Control Mean" "Observations"))  
mtitles ("Friendship" "Emotional Support"  "Academic Support") nogaps
;
#delimit cr

******************************************************************
*Table 3, Panel 2: Social Exclusion-In-degree Ties
******************************************************************

*Friend
xi: reg fnode_in_friend treatment bnode_in_friend  $controls i.bstrata, cluster(b_schoolid)
est store spec1
sum fnode_in_friend if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

*Emotional Support
xi: reg fnode_in_supportself treatment bnode_in_supportself  $controls i.bstrata, cluster(b_schoolid)
est store spec2
sum fnode_in_supportself if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

*Academic Support
xi: reg fnode_in_studyself treatment bnode_in_studyself  $controls i.bstrata, cluster(b_schoolid)
est store spec3
sum fnode_in_studyself if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1


#delimit ;
esttab spec1 spec2 spec3 using "$path2/table3b.tex",  replace label compress b(%8.3f) se(3) nonumbers keep(treatment) 
order(treatment) 
coeflabels(treatment "Treatment")
star(* 0.10 ** 0.05 *** 0.01) nonotes s( Cmean N, fmt( %9.2f %9.0g) 
labels( "Control Mean" "Observations")) nogaps
mtitles ("Friendship" "Emotional Support" "Academic Support")
;
#delimit cr

******************************************************************
*Table 3, Panel 3: Ethnic Segregation
******************************************************************

preserve


drop if studentstatus=="GONE" | studentstatus=="NEW" 

collapse fhomophily_eth_rh_friend bhomophily_eth_rh_friend ///
fhomophily_eth_rh_supportself bhomophily_eth_rh_supportself ///
fhomophily_eth_rh_studyself bhomophily_eth_rh_studyself  ///
b_schoolsize f_csize dist1-dist11 bstrata  gradetwo f_experimenter1-f_experimenter9 treatment b_schoolid age male, by(f_classid)


global controls2 b_schoolsize f_csize dist1-dist10 gradetwo f_experimenter1-f_experimenter9 age male

*Friendship Segregation
reg fhomophily_eth_rh_friend treatment bhomophily_eth_rh_friend $controls2  i.bstrata, cluster(b_schoolid)
est store spec1
sum fhomophily_eth_rh_friend if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

*Emotional Support Segregation
reg fhomophily_eth_rh_supportself treatment bhomophily_eth_rh_supportself $controls2 i.bstrata, cluster(b_schoolid)
est store spec2
sum fhomophily_eth_rh_supportself if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

*Academic Support Segregation
reg fhomophily_eth_rh_studyself treatment bhomophily_eth_rh_studyself  $controls2 i.bstrata, cluster(b_schoolid)
est store spec3
sum fhomophily_eth_rh_studyself if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1


#delimit ;
esttab spec1 spec2 spec3 using "$path2/table3c.tex",  replace label compress b(%8.3f) se(3) nonumbers keep(treatment) 
order(treatment) 
coeflabels(treatment "Treatment")
star(* 0.10 ** 0.05 *** 0.01) nonotes s(Cmean N, fmt(%9.2f %9.0g) 
labels("Control Mean" "Observations"))  
mtitles ("Friendship" "Emotional Support" "Academic Support") nogaps
;
#delimit cr

restore



*************************************************************
*Table 4: Heterogeneous Treatment Effects on Social Exclusion
*************************************************************

*************************************************************
*Table 4, Panel 1: Social Exclusion-Binary
*************************************************************



*Best Friend
reg ffriend bfriend i.treatment##i.refugee $controls  i.bstrata, cluster(b_schoolid)
margins, dydx(treatment) over(refugee) post
test [1.treatment]0.refugee = [1.treatment]1.refugee 
scalar b2=r(p)
est store spec1
estadd scalar pvalue=b2,  : spec1
sum ffriend if  treatment==0 & refugee==1
scalar b3=r(mean)
estadd scalar CRmean=b3,  : spec1
sum ffriend if  treatment==0 & refugee==0
scalar b4=r(mean)
estadd scalar CHmean=b4,  : spec1



*Emotional Support
reg fsupportself bsupportself i.treatment##i.refugee $controls  i.bstrata, cluster(b_schoolid)
margins, dydx(treatment) over(refugee) post
test [1.treatment]0.refugee = [1.treatment]1.refugee 
scalar b2=r(p)
est store spec2
estadd scalar pvalue=b2,  : spec2
sum fsupportself if  treatment==0 & refugee==1
scalar b3=r(mean)
estadd scalar CRmean=b3,  : spec2
sum fsupportself if  treatment==0 & refugee==0
scalar b4=r(mean)
estadd scalar CHmean=b4,  : spec2


*Academic Support
reg fstudyself bstudyself i.treatment##i.refugee  $controls  i.bstrata, cluster(b_schoolid)
margins, dydx(treatment) over(refugee) post
test [1.treatment]0.refugee = [1.treatment]1.refugee 
scalar b2=r(p)
est store spec3
estadd scalar pvalue=b2,  : spec3
sum fstudyself if  treatment==0 & refugee==1
scalar b3=r(mean)
estadd scalar CRmean=b3,  : spec3
sum fstudyself if  treatment==0 & refugee==0
scalar b4=r(mean)
estadd scalar CHmean=b4,  : spec3



#delimit ;
esttab spec1 spec2 spec3  using "$path2/table4a.tex",  replace label compress b(%8.3f) se(3) nonumbers 
keep(1.treatment:0.refugee 1.treatment:1.refugee)
order(1.treatment:0.refugee 1.treatment:1.refugee)
coeflabels(1.treatment:0.refugee "Treatment (Hosts)" 1.treatment:1.refugee "Treatment (Refugees)")
star(* 0.10 ** 0.05 *** 0.01) nonotes stats(pvalue CHmean CRmean N, fmt(%9.3f %9.2f %9.2f %9.0g)
labels("p-value : Hosts=Refugees" "\hline Control Mean (Hosts)" "Control Mean (Refugees)" "Observations")) nogaps eqlabels(none)
mtitles ("Friendship" "Emotional Support" "Academic Support")  ;
#delimit cr
eststo clear


*************************************************************
*Table 4, Panel 2: Social Exclusion-In-degree Ties
*************************************************************


*Best friend
reg fnode_in_friend bnode_in_friend i.treatment##i.refugee $controls i.bstrata, cluster(b_schoolid)
margins, dydx(treatment) over(refugee) post
test [1.treatment]0.refugee = [1.treatment]1.refugee 
scalar b2=r(p)
est store spec1
estadd scalar pvalue=b2,  : spec1
sum fnode_in_friend if  treatment==0 & refugee==1
scalar b3=r(mean)
estadd scalar CRmean=b3,  : spec1
sum fnode_in_friend if  treatment==0 & refugee==0
scalar b4=r(mean)
estadd scalar CHmean=b4,  : spec1

*Emotional Support
reg fnode_in_supportself bnode_in_supportself i.treatment##i.refugee  $controls i.bstrata, cluster(b_schoolid)
margins, dydx(treatment) over(refugee) post
test [1.treatment]0.refugee = [1.treatment]1.refugee 
scalar b2=r(p)
est store spec2
estadd scalar pvalue=b2,  : spec2
sum fnode_in_supportself if  treatment==0 & refugee==1
scalar b3=r(mean)
estadd scalar CRmean=b3,  : spec2
sum fnode_in_supportself if  treatment==0 & refugee==0
scalar b4=r(mean)
estadd scalar CHmean=b4,  : spec2

*Academic Support
reg fnode_in_studyself bnode_in_studyself i.treatment##i.refugee  $controls i.bstrata, cluster(b_schoolid)
margins, dydx(treatment) over(refugee) post
test [1.treatment]0.refugee = [1.treatment]1.refugee 
scalar b2=r(p)
est store spec3
estadd scalar pvalue=b2,  : spec3
sum fnode_in_studyself if  treatment==0 & refugee==1
scalar b3=r(mean)
estadd scalar CRmean=b3,  : spec3
sum fnode_in_studyself if  treatment==0 & refugee==0
scalar b4=r(mean)
estadd scalar CHmean=b4,  : spec3


#delimit ;
esttab spec1 spec2 spec3  using "$path2/table4b.tex",  replace label compress b(%8.3f) se(3) nonumbers 
keep(1.treatment:0.refugee 1.treatment:1.refugee)
order(1.treatment:0.refugee 1.treatment:1.refugee)
coeflabels(1.treatment:0.refugee "Treatment (Hosts)" 1.treatment:1.refugee "Treatment (Refugees)")
star(* 0.10 ** 0.05 *** 0.01) nonotes stats(pvalue CHmean CRmean N, fmt(%9.3f %9.2f %9.2f %9.0g)
labels("p-value : Hosts=Refugees" "\hline Control Mean (Hosts)" "Control Mean (Refugees)" "Observations")) nogaps eqlabels(none)
mtitles ("Friendship" "Emotional Support" "Academic Support")  ;
#delimit cr
eststo clear


********************************************************
*Table 4, Panel 3: Social Ties with Host Children-Binary
********************************************************


*Best friend
reg fhostfriend  bhostfriend i.treatment##i.refugee $controls i.bstrata, cluster(b_schoolid)
margins, dydx(treatment) over(refugee) post
test [1.treatment]0.refugee = [1.treatment]1.refugee 
scalar b2=r(p)
est store spec1
estadd scalar pvalue=b2,  : spec1
sum fhostfriend if  treatment==0 & refugee==1
scalar b3=r(mean)
estadd scalar CRmean=b3,  : spec1
sum fhostfriend if  treatment==0 & refugee==0
scalar b4=r(mean)
estadd scalar CHmean=b4,  : spec1


*Emotional Support
reg fhostsupportself  bhostsupportself i.treatment##i.refugee $controls i.bstrata, cluster(b_schoolid)
margins, dydx(treatment) over(refugee) post
test [1.treatment]0.refugee = [1.treatment]1.refugee 
scalar b2=r(p)
est store spec2
estadd scalar pvalue=b2,  : spec2
sum fhostsupportself if  treatment==0 & refugee==1
scalar b3=r(mean)
estadd scalar CRmean=b3,  : spec2
sum fhostsupportself if  treatment==0 & refugee==0
scalar b4=r(mean)
estadd scalar CHmean=b4,  : spec2


*Academic Support
reg fhoststudyself  bhoststudyself i.treatment##i.refugee $controls i.bstrata, cluster(b_schoolid)
margins, dydx(treatment) over(refugee) post
test [1.treatment]0.refugee = [1.treatment]1.refugee 
scalar b2=r(p)
est store spec3
estadd scalar pvalue=b2,  : spec3
sum fhoststudyself if  treatment==0 & refugee==1
scalar b3=r(mean)
estadd scalar CRmean=b3,  : spec3
sum fhoststudyself if  treatment==0 & refugee==0
scalar b4=r(mean)
estadd scalar CHmean=b4,  : spec3

#delimit ;
esttab spec1 spec2 spec3  using "$path2/table4c.tex",  replace label compress b(%8.3f) se(3) nonumbers 
keep(1.treatment:0.refugee 1.treatment:1.refugee)
order(1.treatment:0.refugee 1.treatment:1.refugee)
coeflabels(1.treatment:0.refugee "Treatment (Hosts)" 1.treatment:1.refugee "Treatment (Refugees)")
star(* 0.10 ** 0.05 *** 0.01) nonotes stats(pvalue CHmean CRmean N, fmt(%9.3f %9.2f %9.2f %9.0g)
labels("p-value : Hosts=Refugees" "\hline Control Mean (Hosts)" "Control Mean (Refugees)" "Observations")) nogaps eqlabels(none)
mtitles ("Host Friendship" "Host Emotional Support" "Host Academic Support")  ;
#delimit cr
eststo clear



***************************************************
*Table 5: Prosocial Behavior and Expected Payoffs
***************************************************

*****************************************************
*Table 5, Panel 1: Trust, Reciprocity and Cooperation
*****************************************************

*In-Class Trust
reg fs_decision_in treatment $controls i.bstrata, cluster(b_schoolid)
estimates store spec1a
sum fs_decision_in if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

*Out-School Trust
reg fs_decision_out treatment $controls i.bstrata, cluster(b_schoolid)
estimates store spec1b
sum fs_decision_out if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1


quietly reg fs_decision_in treatment  $controls i.bstrata 
est store s1
quietly reg fs_decision_out treatment $controls  i.bstrata 
est store s2
suest s1 s2, cluster(b_schoolid)
test [s1_mean]treatment=[s2_mean]treatment 
estadd scalar p1=r(p),  : spec1b

*In-Class Reciprocity
reg fr_decision_in_perc treatment $controls i.bstrata, cluster(b_schoolid)
estimates store spec2a
sum fr_decision_in_perc if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1

*Out-School Reciprocity
reg fr_decision_out_perc treatment $controls i.bstrata, cluster(b_schoolid)
estimates store spec2b
sum fr_decision_out_perc if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1


quietly reg fr_decision_in_perc treatment  $controls i.bstrata 
est store s1
quietly reg fr_decision_out_perc treatment $controls  i.bstrata 
est store s2
suest s1 s2, cluster(b_schoolid)
test [s1_mean]treatment=[s2_mean]treatment 
estadd scalar p1=r(p),  : spec2b

*In-Class Cooperation
reg fcoordin treatment bcoord $controls i.bstrata, cluster(b_schoolid)
estimates store spec3a
sum fcoordin if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1

*Out-School Cooperation
reg fcoordout treatment $controls i.bstrata, cluster(b_schoolid)
estimates store spec3b
sum fcoordout if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1


quietly reg fcoordin bcoord treatment $controls i.bstrata
est store s1
quietly reg fcoordout treatment $controls i.bstrata
est store s2
suest s1 s2, cluster(b_schoolid)
test [s1_mean]treatment=[s2_mean]treatment 
estadd scalar p1=r(p),  : spec3b



#delimit ;
esttab spec1a spec1b spec2a spec2b  spec3a spec3b using "$path2/table5a.tex",  replace label compress b(%8.3f) se(3) nonumbers keep(treatment) 
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
***********************************************************
*Table 5, Panel 2: Payoffs from Trust and Cooperation Games
***********************************************************

*Trust In-Class Payoffs

*Sender
reg mpayoff treatment $controls i.bstrata, cluster(b_schoolid)
estimates store spec1
sum mpayoff if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1

*Receiver
reg rpayoff treatment $controls i.bstrata, cluster(b_schoolid)
estimates store spec2
sum rpayoff if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1

*Overall
reg tpayoff treatment $controls i.bstrata, cluster(b_schoolid)
estimates store spec3
sum tpayoff if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1


*Trust Out-School Payoffs

*Note: District FE are taken out 

*Sender
reg ompayoff treatment age male refugee astudent braven_sd beyes_sd b_schoolsize f_csize i.bstrata, cluster(b_schoolid)
estimates store spec4
sum ompayoff if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1

*Receiver
reg orpayoff treatment  age male refugee astudent braven_sd beyes_sd b_schoolsize f_csize i.bstrata, cluster(b_schoolid)
estimates store spec5
sum orpayoff if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1

*Overall
reg otpayoff treatment age male refugee astudent braven_sd beyes_sd b_schoolsize f_csize i.bstrata, cluster(b_schoolid)
estimates store spec6
sum otpayoff if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1

**Cooperation In-Class Payoffs

reg cpayoff_in treatment bcpayoff $controls i.bstrata, cluster(b_schoolid)
estimates store spec7
sum cpayoff_in if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1

*Cooperation Out-School Payoffs
*Note: District FE are taken out 
reg cpayoff_out treatment age male refugee astudent braven_sd beyes_sd b_schoolsize f_csize i.bstrata, cluster(b_schoolid)
estimates store spec8
sum cpayoff_out if treatment==0
scalar b1=r(mean)
estadd scalar Cmean=b1

#delimit ;
esttab spec1 spec2 spec3 spec4 spec5 spec6 spec7 spec8 using "$path2/table5b.tex",  replace label compress b(%8.3f) se(3) nonumbers 
keep(treatment) order(treatment) 
coeflabels(treatment "Treatment")
star(* 0.10 ** 0.05 *** 0.01) nonotes s(Cmean N, fmt(%9.2f %9.0g) 
labels("Control Mean" "Observations")) nogaps
mtitles("Sender" "Receiver" "Overall" "Sender" "Receiver" "Overall" "In-Class" "Out-School")  
mgroups("Trust In-Class" "Trust Out-School" "Cooperation", pattern(1 0 0 1 0 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ;
#delimit cr


*****************************************************************
*Table 6: Altruism
*****************************************************************

ge inter1=a2*treatment

*Willingness to Donate
reg fdonate treatment a2 inter1 bdonation_sd $controls i.bstrata, cluster(b_schoolid)
estimates store spec1
sum fdonate if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1

*Fraction Donated
reg fdonation_perc treatment a2 inter1 bdonation_sd $controls i.bstrata, cluster(b_schoolid)
estimates store spec2
sum fdonation_perc if  treatment==0 
scalar b1=r(mean)
estadd scalar Cmean=b1


#delimit;
esttab spec1 spec2  using "$path2/table6.tex",  replace label compress b(%8.3f) se(3) nonumbers keep(treatment a2 inter1) 
order(treatment a2 inter1) 
coeflabels(treatment "Treatment" a2 "Ethnic Reference" inter1 "Treatment*Ethnic Reference")
star(* 0.10 ** 0.05 *** 0.01) nonotes s(Cmean N, fmt(%9.2f %9.0g) 
labels("Control Mean" "Observations")) nogaps eqlabels("")
mtitles ("Willing to Donate" "Fraction Donated")
;
#delimit cr



***************************************************************
*Table 7: Heterogeneous Treatment Effects on Prosocial Behavior
***************************************************************



*Trust In-Class
reg fs_decision_in i.treatment##i.refugee $controls i.bstrata, cluster(b_schoolid)
margins, dydx(treatment) over(refugee) post
test [1.treatment]0.refugee = [1.treatment]1.refugee 
scalar b2=r(p)
est store spec1a
estadd scalar pvalue=b2,  : spec1a
sum fs_decision_in if  treatment==0 & refugee==1
scalar b3=r(mean)
estadd scalar CRmean=b3,  : spec1a
sum fs_decision_in if  treatment==0 & refugee==0
scalar b4=r(mean)
estadd scalar CHmean=b4,  : spec1a

*Trust Out-School
reg fs_decision_out i.treatment##i.refugee $controls i.bstrata, cluster(b_schoolid)
margins, dydx(treatment) over(refugee) post
test [1.treatment]0.refugee = [1.treatment]1.refugee 
scalar b2=r(p)
est store spec1b
estadd scalar pvalue=b2,  : spec1b
sum fs_decision_out if  treatment==0 & refugee==1
scalar b3=r(mean)
estadd scalar CRmean=b3,  : spec1b
sum fs_decision_out if  treatment==0 & refugee==0
scalar b4=r(mean)
estadd scalar CHmean=b4,  : spec1b

*Reciprocity In-Class
reg fr_decision_in_perc i.treatment##i.refugee $controls i.bstrata, cluster(b_schoolid)
margins, dydx(treatment) over(refugee) post
test [1.treatment]0.refugee = [1.treatment]1.refugee 
scalar b2=r(p)
est store spec2a
estadd scalar pvalue=b2,  : spec2a
sum fr_decision_in_perc if  treatment==0 & refugee==1
scalar b3=r(mean)
estadd scalar CRmean=b3,  : spec2a
sum fr_decision_in_perc if  treatment==0 & refugee==0
scalar b4=r(mean)
estadd scalar CHmean=b4,  : spec2a

*Reciprocity Out-School
reg fr_decision_out_perc i.treatment##i.refugee $controls i.bstrata, cluster(b_schoolid)
margins, dydx(treatment) over(refugee) post
test [1.treatment]0.refugee = [1.treatment]1.refugee 
scalar b2=r(p)
est store spec2b
estadd scalar pvalue=b2,  : spec2b
sum fr_decision_out_perc if  treatment==0 & refugee==1
scalar b3=r(mean)
estadd scalar CRmean=b3,  : spec2b
sum fr_decision_out_perc if  treatment==0 & refugee==0
scalar b4=r(mean)
estadd scalar CHmean=b4,  : spec2b

*Cooperation In-Class
reg fcoordin bcoord i.treatment##i.refugee $controls i.bstrata, cluster(b_schoolid)
margins, dydx(treatment) over(refugee) post
test [1.treatment]0.refugee = [1.treatment]1.refugee 
scalar b2=r(p)
est store spec3a
estadd scalar pvalue=b2,  : spec3a
sum fcoordin if  treatment==0 & refugee==1
scalar b3=r(mean)
estadd scalar CRmean=b3,  : spec3a
sum fcoordin if  treatment==0 & refugee==0
scalar b4=r(mean)
estadd scalar CHmean=b4,  : spec3a

*Cooperation Out-School
reg fcoordout i.treatment##i.refugee $controls i.bstrata, cluster(b_schoolid)
margins, dydx(treatment) over(refugee) post
test [1.treatment]0.refugee = [1.treatment]1.refugee 
scalar b2=r(p)
est store spec3b
estadd scalar pvalue=b2,  : spec3b
sum fcoordout if  treatment==0 & refugee==1
scalar b3=r(mean)
estadd scalar CRmean=b3,  : spec3b
sum fcoordout if  treatment==0 & refugee==0
scalar b4=r(mean)
estadd scalar CHmean=b4,  : spec3b

*Willingness to Donate
reg fdonate a2 bdonation_sd i.treatment##i.refugee $controls i.bstrata,cluster(b_schoolid)
margins, dydx(treatment) over(refugee) post
test [1.treatment]0.refugee = [1.treatment]1.refugee 
scalar b2=r(p)
est store spec4
estadd scalar pvalue=b2,  : spec4
sum fdonate if  treatment==0 & refugee==1
scalar b3=r(mean)
estadd scalar CRmean=b3,  : spec4
sum fdonate if  treatment==0 & refugee==0
scalar b4=r(mean)
estadd scalar CHmean=b4,  : spec4


#delimit ;
esttab spec1a spec1b spec2a spec2b spec3a spec3b spec4 using "$path2/table7.tex",  replace label compress b(%8.3f) se(3) nonumbers 
keep(1.treatment:0.refugee 1.treatment:1.refugee)
order(1.treatment:0.refugee 1.treatment:1.refugee)
coeflabels(1.treatment:0.refugee "Treatment (Hosts)" 1.treatment:1.refugee "Treatment (Refugees)")
star(* 0.10 ** 0.05 *** 0.01) nonotes stats(pvalue CHmean CRmean N, fmt(%9.3f %9.2f %9.2f %9.0g)
labels("p-value : Hosts=Refugees" "\hline Control Mean (Hosts)" "Control Mean (Refugees)" "Observations")) nogaps eqlabels(none)
mtitles ("In-Class" "Out-School" "In-Class" "Out-School" "In-Class" "Out-School" "Willing to Donate")
mgroups("Trust" "Reciprocity" "Cooperation" "Altruism", pattern(1 0 1 0 1 0 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))  ;
#delimit cr
eststo clear



**************************************************************
*Table 8: Heterogeneous Treatment Effects on Achievement Tests
**************************************************************

*Turkish - Host
reg fturk_sd treatment bturk_sd $controls i.bstrata if refugee==0,cluster(b_schoolid)
estimates store spec1

*Turkish - Refugee
reg fturk_sd treatment bturk_sd $controls i.bstrata if refugee==1,cluster(b_schoolid)
estimates store spec2

quietly reg fturk_sd treatment bturk_sd $controls i.bstrata if refugee==0
est store s1
quietly reg fturk_sd treatment bturk_sd $controls i.bstrata if refugee==1
est store s2
suest s1 s2, cluster(b_schoolid)
test [s1_mean]treatment=[s2_mean]treatment 
estadd scalar p1=r(p),  : spec2


*Math - Host
reg fmath_sd treatment bmath_sd $controls i.bstrata if refugee==0,cluster(b_schoolid)
estimates store spec3

*Math - Refugee
reg fmath_sd treatment bmath_sd $controls i.bstrata if refugee==1,cluster(b_schoolid)
estimates store spec4

quietly reg fmath_sd treatment bmath_sd $controls i.bstrata if refugee==0
est store s1
quietly reg fmath_sd treatment bmath_sd $controls i.bstrata if refugee==1
est store s2
suest s1 s2, cluster(b_schoolid)
test [s1_mean]treatment=[s2_mean]treatment 
estadd scalar p1=r(p),  : spec4

#delimit ;
esttab spec1 spec2 spec3 spec4 using "$path2/table8.tex",  replace label compress b(%8.3f) se(3) nonumbers keep(treatment) 
order(treatment) coeflabels(treatment "Treatment")
star(* 0.10 ** 0.05 *** 0.01) nonotes s(p1 N, fmt(%9.3f %9.0g) 
labels("p-value : Hosts=Refugees" "\hline Observations")
layout( "\multicolumn{2}{S}{@}" @ )) nogaps
mtitles ("Host" "Refugee" "Host" "Refugee")
mgroups("Turkish" "Math", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
substitute("&\multicolumn{2}{S}{" "\multicolumn{2}{c}{") ;
#delimit cr



****************************************************
*Figure 4: Treatment Effects on Potential Mechanisms
****************************************************


*Norms

reg desnorms_sd treatment $controls i.bstrata if refugee==0, cluster(b_schoolid)
estimates store spec1a

reg desnorms_sd treatment $controls i.bstrata if refugee==1, cluster(b_schoolid)
estimates store spec1b

*Ethnic Bias

reg fethnicbias_sd treatment bethnicbias_sd $controls i.bstrata if refugee==0, cluster(b_schoolid)
estimates store spec2a

reg fethnicbias_sd treatment bethnicbias_sd $controls i.bstrata if refugee==1, cluster(b_schoolid)
estimates store spec2b

*Perspective taking

reg fempathy_pt_sd treatment bempathy_pt_sd $controls i.bstrata  if refugee==0, cluster(b_schoolid)
estimates store spec3a

reg fempathy_pt_sd treatment bempathy_pt_sd $controls i.bstrata  if refugee==1, cluster(b_schoolid)
estimates store spec3b

*Impulsivity

reg fimpulsivity_sd treatment bimpulsivity_sd $controls i.bstrata  if refugee==0, cluster(b_schoolid)
estimates store spec4a

reg fimpulsivity_sd treatment bimpulsivity_sd $controls i.bstrata  if refugee==1, cluster(b_schoolid)
estimates store spec4b

*Empathetic Concern

reg fempathy_ec_sd treatment bempathy_ec_sd $controls i.bstrata  if refugee==0, cluster(b_schoolid)
estimates store spec5a

reg fempathy_ec_sd treatment bempathy_ec_sd $controls i.bstrata  if refugee==1, cluster(b_schoolid)
estimates store spec5b


*Eyes Test

reg feyes_sd  treatment $controls i.bstrata if refugee==0, cluster(b_schoolid)
estimates store spec6a

reg feyes_sd  treatment $controls i.bstrata if refugee==1, cluster(b_schoolid)
estimates store spec6b


#delimit ;

coefplot 
(spec1a, keep(treatment))
(spec2a, keep(treatment))
(spec3a, keep(treatment))
(spec4a, keep(treatment))
(spec5a, keep(treatment))
(spec6a, keep(treatment))

|| 
(spec1b, keep(treatment))
(spec2b, keep(treatment))
(spec3b, keep(treatment))
(spec4b, keep(treatment))
(spec5b, keep(treatment))
(spec6b, keep(treatment))

||
, horizontal  xline(0) xscale(range (-0.15 0.35))  
xtitle(Point Estimates with 95% CIs) 
mlabel(cond(@pval<.01, string(@b,"%9.3f") + "***", cond(@pval<.05, string(@b,"%9.3f") + "**", cond(@pval<.1, string(@b,"%9.3f") + "*", string(@b,"%9.3f"))))) format(%8.2f)  mlabposition(12) mlabgap(*2) mlabcolor(navy) ciopts(recast(rcap) color(navy)) color(navy) citop
byopts(legend(off)
graphregion(color(white)) graphregion(color(white)) )
lcolor(white) bgcolor(white)
xline(0, lcolor(black))
subtitle(,color(white) bcolor(navy))
bylabels("Host" "Refugee")
ylabel(0.63 "Behavioral Norms" 0.79 "Ethnic Bias" 0.94 "Perspective Taking" 1.07 "Impulsivity" 1.22 "Empathetic Concern" 1.36 "Eyes Test")
name(figure2,replace)
;

#delimit cr


graph export "$path1/figure4.png", replace
