
*******************************************************************************************************
*******************************************************************************************************
*** Title: Building Social Cohesion in Ethnically Mixed Schools: An Intervention on Perspective Taking
*** Authors: Sule Alan, Ceren Baysan, Elif Kubilay, Mert Gumren

*** Notes: This do-file prepares the data for analysis.
*** We proceed in the following order:
	**PART 1: Generate Variables
	**PART 2: Standardization
*******************************************************************************************************
*******************************************************************************************************

clear

set more 1

*Define Paths

global path "~/Dropbox/Empathy Project/Empathy Revision/Final QJE Files/Data Files"
global path1 "~/Dropbox/Empathy Project/Empathy Revision/Final QJE Files/Paper Source Files/figures"
global path2 "~/Dropbox/Empathy Project/Empathy Revision/Final QJE Files/Paper Source Files/tables"


use "$path/AlanBaysanGumrenKubilay_final.dta"

*****************************************************************
*****************************************************************
*PART 1: GENERATE VARIABLES TO BE USED IN ANALYSIS
*****************************************************************
*****************************************************************

************************************************************************
*Construct strata based on province id and baseline official absenteeism 
*************************************************************************

preserve

keep if studentstatus=="OLD" | studentstatus=="GONE" | studentstatus=="CLASS DISTRIBUTED"


*Genereate school level abseenteism from official records 
by b_schoolid b_classid, sort: gen nvals = _n == 1 
bysort b_schoolid: egen absent_sumschool = total(babsent_official) if nvals==1
bysort b_schoolid: egen max=max(absent_sumschool) 
drop absent_sumschool
rename max absent_sumschool

*Generate number of students per school
bysort b_schoolid: egen classsize_sumschool = total(bclasssize_official) if nvals==1
bysort b_schoolid: egen max=max(classsize_sumschool) 
drop classsize_sumschool
rename max classsize_sumschool

*Generate share of absent during the experiment by official records
gen absenteeism_school=absent_sumschool/classsize_sumschool

keep b_schoolid absenteeism_school b_provinceid treatment
duplicates drop b_schoolid, force
sum absenteeism_school
egen absent_tert = xtile(absenteeism_school), by(b_provinceid) nq(3)
gen bstrata = b_provinceid*10 + absent_tert
tempfile strata

save `strata'
tab absent_tert

restore

merge m:1 b_schoolid using `strata', nolabel nogenerate

*****************************************************************
*Drop classes with no experiment

drop if studentstatus=="NO EXPERIMENT" | studentstatus=="CLASS DISTRIBUTED"

*Official school size
ge b_schoolsize=bstudentnum_2+bstudentnum_3
bysort b_schoolid: egen b_ssize=max(b_schoolsize)
drop b_schoolsize
rename b_ssize b_schoolsize

*School level official refugee share (at baseline)
ge srefshare=(bactive_syrian_2+bactive_syrian_3)/b_schoolsize

*Strata
tab bstrata, gen(bstr)

*Number of project classrooms in each school
by b_schoolid b_classid, sort: gen n_class = _n == 1 
by b_schoolid: replace n_class = sum(n_class)
by b_schoolid: replace n_class = n_class[_N]


*Number of total classrooms in each school
destring bclassnum_2 bclassnum_3,replace force
gen n_totalclass=bclassnum_2+bclassnum_3
bysort b_schoolid: egen max=max(n_totalclass) 
drop n_totalclass
rename max n_totalclass


*District dummies
tab b_districtid, gen(dist)

*Grade Dummy
gen gradetwo=1 if grade1==2
replace gradetwo=0 if grade1!=2 & grade1!=.


*Baseline class size
bysort b_classid: egen fnewcomersum=sum(fnewcomer)
bysort b_classid: egen b_csize=count(_n) 
replace b_csize=b_csize-fnewcomersum 

*Endline class size
bysort f_classid: egen fgonesum=sum(fgone) 
bysort f_classid: egen f_csize=count(_n)
replace f_csize=f_csize-fgonesum
replace f_csize=. if fgone==1 

*Baseline class level refugee share

gen n=1 if fnewcomer!=1 & refugee==1
bysort b_classid: egen b_refsum = sum(n)
gen b_refshare=b_refsum/b_csize
replace b_refshare=. if fnewcomer==1
drop n

*Endline class level refugee share

gen n=1 if fgone!=1 & refugee==1
bysort f_classid: egen f_refsum = sum(n)
gen f_refshare=f_refsum/f_csize
replace f_refshare=. if fgone==1
drop n

*Baseline class level male share

gen n=1 if fnewcomer!=1 & male==1
bysort b_classid: egen b_malesum = sum(n)
gen b_maleshare=b_malesum/b_csize
replace b_maleshare=. if fnewcomer==1
drop n
*Endline class level male share

gen n=1 if fgone!=1 & male==1
bysort f_classid: egen f_malesum = sum(n)
gen f_maleshare=f_malesum/f_csize
replace f_maleshare=. if fgone==1
drop n


*Baseline and Endline friend/support self/study self dummy (out-degree) 

foreach i in friend supportself studyself {
	
gen b`i'=1 if bnode_out_`i'!=0 & bnode_out_`i'!=.
replace b`i'=0 if bnode_out_`i'==0

gen f`i'=1 if fnode_out_`i'!=0 & fnode_out_`i'!=.
replace f`i'=0 if fnode_out_`i'==0
}


*Baseline and Endline Host friend/support self/study self dummy (out-degree) 

foreach i in friend supportself studyself {
	
ge b`i'_total_host=bnode_out_`i'-b`i'_total_refugee

gen bhost`i'=1 if b`i'_total_host!=0 & b`i'_total_host!=.
replace bhost`i'=0 if b`i'_total_host==0


ge f`i'_total_host=fnode_out_`i'-f`i'_total_refugee

gen fhost`i'=1 if f`i'_total_host!=0 & f`i'_total_host!=.
replace fhost`i'=0 if f`i'_total_host==0

}

*************
*Reciprocity
*************

*In-Class
gen fr_decision1_in_perc=fr_decision1_in/3
gen fr_decision2_in_perc=fr_decision2_in/6
gen fr_decision3_in_perc=fr_decision3_in/9
gen fr_decision4_in_perc=fr_decision4_in/12

egen fr_decision_in_perc=rowmean(fr_decision1_in_perc fr_decision2_in_perc fr_decision3_in_perc fr_decision4_in_perc)

*Out-School
gen fr_decision1_out_perc=fr_decision1_out/3
gen fr_decision2_out_perc=fr_decision2_out/6
gen fr_decision3_out_perc=fr_decision3_out/9
gen fr_decision4_out_perc=fr_decision4_out/12

egen fr_decision_out_perc=rowmean(fr_decision1_out_perc fr_decision2_out_perc fr_decision3_out_perc fr_decision4_out_perc)

**********
*Altruism
**********

egen fdonation=rowtotal(fdonation_a2 fdonation_a1), missing

*Fraction Donated
gen fdonation_perc=fdonation/4

*Ethnicity reference dummy
ge a2=1 if fdonation_a2!=. 
replace a2=0 if fdonation_a1!=.

*Willingness to donate at Endline
ge fdonate=0 if fdonation!=.
replace fdonate=1 if fdonation>0 & fdonation!=.

*Willingness to donate at Baseline
ge bdonate=0 if bdonation!=.
replace bdonate=1 if bdonation>0 & bdonation!=.

*Teacher Implementation Intensity
replace ftline=0 if treatment==0
replace ftline=ftline/10





*****************************************
*Variables for Simulated Payoff Analysis
*****************************************


******************************************************
* Calculating per capita payoffs for in-class trust 
******************************************************

sum f_classid
scalar ctotal=r(max)

*Expected payoffs for sender

       bysort f_classid: egen mreturn1=mean(fr_decision1_in)
	   bysort f_classid: egen mreturn2=mean(fr_decision2_in)
	   bysort f_classid: egen mreturn3=mean(fr_decision3_in)
	   bysort f_classid: egen mreturn4=mean(fr_decision4_in)
	   
	   ge mpayoff=4 if fs_decision_in==0
	   replace mpayoff=3+mreturn1 if fs_decision_in==1
	   replace mpayoff=2+mreturn2 if fs_decision_in==2
	   replace mpayoff=1+mreturn3 if fs_decision_in==3
	   replace mpayoff=0+mreturn4 if fs_decision_in==4	   

*Expected payoffs for Reciprocity   

count if fs_decision_in!=. & f_classid==1
scalar total=r(N)

forvalues j=0(1)4{
count if fs_decision_in==`j' & f_classid==1
scalar total`j'=r(N)
}

*generating classroom level probabilities of sent amounts
forvalues j=0(1)4{
	scalar p`j'=total`j'/total 
}
 
ge rpayoff=4+(p1*(3-fr_decision1_in)+p2*(6-fr_decision2_in)+p3*(9-fr_decision3_in)+p4*(12-fr_decision4_in)) if f_classid==1 /*expected payoff of receiver in class 1*/

capture program drop trust
program define trust
local i = 2
       while `i' <= ctotal {
	   
*Total number of students who played the game 	   
count if fs_decision_in!=. & f_classid==`i'
scalar total=r(N)

*Total number of students who sent 0,1,2,3 and 4
forvalues j=0(1)4{
count if fs_decision_in==`j' & f_classid==`i'
scalar total`j'=r(N)
}

*generating classroom level probabilities of sent amounts
forvalues j=0(1)4{
	scalar p`j'=total`j'/total 
}
 

replace rpayoff=4+(p1*(3-fr_decision1_in)+p2*(6-fr_decision2_in)+p3*(9-fr_decision3_in)+p4*(12-fr_decision4_in)) if f_classid==`i'
local i = `i' + 1
         }
	end
	trust 1
	
	
ge tpayoff=0.5*mpayoff+0.5*rpayoff  /*expected total payoff*/



************************************************************
* Calculating per capita payoffs for out-school trust
************************************************************

*Note that control actions are used for OUT probabilities. Sending and receiving is not to a classroom so all calculations 
*are assumed that sending and receiving is to and from student's district

*Expected payoff of sender

	   bysort b_districtid: egen mreturn1_c=mean(fr_decision1_out) if treatment==0
	   bysort b_districtid: egen mreturn2_c=mean(fr_decision2_out) if treatment==0
	   bysort b_districtid: egen mreturn3_c=mean(fr_decision3_out) if treatment==0
	   bysort b_districtid: egen mreturn4_c=mean(fr_decision4_out) if treatment==0
	   
	   bysort b_districtid: egen mreturn_c1=mean(mreturn1_c) 
	   bysort b_districtid: egen mreturn_c2=mean(mreturn2_c) 
	   bysort b_districtid: egen mreturn_c3=mean(mreturn3_c) 
	   bysort b_districtid: egen mreturn_c4=mean(mreturn4_c) 
	   
	
	   ge ompayoff=4 if fs_decision_out==0
	   replace ompayoff=3+mreturn_c1 if fs_decision_out==1
	   replace ompayoff=2+mreturn_c2 if fs_decision_out==2
	   replace ompayoff=1+mreturn_c3 if fs_decision_out==3
	   replace ompayoff=0+mreturn_c4 if fs_decision_out==4	  
	
*Expected payoff of receiver 
	
sum b_districtid if treatment==0
scalar dtotal=r(max)

*Total number of students who played the game 
count if fs_decision_out!=. & b_districtid==1 & treatment==0
scalar total=r(N)

*Total number of students who sent 0,1,2,3 and 4
forvalues j=0(1)4{
count if fs_decision_out==`j' & b_districtid==1 & treatment==0
scalar total`j'=r(N)
}


*generating classroom level probabilities of sent amounts
forvalues j=0(1)4{
	scalar p`j'=total`j'/total 
}
 

ge orpayoff=4+(p1*(3-fr_decision1_out)+p2*(6-fr_decision2_out)+p3*(9-fr_decision3_out)+p4*(12-fr_decision4_out)) if b_districtid==1

	capture program drop trust
program define trust
local i = 2

       while `i' <= dtotal {
	   
count if fs_decision_out!=. & b_districtid==`i' & treatment==0
scalar total=r(N)

forvalues j=0(1)4{
count if fs_decision_out==`j' & b_districtid==`i' & treatment==0
scalar total`j'=r(N)
}

*generating classroom level probabilities of sent amounts
forvalues j=0(1)4{
	scalar p`j'=total`j'/total 
}
	
replace orpayoff=4+(p1*(3-fr_decision1_out)+p2*(6-fr_decision2_out)+p3*(9-fr_decision3_out)+p4*(12-fr_decision4_out)) if b_districtid==`i'
local i = `i' + 1
         }
	end
	trust 1

ge otpayoff=0.5*ompayoff+0.5*orpayoff /*expected total payoff*/


******************************************************************
* Calculating per capita payoffs for in-class in Cooperation Game
******************************************************************
*Note that control actions are used for OUT probabilities as above.

tab bcoord if f_classid==1, matcell(freq) 

*generating classroom level probabilities 
scalar p0=freq[1,1]/(freq[1,1]+freq[2,1]) /*prob of not cooperating*/
scalar p1=freq[2,1]/(freq[1,1]+freq[2,1]) /*prob of cooperating*/
	
gen     bcpayoff_in=(p0*0+p1*6) if f_classid==1 & bcoord==1 /*expected payoff if you cooperate*/
replace bcpayoff_in=(p0*3+p1*9) if f_classid==1 & bcoord==0 /*expected payoff if you don't cooperate*/
	
	
capture program drop cooperate
program define cooperate
local i = 2
       while `i' <= ctotal {
	   
	   tab bcoord if f_classid==`i', matcell(freq) 
	   
	scalar p0=freq[1,1]/(freq[1,1]+freq[2,1]) /*prob of not cooperating*/
	scalar p1=freq[2,1]/(freq[1,1]+freq[2,1]) /*prob of cooperating*/
	
	replace bcpayoff_in=(p0*0+p1*6) if f_classid==`i' & bcoord==1
	replace bcpayoff_in=(p0*3+p1*9) if f_classid==`i' & bcoord==0

local i = `i' + 1
         }
	end
	cooperate 1
	
*Endline
tab fcoordin if f_classid==1, matcell(freq) 

*generating classroom level probabilities 
scalar p0=freq[1,1]/(freq[1,1]+freq[2,1]) /*prob of not cooperating*/
scalar p1=freq[2,1]/(freq[1,1]+freq[2,1]) /*prob of cooperating*/
	
gen     cpayoff_in=(p0*0+p1*6) if f_classid==1 & fcoordin==1 /*expected payoff if you cooperate*/
replace cpayoff_in=(p0*3+p1*9) if f_classid==1 & fcoordin==0 /*expected payoff if you don't cooperate*/
	
	
capture program drop cooperate
program define cooperate
local i = 2
       while `i' <= ctotal {
	   
	   tab fcoordin if f_classid==`i', matcell(freq) 
	   
	scalar p0=freq[1,1]/(freq[1,1]+freq[2,1]) /*prob of not cooperating*/
	scalar p1=freq[2,1]/(freq[1,1]+freq[2,1]) /*prob of cooperating*/
	
	replace cpayoff_in=(p0*0+p1*6) if f_classid==`i' & fcoordin==1
	replace cpayoff_in=(p0*3+p1*9) if f_classid==`i' & fcoordin==0

local i = `i' + 1
         }
	end
	cooperate 1
	
******************************************************************
* Calculating per capita payoffs for out-school in Cooperation Game
******************************************************************

*Expected payoff


tab fcoordout if b_districtid==1 & treatment==0, matcell(freq) 
 
scalar p0=freq[1,1]/(freq[1,1]+freq[2,1])
scalar p1=freq[2,1]/(freq[1,1]+freq[2,1])

gen     cpayoff_out=(p0*0+p1*6) if b_districtid==1 & fcoordout==1
replace cpayoff_out=(p0*3+p1*9) if b_districtid==1 & fcoordout==0

capture program drop cooperate
program define cooperate
local i = 2

       while `i' <= dtotal {
	   
	   tab fcoordout if b_districtid==`i' & treatment==0, matcell(freq) 
	   
	scalar p0=freq[1,1]/(freq[1,1]+freq[2,1])
	scalar p1=freq[2,1]/(freq[1,1]+freq[2,1])
	
	replace cpayoff_out=(p0*0+p1*6) if b_districtid==`i' & fcoordout==1
	replace cpayoff_out=(p0*3+p1*9) if b_districtid==`i' & fcoordout==0
local i = `i' + 1
         }
	end
	cooperate 1
	
	
*****************************************************************
*****************************************************************
*PART 2: STANDARDIZATION
*****************************************************************
*****************************************************************
*Note: Use control mean and sd 

*********	
*Student* 
*********

foreach i in beyes_correct feyes_correct bempathy_pt fempathy_pt bempathy_ec fempathy_ec bdonation braven bimpulsivity fimpulsivity bethnicbias fethnicbias fturk_correct turk_correct fmath_correct math_correct desnorms{
	sum `i' if treatment==0
	gen `i'_sd=.
	replace `i'_sd=(`i'-r(mean))/r(sd)
}
		rename fmath_correct_sd fmath_sd
		rename math_correct_sd  bmath_sd
		rename fturk_correct_sd fturk_sd
		rename turk_correct_sd  bturk_sd
		rename beyes_correct_sd beyes_sd
		rename feyes_correct_sd feyes_sd

		
*********	
*Teacher* 
*********

foreach var of varlist bteyes btraven {
	preserve
	collapse `var' treatment, by(b_classid)
	sum `var' if treatment==0
	local mean1 = r(mean) 
	local sd1 = r(sd)
	restore
	gen `var'_sd=(`var'-`mean1')/`sd1'
}


foreach var of varlist fteyes {
	preserve
	collapse `var' treatment, by(f_classid)
	sum `var' if treatment==0
	local mean1 = r(mean) 
	local sd1 = r(sd)
	restore
	gen `var'_sd=(`var'-`mean1')/`sd1'
}

