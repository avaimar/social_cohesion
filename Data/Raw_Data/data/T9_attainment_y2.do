****REGRESSIONS****
** INSTRUMENT FOR OFFERED WITH TREATMENT DUMMY SINCE NOT ALL "PACKAGED" STUDENTS ACTUALLY GET AN OFFER
** EDITED 9/21: ADD NSC DATA TO MEASURE TRANSFERS AND DEGREE RECEIPT AT OTHER INSTITUTIONS ;

#d;

set more off; 

gl dir = " ";
gl original = "${dir}\data\original";
gl intermediate = "${dir}\data\intermediate";
gl output = "${dir}\output";
gl programs = "${dir}\programs\callable_programs";

cd "${dir}\programs";


* CLEAN AND FORMAT NSC DATA ;
import excel "${original}\NSC_101117.xlsx", sheet("_003040st_290033_DETLRPT_SE_090") firstrow case(lower) clear;

rename youruniqueidentifier ResearchID;
rename collegecodebranch opeid; 
rename collegename name; 

replace name = proper(name); 

* SEMESTER START/END MONTH AND YEAR;
gen enrl_st_year = real(substr(enrollmentbegin,1,4)); 
gen enrl_st_month = real(substr(enrollmentbegin,5,2));
gen enrl_end_year = real(substr(enrollmentend,1,4)); 
gen enrl_end_month = real(substr(enrollmentend,5,2)); 

* DROP PRE-TREATMENT OBS (NOT CONSISTENTLY RELIABLE); 
drop if enrl_st_year<2015;

* INDICATORS FOR SEMESTER AND YEAR OF ENROLLMENT;  
gen spring = inlist(enrl_st_month,1,2,3,4); 
gen summer = inlist(enrl_st_month,5,6,7); 
gen fall = inlist(enrl_st_month,8,9,10,11,12); 

gen year = enrl_st_year; 

* DEAL WITH SMALL NUMBER OF OBS WITH ENROLLMENT SPANNING MULTIPLE TRADITIONAL SEMESTERS;
preserve;

	keep if enrl_st_year <enrl_end_year & enrl_end_month >=4 & enrl_end_month<.;
	drop spring summer fall year; 
	gen year = enrl_end_year; 
	gen spring = inlist(enrl_end_month,3,4,5); 
	gen summer = inlist(enrl_end_month,6,7,8,9); 
	gen fall = inlist(enrl_end_month,10,11,12); 
	
	save scrap, replace; 
	
restore;

append using scrap; 
erase scrap.dta; 
	
gen sem_cat = 1 if spring ==1; replace sem_cat = 2 if summer == 1; replace sem_cat = 3 if fall == 1; 
lab define sem_cat 1 "spring" 2 "summer" 3 "fall"; lab val sem_cat sem_cat; 
drop spring summer fall; 

drop if inlist(sem_cat,1,2) & year == 2015;
drop if sem_cat ==3 & year == 2017;

drop enrollmentbegin enrollmentend enrl_st_* enrl_end_*;

duplicates drop; 

* INDICATORS FOR ENROLLMENT LOCATION (FOR NOW JUST SECTOR AND LEVEL); 
gen enrl_cca = name == ""; /* OMITTED TO PRESERVE CCA CONFIDENTIALITY */
gen enrl_pub4 = enrl_cca ==0 &  year4year == "4" & publicprivate == "Public";
gen enrl_pub2 = enrl_cca ==0 & (year4year == "2" | year4year == "L") & publicprivate == "Public"; 
gen enrl_priv4 = enrl_cca ==0 & year4year == "4"  & publicprivate == "Private"; 
gen enrl_priv2 = enrl_cca ==0 & (year4year == "2" | year4year == "L") & publicprivate == "Private"; 
 
* DATE OF GRADUATION; 
gen grad_year = real(substr(graduationdate,1,4)); 
replace year = grad_year if year == . & graduated == "Y"; 
gen grad_month = real(substr(graduationdate,5,2)); 
replace sem_cat = 1 if inlist(grad_month,1,2,3,4,5,6); 
replace sem_cat = 2 if inlist(grad_month,7,8,9); 
replace sem_cat = 3 if inlist(grad_month,10,11,12); 
drop graduationdate grad_month; 

* DROP PRE-TREATMENT OBS (NOT CONSISTENTLY RELIABLE); 
drop if year<2015; drop if year == 2015 & inlist(sem,1,2);

drop if recordfoundyn == "N"; 

* DEGREE CATEGORY;
gen degree_cat = 0;
foreach d in "SHORT TERM CERTIFICATE"  "ADDICTION COUNSELOR - SHORT TERM TECHNICAL CERTIFICATE" "ONE YEAR TECHNICAL CERTIFICATE" "SHORT TERM TECHNICAL CERTIFICATE"
	 {;
	replace degree_cat = 1 if degreetitle == "`d'"; 
};

replace degree_cat = 2 if substr(degreetitle,1,3) == "CER" | degreetitle == "DIPLOMA" | degreetitle == "52.0101 BASIC BUSINESS"; 
replace degree_cat = 2 if degreetitle == "" & graduated == "Y" & degreemajor1 == "" & year4year == "L"; 
replace degree_cat = 2 if degreetitle == "" & graduated == "Y" & name == "Wyotech - Blairsville"; 

foreach d in "ASSOCIATE OF ARTS" "ASSOC OF ARTS"  "ASSOCIATE IN GENERAL EDUCATION" "ASSOCIATE IN ARTS" "ASSOCIATE OF ARTS (AA)" "OHIO TRANSFER MODULE" AA {;
	replace degree_cat = 3 if degreetitle == "`d'"; 
};

foreach d in "ASSOCIATE OF SCIENCE" "ASSOC OF SCIENCE" "ASSOCIATE IN SCIENCE" "ASSOCIATE OF SCIENCE (AWARDED)" {; 
	replace degree_cat = 4 if degreetitle == "`d'"; 
};

foreach d in "AAS"  "AAHS"  "ASSOCIATE IN APPLIED SCIENCE" "ASSOCIATE OF APPLIED BUSINESS" "ASSOCIATE OF APPLIED SCIENCE" "ASSOCIATE IN SCIENCE IN NURSING" 
	"ASSOCIATE OF APPLIED SCIENCE IN CRIMINAL JUSTICE" "ASSOCIATE OF OCCUPATIONAL STUDIES" "ASSOCIATE OF TECHNICAL STUDIES" "ASSOCIATES" AAB {; 
	replace degree_cat = 5 if degreetitle == "`d'"; 
};

foreach d in "ACHELOR OF ARTS" "B.S. HEALTH & SPORT STUDIES" "B.S. IN KINESIOLOGY & HEALTH" BA BS BSE BSN "52.0901 HOSPITALITY ADMINISTRN" "(B.A.)BACHELOR OF ARTS" 
	BASW BBA {;
	replace degree_cat = 6 if degreetitle == "`d'"; 
};

replace degree_cat = 6 if substr(degreetitle, 1,4) == "BACH" | substr(degreetitle,1,8) == "BACHELOR" | substr(degreetitle,1,2) == "BS" ;
replace degree_cat = 6 if degreetitle == "" & graduated == "Y" & degreemajor1 == "" & year4year == "4"; 


replace degree_cat = 7 if degreetitle == "POST DEGREE CERTIFICATE"; 

replace degree_cat = 8 if substr(degreetitle,1,6) == "MASTER" | degreetitle == "DOCTOR OF PHILOSOPHY"; 

bysort ResearchID (year sem_cat ): gen classlevel_1 = classlevel[_n-1] if ResearchID == ResearchID[_n-1];
replace degree_cat = 3 if degreetitle == "" & graduated == "Y" & degreemajor1 == "" & year4year == "2" & (classlevel_1 == "S" | classlevel_1 == "J");
replace degree_cat = 2 if degreetitle == "" & graduated == "Y" & degreemajor1 == "" & year4year == "2" & classlevel_1 == "C";
replace degree_cat = 5 if degreetitle == "" & graduated == "Y" & degreemajor1 == "" & year4year == "2" & classlevel_1 == "A";

gen anydeg = graduated == "Y" ; 

* CUMULATIVE DEGREE RECEIPT;
xi, prefix(D) i.degree_cat; 

* COLLAPSE TO HAVE ONE OB PER STUDENT SEMESTER; 
collapse (max) Ddegree* enrl_* outofstate anydeg , by(ResearchID year sem_cat );

egen t = group(year sem_cat);
lab def t 1 "F15" 2 "S16" 3 "Su16" 4 "F16" 5 "S17" 6 "Su17"; 
lab val t t ; 

drop year sem_cat; 
 
save scrap_nsc, replace; 

use "${intermediate}/formatted_analysis_data",clear; 

tostring ResearchID, replace; 

keep ResearchID; 

merge 1:m ResearchID using scrap_nsc, update; 

drop if _m ==2; gen in_nsc = _m >1; 

forv i = 1/8 {; rename Ddegree_cat_`i' D`i'degree_cat; };

replace t = 99 if _m == 1; drop _m; 
reshape wide D*  enrl_* outofstate anydeg, i(ResearchID ) j(t);
reshape long D1degree_cat D2degree_cat D3degree_cat D4degree_cat D5degree_cat D6degree_cat D7degree_cat D8degree_cat 
	enrl_tric enrl_pub4 enrl_pub2 enrl_priv4 enrl_priv2  outofstate anydeg, i(ResearchID ) j(t);

drop if t == 99; foreach v of var D* enrl_* outofstate anydeg {; replace `v' = 0 if `v' == . ;};

save scrap_nsc, replace; 

use "${intermediate}/formatted_analysis_data",clear; 

tostring ResearchID, replace; 

merge 1:m ResearchID using scrap_nsc, update; drop _m;

* FILL IN ENROLLMENT INFO FOR STUDENTS WHO AREN'T IN NSC FILE; 
replace enrl_cca = 1 if t==1 & enrolled_fall ==1; 
replace enrl_cca = 1 if t==2 & enrolled_sp ==1; 

save scrap_nsc, replace; 

* INSHEET AND FORMAT F16 END OF SEMESTER ATTAINMENT, SP17 ATTAINMENT DATA;
import excel "${original}/Lumina Loan Report Summer 2017.xlsx", first clear;	
keep ResearchID PriorSemesterAttemptHrs SelectedSemesterEnrollment; 
duplicates drop; 

tostring ResearchID, replace; 

merge 1:m ResearchID using scrap_nsc, update; drop if _m == 1; drop _m; 

replace enrl_cca = 1 if PriorSemesterAttemptHrs >0 & PriorSemesterAttemptHrs <.  & t == 5;
replace enrl_cca = 1 if SelectedSemesterEnrollment >0 & SelectedSemesterEnrollment <.  & t == 6;

drop SelectedSemester* PriorSemester*; 

save scrap_nsc, replace; 

* INSHEET AND FORMAT F16 ATTAINMENT DATA;
import excel "${original}/Lumina Loan Report Fall 2016.xlsx", first clear;	
keep ResearchID SelectedSemesterEnrollment PriorSemesterAttemptHrs ;
duplicates drop; 

tostring ResearchID, replace; 

merge 1:m ResearchID using scrap_nsc, update; drop if _m == 1; drop _m; 

replace enrl_tric = 1 if SelectedSemesterEnrollment >0 & SelectedSemesterEnrollment < .  & t == 4;
replace enrl_tric = 1 if PriorSemesterAttemptHrs >0 & PriorSemesterAttemptHrs < .  & t == 3;

drop PriorSemester* SelectedSemester* ; 

save scrap_nsc, replace; 

* CCA CREDENTIAL COMPLETION DATA;
import excel "${original}/Lumina Demographics Report_063016.xlsx", first clear;	

* FOR NOW, JUST KEEP CREDENTIAL COMPLETION VARS;
keep ID DegreeSequence DegreeCode DegreeStatus GraduationDate Ethnicity;
gen year = year(GraduationDate );
gen grad_month = month(GraduationDate);  

rename ID ResearchID; tostring ResearchID, replace; 

keep ResearchID year grad_month DegreeStatus DegreeCode; 

drop if year< 2015 | (year == 2015 & grad_month<12);
keep if DegreeStatus == "Awarded";  drop DegreeStatus; 

gen anydeg = 1; 

gen t = 1 if year == 2015 & grad_month == 12; 
replace t = 2 if year == 2016 & grad_month == 5; 
replace t = 3 if year == 2016 & grad_month == 8; 
replace t = 4 if year == 2016 & grad_month == 12;
replace t = 5 if year == 2017 & grad_month == 5;
replace t = 6 if year == 2017 & grad_month == 8;

drop grad_month ; 
duplicates drop; 

bysort ResearchID t: gen count = _n;
reshape wide DegreeCode , i(ResearchID t) j(count); 

merge 1:1 ResearchID t using scrap_nsc, update; drop if _m == 1; drop _m;

replace D1degree_cat = 1 if DegreeCode1 == "STCER" | DegreeCode2 == "STCER" | DegreeCode3 == "STCER";
replace D2degree_cat = 1 if DegreeCode1 == "CER" | DegreeCode2 == "CER" | DegreeCode3 == "CER";
replace D3degree_cat = 1 if DegreeCode1 == "AA" | DegreeCode2 == "AA" | DegreeCode3 == "AA";
replace D4degree_cat = 1 if DegreeCode1 == "AS" | DegreeCode2 == "AS" | DegreeCode3 == "AS";
replace D5degree_cat = 1 if DegreeCode1 == "AAB" | DegreeCode2 == "AAB" | DegreeCode3 == "AAB";
replace D5degree_cat = 1 if DegreeCode1 == "AAS" | DegreeCode2 == "AAS" | DegreeCode3 == "AAS";
replace D5degree_cat = 1 if DegreeCode1 == "ATS" | DegreeCode2 == "ATS" | DegreeCode3 == "ATS";
replace D7degree_cat = 1 if DegreeCode1 == "PDCER" | DegreeCode2 == "PDCER" | DegreeCode3 == "PDCER";

drop DegreeCode* ;

forv i = 1/8 {; bysort ResearchID (t): replace D`i'degree_cat = 1 if D`i'degree[_n-1] == 1 & ResearchID == ResearchID[_n-1]  & t == t[_n-1]+1; };
drop anydeg; egen anydeg = rowmax(D*); 

* ANY DEGREE;
egen any_deg = rowmax(D*); 

* LIMIT TO 2016-17 AY; 
keep if inlist(t,4,5); 

* F15 ATTAINMENT SAMPLE (N = 11774); 
keep if enrolled_fall ==1; 

* ONE OUTCOME FOR THE AY;
collapse (max) enrl_tric enrl_pub4 any_deg borrowed offered package month_packaged Prmry_EFC CumulativeGPA  CumulativeEarnedHours 
	pell_elig indep has_outstanding stratum_code AcceptedAmount, by(ResearchID); 

gen amount = AcceptedAmount/1000; 
lab var amount "Amount borrowed ($1k)"; 

loc j = 0;

foreach v of var enrl_tric enrl_pub4 any_deg {;

loc dec = 3; 

loc j = `j'+1; 

** OLS;
	xi: xtreg `v' package i.month_packaged Prmry_EFC CumulativeGPA  CumulativeEarnedHours pell_elig indep has_outstanding ,
		cluster(stratum_code) fe i(stratum_code);
	sum `v' if package==0 & e(sample) == 1; 	local mean=r(mean);
	loc ap = "ap"; if `j' == 1 {; loc ap = "replace"; }; 
	outreg2 using "${output}/T9_y2_nsc", `ap'  excel keep(package) addstat("Mean control", `mean') ctitle("`title_`v''") dec(`dec') aster(se) sym(**,*,+) nocons nor label;

** IV - OFFERED;
	xi: xtivreg2 `v' (offered= package) i.month_packaged Prmry_EFC CumulativeGPA  CumulativeEarnedHours pell_elig indep has_outstanding , 
		partial(i.month_packaged Prmry_EFC pell_elig indep has_outstanding) cluster(stratum_code) fe i(stratum_code);
	sum `v' if package==0 & e(sample) == 1; 	local mean=r(mean);
	outreg2 using "${output}/T9_y2_nsc", ap excel keep(offered) addstat("Mean control", `mean') ctitle("`title_`v''") dec(`dec') aster(se) sym(**,*,+) nocons nor label;

** IV - BORROWED;
	xi: xtivreg2 `v' (borrowed= package) i.month_packaged Prmry_EFC CumulativeGPA  CumulativeEarnedHours pell_elig indep has_outstanding , 
		partial(i.month_packaged Prmry_EFC pell_elig indep has_outstanding) cluster(stratum_code) fe i(stratum_code);
	sum `v' if package==0 & e(sample) == 1; 	local mean=r(mean);
	outreg2 using "${output}/T9_y2_nsc", ap excel keep(borrowed) addstat("Mean control", `mean') ctitle("`title_`v''") dec(`dec') aster(se) sym(**,*,+) nocons nor label;
	
** IV - AMOUNT;
	xi: xtivreg2 `v' (amount= package) i.month_packaged Prmry_EFC CumulativeGPA  CumulativeEarnedHours pell_elig indep has_outstanding , 
		partial(i.month_packaged Prmry_EFC pell_elig indep has_outstanding) cluster(stratum_code) fe i(stratum_code);
	sum `v' if package==0 & e(sample) == 1; 	local mean=r(mean);
	outreg2 using "${output}/T9_y2_nsc", ap excel keep(amount) addstat("Mean control", `mean') ctitle("`title_`v''") dec(`dec') aster(se) sym(**,*,+) nocons nor label;
	
* CLOSE LOOP OVER VARS;
};


