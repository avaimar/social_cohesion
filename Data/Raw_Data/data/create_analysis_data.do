#d;
clear;
set more off;
capture log close;

gl dir = " ";
gl programs "${dir}\programs\callable_programs";
gl original "${dir}\data\original";
gl intermediate "${dir}\data\intermediate";

cd "${dir}\programs";

** PROGRAM THAT ADDS DATE PACKAGED;
qui do "${programs}/add_date_packaged.do";

/****************************************************************************************************************/

*** COMBINE RAW DATA FILES;
** LOAN OFFER REPORTS;
* PULL TREATMENT ASSIGNMENT, ORIGINAL OFFER, COST OF ATTENDANCE, GROSS NEED, TOTAL OFFERED AID, TOTAL OTHER RESOURCES FROM EARLIEST REPORT THAT A STUDENT SHOWS UP IN;
import excel "${original}/Lumina Loan Offer Report 022416.xlsx", first clear	;
keep ResearchID Fund OriginalOffer LuminaValue CostofAttendance GrossNeed TotalOfferedAid TotalOtherResources;

encode Fund, gen(fund);
drop Fund;

reshape wide OriginalOffer, i(ResearchID) j(fund);
for any 1 2 \ any sub unsub : rename OriginalOfferX OriginalOffer_Y;
foreach v of var OriginalOffer* {;
	recode `v' (. = 0);
};

save scrap, replace;

foreach d in 020116 010316 113015 110915 092815_2 082415 081715 080615 072915 072215 {;

	import excel "${original}/Lumina Loan Offer Report `d'.xlsx", first clear	;
	keep ResearchID Fund OriginalOffer LuminaValue CostofAttendance GrossNeed TotalOfferedAid TotalOtherResources;

	encode Fund, gen(fund);
	drop Fund;

	reshape wide OriginalOffer, i(ResearchID) j(fund);
	drop OriginalOffer1;

	for any 2 3 \ any sub unsub : rename OriginalOfferX OriginalOffer_Y;
	
	foreach v of var OriginalOffer* {;
		recode `v' (. = 0);
	};

merge 1:1 ResearchID using scrap, update;

drop _m;

save scrap, replace;

* CLOSE LOOP OVER LOAN OFFER REPORTS;
};

** PULL CREDITS ATTEMPTED FROM BEGINNING AND END OF SEMESTER REPORTS TO CAPTURE INSTANCES WHERE STUDENTS DROP COURSES MID-SEMESTER;
import excel "${original}/Lumina Loan Offer Report 110915.xlsx", first clear	;
keep ResearchID SemesterEnrollment ;
rename SemesterEnrollment crdattm_fall_beg;
recode crdattm_fall_beg (. = 0);
duplicates drop;

merge 1:1 ResearchID using scrap, update;
drop _m;
save scrap, replace;

import excel "${original}/Lumina Loan Offer Report 092815_2.xlsx", first clear	;
keep ResearchID SemesterEnrollment ;
rename SemesterEnrollment crdattm_fall_beg;
recode crdattm_fall_beg (. = 0);
duplicates drop;

merge 1:1 ResearchID using scrap, update;
drop _m;
save scrap, replace;

import excel "${original}/Lumina Loan Offer Report 113015.xlsx", first clear	;

* ADJUST CUMULATIVE CREDITS AND CUMULATIVE GPA FOR STUDENTS WHO'VE ALREADY EARNED SOME CREDITS IN F15;
replace CumulativeEarnedHours = CumulativeEarnedHours - FallSemesterEarnedHours if FallSemesterEarnedHours ~=.;
replace CumulativeGPA = ((CumulativeGPA*(CumulativeEarnedHours+FallSemesterEarnedHours))-(FallSemesterEarnedHours*FallSemesterGPA))/CumulativeEarnedHours if CumulativeGPA<. & FallSemesterEarnedHours<. ;
replace CumulativeGPA = 0 if CumulativeGPA<0;
replace CumulativeGPA = 4 if CumulativeGPA>4 & CumulativeGPA<. ;

keep ResearchID SemesterEnrollment CumulativeEarnedHours CumulativeGPA;
rename SemesterEnrollment crdattm_fall_end;

duplicates drop;

merge 1:1 ResearchID using scrap, update;
drop _m;
save scrap, replace;

** DO THE SAME FOR STUDENTS THAT ONLY SHOW UP IN FEB REPORTS ;
import excel "${original}/Lumina Loan Offer Report 022416.xlsx", first clear	;
replace CumulativeEarnedHours = CumulativeEarnedHours - FallSemesterEarnedHours if FallSemesterEarnedHours ~=.;
replace CumulativeGPA = ((CumulativeGPA*(CumulativeEarnedHours+FallSemesterEarnedHours))-(FallSemesterEarnedHours*FallSemesterGPA))/CumulativeEarnedHours if CumulativeGPA<. & FallSemesterEarnedHours<. ;
replace CumulativeGPA = 0 if CumulativeGPA<0;
replace CumulativeGPA = 4 if CumulativeGPA>4 & CumulativeGPA<. ;

keep ResearchID CumulativeEarnedHours CumulativeGPA;

duplicates drop;

merge 1:1 ResearchID using scrap, update;
drop _m;
save scrap, replace;

** PULL AWARD STATUS, ACCEPTED AMOUNT, FALL CREDITS EARNED, FALL SEMESTER GPA, SPRING CREDITS ATTEMPTED, MAJOR, PELL, CWS, ALL OTHER AID FROM LAST F2015 REPORT;
import excel "${original}/Lumina Loan Offer Report 022416.xlsx", first clear	;
keep ResearchID Fund AwardStatus AcceptedAmount SemesterEnrollment FallSemesterEarnedHours FallSemesterGPA Major CIPCode PELL_Offer CWS_Orig_Offer CWS_Accept All_Other_Aid_Accept;

encode Fund, gen(fund);
drop Fund;

rename SemesterEnrollment crdattm_sp_beg;

reshape wide AwardStatus AcceptedAmount, i(ResearchID) j(fund);

foreach v in AwardStatus AcceptedAmount {;
	drop `v'1;
	for any 2 3 \ any sub unsub : rename `v'X `v'_Y;
};

foreach v of var PELL CWS* All_Other_Aid_Accept FallSemester* {;
	recode `v' (. = 0);
};

merge 1:1 ResearchID using scrap, update;

drop _m;

save scrap, replace;


** INITIAL TREATMENT ASSIGNMENT, STRATA, AND DATE PACKAGED;
preserve;

	use "${intermediate}/fullsample_CCA.dta",clear;

* FOR NOW, DROP 1 INSTANCE OF DUPLICATE OBSERVATION IN fullsample_CCA DATA (KEEP OBS WITH LOANS PACKAGED - INITIAL ASSIGNMENT);
	bysort ResearchID: gen count = _N ;
	drop if count == 2 & package ==0;
	drop count;

* KEEP INFO ON COA AT TIME OF PACKAGING;
	replace CostofAttendance = CostofAttendnace if CostofAttendance == . & CostofAttendnace~=.;
	drop CostofAttendnace;
	rename CostofAttendance CostofAttendance_at_packaging;
	
* FORMAT DATE PACKAGED INFO;
	gen year_packaged = 2016 if real(substr(date_packaged,5,2)) == 16;
	gen month_packaged = real(substr(date_packaged,1,2));
	gen day_packaged = real(substr(date_packaged,3,2));

** ADD DATE PACKAGED WHERE MISSING BASED ON REPORT DATE;
	merge 1:1 ResearchID using "${intermediate}/date_packaged_info.dta", update;
	drop _m;

	save scrap, replace;

restore;
 
merge m:1 ResearchID using scrap, update ;
erase scrap.dta;

* DROP STUDENTS THAT HAVE BEEN PACKAGED AFTER THE LAST LOAN PACKAGING REPORT WAS PULLED;
* NOTE THAT STUDENTS PACKAGED ON 2/23 OR 2/24 ARE NOT IN OUTCOME REPORT DATED 2/24;
* NOTE THAT 1 STUDENT IN THE OUTCOME REPORT DON'T SHOW UP IN THE RANDOMIZED SAMPLE DATA (RESEARCH ID = 1420955);
keep if _merge>2;
drop _m;

** CLEAN UP DATA;
* DROP OB OF COLLEGE-GENERATED ID FOR TESTING SYSTEMS;
drop if ResearchID==1225014		;

* DROP UNNECESSARY VARS;
drop stratum_noloans_count stratum_count stratum_package_count stratum_package_count_temp package_extra_stratum package_extra unevenscutoff obs unevens cutoff random newobs_even count_stratum addpackage;
drop AcademicLevel FirstTimeatTriC DependencyStatus AidYear ;

** GENERATED VARIABLES;
* TOTAL LOANS OFFERED;
egen awardoffer = rowtotal(OriginalOffer_sub OriginalOffer_unsub);

* TOTAL LOANS ACCEPTED;
egen AcceptedAmount= rowtotal(AcceptedAmount_sub AcceptedAmount_unsub);

* ASSUME THAT CONTROL GROUP STUDENTS RECEIVED $0 OFFER ;
replace awardoffer = 0 if LuminaValue == 0 ;

* INITIALLY OFFERED A LOAN? (NOTE: NOT ALL TREATMENT GROUP MEMBERS WERE OFFERED LOANS);
gen offered = awardoffer>0;

* ACTIVELY REJECT LOAN (ACCEPT $0 OFFER, CANCEL, OR DECLINE);
* NOTE: AS OF 1/3 REPORT, THERE ARE NOW 457 OBSERVATIONS WITH $0 SUB OFFERS ACCEPTED, 353 OBS W $0 UNSUB OFFER ACCEPTED;
foreach t in sub unsub {;
	gen active_zero_`t' = (AwardStatus_`t' == "ACPM" & AcceptedAmount_`t' == 0) | (AwardStatus_`t' =="CNCL") | (AwardStatus_`t' == "DECL") ;
};

* BORROWED?;
gen borrowed = AcceptedAmount>0 ;

* TOOK DEFAULT OFFER;
gen took_default = AcceptedAmount==awardoffer ;

foreach t in sub unsub {;
	gen took_default_`t' = AcceptedAmount_`t' == OriginalOffer_`t';
};

* IF EFC IS MISSING, ASSUME IT IS LARGEST POSSIBLE VALUE;
recode Prmry_EFC (. = 999999);

** CATEGORICAL VARS FOR HETEROGENEITY ANALYSIS;
gen pell_elig=Prmry_EFC<=5198;
gen pell_cat = 0 if Prmry_EFC> 5198;
replace pell_cat = 1 if Prmry_EFC <=5198 & Prmry_EFC>0;
replace pell_cat = 2 if Prmry_EFC ==0;
gen has_outstanding=totalloans>0;

** ACADEMIC OUTCOME VARS;
* CREDITS ATTEMPTED & ANY ENROLLMENT (ONLY MEASURE FOR STUDENTS WHO HAD BEEN PACKAGED BEFORE SEPTEMBER 15);
recode crdattm_fall_end (. = 0) ;
recode crdattm_sp_beg (. = 0);

gen enrolled_fall=crdattm_fall_beg>0 if crdattm_fall_beg ~=.;
gen enrolled_sp=crdattm_sp_beg>0 if crdattm_sp_beg ~=.;

* DEADLINE FOR DROPPING COURSES (RECEIVING W GRADE) FOR F15 WAS 11/6;
gen any_dropped_courses_fall = crdattm_fall_beg > crdattm_fall_end if enrolled_fall == 1 & crdattm_fall_end ~=. ;

* DISCRETE ATTENDANCE INTENSITY CATEGORIES (FT VERSUS PT);
gen fulltime_fall = crdattm_fall_beg >=12 if crdattm_fall_beg ~=.;
gen fulltime_sp = crdattm_sp_beg >=12 if crdattm_sp_beg ~=.;
gen parttime_fall = crdattm_fall_beg >=6 if crdattm_fall_beg ~=.;
gen parttime_sp = crdattm_sp_beg >=6 if crdattm_sp_beg ~=.;

* CREDITS EARNED & GPA (MEASURE FOR STUDENTS WHO HAD BEEN PACKAGED BEFORE MID-SEMESTER (OCTOBER 15);
gen credits_earned = FallSemesterEarnedHours if year_packaged == 2015 & (month_packaged <11 | (month_packaged == 10 & day_packaged<=15));
recode credits_earned (. = 0) if year_packaged == 2015 & (month_packaged <11 | (month_packaged == 10 & day_packaged<=15));

gen gpa = FallSemesterGPA if year_packaged == 2015 & (month_packaged <11 | (month_packaged == 10 & day_packaged<=15));
recode gpa (. = 0);

encode stratum, gen(stratum_code);
drop stratum;

count;

label var freshman "<30 credits earned";
label var new "New";
label var indep "Independent";
label var pell_elig "Eligible for Pell Grant";
label var has_outstanding "Has outstanding student debt";
label var package "Loan Offered";
label var Prmry_EFC "Expected Family Contribution (EFC)";
label var totalloans "Outstanding loan debt";
label var CostofAttendance "Cost of Attendance";
label var GrossNeed "Gross Need";
label var TotalOtherResources "Total Other Resources";
lab var pell_cat "Pell Eligibility Category";
lab var package "Assigned to Treatment Group";
lab var offered "Offered Loan";
lab def pell_cat 0 "Ineligible" 1 "Eligible < max Pell" 2 "Eligible max Pell";
lab val pell_cat pell_cat;


** IMPUTE ORIGINAL SUB LOAN OFFERS FOR NONPACKAGED STUDENTS;
* IMPUTE PELL AWARDS FOR FTFY STUDENTS BASED ON EFC;
gen imputed_pell = .;
replace imputed_pell = 5775 if Prmry_EFC ==0;
recode imputed_pell (. = 5725) if Prmry_EFC<=100;
foreach n of num 200(100)5100 {;
	loc m = 5825-`n';
	recode imputed_pell (. = `m') if Prmry_EFC<=`n';
};
recode imputed_pell (. = 626) if Prmry_EFC<=5198;
recode imputed_pell (. = 0);

* IMPUTED SUB ELIGIBILITY;
gen imputed_sub_elig = GrossNeed - imputed_pell - All_Other_Aid_Accept - CWS_Orig_Offer - TotalOtherResources;
replace imputed_sub_elig = 0 if imputed_sub_elig <200;
replace imputed_sub_elig = 3500 if imputed_sub_elig >3500 & freshman ==1;
replace imputed_sub_elig = 4500 if imputed_sub_elig >4500 & freshman ==0;
gen dum = 23000-totalloans;
replace imputed_sub_elig = dum if dum<imputed_sub_elig & dum>0;
replace imputed_sub_elig = . if imputed_sub_elig >4500 ;

save "${intermediate}/formatted_analysis_data", replace;

* INSHEET AND FORMAT SPRING OUTCOME DATA;
import excel "${original}/Lumina Loan Offer Report_spring 053016.xlsx", first clear;	

* KEEP SPRING ATTAINMENT INFO + MAJOR (TO CHECK FOR SWITCHING);
keep ResearchID SemesterEnrollment SelectedTermEarnedHours SelectedTermGPA Major CIPCode;
duplicates drop; 

rename SelectedTermGPA gpa_sp; 
rename SemesterEnrollment crdattm_sp_end;
rename SelectedTermEarnedHours credits_earned_sp;
rename Major major_sp_end;
rename CIPCode cip_sp_end;

foreach v of var gpa_sp crdattm_sp_end credits_earned_sp {; recode `v' (. = 0); };

save scrap, replace;

* ADD ADDITIONAL DEMS;
import excel "${original}/Lumina Demographics Report_063016.xlsx", first clear;	

keep ID BirthDate Ethnicity Sex Parent1Education Parent2Education PlannedHousing UnmetNeed;
duplicates drop; 
rename ID ResearchID; 

gen par_coll = Parent1Education == "College or beyond" | Parent2Education == "College or beyond";
drop Parent1Education Parent2Education;

gen female = Sex == "F"; 
drop Sex;

gen white = Ethnicity == "Caucasian" if Ethnicity ~= "No Response"; 
drop Ethnicity;

* AGE AT THE START OF THE FALL SEMESTER (9/1/2015);
gen birth_year = year(BirthDate);
gen birth_month = month(BirthDate);

gen age = 2015-birth_year if birth_month<9;
replace age = 2015-birth_year+1 if birth_month>=9;

drop BirthDate birth_year birth_month;  

save scrap2, replace; 

* INSHEET CREDENTIAL COMPLETION DATA;
import excel "${original}/Lumina Demographics Report_063016.xlsx", first clear;	

* FOR NOW, JUST KEEP CREDENTIAL COMPLETION VARS;
keep ID DegreeSequence DegreeCode DegreeStatus GraduationDate Ethnicity;
gen grad_year = year(GraduationDate );
gen grad_month = month(GraduationDate);  

rename ID ResearchID; 
gen anydeg = DegreeStatus == "Awarded" & ((grad_year == 2015 & grad_month == 12) | (grad_year == 2016)) ; 
foreach t in 0 AA AAB AAS AS ATS CER NDM PCA PDCER PERS STCER XXXXXX {;
	gen deg_`t' = DegreeCode == "`t'" & DegreeStatus == "Awarded" & (grad_year<2015 | (grad_year == 2015 & grad_month<12)); 
};

collapse (max) deg_* anydeg, by(ResearchID); 

* SINGLE INDICATOR FOR ANY ASSOCIATE'S DEGREE);
egen deg_anyaa = rowmax(deg_AA deg_AAB deg_AAS deg_AS deg_ATS); 

save scrap3, replace; 

use "${intermediate}/formatted_analysis_data",clear; 

merge 1:1 ResearchID using scrap, update; 

keep if _m ==3; drop _m; 

erase scrap.dta; 

merge 1:1 ResearchID using scrap2, update; 

keep if _m ==3; drop _m;

erase scrap2.dta;

merge 1:1 ResearchID using scrap3, update; 

keep if _m ==3; drop _m;

erase scrap3.dta;

recode gpa (. = 0); 

* FIX THE FEW OBS THAT ARE MISSING DATE PACKAGED INFO (ALL WERE PACKAGED IN FEB);
recode month_packaged (. = 2);
recode year_packaged (. = 2016);

* TOTAL CREDITS ATTEMPTED;
egen crdattm_total = rowtotal(crdattm_fall_beg crdattm_sp_beg); 

* TOTAL CREDITS EARNED;
egen credits_total = rowtotal(credits_earned credits_earned_sp); 

gen enrolled_fall_end = crdattm_fall_end>0; 
gen ft_fall_end = crdattm_fall_end>=12;
gen pt_fall_end = crdattm_fall_end>=6;
gen credits_earned2 = credits_earned; recode credits_earned2 (. = 0);
gen gpa_total = ((gpa*crdattm_fall_beg)+(gpa_sp*crdattm_sp_beg))/(crdattm_total );

recode CumulativeGPA (. = 0);recode CumulativeEarnedHours (. = 0); 

* COMBINE SMALL STRATA TO CREATE SUPER STRATA;
for any 13 17 19 31 34 37 39 42 44 49 52 56 61 63 65 74 76 79 81 83
  \ any "14 15" 18 "20	21	22	23	24	25	26	27	28	29	30" "32	33" 35 38 40 43 45 50 "53 54 55" "57 58	59" 62 64 "66 67 68" 75 77 80 82 "84 85	86"
  : recode stratum_code (Y = X);   

replace CostofAttendance = CostofAttendance_at_packaging if CostofAttendance_at_packaging~=.;

save "${intermediate}/formatted_analysis_data", replace; 