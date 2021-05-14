#d cr

clear
set more off

* FIRST FILE
import excel "${original}/Lumina Export 050115.xlsx", first clear	
	
keep ResearchID
gen date_packaged = "050115"

save scrap, replace

** 2015
loc list_5 "11(1)14 18(1)22 26(1)28 31"
loc list_6 "3 4 7 9 11 12 15 18 21(2)25 28 30"
loc list_7 "2 7 9 12(2)16 20(1)22 26 29 31"
loc list_8 "3(1)5 7 9(1)11 13 14 17 19 20 21 24(1)26 30 31"
loc list_9 "2 3 8 10 14(1)17 21(1)24 27(1)30"
loc list_10 "1 4(1)8 12(1)15 19 21 22 27 28"
loc list_11 "1 3(1)5 8(2)12 15(1)19 23 25 29 30"
loc list_12 "1 2 4 6(1)7 10 14(1)17 21 31"

foreach m of num 5(1)12 {

foreach d of num `list_`m'' {

if `d'<10 & `m'<10 {
	import excel "${original}/Lumina Export 0`m'0`d'15.xlsx", first clear	
	gen date_packaged = "0`m'0`d'15"
}

if `d'<10 & `m'>9 {
	import excel "${original}/Lumina Export `m'0`d'15.xlsx", first clear	
	gen date_packaged = "`m'0`d'15"
}

if `d'>9 & `m'<10 {
	import excel "${original}/Lumina Export 0`m'`d'15.xlsx", first clear	
	gen date_packaged = "0`m'`d'15"
}

if `d'>9 & `m'>9 {
	import excel "${original}/Lumina Export `m'`d'15.xlsx", first clear	
	gen date_packaged = "`m'`d'15"
}
	
	keep ResearchID date_packaged

	append using scrap
	save scrap, replace

* CLOSE LOOP OVER PACKAGED DATES
}

* CLOSE LOOP OVER MONTHS
}

** 2016 
loc list_1 "4 6 7"

foreach m of num 1 {

foreach d of num `list_`m'' {

if `d'<10 {
	import excel "${original}/Lumina Export 0`m'0`d'16.xlsx", first clear	
	gen date_packaged = "0`m'0`d'16"
}

if `d'>9 {
	import excel "${original}/Lumina Export 0`m'`d'16.xlsx", first clear	
	gen date_packaged = "0`m'`d'16"
}
	
	keep ResearchID date_packaged

	append using scrap
	save scrap, replace

* CLOSE LOOP OVER PACKAGED DATES
}

* CLOSE LOOP OVER MONTHS
}


** FILES WITH WEIRD FORMATS
* JULY 28 
import excel "${original}/Lumina Export 07282015.xlsx", first clear	
gen date_packaged = "072815"

keep ResearchID date_packaged

append using scrap
save scrap, replace

* AUGUST 28 
import excel "${original}/Lumina Exports 082815.xlsx", first clear	
gen date_packaged = "082815"

keep ResearchID date_packaged

append using scrap
save scrap, replace

* OCTOBER 25 
import excel "${original}/Lumina Export 102512.xlsx", first clear	
gen date_packaged = "102515"

keep ResearchID date_packaged

append using scrap
save scrap, replace

* DECEMBER 8
import excel "${original}/Lumina Exports 120815.xlsx", first clear	
gen date_packaged = "120815"

keep ResearchID date_packaged

append using scrap
save scrap, replace

* SECOND FILES FOR A GIVEN DATE
foreach d in 072615 080515 082415 090315 090815 091015 092115 101215 102115 102715 111015 112315 121415 {

	import excel "${original}/Lumina Export `d'_2.xlsx", first clear	
	gen date_packaged = "`d'"

	keep ResearchID date_packaged

	append using scrap
	save scrap, replace

* CLOSE LOOP
}

* ONE DUPLICATE OB - KEEP FIRST INSTANCE
bysort ResearchID (date_packaged): gen first = _n == 1
drop if first ==0
drop first

gen month_packaged = real(substr(date_packaged,1,2))
gen day_packaged = real(substr(date_packaged,3,2))
gen year_packaged = 2015
replace year_packaged = 2016 if month_packaged == 1
drop date_packaged

save "${intermediate}/date_packaged_info.dta", replace

erase scrap.dta

#d;