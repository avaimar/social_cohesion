*Table 3 Treatment-control balance
use formatted_analysis_data,clear

keep if enrolled_fall ==1

loc i = 1
foreach var of varlist freshman new indep {

	crmeans `var', by(package) cl(stratum_code)
	if `i' == 1 	mat table3 = r(rmeans_`var')
	else 		mat table3 = table3 \ r(rmeans_`var')
	local varlab`i': variable label `var'
	display "`varlab`i''"

	loc i=`i'+2
}

foreach var of varlist Prmry_EFC totalloans PELL_Offer CWS_Accept All_Other_Aid_Accept TotalOtherResources CumulativeEarnedHours CumulativeGPA {
	crmeans `var', by(package) cl(stratum_code) fe
	mat table3 = table3 \ r(rmeans_`var')
	local varlab`i': variable label `var'
	display "`varlab`i''"
	loc i=`i'+2
}	
		
* Export
preserve

loc matrices table3

foreach m of local matrices {
	
	* Decimal points
	loc decimals = 4

	* Puts table as data set
	drop _all
	svmat double `m'

	* Defines columns that will have the asterisk
	loc D = "2"

	* Adds asterisks
	foreach d of local D {
		
		* Looks for p-value
		loc e = `d' + 1

		gen aux`d'=string(`m'`d',"%15.`decimals'fc") if `m'`d' ~= .

		replace aux`d' = aux`d' + "**" if `m'`e' < 0.01
		replace aux`d' = aux`d' + "*" if `m'`e' >= 0.01 & `m'`e' < 0.05	
		replace aux`d' = aux`d' + "+" if `m'`e' >= 0.05 & `m'`e' < 0.10	

		replace aux`d' = "" if aux`d' == "." 		
		move aux`d'  `m'`e'
		drop `m'`d' `m'`e'
		}
	gen Constant=string(table31, "%9.2g")	
	drop table31
	rename aux2 Treatment
	rename table34 N
	
	gen Characteristics=""
	order Characteristics Constant Treatment N
	
	local N=_N
	
	forvalues j = 2(2)`N'{
		replace Constant="(" + Constant + ")" if _n==`j'
		replace Treatment="(" + Treatment + ")" if _n==`j'
	}
	local N=`N'-1	
	forvalues k= 1(2)`N'{
	replace Characteristics="`varlab`k''" if [_n]==`k'
	}
	local N=`N'+3
	export excel using "colA_`m'.xlsx", replace firstrow(var)
	}

restore

*F-test
qui areg package Prmry_EFC totalloans PELL_Offer CWS_Accept All_Other_Aid_Accept TotalOtherResources CumulativeEarnedHours CumulativeGPA freshman new indep  , a(stratum_code) cl(stratum_code)
test Prmry_EFC totalloans PELL_Offer CWS_Accept All_Other_Aid_Accept TotalOtherResources CumulativeEarnedHours CumulativeGPA freshman new indep  
putexcel A`N'=("F-test") C`N'=(round(r(p),.001)) using colA_table3, modify

