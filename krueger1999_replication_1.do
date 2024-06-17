clear
cd /Users/zhangshuai/Desktop/interest/krueger1999_replication // 修改默认目录

*-------------------------------------------------------------------------------
* Table 1: COMPARISON OF MEAN CHARACTERISTICS OF TREATMENTS AND CONTROLS: UNADJUSTED DATA
*-------------------------------------------------------------------------------
* create data
use krueger1999_data, clear
keep if start == grade 

local varname freelunch WhiteAsian age1985 attrition classsize  SAT
collect clear
table (grade) (classtype), stat(mean `varname') nototals
table (grade), stat(count `varname') nototals append // 添加表1备注中出现的样本数

* format 
collect dims
collect levelsof cmdset
collect levelsof result
collect levelsof var
collect levelsof classtype

* F test
forvalues i = 0/3 {
	foreach var in `varname' {
		capture {
			areg `var'  if grade == `i', absorb(classtype)
			scalar m = e(p_absorb)
			collect get mean = m, tags(grade[`i'] var[`var'] classtype[4]) 
		}
	}
}

* row format 
collect style header grade, title(hide)
collect label levels grade 0 "A. Students who entered STAR in kindergarten" 1 "B. Students who entered STAR in first grade" 2 "C. Students who entered STAR in second grade" 3 "D. Students who entered STAR in third grade", modify

local colname `" "1. Free lunch" "2. White/Asian" "3. Age in 1985" "4. Attrition rate" "5. Class size" "6. Percentile score" "'
local var_count  `:word count `varname''
forvalues i = 1/`var_count' {
	local tmp `:word `i' of `varname''
	local tmp1 `:word `i' of `colname''
	collect label levels var `"`tmp'"' `"`tmp1'"', modify
}

* column format
collect style header classtype, title(hide)
collect style header result[count], level(label)
collect label levels classtype 1 "Small" 2 "Regular" 3 "Regular/Aide" 4 "Joint P-Value", modify
collect label levels result count "Number of observations", modify

* cell format
collect style cell result[count], warn halign(center) valign(center)
collect style cell classtype, warn halign(center) valign(center) nformat(%9.2fc)
collect style cell var[classsize SAT]#classtype[1 2 3], nformat(%9.1fc)

* table format
collect style cell border_block, border(right, pattern(nil))
collect title "TABLE I - COMPARISON OF MEAN CHARACTERISTICS OF TREATMENTS AND CONTROLS: UNADJUSTED DATA"

collect layout (grade#var) (result[count] classtype)
collect export "table1", as(docx) replace	

*-------------------------------------------------------------------------------
* Table 2: P-VALUES FOR TESTS OF WITHIN-SCHOOL DIFFERENCES AMONG SMALL, REGULAR, AND REGULAR/AIDE CLASSES
*-------------------------------------------------------------------------------
use krueger1999_data, clear
keep if start == grade 
local varname freelunch WhiteAsian age1985 attrition classsize  SAT

collect clear
forvalues i = 0/3 {
	foreach var in `varname' {
		capture {
			areg `var' i.classtype if grade == `i', absorb(schid)
			scalar pval = e(p)
			collect get pval = pval, tags(grade[`i'] var[`var'])
		}
	}
}

* format 
collect dims
collect levelsof cmdset
collect levelsof result
collect levelsof var
collect levelsof classtype

* row format 
local colname `" "1. Free lunch" "2. White/Asian" "3. Age in 1985" "4. Attrition rate" "5. Class size" "6. Percentile score" "'
local var_count  `:word count `varname''
forvalues i = 1/`var_count' {
	local tmp `:word `i' of `varname''
	local tmp1 `:word `i' of `colname''
	collect label levels var `"`tmp'"' `"`tmp1'"', modify
}

* column format
collect label levels grade 0 "K", modify

* cell format
collect style cell grade, warn halign(center) valign(center) nformat(%9.2fc)

* table format
collect style cell border_block, border(right, pattern(nil))
collect title "Table II - P-VALUES FOR TESTS OF WITHIN-SCHOOL DIFFERENCES AMONG SMALL, REGULAR, AND REGULAR/AIDE CLASSES"

collect layout (var) (grade)
collect export "table2", as(docx) replace	

* statistics in p504
use krueger1999_data, clear
keep if start == grade 
gen small = classtype < 2 if !mi(classtype)

collect clear
forvalues i = 0/3 {
	qui: areg small freelunch WhiteAsian age if grade == `i', absorb(schid)
	scalar pval = e(p)
	collect get pval = pval, tags(grade[`i'])	
}
collect layout (grade) (result)

* pool together all four waves 与文中结果不同
areg small freelunch WhiteAsian age , absorb(schid)
di "`e(p)'"

* p505 teacher charateristics
keep tyears twhite tmd classtype grade schid tchid
duplicates drop 

collect clear
forvalues i = 0/3 {
	foreach var in tyears twhite tmd {
		qui: areg `var' i.classtype if grade == `i', absorb(schid) 
		scalar pval = e(p)
		collect get pval = pval, tags(grade[`i'] var[`var'])
	}
}
collect layout (grade) (var)

* 脚注 10
areg tmd i.classtype if grade == 3, absorb(schid) 
areg tyears i.classtype if grade == 1, absorb(schid) 

*-------------------------------------------------------------------------------
* Table 3:  DISTRIBUTION OF CHILDREN ACROSS ACTUAL CLASS SIZES BY RANDOM ASSIGNMENT GROUP IN FIRST GRADE
*-------------------------------------------------------------------------------
use krueger1999_data, clear

keep if grade == 1 & flagsg == 1
collect clear
table (classsize) (classtype), nototals
collect: bys classtype: summarize classsize

* format 
collect dims
collect levelsof cmdset
collect levelsof result
collect levelsof var
collect levelsof classtype

* row format 
collect label dim classsize "Actual class size in first grade", modify
collect label levels result mean "Average class size", modify

* column format
collect label dim classtype "Assignment group in first grade", modify
collect label levels classtype 1 "Small" 2 "Regular" 3 "Regular/Aide" 4 "Joint P-Value", modify

* cell format
collect style cell classtype, warn halign(center) valign(center) nformat(%9.0fc)
collect style cell result[mean], nformat(%9.1fc)

* table format
collect style cell border_block, border(right, pattern(nil))
collect title "Table III - DISTRIBUTION OF CHILDREN ACROSS ACTUAL CLASS SIZES BY RANDOM ASSIGNMENT GROUP IN FIRST GRADE"

collect layout (classsize result[mean]) (classtype)
collect export "table3", as(docx) replace	

*-------------------------------------------------------------------------------
* Table 4: TRANSITIONS BETWEEN CLASS-SIZE IN ADJACENT GRADES 
* NUMBER OF STUDENTS IN EACH TYPE OF CLASS
*-------------------------------------------------------------------------------
use krueger1999_data, clear

keep if flagsg == 1 & !mi(classtype)
keep stdntid grade classtype 
reshape wide classtype, i(stdntid) j(grade)

collect clear 
table (classtype0) (classtype1)
collect remap classtype1[`".m"' `"1"' `"2"' `"3"'] = classtype[`".m"' `"1"' `"2"' `"3"'] // 修改列tags

table (classtype1) (classtype2), append
collect remap classtype2[`".m"' `"1"' `"2"' `"3"'] = classtype[`".m"' `"1"' `"2"' `"3"'] // 与上述列tags一致
collect remap classtype1[`".m"' `"1"' `"2"' `"3"'] = classtype0[`".m"' `"1"' `"2"' `"3"'] // 与上述行tags一致

table (classtype2) (classtype3), append
collect remap classtype3[`".m"' `"1"' `"2"' `"3"'] = classtype[`".m"' `"1"' `"2"' `"3"']
collect remap classtype2[`".m"' `"1"' `"2"' `"3"'] = classtype0[`".m"' `"1"' `"2"' `"3"']

* format 
collect dims
collect levelsof cmdset
collect levelsof result
collect levelsof classtype0
collect levelsof classtype1
collect layout (cmdset#classtype0)  (classtype)

* add rows
collect get new1="First grade", tags(cmdset[1] classtype[2])
collect remap result[`"new1"'] = classtype0[`"new1"']
collect get new2="Second grade", tags(cmdset[2] classtype[2])
collect remap result[`"new2"'] = classtype0[`"new1"']
collect get new3="Third grade", tags(cmdset[3] classtype[2])
collect remap result[`"new3"'] = classtype0[`"new1"']

* row format 
collect style header classtype0[new1], level(hide)
collect style header cmdset, title(hide)
collect style header classtype0, title(hide)
collect label levels cmdset 1 "A. Kindergarten to first grade" 2 "B. First grade to second grade" 3 "C. Second grade to third grade" , modify
collect label levels classtype0 1 "Small" 2 "Regular" 3 "Regular/Aide" .m "All", modify

* column format
collect style header classtype, title(hide)
collect label levels classtype 1 "Small" 2 "Regular" 3 "Regular/Aide" .m "All", modify

* cell format
collect style cell classtype, warn halign(center) valign(center) nformat(%9.0f)

* table format
collect style cell border_block, border(right, pattern(nil))
collect title "Table IV - TRANSITIONS BETWEEN CLASS-SIZE IN ADJACENT GRADES  NUMBER OF STUDENTS IN EACH TYPE OF CLASS"

collect layout (cmdset#classtype0)  (classtype)
collect export "table4", as(docx) replace	

*-------------------------------------------------------------------------------
* Figure I- Distribution of Test Percentile Scores by Class Size and Grade 
*-------------------------------------------------------------------------------
use krueger1999_data, clear

* statistics in p508 (与文中结果存在差异)
corr SAT_read BSF_read if grade == 2
sort grade 
by grade : corr SAT BSF

local names `" "Kindergarten" "1st Grade" "2nd Grade" "3rd Grade" "' 
forvalues i = 0/3 {
	local j = `i' + 1
	local tmp `:word `j' of `names''
	twoway  (kdensity SAT if classtype > 1 & grade == `i', lpattern(dash)) ///
		    (kdensity SAT if classtype == 1 & grade == `i'), ///
			ylabel(0(0.005)0.015) xlabel(0(50)100) ytitle("Density") ///
			title(`tmp') xtitle("Standford Achievement Test Percentile") ///
			legend(pos(7) ring(0) col(1) label(1 "Regular") label(2 "Small") ///
			region(lstyle(none))  size(small) ) name(f`j',replace) nodraw 
}		

graph combine f1 f2 f3 f4, title("Figure I" "Distribution of Test Percentile Scores by Class Size and Grade", position(6) size(small)) 		
graph export "figure1.jpg", as(jpg) quality(100) replace


*-------------------------------------------------------------------------------
* APPENDIX: SUMMARY STATISTICS MEANS WITH STANDARD DEVIATIONS IN PARENTHESES
*-------------------------------------------------------------------------------
use krueger1999_data, clear
xtset stdntid grade 
gen attrition1 = flagsg != f.flagsg 
replace attrition1 = . if grade == 3

local varname classsize  SAT BSF freelunch WhiteAsian gender age attrition1 tmd twhite tgen 
local varname1 classsize  SAT BSF age 

collect clear
table () (grade) if flagsg == 1, stat(mean `varname') stat(sd `varname1') stat(count stdntid)

keep if flagsg == 1
keep classtype grade schid tchid
duplicates drop 
table (classtype) (grade), append

keep schid grade
duplicates drop  
table () (grade), stat(count schid) append

* 修改attrition, schid, stdntid的最后一列
use krueger1999_data, clear
keep if start == grade

qui: summarize attrition 
scalar mean = r(mean)
collect get mean = mean, tags(var[attrition1] grade[.m])
 
qui: summarize stdntid 
scalar count = r(N)
collect get count = count, tags(var[stdntid] grade[.m])

keep schid 
duplicates drop 
qui: summarize schid 
scalar count = r(N)
collect get count = count, tags(var[schid] grade[.m])

* format 
collect dims
collect levelsof cmdset
collect levelsof result
collect levelsof var
collect levelsof grade
collect layout (var#result classtype[1 2 3])  (grade)

* row format 
collect style header result, level(hide)
collect style header classtype, title(hide)

local varname classsize  SAT BSF freelunch WhiteAsian gender age attrition1 tmd twhite tgen schid stdntid
collect style autolevels var `varname'
local colname `" "Class size" "Percentile score avg.SAT" "Percentile score avg.BSF" "Free lunch" "White" "Girl" "Age" "Exited sample" "Percent of teachers with MA+ degree" "Percent of teachers who are White" "Percent of teachers who are male" "No. of schools" "No. of students" "'
local var_count  `:word count `varname''
forvalues i = 1/`var_count' {
	local tmp `:word `i' of `varname''
	local tmp1 `:word `i' of `colname''
	collect label levels var `"`tmp'"' `"`tmp1'"', modify
}
collect label levels classtype 1 "No. of small classes" 2 "No. of reg. classes" 3 "No. of reg/aide classes", modify

* column format
collect label dim grade "Grade", modify
collect label levels grade 0 "K" .m "All", modify

* cell format
collect style cell grade, warn halign(center) valign(center) 
collect style cell var[classsize SAT BSF], nformat(%9.1fc)
collect style cell var[freelunch WhiteAsian gender age attrition1 tmd twhite tgen schid stdntid], nformat(%9.2fc)
collect style cell var[schid stdntid], nformat(%9.0fc)
collect style cell classtype, nformat(%9.0fc)
collect style cell result[sd], sformat("(%s)") warn

* table format
collect style cell border_block, border(right, pattern(nil))
collect title "APPENDIX: SUMMARY STATISTICS MEANS WITH STANDARD DEVIATIONS IN PARENTHESES"

collect layout (var#result classtype[1 2 3])  (grade)
collect export "table_appendix", as(docx) replace	

*-------------------------------------------------------------------------------
* Table V: OLS AND REDUCED-FORM ESTIMATES OF EFFECT OF CLASS-SIZE ASSIGNMENT ON AVERAGE PERCENTILE OF STANFORD ACHIEVEMENT TEST
*-------------------------------------------------------------------------------
use krueger1999_data, clear

keep if flagsg == 1
collect clear
forvalues i = 0/3 {
	qui: {
		collect, tags(row[`i'] col1[1]): reg SAT ib2.classtype if grade == `i', vce(cluster tchid)
		collect get fe="No", tags(row[`i'] col1[1])

		collect, tags(row[`i'] col1[2]): areg SAT ib2.classtype if grade == `i', vce(cluster tchid) absorb(schid)
		collect get fe="Yes", tags(row[`i'] col1[2])

		collect, tags(row[`i'] col1[3]): areg SAT ib2.classtype i.WhiteAsian ib1.gender i.freelunch if grade == `i', vce(cluster tchid) absorb(schid)
		collect get fe="Yes", tags(row[`i'] col1[3])

		collect, tags(row[`i'] col1[4]): areg SAT ib2.classtype i.WhiteAsian ib1.gender i.freelunch i.twhite i.tgen tyears i.tmd if grade == `i' , vce(cluster tchid) absorb(schid)
		collect get fe="Yes", tags(row[`i'] col1[4])

		collect, tags(row[`i'] col2[5]): reg SAT ib2.classtype_initial if grade == `i', vce(cluster tchid)
		collect get fe="No", tags(row[`i'] col2[5])

		collect, tags(row[`i'] col2[6]): areg SAT ib2.classtype_initial if grade == `i', vce(cluster tchid) absorb(schid)
		collect get fe="Yes", tags(row[`i'] col2[6])

		collect, tags(row[`i'] col2[7]): areg SAT ib2.classtype_initial i.WhiteAsian ib1.gender i.freelunch if grade == `i', vce(cluster tchid) absorb(schid)
		collect get fe="Yes", tags(row[`i'] col2[7])

		collect, tags(row[`i'] col2[8]): areg SAT ib2.classtype_initial i.WhiteAsian ib1.gender i.freelunch i.twhite i.tgen tyears i.tmd if grade == `i' , vce(cluster tchid) absorb(schid)
		collect get fe="Yes", tags(row[`i'] col2[8])

	}
}

* format 
collect dims
collect levelsof cmdset
collect levelsof coleq
collect levelsof result
collect levelsof colname

* row format
collect recode colname `"1.classtype_initial"' = `"1.classtype"'
collect recode colname `"2.classtype_initial"' = `"2.classtype"'
collect recode colname `"3.classtype_initial"' = `"3.classtype"'

collect style header result, level(hide)
collect style header result[fe r2], level(label)

local varname 1.classtype 3.classtype 1.WhiteAsian 0.gender 1.freelunch 1.twhite 1.tgen tyears 1.tmd
collect style autolevels colname  `varname'
local colname `" "Small class" "Regular/aide class" "White/Asian (1=yes)" "Girl (1=yes)" "Free lunch (1 = yes)" "White teacher" "Male teacher" "Teacher experience" "Master degress" "'
local var_count  `:word count `varname''
forvalues i = 1/`var_count' {
	local tmp `:word `i' of `varname''
	local tmp1 `:word `i' of `colname''
	collect label levels colname `"`tmp'"' `"`tmp1'"', modify
}

collect label levels result fe "School fixed effects" r2 "R2", modify
collect label levels row 0 "A. Kindergarten" 1 "B. First grade" 2 "C. Second grade" 3 "Third grade" , modify

* column format
collect style column, nodelimiter dups(center) position(top) width(asis)
collect style header col1, title(label)
collect style header col2, title(label)
collect label dim col1 "OLS: actual calss size", modify
collect label dim col2 "Reduced form: initial calss size", modify

* cell format
collect style cell col1 col2, warn halign(center) valign(center) nformat(%9.2f)
collect style cell result[_r_se], sformat("(%s)") warn

* table format
collect style cell border_block, border(right, pattern(nil))
collect title "Table V-OLS AND REDUCED-FORM ESTIMATES OF EFFECT OF CLASS-SIZE ASSIGNMENT ON AVERAGE PERCENTILE OF STANFORD ACHIEVEMENT TEST"

collect layout (row[0]#colname#result[_r_b _r_se] row[0]#result[fe r2] row[1]#colname#result[_r_b _r_se] row[1]#result[fe r2] row[2]#colname#result[_r_b _r_se] row[2]#result[fe r2] row[3]#colname#result[_r_b _r_se] row[3]#result[fe r2])  (col1 col2)
collect export "table5", as(docx) replace	

*-------------------------------------------------------------------------------
* Table VI: EXPLORATION OF EFFECT OF ATTRITION 
* DEPENDENT VARIABLE: AVERAGE PERCENTILE SCORE ON SAT
*-------------------------------------------------------------------------------
use krueger1999_data, clear

collect clear
forvalues i = 0/3 {
	qui: {
		areg SAT ib2.classtype_initial i.WhiteAsian ib1.gender if grade == `i', absorb(schid) 
		scalar coef = _b[1.classtype]
		scalar sde= _se[1.classtype]
		scalar N = e(N)
		collect get coef = coef se = sde, tags(row[`i'] col1[1])
		collect get coef = N, tags(row[`i'] col1[2])

		areg SAT_impute ib2.classtype_initial i.WhiteAsian ib1.gender if grade == `i', absorb(schid_impute)
		scalar coef = _b[1.classtype]
		scalar sde = _se[1.classtype]
		scalar N = e(N)
		collect get coef = coef se = sde, tags(row[`i'] col2[1])
		collect get coef = N, tags(row[`i'] col2[2])
	}
}

* format 
collect dims
collect levelsof cmdset
collect levelsof coleq
collect levelsof result
collect levelsof colname
collect layout (row#result) (col1 col2)

* row format
collect style header result, level(hide)
collect label levels row `"0"' `"K"', modify

* column format
collect style column, nodelimiter dups(center) position(top) width(asis)
collect style header col1, title(label)
collect style header col2, title(label)
collect label dim col1 "Actual test data", modify
collect label dim col2 "Actual and imputed test data", modify
collect label levels col1 `"1"' `"Coefficient on small class dum."' `"2"' `"Sample size"', modify
collect label levels col2 `"1"' `"Coefficient on small class dum."' `"2"' `"Sample size"', modify

* cell format
collect style cell col1 col2, warn halign(center) valign(center) nformat(%9.2f)
collect style cell col1[2] col2[2], nformat(%9.0f)
collect style cell result[se], sformat("(%s)") warn

* table format
collect style cell border_block, border(right, pattern(nil))
collect title "Table VI-EXPLORATION OF EFFECT OF ATTRITION DEPENDENT VARIABLE: AVERAGE PERCENTILE SCORE ON SAT"

collect layout (row#result) (col1 col2)
collect export "table6", as(docx) replace	

*-------------------------------------------------------------------------------
* Table 7VII: OLS AND 2SLS ESTIMATES OF EFFECT OF CLASS SIZE ON ACHIEVEMENT 
* DEPENDENT VARIABLE: AVERAGE PERCENTILE SCORE ON SAT
*-------------------------------------------------------------------------------
use krueger1999_data, clear

collect clear
forvalues i = 0/3 {
	qui: {
		reghdfe SAT classsize tyears if grade == `i' , absorb(schid WhiteAsian gender freelunch twhite  tmd) cluster(tchid)
		scalar coef = _b[classsize]
		scalar sde= _se[classsize]
		scalar N = e(N)
		collect get coef = coef se = sde, tags(row[`i'] col[OLS])
		collect get coef = N, tags(row[`i'] col[size])
	
		ivreghdfe SAT tyears (classsize =  i.classtype_initial) if grade == `i' , absorb(schid WhiteAsian gender freelunch twhite  tmd) cluster(tchid)
		scalar coef = _b[classsize]
		scalar sde= _se[classsize]
		collect get coef = coef se = sde, tags(row[`i'] col[2SLS])
		}
}

* format 
collect dims
collect levelsof cmdset
collect levelsof result
collect layout (row#result) (col)

* row format
collect style header result, level(hide)
collect label levels row `"0"' `"K"', modify

* column format
collect label levels col `"size"' `"Sample size"', modify
collect style autolevels col OLS 2SLS size

* cell format
collect style cell col, warn halign(center) valign(center) nformat(%9.2f)
collect style cell col[size], nformat(%9.0fc)
collect style cell result[se], sformat("(%s)") warn

* table format
collect style cell border_block, border(right, pattern(nil))
collect title "Table VII-OLS AND 2SLS ESTIMATES OF EFFECT OF CLASS SIZE ON ACHIEVEMENT  DEPENDENT VARIABLE: AVERAGE PERCENTILE SCORE ON SAT"

collect layout (row#result) (col)
collect export "table7", as(docx) replace	

*-------------------------------------------------------------------------------
* Table 8 VIII: 2SLS ESTIMATES OF EFFECT OF CLASS SIZE ON ACHIEVEMENT, BY ENTRY GRADE AND CURRENT GRADE 
* DEPENDENT VARIABLE: AVERAGE SCORE ON STANFORD ACHIEVEMENT TEST
*-------------------------------------------------------------------------------
use krueger1999_data, clear

collect clear
forvalues i = 0/3 {
	forvalues j = `i'/3 {
		qui: ivreghdfe SAT tyears (classsize =  i.classtype_initial) if grade == `j' & start == `i' , absorb(schid WhiteAsian gender freelunch twhite  tmd) cluster(tchid)
		scalar coef = _b[classsize]
		scalar sde= _se[classsize]
		collect get coef = coef se = sde, tags(row[`j'] col[`i'])
	}
}

* format 
collect dims
collect layout (row#result) (col)

* row format
collect style header row, title(label)
collect style header result, level(hide)
collect label dim row "Current grade"
collect label levels row `"0"' `"K"', modify

* column format
collect style header col, title(label)
collect style column, nodelimiter dups(center) position(top) width(asis)
collect label dim col "Entering grade"
collect label levels col `"0"' `"K"', modify

* cell format
collect style cell col, warn halign(center) valign(center) nformat(%9.2f)
collect style cell result[se], sformat("(%s)") warn

* table format
collect style cell border_block, border(right, pattern(nil))
collect title "Table VIII--2SLS ESTIMATES OF EFFECT OF CLASS SIZE ON ACHIEVEMENT, BY ENTRY GRADE AND CURRENT GRADE DEPENDENT VARIABLE: AVERAGE SCORE ON STANFORD ACHIEVEMENT TEST"

collect layout (row#result) (col)
collect export "table8", as(docx) replace	

*-------------------------------------------------------------------------------
* Table 9 IX: ESTIMATES OF POOLED MODELS 
* DEPENDENT VARIABLE: AVERAGE PERCENTILE RANKING ON SAT TEST 
* COEFFICIENT ESTIMATES WITH ROBUST STANDARD ERRORS IN PARENTHESES
*-------------------------------------------------------------------------------
use krueger1999_data, clear

collect clear
qui: collect: reghdfe SAT ib2.classtype_initial smallyears aidyears, absorb(grade start schid)  vce(cluster stdntid)

qui: collect:reghdfe SAT ib2.classtype_initial smallyears aidyears tyears, absorb(grade start schid WhiteAsian gender freelunch twhite tgen tmd)  vce(cluster stdntid)

qui: collect:reghdfe SAT ib2.classtype_initial smallyears aidyears frac frac_m freelunch_m kinder_m tyears, absorb(grade start schid WhiteAsian gender freelunch twhite tgen tmd)  vce(cluster stdntid)

* format 
collect dims
collect levelsof cmdset
collect levelsof result
collect levelsof colname

* row format
local varname 1.classtype_initial 3.classtype_initial smallyears aidyears frac frac_m freelunch_m kinder_m 
collect style autolevels colname `varname'
local colname `" "Initial class small (1=yes)" "Initial class regular/aide (1=yes)" "Cumulative years in small class" "Cumulative years in reg/aide class" "Fraction of classmates in class previous year" "Average fraction of classmates together previous year" "Fraction of classmates on free lunch" "Fraction of classmates who attended kindergarten" "'
local var_count  `:word count `varname''
forvalues i = 1/`var_count' {
	local tmp `:word `i' of `varname''
	local tmp1 `:word `i' of `colname''
	collect label levels colname `"`tmp'"' `"`tmp1'"', modify
}

collect get fe1 = "No", tags(cmdset[1])
collect get fe1 = "Yes", tags(cmdset[2])
collect get fe1 = "Yes", tags(cmdset[3])

collect get fe2 = "Yes", tags(cmdset[1])
collect get fe2 = "Yes", tags(cmdset[2])
collect get fe2 = "Yes", tags(cmdset[3])

collect label levels result r2 "R2" N "Sample size" fe1 "Student and teacher characteristics" fe2 "3 current grade dummies; 3 dummies indicating first grade appeared in sample; school effects", modify

collect style header result, level(hide)
collect style header result[r2 N fe1 fe2], level(label)

* cell format
collect style cell cmdset, warn halign(center) valign(center) nformat(%9.2f)
collect style cell result[N], nformat(%9.0fc)
collect style cell result[_r_se], sformat("(%s)") warn

* table format
collect style cell border_block, border(right, pattern(nil))
collect title "Table IX--ESTIMATES OF POOLED MODELS DEPENDENT VARIABLE: AVERAGE PERCENTILE RANKING ON SAT TEST COEFFICIENT ESTIMATES WITH ROBUST STANDARD ERRORS IN PARENTHESES"

collect layout (colname#result[_r_b _r_se] result[fe1 fe2 r2 N]) (cmdset)
collect export "table9", as(docx) replace	

* 文中其他统计量
* if grade 0 excluded
reghdfe SAT ib2.classtype_initial smallyears aidyears frac frac_m freelunch_m kinder_m tyears if grade > 0, absorb(grade start schid WhiteAsian gender freelunch twhite tgen tmd)  vce(cluster stdntid)

* value added model
gen small = classtype == 1 if !mi(classtype)
reghdfe SAT_dif i.small frac frac_m freelunch_m kinder_m tyears, absorb(grade start schid WhiteAsian gender freelunch twhite tgen tmd)  vce(cluster stdntid)

reghdfe SAT_dif ib2.classtype frac frac_m freelunch_m kinder_m tyears, absorb(grade start schid WhiteAsian gender freelunch twhite tgen tmd)  vce(cluster stdntid)

*-------------------------------------------------------------------------------
* Table 10 X: SEPARATE ESTIMATES FOR SELECT SAMPLES 
* DEPENDENT VARIABLE: AVERAGE PERCENTILE RANKING ON SAT TEST 
* COEFFICIENT ESTIMATES WITH ROBUST STANDARD ERRORS IN PARENTHESES
*-------------------------------------------------------------------------------
use krueger1999_data, clear

capture program drop create_table10
program create_table10
	args variable value row col
	qui: collect, tags(row[`row'] col[`col']): reghdfe SAT ib2.classtype_initial smallyears aidyears frac frac_m freelunch_m kinder_m tyears if `variable'==`value', absorb(grade start schid WhiteAsian freelunch twhite tgen tmd)  vce(cluster stdntid)
end

collect clear
create_table10 gender 1 1 2
create_table10 gender 0 1 4

create_table10 freelunch 1 2 2
create_table10 freelunch 0 2 4

create_table10 WhiteAsian 0 3 2
create_table10 WhiteAsian 1 3 4

create_table10 surban 1 4 1
create_table10 surban 2 4 2
create_table10 surban 4 4 3
create_table10 surban 3 4 4

* format 
collect dims
collect levelsof cmdset
collect levelsof result
collect levelsof colname
collect layout (row#colname[1.classtype_initial smallyears]#result[_r_b _r_se]) (col) 

* row format
collect get name = "Boys", tags(row[1] col[2])
collect get name = "Girls", tags(row[1] col[4])

collect get name = "Free lunch", tags(row[2] col[2])
collect get name = "Not on free lunch", tags(row[2] col[4])

collect get name = "Black", tags(row[3] col[2])
collect get name = "White", tags(row[3] col[4])

collect get name = "Inner city", tags(row[4] col[1])
collect get name = "Metropolitan", tags(row[4] col[2])
collect get name = "Towns", tags(row[4] col[3])
collect get name = "Rural", tags(row[4] col[4])

collect style header result, level(hide)
collect style header result[N], level(label)
collect label levels colname 1.classtype_initial "Small" smallyears "Cumulative years in small class", modify
collect label levels result N "Sample size", modify

* column format
collect style autolevels col 1 2 3 4

* cell format
collect style cell col, warn halign(center) valign(center) nformat(%9.2f)
collect style cell result[N], nformat(%9.0fc) warn
collect style cell result[_r_se], sformat("(%s)") warn

* table format
collect style cell border_block, border(right, pattern(nil))
collect title "Table X--SEPARATE ESTIMATES FOR SELECT SAMPLES DEPENDENT VARIABLE: AVERAGE PERCENTILE RANKING ON SAT TEST COEFFICIENT ESTIMATES WITH ROBUST STANDARD ERRORS IN PARENTHESES"

collect layout (row[1]#result[name] row[1]#colname[1.classtype_initial smallyears]#result[_r_b _r_se] row[1]#result[N] row[2]#result[name] row[2]#colname[1.classtype_initial smallyears]#result[_r_b _r_se] row[2]#result[N] row[3]#result[name] row[3]#colname[1.classtype_initial smallyears]#result[_r_b _r_se] row[3]#result[N] row[4]#result[name] row[4]#colname[1.classtype_initial smallyears]#result[_r_b _r_se] row[4]#result[N] ) (col) 
collect export "table10", as(docx) replace	

*-------------------------------------------------------------------------------
* Figure II Kernel Density of School-Level Small-Class Effects
*-------------------------------------------------------------------------------
use krueger1999_data, clear

levelsof schid, local(schid)
local i = 1
gen coef = .
gen se = .

foreach l of local schid {
	qui: reghdfe SAT ib2.classtype_initial  if schid == `l', absorb(grade start) vce(cluster stdntid)
	qui: replace coef = _b[1.classtype_initial] if _n == `i'
	qui: replace se = _se[1.classtype_initial] if _n == `i'
	local ++i
}

kdensity coef, title("Kernel Density of School-Level Small-Class Effects") ytitle("Density") ylabel(0(0.02)0.06) xtitle("Small Class Effect") xlabel(-20(10)30) lpattern("solid"  ) recast(connected)

graph export "figure2.jpg", as(jpg) quality(100) replace

egen t_m = mean(coef)
egen t = sd(coef)
gen t1 = t^2
egen t2 = mean(se^2)
gen t3 = sqrt(t1 - t2)

*-------------------------------------------------------------------------------
* Hawthorne and John Henry Effects
*-------------------------------------------------------------------------------
use krueger1999_data, clear

* without school dummy
reghdfe SAT classsize tyears if classtype == 2, absorb(WhiteAsian gender freelunch twhite tgen tmd grade  ) vce(cluster stdntid)

* with school dummy
reghdfe SAT classsize tyears if classtype == 2, absorb(WhiteAsian gender freelunch twhite tgen tmd grade schid ) vce(cluster stdntid)

*-------------------------------------------------------------------------------
* Table 11 XI: ESTIMATES OF POOLED DATA MODEL BY SUBJECT TEST 
* DEPENDENT VARIABLE: PERCENTILE SCORE ON SAT OR BSF TEST 
* COEFFICIENT ESTIMATES WITH ROBUST STANDARD ERRORS IN PARENTHESES
*-------------------------------------------------------------------------------
use krueger1999_data, clear

capture program drop create_table11
program create_table11
	args variable col1 col
	qui: collect, tags(col1[`col1'] col[`col']): reghdfe `variable' ib2.classtype_initial smallyears aidyears frac frac_m freelunch_m kinder_m tyears, absorb(grade start schid WhiteAsian gender freelunch twhite tgen tmd)  vce(cluster stdntid)
end

collect clear
create_table11 SAT_math 1 1 
create_table11 SAT_read 2 1 
create_table11 SAT_word 3 1 

create_table11 BSF_math 1 2 
create_table11 BSF_read 2 2 
create_table11 BSF 3 2

* format 
collect dims
collect levelsof cmdset
collect levelsof result
collect levelsof colname
collect layout (colname[1.classtype_initial smallyears]#result[_r_b _r_se] result[N]) (col#col1) 

* row format
collect style header result, level(hide)
collect style header result[N], level(label)
collect label levels colname 1.classtype_initial "Small" smallyears "Cumulative years in small class", modify
collect label levels result N "Sample size", modify

* column format
collect style column, nodelimiter dups(center) position(top) width(asis)
collect label levels col 1 "Standford Achievement Test" 2 "Basic Skills First", modify
collect label levels col1 1 "Math" 2 "Reading" 3 "Word", modify

* cell format
collect style cell col, warn halign(center) valign(center) nformat(%9.2f)
collect style cell result[N], nformat(%9.0fc) warn
collect style cell result[_r_se], sformat("(%s)") warn

* table format
collect style cell border_block, border(right, pattern(nil))
collect title "Table XI--ESTIMATES OF POOLED DATA MODEL BY SUBJECT TEST DEPENDENT VARIABLE: PERCENTILE SCORE ON SAT OR BSF TEST COEFFICIENT ESTIMATES WITH ROBUST STANDARD ERRORS IN PARENTHESES"

collect layout (colname[1.classtype_initial smallyears]#result[_r_b _r_se] result[N]) (col#col1) 
collect export "table11", as(docx) replace	



















