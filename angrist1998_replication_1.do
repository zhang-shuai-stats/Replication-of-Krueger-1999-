* 2024/6/13
clear
cd /Users/zhangshuai/Desktop/interest/angrist1998_replication // 修改默认目录

*-----------------------------------------------------------------------------
* data preparation and label variable 
*-----------------------------------------------------------------------------
use ssaone, clear

* lable variable
rename earnvar earn // 方便计算
label variable afqtgrp "AFQT group"
label variable dnwhite "Race"
label variable dobyy "Birth Year"
label variable dvet "Veteran status"
label variable edgrp "Schooling completed at time of application"
label variable transyy "Application Year"
label variable probvet "Probability of being veteran"
label variable popcount "每个cell的总体人口数"
label variable vetcount "相同属性的两个cell（入伍/未入伍）的总体人口数"
label variable samcount "每个cell的样本人口数"
label variable smplprop "抽样比samcount/popcount"
label variable earn "每组平均实际收入 in 1991 dollars"
label variable stdvar "每组收入标准差"
label variable zerovar "每组0收入人数"

* 平均收入方差
gen earn_bar_var = stdvar^2/samcount
gen earn_var = earn_bar_var * popcount
label variable earn_bar_var "每组平均收入方差"
label variable earn_var "每组收入总体方差"

* employment rate 就业率相关变量
gen employ = 100 - zerovar/samcount*100
gen employ_bar_var = employ * (100 - employ)/samcount 
gen employ_var = employ_bar_var * popcount

label variable employ "Employment rates(%)"
label variable employ_var "每组就业率总体方差"
label variable employ_bar_var "每组平均就业率方差"

* label values 根据p264的统计数字
label define afqt 8  "I" 7 "II" 6 "IIIa" 5 "IIIb" 4 "IVa" 3 "IVb" 2 "IVc" 1 "V"
label values afqtgrp afqt

gen afqtgrp1 = afqtgrp
replace afqtgrp1 = 2 if inrange(afqtgrp,2,6)
label define afqt1 8  "I" 7 "II" 2 "III and IV" 1 "V"
label values afqtgrp1 afqt1

label define edg 3 "Grade 9 or 10" 5 "Grade 11" 7 "GED certified" 6 "High school graduate" 13 "Some college" 10 "College graduate"
label values edgrp edg

label define dnw 0 "White" 1 "Nonwhite"
label values dnwhite dnw

label define dvet 0 "Nonveteran" 1 "Veteran"
label values dvet dvet

save angrist1998_data, replace 

*-----------------------------------------------------------------------------
* Table I APPLICANT POPULATION AND SAMPLE
*-----------------------------------------------------------------------------
use angrist1998_data, clear 
keep if year == 80   // 随意保留一个年份即可，由于
replace popcount = popcount/1000  // 人数显示以千为单位
replace samcount = samcount/1000
replace transyy = 1900 + transyy

* population
table (dnwhite) (transyy) [iweight = popcount], nototals
table (dnwhite dvet) (transyy) [iweight = popcount], stat(percent, across(dvet)) nototals append 

* sample
table (dnwhite) (transyy) [iweight = samcount], nototals append  
table (dnwhite dvet) (transyy) [iweight = samcount], stat(percent, across(dvet)) nototals append

* format 
collect dims
collect levelsof colname
collect levelsof result
collect levelsof transyy
collect levelsof across

* row format
collect addtags dvet[1], fortags(result[sumw])
collect recode cmdset `"2"' = `"1"'
collect recode cmdset `"4"' = `"3"'

collect style header cmdset, title(hide)
collect style header dnwhite, title(hide)
collect style header dvet, title(hide) level(hide)
collect style header result[sumw], level(hide)

collect label levels cmdset 1 "A. Population" 3 "B. Sample", modify
collect label levels result percent "Percent veteran", modify

* cell format
collect style cell transyy, warn halign(center) valign(center) nformat(%9.1f)
collect style cell result[percent], warn nformat(%9.0f)

* table format
collect style cell border_block, border(right, pattern(nil))
collect title "Table I-- APPLICANT POPULATION AND SAMPLE"

collect layout (cmdset#dnwhite#dvet[1]#result) (transyy)
collect export "table1", as(docx) replace	

*------------------------------
* statistics p263-4
*------------------------------
* age at the time of application
use angrist1998_data, clear
keep if year == 79
gen age_application = transyy - dobyy
table age_application [iweight = samcount],stat(percent)   nototals  // 年龄
table edgrp [iweight = samcount], stat(sumw) stat(percent)  // 教育
table afqtgrp1 dnwhite [fweight = samcount] if transyy == 79, stat(percent, across(afqtgrp1)) nototals  // afqt得分

*-----------------------------------------------------------------------------
* FIGURE 1.- Social Security earnings profiles of men who  applied to the military in 1976-1982, by year of application. Earnings are in 1991 dollars
*-----------------------------------------------------------------------------
use angrist1998_data, clear 

collapse (mean) earn [fweight = popcount], by(transyy year)

twoway  (line earn year if transyy == 76, lpattern(dash))  ///
		(line earn year if transyy == 77) ///
		(line earn year if transyy == 78 , lpattern(dash))  ///
		(line earn year if transyy == 79, lpattern(dash)) ///
		(line earn year if transyy == 80)  ///
		(line earn year if transyy == 81, lpattern(dash)) ///
		(line earn year if transyy == 82, lpattern(dash)) , ///
		text( 10000 77.6 "1976", place(e) size(tiny)) ///
		text( 9500 78.6 "1977", place(e) size(tiny)) ///
		text( 8500 79.6 "1978", place(e) size(tiny)) ///
		text( 6000 79 "1979", place(e) size(tiny)) ///
		text( 5000 80 "1980", place(e) size(tiny)) ///
		text( 4000 80.5 "1981", place(e) size(tiny)) ///
		text( 3000 81 "1982 applicants", place(e) size(tiny)) ///
		ylabel(0(2000)16000) xlabel(74(2)90) legend(off) aspect(1.5) ///
		ytitle("FICA Earnings",size(small)) ///
		xtitle("Year", size(small)) ///	
		title("FIGURE 1.- Social Security earnings profiles of men who  applied to the military in 1976-1982, by year of application. Earnings are in 1991 dollars", size(tiny) position(6))
		
graph export "figure1.jpg", as(jpg) quality(100) replace

*-----------------------------------------------------------------------------
* FIGURE 2.-Earnings profiles by veteran status and application for men who applied 1979-82, with AFQT scores in categories III and IV. The plot shows the actual earnings for men who applied in 1982, earnings + $3000 for men who applied in 1981, earnings + $6000 for men who applied in 1980, earnings + $9000 for men who applied in 1979."
*--------------------------------------------------------------------------	
* restricted samples
use angrist1998_data, clear
keep if inrange(transyy,79,82) & afqtgrp1 == 2

* statistics in p265
table dnwhite [fweight = samcount] if year == 74

collapse (mean) earn [fweight = popcount], by(transyy year dvet)
replace earn = earn + 3000 if transyy == 81
replace earn = earn + 6000 if transyy == 80
replace earn = earn + 9000 if transyy == 79

twoway  (line earn year if transyy == 79 & dvet == 0, lpattern(dash))  ///
		(line earn year if transyy == 79 & dvet == 1) ///
		(line earn year if transyy == 80 & dvet == 0, lpattern(dash))  ///
		(line earn year if transyy == 80 & dvet == 1) ///		
		(line earn year if transyy == 81 & dvet == 0, lpattern(dash))  ///
		(line earn year if transyy == 81 & dvet == 1) ///	
		(line earn year if transyy == 82 & dvet == 0, lpattern(dash))  ///
		(line earn year if transyy == 82 & dvet == 1), ///	
		text( 14000 79 "1979", place(e) size(small)) ///
		text( 10000 80 "1980", place(e) size(small)) ///
		text( 6000 81 "1981", place(e) size(small)) ///
		text( 3000 82 "1982 applicants", place(e) size(small)) ///
		ytitle("FICA Earnings") legend(pos(10) ring(0)  order(1 2) col(1)  ///
		label(1 "Nonveterans") label(2 "Veterans") ///
		region(lcolor(black)) subtitle("Veterans status:") size(small) ) ///
		ylabel(0(5000)25000) xlabel(74(2)90) xtitle("Year") ///
		title("FIGURE 2.-Earnings profiles by veteran status and application for men who applied 1979-82, with AFQT scores in categories III and IV. ", size(vsmall) position(6))
		
graph export "figure2.jpg", as(jpg) quality(100) replace

*-----------------------------------------------------------------------------
* TABLE II--ALTERNATIVE ESTIMATES OF THE EFFECTS OF MILITARY SERVICE
*-----------------------------------------------------------------------------
/*
表2不同列的差别在于加权权重：（1）直接对比相当于分成处理组和对照组两组，计算每组的均值和方差即可得到差值及其方差；（2）匹配是根据协变量分成许多处理组和对照组，首先计算每组的差异，然后按照处理组协变量发生的概率为权重计算加权和；（3）回归也是根据协变量分成许多处理组和对照组，首先计算每组的差异，但是加权权重为（每组事件发生的方差*协变量出现的概率）/sum(每组事件发生的方差*协变量出现的概率)
*/
use angrist1998_data, clear
keep if inrange(transyy,79,82) & afqtgrp1 == 2 
drop if mi(earn)
save angrist1998_data1, replace 

*------------------------------
* column 1 and 5 （overall mean)
*------------------------------
table (year) (dnwhite) [fweight = popcount], stat(mean earn employ) nototals

*-----------------------------------
* column 2 and 6 (simple comparison)
*-----------------------------------
collapse (rawsum) popcount (mean) earn earn_var employ employ_var [fweight = popcount], by(year dvet dnwhite)

replace earn_var = earn_var/popcount
replace employ_var = employ_var/popcount
drop popcount
reshape wide earn earn_var employ employ_var, i(year dnwhite) j(dvet)

gen dif1 = earn1 - earn0
gen dif2 = sqrt(earn_var1 + earn_var0)
gen dif3 = employ1 - employ0
gen dif4 = sqrt(employ_var1 + employ_var0)

table (year) (dnwhite), stat(mean dif* ) nototals append

*-------------------------------------
* column 3 and 7 (controlled contrast) 
* column 4 and 8 (regression estimates)
*-------------------------------------
use angrist1998_data1, clear

keep earn earn_bar_var employ employ_bar_var popcount afqtgrp dnwhite dobyy dvet edgrp transyy year

reshape wide earn earn_bar_var employ employ_bar_var popcount, i(afqtgrp dnwhite dobyy edgrp transyy year) j(dvet)
drop if mi(earn0) | mi(earn1)

egen s1 = sum(popcount1),by(year dnwhite)
egen s0 = sum(popcount0),by(year dnwhite)
gen s = s0 + s1

save angrist1998_data2, replace 

capture program drop create_table2
program create_table2
	args weight
	
	gen dif1 = earn1 - earn0
	gen dif2 = (earn_bar_var1 + earn_bar_var0) * `weight'
	gen dif3 = employ1 - employ0
	gen dif4 = (employ_bar_var1 + employ_bar_var0)* `weight'

	collapse (rawsum) `weight' (mean) dif* [iweight=`weight'], by(year dnwhite)
	replace dif2 = sqrt(dif2)
	replace dif4 = sqrt(dif4)

end

* contrast (方差的计算方式using note in p274)
use angrist1998_data2, clear

gen weight = popcount1/s1 // contrast
create_table2 weight
table (year) (dnwhite), stat(mean dif* ) nototals append

save col3, replace  // 为了后面计算contrast与regression的协方差
 
* regression
use angrist1998_data2, clear

gen weight = popcount0*popcount1/(popcount0+popcount1)*(s/(s1*s0))  // 按照公式9设置回归权重
create_table2 weight //  注意文章默认 sum(weight)=1，但这里并不是
table (year) (dnwhite), stat(mean dif* ) nototals append

save col4, replace 

*-------------------------------------
* 设置表格格式
*-------------------------------------
* format 
collect dims
collect levelsof colname
collect levelsof result
collect levelsof type
collect layout (year#colname) (dnwhite#cmdset)

* row format
collect recode colname `"earn"' = `"dif1"'
collect recode colname `"employ"' = `"dif3"'
collect addtags rowname[1], fortags(colname[dif1]#colname[dif2])
collect addtags rowname[2], fortags(colname[dif3]#colname[dif4])

collect style header year, title(hide)
collect style header colname, level(hide)
collect label levels rowname 1 "A. Earnings" 2 "B. Employment Rates", modify

* column format
collect style header dnwhite, title(hide)
collect style header cmdset, title(hide)
collect label levels cmdset 1 "Mean" 2 "Difference in Means" 3 "Controlled Contrast" 4 "Regression Estimates", modify

* cell format
collect style cell dnwhite, warn halign(center) valign(center) nformat(%9.1f)
collect style cell colname[dif4], nformat(%9.2f) warn
collect style cell colname[dif2 dif4],sformat("(%s)") warn

* table format
collect style cell border_block, border(right, pattern(nil))
collect title "TABLE II--ALTERNATIVE ESTIMATES OF THE EFFECTS OF MILITARY SERVICE"

collect layout (rowname#year#colname[dif1 dif2] rowname#year#colname[dif3 dif4]) (dnwhite#cmdset)
collect export "table2", as(docx) replace	

*---------------------------------------------------
* correlation between contrast and regression (p274)
*---------------------------------------------------
use angrist1998_data2, clear

gen weight1 = popcount1/s1
gen weight2 = popcount0*popcount1/(popcount0+popcount1)*(s/(s1*s0))

gen cov2 = (earn_bar_var1 + earn_bar_var0) * weight1 * weight2
gen cov4 = (employ_bar_var1 + employ_bar_var0)* weight1 * weight2

collapse (sum) cov*, by(year  dnwhite) // 协方差

merge 1:1 dnwhite year  using col3
rename dif* ctrol*
drop _merge 

merge 1:1 dnwhite year using col4
rename dif* reg*
drop _merge 

gen std1 = sqrt(ctrol2^2 +reg2^2 - 2*cov2)
gen std2 = sqrt(ctrol4^2 +reg4^2 - 2*cov4)

list std1 if year == 91 & dnwhite == 1 
* 与作者的SAS程序结果一致，但是与文中报告结果不一致

*---------------
* 对文章结果的修正
*---------------
use angrist1998_data2, clear

gen weight = popcount0*popcount1/(popcount0+popcount1)*(s/(s1*s0))
egen weight_s = sum(weight), by(year dnwhite)
replace weight = weight/weight_s // 将regress的weight之和等于1

create_table2 weight  

save col4, replace 

* correlation between contrast and regression (p274)
use angrist1998_data2, clear

gen weight1 = popcount1/s1
gen weight2 = popcount0*popcount1/(popcount0+popcount1)*(s/(s1*s0))
egen weight_s = sum(weight2), by(year dnwhite)
replace weight2 = weight2/weight_s

gen cov2 = (earn_bar_var1 + earn_bar_var0) * weight1 * weight2
gen cov4 = (employ_bar_var1 + employ_bar_var0)* weight1 * weight2

collapse (sum) cov*, by(year  dnwhite) // 协方差

merge 1:1 dnwhite year using col3
rename dif* ctrol*
drop _merge 

merge 1:1 dnwhite year using col4
rename dif* reg*
drop _merge 

gen std1 = sqrt(ctrol2^2 +reg2^2 - 2*cov2)
gen std2 = sqrt(ctrol4^2 +reg4^2 - 2*cov4)

list std1 if year == 91 & dnwhite == 1 
* 与文章结果一致

*-------------------------------------------------------------------------
* 假设直接使用回归的方法估算column 4 and 8 (regression estimates using regression)
*-------------------------------------------------------------------------
* 首先整理加权方式计算的回归结果，用于对比
use col4, clear
table (year) (dnwhite), stat(mean dif1 dif2 ) nototals name(t1) replace 
table (year) (dnwhite), stat(mean dif3 dif4 ) nototals name(t1) append

collect remap colname[`"dif1"'] = result1[`"mean"']
collect remap colname[`"dif2"'] = result1[`"se"']
collect remap colname[`"dif3"'] = result1[`"mean"']
collect remap colname[`"dif4"'] = result1[`"se"']
collect remap result[`"mean"'] = result2[`"mean"']  // 防止与下文的result冲突
collect addtags col[1], fortags(dnwhite[0 1])

collect layout (cmdset#year#result1) (col#dnwhite), name(t1) // 直接采用回归的结果也按照这个方式添加tags

* 回归的方法
use angrist1998_data1, clear

keep earn  employ  popcount afqtgrp dnwhite dobyy dvet edgrp transyy year
reshape wide earn  employ  popcount, i(afqtgrp dnwhite dobyy edgrp transyy year) j(dvet)
drop if mi(earn0) | mi(earn1)

* statistics in p273
table dnwhite if year == 91

egen X = group(afqtgrp dobyy edgrp transyy)
reshape long earn employ popcount, i(afqtgrp dnwhite dobyy  edgrp transyy year X) j(dvet)

forvalues i = 0/1 {
	forvalues j = 74/91 {
		qui: areg earn dvet [aweight = popcount] if year ==`j' & dnwhite ==`i', absorb(X)
		collect get mean = _b[dvet] se = _se[dvet], name(t1) tags(year[`j'] dnwhite[`i'] cmdset[1] col[2])	
		qui: areg employ dvet [aweight = popcount] if year ==`j' & dnwhite ==`i', absorb(X)
		collect get mean = _b[dvet] se = _se[dvet], name(t1) tags(year[`j'] dnwhite[`i'] cmdset[2] col[2])	
	}	
}

collect remap result[`"mean"' `"se"'] = result1[`"mean"' `"se"'], name(t1)
collect layout (cmdset#year#result1) (dnwhite#col), name(t1) 

* 与加权计算方法对比，方差存在偏差
*-----------------------------------------------------------------------------
* FIGURE 3.-Controlled contrasts by application year and calendar year for whites (a) and nonwhites (b).
*-----------------------------------------------------------------------------
use angrist1998_data1, clear

keep afqtgrp dnwhite dobyy dvet edgrp transyy earn  popcount year 
reshape wide earn  popcount, i(afqtgrp dnwhite dobyy  edgrp transyy year) j(dvet)
drop if mi(earn0) | mi(earn1)
gen earn_dif = earn1 - earn0
collapse (mean) earn_dif  [fweight = popcount1], by(year transyy dnwhite)

twoway  (line earn_dif year if transyy == 79 & dnwhite == 0)  ///
		(line earn_dif year if transyy == 80 & dnwhite == 0, lpattern(dash))  ///
		(line earn_dif year if transyy == 81 & dnwhite == 0, lpattern(dot))  ///
		(connected earn_dif year if transyy == 82 & dnwhite == 0, lpattern(dash_dot)) , ///
		ytitle("Earnings gap") legend(pos(1) ring(0)  col(1)  ///
		label(1 "1979") label(2 "1980") label(3 "1981") label(4 "1982") ///
		region(lcolor(black)) subtitle("Application Year:") size(small) ) ///
		ylabel(-1000(1000)3000) xlabel(74(2)90) title("a.Whites") ///
		name(f1,replace) nodraw 
	
twoway  (line earn_dif year if transyy == 79 & dnwhite == 1)  ///
		(line earn_dif year if transyy == 80 & dnwhite == 1, lpattern(dash))  ///
		(line earn_dif year if transyy == 81 & dnwhite == 1, lpattern(dot))  ///
		(connected earn_dif year if transyy == 82 & dnwhite == 1, lpattern(dash_dot)) , ///
		ytitle("Earnings gap") legend(off ) ylabel(-1000(1000)3000) ///
		xlabel(74(2)90) title("b.Nonwhites") ///
		name(f2,replace) nodraw 
		
graph combine f1 f2, col(1) iscale(0.5) xsize(5) ysize(5) title("FIGURE 3.-Controlled contrasts by application year and calendar year for whites (a) and nonwhites (b).",size(tiny) position(6))
graph export "figure3.jpg", as(jpg) quality(100) replace

*-----------------------------------------------------------------------------
* FIGURE 4.-Controlled contrasts by race and probability of service. These estimates are for pooled 1988-91 earnings.
*-----------------------------------------------------------------------------
use angrist1998_data1, clear

keep if inrange(year,88,91)
collapse (mean) earn popcount probvet, by(afqtgrp dnwhite dobyy dvet edgrp transyy)
reshape wide  earn  popcount probvet, i(afqtgrp dnwhite dobyy  edgrp transyy ) j(dvet)
drop if mi(earn0) | mi(earn1)

gen probvet_ = popcount1/(popcount0+popcount1)  // 检验概率是否一致

summarize probvet1
scalar mini = r(min)

gen pvet = 0.075
forvalues i = 1/10 {
	local j = `i' + 1
	replace pvet = 0.075*`j' if probvet1 >  mini + 0.075*`i'
}
table pvet

gen earn_dif = earn1 - earn0
collapse (mean) earn_dif  [fweight = popcount1], by(dnwhite pvet)

twoway  (line earn_dif pvet if dnwhite == 0)  ///
		(line earn_dif pvet if dnwhite == 1, lpattern(dash)) , ///
		ytitle("Earnings gap",size(small)) ///
		xtitle("Probability of service",size(small)) ///
		legend(pos(1) ring(0)  col(1)  ///
		label(1 "White") label(2 "Nonwhite") region(lcolor(black)) ///
		subtitle("Race:") size(small) ) ///
		ylabel(-1000(1000)5000) xlabel(0(0.1)0.9) yscale(r(-1000 5000)) ///
		title("FIGURE 4.-Controlled contrasts by race and probability of service", size(small) pos(6))
		
graph export "figure4.jpg", as(jpg) quality(100) replace
* 概率分组可能存在一定问题

*-----------------------------------------------------------------------------
* TABLE III--PROBABILITIES OF ENLISTMENT BY AFQT SCORE AND APPLICATION YEAR
*----------------------------------------------------------------------------
use angrist1998_data1, clear
keep if  year == 91
replace dvet = 100 * dvet

* note of table 3
table  dnwhite [fweight = popcount] 

collect clear
table (dnwhite transyy) (afqtgrp) [fweight = popcount], stat(mean dvet) nototals

* 计算差值
levelsof afqtgrp, local(afqt)
foreach i in 0 1 {
	foreach j in `afqt' {
		qui: summarize dvet [fweight = popcount] if dnwhite == `i' & afqtgrp == `j' & transyy == 79
		local m1 = r(mean)
		
		qui: summarize dvet [fweight = popcount] if dnwhite == `i' & afqtgrp == `j' & transyy == 82
		local m2 = r(mean)
		
		local m = `m1' - `m2'
		collect get mean = `m', tags(dnwhite[`i'] transyy[83] afqtgrp[`j'] )	
	}
}

* format 
collect dims
collect levelsof result
collect layout (dnwhite#transyy) (afqtgrp)

* row format 
collect style header dnwhite, title(hide)
collect style header transyy, title(hide)
collect label levels transyy 83 "1972-1982 Difference", modify

* cell format
collect style cell afqtgrp, warn halign(center) valign(center) nformat(%9.1fc)

* table format
collect style cell border_block, border(right, pattern(nil))
collect title "TABLE III--PROBABILITIES OF ENLISTMENT BY AFQT SCORE AND APPLICATION YEAR"

collect layout (dnwhite#transyy) (afqtgrp)
collect export "table3", as(docx) replace	

*-----------------------------------------------------------------------------
* Create data for IV regression
*-----------------------------------------------------------------------------
use angrist1998_data1, clear

drop if year == transyy
* calculate y,x and epsilon
collapse (mean) dvet earn earn_var employ employ_var (rawsum) popcount [fweight = popcount], by(dobyy edgrp afqtgrp transyy dnwhite year)

gen earn_epsilon = earn_var/popcount
gen employ_epsilon = employ_var/popcount

* group identifier
egen X = group(dobyy edgrp afqtgrp)
egen Y = group(dobyy edgrp transyy)
gen t = 82 - transyy

save angrist1998_iv, replace 
*-----------------------------------------------------------------------------
* Figure 5 and 6
*-----------------------------------------------------------------------------
* 注意：尽管作者声称是iv估算，但实际估算过程仍然是ols
* 在另一个版本，我尝试了iv但是结果与文中差别巨大
use angrist1998_iv, clear

foreach i in earn employ {
	qui: {
		egen alpha_`i' = mean(1/`i'_epsilon), by(year dnwhite) // adjust rmse 
		gen coef_`i' = .
		gen var_`i' = .
		gen std_`i' = .
		gen rmse_`i' = .
		gen df_`i' = .
		gen gamma_`i' = .
		
		forvalues j = 0/1 {
			forvalues y = 74/91 {
				areg `i' dvet transyy [aweight = 1/`i'_epsilon]  if year == `y' & dnwhite == `j', absorb(X)			
				replace coef_`i' = r(table)[1,1]  if year == `y' & dnwhite == `j'
				replace var_`i' = r(table)[2,1] if year == `y' & dnwhite == `j'
				replace df_`i' = e(df_r) if year == `y' & dnwhite == `j'
				replace rmse_`i' = e(rmse) * sqrt(alpha_`i') if year == `y' & dnwhite == `j' //adjust rmse
				replace std_`i' = var_`i' / rmse_`i' if year == `y' & dnwhite == `j'
				replace gamma_`i' = rmse_`i'^2*df_`i' if year == `y' & dnwhite == `j'
			}
		}
	}
}

keep year dnwhite coef* std* df* gamma*  // results compared to table4
duplicates drop

keep year dnwhite coef* std*

gen coef_earn0 = coef_earn - 1.96 * std_earn
gen coef_earn1 = coef_earn + 1.96 * std_earn
gen coef_employ0 = coef_employ - 1.96 * std_employ
gen coef_employ1 = coef_employ + 1.96 * std_employ

sort dnwhite year

* FIGURE 5.-Instrumental variables estimates of earnings effects from a model with linear controls for time-since-application for whites (a) and nonwhites (b).
twoway  (line coef_earn year if dnwhite == 0)  ///
		(line coef_earn0 year if  dnwhite == 0, lpattern(dash))  ///
		(line coef_earn1 year if  dnwhite == 0, lpattern(dash)), ///
		ytitle("IV veteran coefficient") legend(pos(1) ring(0)  col(1)  ///
		label(1 "Treatment effect") label(2 "Confidence bands") order(1 2) ///
		region(lcolor(black)) size(small) ) xtitle("Year") ///
		ylabel(-1000(1000)5000) xlabel(74(2)90) title("a.Whites") ///
		name(f1,replace) nodraw		
		
twoway  (line coef_earn year if dnwhite == 1)  ///
		(line coef_earn0 year if  dnwhite == 1, lpattern(dash))  ///
		(line coef_earn1 year if  dnwhite == 1, lpattern(dash)), ///
		ytitle("IV veteran coefficient")  legend(off) xtitle("Year") ///
		ylabel(-1000(1000)5000) xlabel(74(2)90) title("b.Nonwhites") ///
		name(f2,replace) nodraw
		
graph combine f1 f2, col(1) iscale(0.5) xsize(5) ysize(5) title("FIGURE 5.-Instrumental variables estimates of earnings effects from a model with linear controls for time-since-application for whites (a) and nonwhites (b).",size(tiny) position(6))
graph export "figure5.jpg", as(jpg) quality(100) replace
		
* FIGURE 6.-Instrumental variables estimates of employment effects from a model with linear controls for time-since-application for whites (a) and nonwhites (b).
twoway  (line coef_employ year if dnwhite == 0)  ///
		(line coef_employ0 year if  dnwhite == 0, lpattern(dash))  ///
		(line coef_employ1 year if  dnwhite == 0, lpattern(dash)), ///
		ytitle("IV veteran coefficient") legend(pos(1) ring(0)  col(1)  ///
		label(1 "Treatment effect") label(2 "Confidence bands") order(1 2) ///
		region(lcolor(black)) size(small) ) xtitle(Year) ///
		xlabel(74(2)90) ylabel(-10(10)30) title("a.Whites") ///
		name(f1,replace) nodraw

			
twoway  (line coef_employ year if dnwhite == 1)  ///
		(line coef_employ0 year if  dnwhite == 1, lpattern(dash))  ///
		(line coef_employ1 year if  dnwhite == 1, lpattern(dash)), ///
		ytitle("IV veteran coefficient") legend(off) xtitle("Year") ///
		xlabel(74(2)90) ylabel(-10(10)30) title("b.Nonwhites") ///
		name(f2,replace) nodraw

graph combine f1 f2, col(1) iscale(0.5) xsize(5) ysize(5) title("FIGURE 6.-Instrumental variables estimates of employment effects from a model with linear controls for time-since-application for whites (a) and nonwhites (b).",size(tiny) position(6))
graph export "figure6.jpg", as(jpg) quality(100) replace
	
*-----------------------------------------------------------------------------
* TABLE IV--IV ESTIMATES
*-----------------------------------------------------------------------------
* 注意：尽管作者声称是iv估算，但实际估算过程仍然是ols
use angrist1998_iv, clear

collect clear
local ii = 1
foreach i in earn employ {
	egen alpha_`i' = mean(1/`i'_epsilon), by(year dnwhite) // adjust rmse stata报告的rmse是除以加权值均值后的结果
	local jj = 2*`ii' - 1  // 为了最终表的排列
	local jj1 = `jj' + 1
	forvalues j = 0/1 {  // 民族
		forvalues y = 74/91 {  // 年份
			qui: summarize alpha_`i' if year == `y' & dnwhite == `j'
			local alpha = r(mean)
			
			qui: areg `i' dvet i.X [aweight = 1/`i'_epsilon]  if year == `y' & dnwhite == `j', absorb(Y)
			local coef = _b[dvet]
			local sd = _se[dvet]
			local df = e(df_r)
			local rmse = e(rmse) * sqrt(`alpha')
			local sd1 = `sd'/`rmse'  // 既然是加权回归，就认为rmse为1
			local chi = `rmse'^2*`df'
			
			collect get coef = `coef' se = `sd1', tags(year[`y'] dnwhite[`j'] var[`jj'] )
			collect get coef = `chi' se = `df', tags(year[`y'] dnwhite[`j'] var[`jj1'] )			
		}
	}
	local ++ii
}

* format 
collect dims
collect layout (year#result) (dnwhite#var)

* row format 
collect style header result, level(hide)

* column format
collect style column, nodelimiter dups(center) position(top) width(asis)
collect label levels var 1 "Earnings Estimate" 2 "Chi2" 3 "Employment Estimate" 4 "Chi2", modify

* cell format
collect style cell dnwhite, warn halign(center) valign(center) 
collect style cell var[1], warn nformat(%9.1f)
collect style cell var[2 4], warn nformat(%9.0f)
collect style cell var[3], warn nformat(%9.2f)
collect style cell result[se], sformat("(%s)") warn

* table format
collect style cell border_block, border(right, pattern(nil))
collect title "TABLE IV--IV ESTIMATES"

collect layout (year#result) (dnwhite#var)
collect export "table4", as(docx) replace	
* 注意： 80-81年结果与文章存在差异

*-----------------------------------------------------------------------------
* Create data for Table 5 表5所使用的数据
*-----------------------------------------------------------------------------
* 1983 during service
use angrist1998_data1, clear
keep if inrange(transyy,79,82) & afqtgrp1 == 2 & year == 83
keep dnwhite edgrp dobyy transyy afqtgrp dvet  popcount earn* employ* year

save temp83, replace 


* 1984-87 transition years
capture program drop pooldata 
program pooldata 
	args var1 var2 year1 year2 
	tempfile temp temp0 temp1 
	qui: {
		use angrist1998_data, clear
		keep if inrange(transyy,79,82) & afqtgrp1 == 2 & inrange(year,`year1',`year2')
		rename (`var1' `var2' popcount) (y s p)
		keep dnwhite edgrp dobyy transyy afqtgrp dvet y s p year
		reshape wide y s p, i(dnwhite edgrp dobyy transyy afqtgrp dvet) j(year)

		save `temp', replace 

		forvalues white = 0/1 {
			use `temp', clear
			keep if dnwhite == `white'
			
			corr y*
			mat list r(C)
			gen rho = .
			local id = 1
			forvalues i = 1/3 {
				local ii = `i' + 1
				forvalues j = `ii'/4 {
					replace rho = r(C)[`j',`i'] if _n == `id'
					local ++id
				}
			}

			gen p_sum = 0
			gen y_sum = 0
			gen s_sum = 0
			forvalues i = `year1'/`year2' {
				replace p_sum = p_sum +  p`i'
				replace y_sum = y_sum +  y`i' * p`i'
				replace s_sum = s_sum +  s`i' * p`i'^2
			}

			local id = 1
			forvalues i = `year1'/`year2' {
				local ii = `i' + 1
				forvalues j = `ii'/`year2' {
					replace s_sum = s_sum +  2 * sqrt(s`i') * p`i' * sqrt(s`j') * p`j' * rho[`id']
					local ++id
				}
			}

			gen y_mean = y_sum / p_sum
			gen s_mean = s_sum / p_sum^2
			
			save `temp`white'', replace 

		}

		use `temp0', clear
		append using `temp1'
		
		gen popcount = p_sum / 4
		rename y_mean `var1'
		rename s_mean `var2'
		gen `var1'_var = `var2' * popcount
		
		if `"`var1'"' == "employ" { // 标记就业率为1或0，即方差为0，无法进行加权回归
			rename s?? employ??  
		}
		
		save temp`year1'_`var1', replace 
	}
end

foreach i in earn employ {
	pooldata `i' `i'_bar_var 84 87
	pooldata `i' `i'_bar_var 88 91
}

* 1984-87 data
use temp84_earn, clear
merge 1:1 dnwhite edgrp dobyy transyy afqtgrp dvet using temp84_employ
keep dnwhite edgrp dobyy transyy afqtgrp dvet  popcount earn* employ*
gen year = 84

save temp84, replace 

* 1988-91 data
use temp88_earn, clear
merge 1:1 dnwhite edgrp dobyy transyy afqtgrp dvet using temp88_employ
keep dnwhite edgrp dobyy transyy afqtgrp dvet  popcount earn* employ*
gen year = 88

save temp88, replace 

* 合并所有数据
use temp83, clear
append using temp84 
append using temp88 

save angrist1998_data3, replace 

*-----------------------------------------------------------------------------
* TABLE V--SUMMARY OF RESULTS
*-----------------------------------------------------------------------------
********************
* simple comparison
********************
use angrist1998_data3, clear 
collapse (rawsum) popcount (mean) earn earn_var employ employ_var [fweight = popcount], by(year dvet dnwhite)

replace earn_var = earn_var/popcount
replace employ_var = employ_var/popcount
drop popcount
reshape wide earn earn_var employ employ_var, i(year dnwhite) j(dvet)

gen dif1 = earn1 - earn0
gen dif2 = sqrt(earn_var1 + earn_var0)
gen dif3 = employ1 - employ0
gen dif4 = sqrt(employ_var1 + employ_var0)

table (dnwhite) (year), stat(mean dif* ) nototals 

******************************************
* matching and regression 与表2的计算方式一样
******************************************
use angrist1998_data3, clear

keep earn earn_bar_var employ employ_bar_var popcount afqtgrp dnwhite dobyy dvet edgrp transyy year

reshape wide earn earn_bar_var employ employ_bar_var popcount, i(afqtgrp dnwhite dobyy edgrp transyy year) j(dvet)
drop if mi(earn0) | mi(earn1)

egen s1 = sum(popcount1),by(year dnwhite)
egen s0 = sum(popcount0),by(year dnwhite)
gen s = s0 + s1

save angrist1998_data4, replace 

capture program drop create_table2
program create_table2
	args weight
	
	gen dif1 = earn1 - earn0
	gen dif2 = (earn_bar_var1 + earn_bar_var0) * `weight'
	gen dif3 = employ1 - employ0
	gen dif4 = (employ_bar_var1 + employ_bar_var0)* `weight'

	collapse (rawsum) `weight' (mean) dif* [iweight=`weight'], by(year dnwhite)
	replace dif2 = sqrt(dif2)
	replace dif4 = sqrt(dif4)

end

* matching
use angrist1998_data4, clear

gen weight = popcount1/s1 // contrast
create_table2 weight
table (year) (dnwhite), stat(mean dif* ) nototals append
 
* regression
use angrist1998_data4, clear

gen weight = popcount0*popcount1/(popcount0+popcount1)*(s/(s1*s0))  // 注意weight仍然按照前文设置方式，和不为1
create_table2 weight 
table (year) (dnwhite), stat(mean dif* ) nototals append

* 把colname result默认维度名称修改了
collect remap colname[`"dif1"' `"dif2"' `"dif3"' `"dif4"'] = result1[`"dif1"' `"dif2"' `"dif3"' `"dif4"']
collect addtags row[1], fortags(result1[`"dif1"' `"dif2"'])
collect addtags row[2], fortags(result1[`"dif3"' `"dif4"'])
collect remap result[`"mean"'] = result2[`"mean"']  // 防止与下文的result冲突

collect layout (dnwhite#year#row#result1) (cmdset) // 工具变量的结果也按照这个方式添加tags

********************
* 工具变量 IV
********************
use angrist1998_data3, clear

* calculate y,x and epsilon
collapse (mean) dvet earn earn_var employ employ_var employ?? (rawsum) popcount [fweight = popcount], by(dobyy edgrp afqtgrp transyy dnwhite year)

gen earn_epsilon = earn_var/popcount
gen employ_epsilon = employ_var/popcount

* 标记就业方差为0
egen w84 = anymatch(employ84 employ85 employ86 employ87), values(0)
egen w88 = anymatch(employ88 employ89 employ90 employ91), values(0)
egen w = rowtotal(w84 w88)

* group identifier
egen X = group(dobyy edgrp afqtgrp)
egen Y = group(dobyy edgrp transyy)

levelsof year, local(year)
local ii = 1
foreach i in earn employ {
	tempvar epsilon
	gen `epsilon' = 1/`i'_epsilon
	forvalues j = 0/1 {  // 民族
		foreach y in `year' {  // 年份
			qui: summarize `epsilon' if year == `y' & dnwhite == `j'
			local alpha = r(mean)
			
			* IV 1
			qui: areg `i' dvet transyy [aweight = `epsilon']  if year == `y' & dnwhite == `j', absorb(X)
			local coef = _b[dvet]
			local sd = _se[dvet]
			local rmse = e(rmse) * sqrt(`alpha')
			local sd1 = `sd'/`rmse'  
			
			collect get coef = `coef' se = `sd1', tags(year[`y'] dnwhite[`j'] row[`ii'] cmdset[4] )			
			
			* IV 2
			qui: areg `i' dvet i.X [aweight = `epsilon']  if year == `y' & dnwhite == `j', absorb(Y)
			local coef = _b[dvet]
			local sd = _se[dvet]
			local rmse = e(rmse) * sqrt(`alpha')
			local sd1 = `sd'/`rmse'  
			
			collect get coef = `coef' se = `sd1', tags(year[`y'] dnwhite[`j'] row[`ii'] cmdset[5] )
			
			if `"`i'"' == "employ" & `y' !=83 {  // 如果某个年份的就业率方差为0，删除
				qui: summarize `epsilon' if year == `y' & dnwhite == `j' & w==0
				local alpha = r(mean)	
				
				* IV 1
				qui: areg `i' dvet transyy [aweight = `epsilon']  if year == `y' & dnwhite == `j' & w == 0, absorb(X)
				local coef = _b[dvet]
				local sd = _se[dvet]
				local rmse = e(rmse) * sqrt(`alpha')
				local sd1 = `sd'/`rmse'  
				
				collect get coef = `coef' se = `sd1', tags(year[`y'] dnwhite[`j'] row[3] cmdset[4] )	
				
				* IV 2
				qui: areg `i' dvet i.X [aweight = `epsilon']  if year == `y' & dnwhite == `j' & w == 0, absorb(Y)
				local coef = _b[dvet]
				local sd = _se[dvet]
				local rmse = e(rmse) * sqrt(`alpha')
				local sd1 = `sd'/`rmse'  
				
				collect get coef = `coef' se = `sd1', tags(year[`y'] dnwhite[`j'] row[3] cmdset[5] )				
			}			
			
		}
	}
	local ++ii
}

************
* 设置表格格式
************
* row format 
collect remap result[`"coef"' `"se"'] = result1[`"dif1"' `"dif2"']
collect recode result1 `"dif3"' = `"dif1"' `"dif4"' = `"dif2"'

collect style header dnwhite year row result1, title(hide)
collect style header result1, level(hide)
collect label levels year 83 "During Service (1983)" 84 "Transition Years (1984-1987)" 88 "After Service (1988-1991)", modify
collect label levels row 1 "Earnings" 2 "Employment" 3 "Employment_b", modify

* column format
collect label dim cmdset "Estimator", modify
collect label levels cmdset 1 "Difference in Means" 2 "Matching" 3 "Regression" 4 "IV-1 [eq.(11)]" 5 "IV-2 [eq(12)]", modify

* cell format
collect style cell cmdset, warn halign(center) valign(center) nformat(%9.1f)
collect style cell result1[dif2],sformat("(%s)") warn

* table format
collect style cell border_block, border(right, pattern(nil))
collect title "TABLE V--SUMMARY OF RESULTS"

collect layout (dnwhite#year#row#result1) (cmdset) 
collect export "table5", as(docx) replace		

*-----------------------------------------------------------------------------
* 附录中的统计量
*-----------------------------------------------------------------------------
use ssaone, clear
keep if year == 90  // 每一年的cell数目是相同的
count // 5654
summarize popcount  // 每组的最低人数为25
egen pop = sum(popcount), by(dnwhite)  // 总人数
egen pop_s = sum(popcount) 
egen sam = sum(samcount), by(dnwhite)  // SSA匹配后的样本人数
egen sam_s = sum(samcount)
gen smplprop1 = samcount/popcount/smplprop  // 抽样比，验证是否与smplprop一致
bys dnwhite: summarize samcount  // 白人组cell最小样本数为17，非白人组为15

*-----------------------------------------------------------------------------
* 删除中间数据
*-----------------------------------------------------------------------------
local files : dir . files "*.dta" // 遍历STATA格式文件
foreach f in `files' {
	local ff = substr(`"`f'"',1,3)
	if inlist(`"`ff'"',"tem","col") {
		erase `f'
	}
}


