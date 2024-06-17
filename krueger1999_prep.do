* 2023/10/24
* 2024/6/4 浏览并添加了生成angrist1999_data
clear
cd /Users/zhangshuai/Desktop/interest/krueger1999_replication // 修改默认目录

*------------------------------------------------------------------------------
* I. Create / Reformat Variables
*------------------------------------------------------------------------------
use STAR_Students, clear

* Rename gk variables as g0 to make loops possible later on
rename gk* g0*
rename *gk *g0

keep stdntid gender race birthmonth birthday birthyear f*0 f*1 f*2 f*3 g0* g1* g2* g3*
rename g0* *0
rename g1* *1
rename g2* *2
rename g3* *3

* 变量名称用于转换数据结构
gen names = ""
local j = 1
foreach var of varlist flagsg0-selfconcraw3  {
	replace names = "`var'" if _n == `j'
	local ++j
}
replace names = substr(names, 1, length(names) - 1)

levelsof names, local(names)
foreach i of local names {
	di "`i'"
}
drop names

* 转换结构
reshape long `names', i(stdntid gender race birthmonth birthday birthyear) j(grade)

* Create gradeenter as when a student entered TNSTAR
* 开始进入实验的年级
gsort stdntid -flagsg grade
by stdntid : gen start = grade[1] 

* Create White/Asian indicator
gen WhiteAsian = (inlist(race, 1, 3)) if !missing(race)
label var WhiteAsian "White/Asian_Student"
label define WhiteAsian 1  "White/Asian" 0 "Others"
label values WhiteAsian WhiteAsian

* Create Age as of September 1st, 1985 (start of 1985 school year)
gen bday = mdy(birthmonth, birthday, birthyear)
gen startschool = mdy(9,1,1985)
gen age1985 = (startschool - bday)/365
replace age1985 = 1985 - birthyear if age1985 == .
label var age1985 "Age in 1985"
drop bday startschool

gen age = age1985
replace age = age + 1 if grade == 1
replace age = age + 2 if grade == 2
replace age = age + 3 if grade == 3

* Create Attrition rate: percent that exits sample before completing third grade. 
* 退出率
egen years = sum(flagsg), by(stdntid)
gen attrition = (years + start) != 4 
replace attrition = . if start == 3
drop years
label var attrition "Attrition rate"

* Create variable: Percentile Score as SAT average for reading, math, and listening
forvalues i = 0/3 {
	foreach sub in treadss tmathss wordskillss {
		cumul `sub' if  classtype > 1 & grade == `i' & !mi(`sub'), gen(cum_`sub'_`i')
		egen cum_`sub'_`i'i = mean(cum_`sub'_`i') if  classtype > 1 & grade == `i' & !mi(`sub'),by(`sub')
		ipolate cum_`sub'_`i'i `sub' if grade == `i' &  !mi(`sub') , gen(`sub'_`i')  epolate
		replace `sub'_`i' = cum_`sub'_`i'i if  classtype > 1 & grade == `i' & !mi(`sub')
	}
	
	egen SAT_`i' = rowmean(treadss_`i' tmathss_`i' wordskillss_`i')	
}

egen SAT = rowmean(SAT_0 SAT_1 SAT_2 SAT_3)
egen SAT_math = rowmean(tmathss_0 tmathss_1 tmathss_2 tmathss_3)
egen SAT_reading = rowmean(treadss_0 treadss_1 treadss_2 treadss_3)
egen SAT_word = rowmean(wordskillss_0 wordskillss_1 wordskillss_2 wordskillss_3)

replace SAT = 100 * SAT
label variable SAT "SAT percentile score"
replace SAT_math = SAT_math * 100
replace SAT_reading = SAT_reading * 100
replace SAT_word = SAT_word * 100

drop cum_* treadss_* tmathss_* wordskillss_* SAT_?


* BSF
forvalues i = 0/3 {
	foreach sub in readbsraw mathbsraw {
		cumul `sub' if  classtype > 1 & grade == `i' & !mi(`sub'), gen(cum_`sub'_`i')
		egen cum_`sub'_`i'i = mean(cum_`sub'_`i') if  classtype > 1 & grade == `i' & !mi(`sub'),by(`sub')
		ipolate cum_`sub'_`i'i `sub' if grade == `i' &  !mi(`sub') , gen(`sub'_`i')  epolate
		replace `sub'_`i' = cum_`sub'_`i'i if  classtype > 1 & grade == `i' & !mi(`sub')			
	}
	
	egen BSF_`i' = rowmean(readbsraw_`i' mathbsraw_`i')	
}

egen BSF = rowmean(BSF_*)
egen BSF_read = rowmean(readbsraw_*)
egen BSF_math = rowmean(mathbsraw_*)
drop cum_* readbsraw_* mathbsraw_* BSF_?
replace BSF = 100 * BSF
label variable BSF "BSF percentile score"
replace BSF_read = BSF_read * 100
replace BSF_math = BSF_math * 100

* free lunch
replace freelunch = 0 if freelunch == 2
label define freelunch 1 "FREE LUNCH" 0 "NON-FREE LUNCH"
label variable freelunch "Free lunch"
label values freelunch freelunch

* gender and teacher gender
replace gender = 0 if gender == 2
replace tgen = 0 if tgen == 2

label define gender 1 "male" 0 "female"
label variable gender "Students gender"
label variable tgen "Teachers gender"
label values gender gender
label values tgen gender


* Create indicator for white teacher 
gen twhite = (inlist(trace, 1, 3)) if !missing(trace)
label var twhite "White/Asian Teacher"
label values twhite WhiteAsian	

* Create indicator for teacher with master's degree or higher
label define master 1 "Master's degree or above" 0 "No Master's degree"
gen tmd = (inlist(thighdegree, 3,4,5,6)) if !missing(thighdegree)
label var tmd "Master degree or above"
label values tmd master	

* initial assignment
gsort stdntid -flagsg grade
by stdntid : gen classtype_initial = classtype[1] 
by stdntid : gen schid_initial = schid[1] 
by stdntid : gen tchid_initial = tchid[1] 

* imputed SAT
sort stdntid grade 

gen SAT_impute = SAT
by stdntid: replace SAT_impute = (SAT_impute[1] + SAT_impute[3])/2 if grade == 1 & mi(SAT_impute)
by stdntid: replace SAT_impute = (SAT_impute[2] + SAT_impute[4])/2 if grade == 2 & mi(SAT_impute)
by stdntid: replace SAT_impute = SAT_impute[1] if grade == 1  & mi(SAT_impute)
by stdntid: replace SAT_impute = SAT_impute[2] if grade == 2  & mi(SAT_impute)
by stdntid: replace SAT_impute = SAT_impute[3] if grade == 3  & mi(SAT_impute)

* imputed schid
gen schid_impute = schid
by stdntid: replace schid_impute = schid_impute[1] if grade == 1  & mi(schid_impute)
by stdntid: replace schid_impute = schid_impute[2] if grade == 2  & mi(schid_impute)
by stdntid: replace schid_impute = schid_impute[3] if grade == 3  & mi(schid_impute)

* cumulative years 
sort stdntid grade 
by stdntid: gen smallyears = sum(flagsg) if classtype == 1
by stdntid: gen aidyears = sum(flagsg) if classtype == 3
replace smallyears = 0 if mi(smallyears)
replace aidyears = 0 if mi(aidyears)

* fraction of old classmates
* keep stdntid grade tchid classsize
xtset stdntid grade 
gen long tchid_lag = l.tchid
egen classmate = count(stdntid) if !mi(tchid) & !mi(tchid_lag), by(tchid tchid_lag)
replace classmate = classmate - 1 
replace classmate = 0 if !mi(tchid) & mi(tchid_lag)

gen frac = classmate / (classsize - 1)
egen frac_s = sum(frac) if !mi(frac), by(tchid)
gen frac_m = (frac_s-frac)/(classsize-1) // 加总减去自己再平均
* egen frac_m = mean(frac) if !mi(frac), by(tchid)

drop tchid_lag classmate frac_s

* fraction of freelunch 
egen freelunch_m = mean(freelunch) if !mi(freelunch), by(tchid)

* fraction of kindergarten classmates
gen start1 = start == 0
egen kinder = sum(start1) if !mi(tchid), by(tchid)
gen kinder_m = kinder/classsize

* SAT dif
gen SAT_dif = SAT - l.SAT

save data, replace 
save krueger1999_data, replace
