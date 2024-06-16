/*
1. 2024/6/12 开始修改
2. 2024/6/15 修改完成
*/
clear
cd /Users/zhangshuai/Desktop/interest/angrist1990_replication // 修改默认目录

*-----------------------------------------------------------------------------
* data preparation and label variable 
*-----------------------------------------------------------------------------
use cwhsa, clear
gen type="TAXAB"
append using cwhsb

* data scope
keep if inrange(byr,50,53) & year > 65

* cpi
merge n:1 year using cpi_angrist1990, nogen
qui: summarize cpi if year == 78
local cpi78 = r(mean)
replace cpi = round(cpi / `cpi78',0.001)

* DRAFT ELIGIBLE
gen byte eligible=0
replace eligible=1 if (byr>=44 & byr<=50 & interval<=39) /*CUTOFF=195*/
replace eligible=1 if byr==51 & interval<=25 /*CUTOFF=125*/
replace eligible=1 if (byr==52 | byr==53) & interval<=19  /*CUTOFF=95*/

* positive earning, population and variance 
gen earn = vmn1/(1-vfin1)
gen pop = vnu1*(1-vfin1)
gen var = vsd1^2/(1-vfin1)  /* used in Angrist's paper */
gen var1 = (vnu1*(vsd1^2+vmn1^2)-pop*earn^2)/pop

* label variables and values
label variable byr "Birth year"
label variable race "Race"
label variable year "Year"
label variable cpi "CPI"
label variable eligible "Draft eligibility"
label variable earn "Averaged positive earnings in cell"
label variable pop "Number of observations with positive earnings"
label variable var "Variance of observations with positive earnings used in Angrist's paper"
label variable var1 "Variance of observations with positive earnings"

labmask byr, values(byr)
label define race 1 "White" 2 "Nonwhite"		
label values race race 
labmask interval, values(interval)
labmask year, values(year)
label define eligibility 1 "Draft-eligible" 0 "Draft-ineligible"
label values eligible eligibility

* fica identifier
encode type, gen(fica)
label variable fica "Type of earnings"

drop type ctr1
rename fica type

save angrist1990_data, replace 

*******************
* adjusted earnings 				
*******************
use cwhsc_new, clear

* merge cpi
merge m:1 year using cpi_angrist1990, nogen keep(3) 
qui: summarize cpi if year == 78
local cpi78 = r(mean)
replace cpi = round(cpi / `cpi78',0.001)

* nominal earnings and its variance 
gen pop = nj - nj0
gen earn = earnings * cpi
gen var = 1/iweight_old * cpi^2 * pop

* DRAFT ELIGIBLE
gen byte eligible=0
replace eligible=1 if byr==50 & interval==1 /*CUTOFF=195 -- recoded to 2 intervals here*/
replace eligible=1 if byr==51 & interval<=25 /*CUTOFF=125*/
replace eligible=1 if (byr==52 | byr==53) & interval<=19  /*CUTOFF=95*/

* label variables and values
label variable byr "Birth year"
label variable race "Race"
label variable year "Year"
label variable nj0 "Number of observations with zero earnings"
label variable earnings "Averaged positive earnings (in 1978 $) in cell"
label variable iweight_old "1/Variance of observations with positive earnings (in 1978 $)"
label variable nsrvd "Number of veterans"
label variable ps_r "Probality of served in the army"
label variable ern74 "Correlation bewtween 1974 earnings and other year"
label variable cpi "CPI"
label variable pop "Number of observations with positive earnings"
label variable earn "mean of nominal positive earnings in cell"
label variable var "Variance of observations with positive earnings used in Angrist's paper"
label variable eligible "Draft eligibility"

labmask byr, values(byr)
label define race 1 "WHITES" 2 "NONWHITES"		
label values race race 
labmask interval, values(interval)
labmask year, values(year)
label define eligibility 1 "Draft-eligible" 0 "Draft-ineligible"
label values eligible eligibility

* fica identifier
encode type, gen(fica)
label variable fica "Type of earnings"

drop p_xl p_l  type
rename fica type

save angrist1990_new, replace 

*******************
* SIPP data 				
*******************
use sipp2.dta, clear

drop if rsncode == 999

* rename variables
rename u_brthyr byr
rename nrace race 
rename fnlwgt_5 pop
rename rsncode eligible

replace race = race + 1
replace byr = byr - 1900
keep byr race pop eligible nvstat

* label variables and values
label variable byr "Birth year"
label variable race "Race"
label variable pop  "Number of observations"
label variable eligible "Draft eligibility"

labmask byr, values(byr)
label define race 1 "WHITES" 2 "NONWHITES"		
label values race race 
label define eligibility 1 "Draft-eligible" 0 "Draft-ineligible"
label values eligible eligibility
label define nvstat 1 "Veteran" 0 "Non-veteran"
label values nvstat nvstat

save angrist1990_sipp2, replace 

*******************
* DMDC data 				
*******************
* dmdc没有包含1950年的数据
use dmdcdat, clear

* DRAFT ELIGIBLE
gen byte eligible=0
replace eligible=1 if byr==51 & interval<=25 /*CUTOFF=125*/
replace eligible=1 if (byr==52 | byr==53) & interval<=19  /*CUTOFF=95*/

* label variables and values
label variable byr "Birth year"
label variable nsrvd "Number of veterans in DMDC"
label variable race "Race"
label variable nj "Number of observations 1970"
label variable ps_r "Probality of being veteran"
label variable eligible "Draft eligibility"

labmask byr, values(byr)
label define race 1 "WHITES" 2 "NONWHITES"		
label values race race 
labmask interval, values(interval)
label define eligibility 1 "Draft-eligible" 0 "Draft-ineligible"
label values eligible eligibility

save angrist1990_dmdc, replace 	
	
**********************	
* 检验dmdc的总人数为1970
**********************
use angrist1990_data, clear 
keep byr interval race  vnu1 year 
keep if byr > 50
merge m:1 byr interval race using dmdcdat, nogen
gen test = vnu1 / nj
keep if year == 70	
	
*-----------------------------------------------------------------------------
* Figure 1: SOCIAL SECURITY EARNINGS PROFILES BY DRAFT-ELIGIBILITY STATUS
*-----------------------------------------------------------------------------
use angrist1990_data, clear

* keep cohorts born in 50-53
keep if type == 1

* real earning
replace earn = earn/cpi

collapse (mean) earn [iw=pop], by(byr year race eligible)
replace earn = earn + 3000 if byr == 50
replace earn = earn + 2000 if byr == 51
replace earn = earn + 1000 if byr == 52

twoway 	(connected earn year if byr == 50  & eligible == 1, msize(2-pt)) ///
		(line earn year if byr == 50  & eligible == 0) ///
		(connected earn year if byr == 51  & eligible == 1,msize(2-pt) lpattern(dash)) ///
		(line earn year if byr == 51  & eligible == 0, lpattern(dash)) ///
		(connected earn year if byr == 52  & eligible == 1,msize(2-pt) lpattern(longdash)) ///
		(line earn year if byr == 52  & eligible == 0, lpattern(longdash)) ///	
		(connected earn year if byr == 53  & eligible == 1,msize(2-pt) lpattern(dash_dot)) ///
		(line earn year if byr == 53  & eligible == 0, lpattern(dash_dot)),  ///
		legend( label(1 "COHORT 1950") label(2 "") label(3 "1951") label(4 "") label(5 "1952") label(6 "") label(7 "1953") label(8 "") textfirst region(lcolor(black)) subtitle("DRAFT" "ELIGIBLE       INELIGIBLE", size(small) position(1) justification(center)) size(small) ) /// 
		ytitle("EARNINGS IN 1978 DOLLARS") xtitle(Year) by(race, note("") title("Figure 1. SOCIAL SECURITY EARNINGS PROFILES BY DRAFT-ELIGIBILITY STATUS",position(6) size(small)))

graph export "figure1.jpg", as(jpg) quality(100) replace

*-----------------------------------------------------------------------------
* Figure 2: THE DIFFERENCE IN EARNINGS By DRAFT-ELIGIBILITY STATUS
*-----------------------------------------------------------------------------
use angrist1990_data, clear

keep if type == 1

* real earning
replace earn = earn/cpi

collapse (mean) earn [iw=pop], by(byr year race eligible)
reshape wide earn, i(byr year race) j(eligible)
gen dif = earn1 - earn0
replace dif = dif + 3500 if byr == 50
replace dif = dif + 2500 if byr == 51
replace dif = dif + 1500 if byr == 52
replace dif = dif + 500 if byr == 53

twoway 	(line dif year if byr == 50 ) ///
		(line dif year if byr == 51, lpattern(dash)) ///
		(line dif year if byr == 52, lpattern(longdash)) ///
		(line dif year if byr == 53, lpattern(dash_dot)), ///
		yline(500, lpattern(dash_dot)  lwidth(thin) noextend) /// 
		yline(1500, lpattern(longdash) lwidth(thin) noextend) /// 
		yline(2500, lpattern(dash) lwidth(thin) noextend) /// 
		yline(3500, lwidth(thin) noextend) ///
		legend(col(1) label(1 "COHORT 1950") label(2 "1951") label(3 "1952") ///
		label(4 "1953")  textfirst region(lcolor(black)) size(small) ) /// 
		ytitle("EARNINGS DIFFERENCE")  xtitle(Year) by(race, note("") title("Figure 2. THE DIFFERENCE IN EARNINGS By DRAFT-ELIGIBILITY STATUS",size(small) position(6) )) 
		
graph export "figure2.jpg", as(jpg) quality(100) replace

*-----------------------------------------------------------------------------
* TABLE I--DRAFT-ELIGIBILITY TREATMENT EFFECTS FOR EARNINGS
*-----------------------------------------------------------------------------
use angrist1990_data, clear

* note: regression can get same coefficient estimates as calculation, but not the standard error, because it didn't exploit the information on cell variance
reg earn eligible [aw = pop] if byr == 50 & race == 1 & year == 66 

* keep cohorts born in 50-53
keep if inrange(byr,50,53) & year > 65
drop if mi(earn)

collapse (rawsum) pop (mean) earn var [iweight=pop], by(byr year race eligible type)
replace var = var/pop
drop pop
reshape wide earn var, i(byr year race type) j(eligible)

gen f1 = earn1 - earn0
gen f2 = sqrt(var1 + var0)

table (race year) (type byr), stat(mean f1 f2) nototals

* format 
collect dims
collect levelsof colname
collect levelsof result
collect levelsof type

* row format
collect style header race, title(hide)
collect style header year, title(hide)
collect style header colname, title(hide) level(hide)

* column format
collect style header type, title(hide)
collect style header byr, title(hide)
collect label levels type 1 "FICA Taxable Earnings" 2 "Total W-2 Compensation", modify

* cell format
collect style cell byr, warn halign(center) valign(center) nformat(%9.1fc)
collect style cell colname[f2],sformat("(%s)") warn

* table format
collect style cell border_block, border(right, pattern(nil))
collect title "TABLE 1--DRAFT-ELIGIBILITY TREATMENT EFFECTS FOR EARNINGS"

collect layout (race#year#colname) (type#byr)
collect export "table1", as(docx) replace	

*-----------------------------------------------------------------------------
* TABLE 2--VETERAN STATUS AND DRAFT ELIGIBILITY
*-----------------------------------------------------------------------------
* 注意：(1)sipp2的结果与angrist论文存在差异，但与其网页代码结果一致；
* （2）dmdc的方差，angrsit采用p（1-p）/n来估算
*-----------------
* SIPP data
*-----------------
use angrist1990_sipp2, clear 
 
table (byr) (race) if inrange(byr,50,53), stat(count nvstat) nototals
collect recode result `"count"' = `"mean"'
collect addtags row[1], fortags(result[mean])

foreach r in 1 2 { // race
	foreach l in 50 51 52 53 {  // byr
		qui: {
			summarize nvstat if inrange(byr, `l'-1, `l'+1) & race == `r' [aw=pop]
			local p11 = r(mean) 
			local p12 = r(sd)/sqrt(r(N)) 
			
			summarize nvstat if inrange(byr, `l'-1, `l'+1) & race == `r' & eligible == 1 [aw=pop]
			local p21 = r(mean) 
			local p22 = r(sd)/sqrt(r(N)) 
			
			summarize nvstat if inrange(byr, `l'-1, `l'+1) & race == `r' & eligible == 0 [aw=pop]
			local p31 = r(mean)
			local p32 = r(sd)/sqrt(r(N))
			
			local p41 = `p21' - `p31'
			local p42 = sqrt(`p22'^2 + `p32'^2)	
			
			scalar p`r'_`l' = `p41' // 表3使用
			scalar s`r'_`l' = `p42'
		}
		
		forvalues i = 1/4 {
			local j = `i' + 1
			collect get mean = `p`i'1' se = `p`i'2', tags(race[`r'] byr[`l'] cmdset[`j'] row[1])
		}
	}
}

*-----------------
* DMDC data
*-----------------
use angrist1990_dmdc, clear 

foreach r in 1 2 { // race
	foreach l in 51 52 53 {  // byr
		qui: {
			summarize ps_r [aw=nj] if byr == `l' & race == `r'
			local p = r(sum_w)
			local p11 = r(mean) 
			local p12 = sqrt(`p11'*(1-`p11')/`p') 
			
			summarize ps_r [aw=nj] if byr ==`l' & race ==`r' & eligible == 1 
			local p21 = r(mean) 
			local p22 = sqrt(`p11'*(1-`p11')/r(sum_w))
			
			summarize ps_r [aw=nj] if byr ==`l' & race ==`r' & eligible == 0 
			local p31 = r(mean)
			local p32 = sqrt(`p11'*(1-`p11')/r(sum_w))
			
			local p41 = `p21' - `p31'
			local p42 = sqrt(`p22'^2 + `p32'^2)	
		}
		
		collect get mean = `p', tags(race[`r'] byr[`l'] cmdset[1] row[2])
		
		forvalues i = 1/4 {
			local j = `i' + 1
			collect get mean = `p`i'1' se = `p`i'2', tags(race[`r'] byr[`l'] cmdset[`j'] row[2])
		}
	}
}

* row format
collect style header race, title(hide)
collect style header byr, title(hide)
collect style header row, title(hide)
collect style header result, level(hide) 
collect label levels row 1 "SIPP (84)" 2 "DMDC/CWHS", modify

* column format
collect style header cmdset, title(hide)
collect label levels cmdset 1 "Sample" 2 "P(Veteran)" 3 "Pe" 4 "Pn" 5 "Pe-Pn", modify

* cell format
collect style cell cmdset, warn halign(center) valign(center) nformat(%9.4f)
collect style cell cmdset[1], warn nformat(%9.0f)
collect style cell result[se],sformat("(%s)") warn

* table format
collect style cell border_block, border(right, pattern(nil))
collect title "TABLE 2--VETERAN STATUS AND DRAFT ELIGIBILITY"

collect layout (race#row#byr#result) (cmdset)
collect export "table2", as(docx) replace	

*-----------------------------------------------------------------------------
* TABLE 3--WALD ESTIMATES
*-----------------------------------------------------------------------------
use angrist1990_new, clear

* keep selected observations
keep if inrange(year,81,84)

collapse (rawsum) pop (mean) earn var [iweight=pop], by(byr year race eligible type cpi)
replace var = var/pop

drop pop
reshape wide earn var, i(byr year race type cpi) j(eligible)
gen f1 = earn1 - earn0
gen f2 = sqrt(var1 + var0)

* 添加表2的概率
gen p = .
gen s = .
foreach r in 1 2 { // race
	foreach l in 50 51 52 53 {  // byr
		replace p = p`r'_`l' if byr == `l' & race == `r'
		replace s = s`r'_`l' if byr == `l' & race == `r'
	}
}

* 方差的计算方式见p322脚注
gen f3 = f1/p/cpi
gen f4 = f2/p/cpi

* 表格
table (race byr year) (type), stat(mean f1 f2) nototals
replace type = type*10 //方便表格整理
table (race byr year) (type) if year==81&type==10, stat(mean p s) nototals append
replace type = type*10 
table (race byr year) (type), stat(mean f3 f4) nototals append

* row format
collect style header race, title(hide)
collect style header byr, title(hide)
collect style header year, title(hide)
collect style header colname, level(hide)
collect label levels byr 50 "Born in 1950" 51 "Born in 1951" 52 "Born in 1952" 53 "Born in 1953", modify

collect recode colname `"f3"' = `"f1"'
collect recode colname `"f4"' = `"f2"'
collect recode colname `"p"' = `"f1"'
collect recode colname `"s"' = `"f2"'

* column format
collect style header cmdset, title(hide)
collect style header type, title(hide)

collect label levels cmdset 1 "Draft-eligible Effects in Current $" 2 "Pe-Pn" 3 "Service Effects in 1978 $", modify
collect label levels type 1 "Adjusted FICA Earnings" 2 "FICA Earnings" 3 "Total W-2 Earnings", modify

* cell format
collect style cell cmdset, warn halign(center) valign(center) nformat(%9.1f)
collect style cell cmdset[2], warn nformat(%9.3f)
collect style cell colname[f2],sformat("(%s)") warn

* table format
collect style cell border_block, border(right, pattern(nil))
collect title "TABLE 3--WALD ESTIMATES"

collect layout (race#byr#year#colname) (cmdset#type) // 完整表格
collect style header type[10 100 200 300], level(hide)
collect layout (race[1]#byr[50 51 52]#year#colname) (cmdset#type[2 1 3 10 100]) // 文中表格
collect export "table3", as(docx) replace	

* Note: the results are different from Angrist's table 3, and Angrist has emphasized the points on his website

*-----------------------------------------------------------------------------
* Figure 3: EARNINGS AND THEP ROBABILITY OF VETERAN STATUS BY LOTTERY NUMBER
*-----------------------------------------------------------------------------
use angrist1990_new, clear

* keep selected observations
keep if type == 3 & race == 1 & inrange(year,81,84)

* residual y
reg earnings i.byr i.year 
predict y, residual

* residual x
reg ps_r i.byr i.year
predict x, residual

collapse y x, by(interval byr)
reg y x, nocon

twoway (scatter y x) (lfit y x), xtitle("PROBABILITY RESIDUAL") ytitle("EARNINGS RESIDUAL") xlabel(-0.08(0.04)0.16) xmtick(##2) ylabel(-3000(1000)3000) ymtick(##2) title("Figure 3. EARNINGS AND THEP ROBABILITY OF VETERAN STATUS BY LOTTERY NUMBER",size(small) position(6) )  legend(off)
	
graph export "figure3.jpg", as(jpg) quality(100) replace

*-----------------------------------------------------------------------------
* TABLE 4 TWO-STAGE INSTRUMENTAL VARIABLES ESTIMATES
*-----------------------------------------------------------------------------
* 采用angrist网站上的程序
* 知道残差的结构做GLS,需要对所有变量进行转换
use angrist1990_new, clear

* keep selected observations
keep if inrange(year,81,84) & inrange(byr,50,53)

* UNCOMMENT TO USE OLD WEIGHTS
drop iweight
ren iweight_old iweight
	
* YEAR DUMMIES
qui tab year, ge(yrdum)
qui tab byr, ge(byrdum)

* 不同出生年份设置不同处理效应
g ps_r50= ps_r*(byr==50)
g ps_r51= ps_r*(byr==51)
g ps_r52= ps_r*(byr==52)
g ps_r53= ps_r*(byr==53)

****************************
* ALPHA ESTIMATES 先估算alpha
****************************
g alpha1=.
g alpha2=.

* FOR POOLED REGRESSIONS
foreach race in 1 2 {
	foreach type in 1 2 3 {
		qui reg earnings byrdum1-byrdum3 yrdum2-yrdum4 /// 
			ps_r if type==`type' & race==`race' [w=iweight]
		qui replace alpha1= _b[ps_r] if e(sample)
	}
}

* FOR BY COHORT REGRESSIONS
foreach race in 1 2 {
	foreach type in 1 2 3 {
		qui reg earnings byrdum1-byrdum3 yrdum2-yrdum4 ps_r50-ps_r53 ///
			if type==`type' & race==`race' [w=iweight]
		qui replace alpha2= _b[ps_r50] if e(sample) & byr==50
		qui replace alpha2= _b[ps_r51] if e(sample) & byr==51
		qui replace alpha2= _b[ps_r52] if e(sample) & byr==52
		qui replace alpha2= _b[ps_r53] if e(sample) & byr==53
	}
}

save temp, replace 

******************************************
* merge population 合并人口数，计算入伍概率方差
******************************************
* sipp
use angrist1990_sipp2, clear

keep if byr == 50
collapse (count) nvstat, by(byr race)
rename nvstat smpl

save temp1, replace 

* dmdc
* 在计算表2时，统一采用了1970年抽样人数，但这里51年采用71年，52年采用72年，，，
use angrist1990_data, clear

keep if inrange(byr, 51, 53)
collapse (sum) vnu1, by(byr race year)

gen dif = year - byr
keep if dif == 20

rename vnu1 smpl
keep byr race smpl
append using temp1

save temp2, replace 

* combine
use temp, clear 
merge m:1 byr race using temp2, nogen

* second half of variance 
g term1= (alpha1^2)*ps_r*(1-ps_r)*(1/smpl)
g term2= (alpha2^2)*ps_r*(1-ps_r)*(1/smpl)

g byte intercep=1
g wts= 1/iweight^.5

sort byr type race interval year
 
* MATRICES FOR TRANSFORMATION TO GLS
***********************************************************
		/*MATA - MUST BE USING STATA 9*/
***********************************************************
mata 

	st_view(Y=.,.,"earnings")
	st_view(X1=.,.,("intercep", "byrdum1", "byrdum2", "byrdum3", /*
		*/ "yrdum2", "yrdum3", "yrdum4", "ps_r"))
	st_view(X2=.,.,("intercep", "byrdum1", "byrdum2", "byrdum3", /* 
		*/ "yrdum2", "yrdum3", "yrdum4", "ps_r50", "ps_r51", "ps_r52", "ps_r53"))
	st_view(clss=.,.,("type", "byr", "race", "interval", "year"))
	st_view(covmtrx=.,.,("ern81", "ern82", "ern83", "ern84"))
	st_view(term1=.,.,"term1")
	st_view(term2=.,.,"term2")
	st_view(wtvec=.,.,"wts")

	Y1_t= Y:*0:+-999
	Y2_t= Y:*0:+-999
	X1_t= X1:*0:-999 
	X2_t= X2:*0:-999
	 
	j=0

	while (j<=1325) {

		wtvec_t= wtvec[|4*j+1,1 \ 4*j+4,1|]
		covmtrx_t= covmtrx[|4*j+1,1 \ 4*j+4,4|]
		term1_t= term1[|4*j+1,1 \ 4*j+4,1|]
		term2_t= term2[|4*j+1,1 \ 4*j+4,1|]

		Y_e= Y[|4*j+1,1 \ 4*j+4,1|]
		X1_e= X1[|4*j+1,1 \ 4*j+4,8|]
		X2_e= X2[|4*j+1,1 \ 4*j+4,11|]

		covmtrx1_t= wtvec_t:*covmtrx_t:*wtvec_t':+term1_t 
		covmtrx2_t= wtvec_t:*covmtrx_t:*wtvec_t':+term2_t 

		final1= cholesky(invsym(covmtrx1_t))   
		final2= cholesky(invsym(covmtrx2_t))  

		Y1_e= final1'*Y_e
		X1_e= final1'*X1_e

		Y2_e= final2'*Y_e
		X2_e= final2'*X2_e
		

		Y1_t[|4*j+1,1\4*j+4,1|]=Y1_e
		X1_t[|4*j+1,1\4*j+4,8|]=X1_e

		Y2_t[|4*j+1,1\4*j+4,1|]=Y2_e
		X2_t[|4*j+1,1\4*j+4,11|]=X2_e

		j++

	}

	alldata1= (Y1_t,X1_t,clss)
	alldata2= (Y2_t,X2_t,clss)

	st_matrix("alldata1",alldata1)
	st_matrix("alldata2",alldata2)

end
*********************************************************
				/* END MATA */
*********************************************************
* save transformed data

mat colnames alldata1= earnings intercept bd1 bd2 bd3 /// 
	yd2 yd3 yd4 ps_r type byr race interval year
mat colnames alldata2= earnings intercept bd1 bd2 bd3 /// 
	yd2 yd3 yd4 ps_r50 ps_r51 ps_r52 ps_r53 type byr race interval year

* FIRST DO POOLED COHORTS
drop _all
svmat alldata1, names(col)
save angrist1990_table4, replace 

* an example 
reg earnings intercep bd1-bd3 yd2-yd4 ps_r if type== 1 & race== 1, nocon
scalar coef = _b[ps_r]
scalar sd = _se[ps_r]/e(rmse)
scalar chi2 = e(rss)
scalar df = e(df_r)
scalar list coef sd chi2 df

collect clear
foreach race in 1 2 {
	foreach type in 1 2 3 {
		qui: {
			reg earnings intercep bd1-bd3 yd2-yd4 ps_r if type==`type' & race==`race', nocon
			local coef = _b[ps_r] 
			local sd = _se[ps_r]/e(rmse) 
			local chi2 = e(rss)
			local df = e(df_r)
			
			collect get coef = `coef' sd = `sd', tags(type[`type'] race[`race'] model[2] year[1950-53])
			collect get coef = `chi2' sd = `df', tags(type[`type'] race[`race'] model[2] year[chi2])	
		}
	}
}

* NEXT INDIVIDUAL COEFFS
drop _all
svmat alldata2, names(col)
save angrist1990_table41, replace 

foreach race in 1 2 {
	foreach type in 1 2 3 {
		qui: {
			reg earnings intercep bd1-bd3 yd2-yd4 ps_r50-ps_r53 if type==`type' & race==`race', nocon
			foreach v in 50 51 52 53 {
				local coef = _b[ps_r`v'] 
				local sd = _se[ps_r`v']/e(rmse) 
				collect get coef = `coef' sd = `sd', tags(type[`type'] race[`race'] model[1] year[19`v'])				
			}
			local chi2 = e(rss)
			local df = e(df_r)
			collect get coef = `chi2' sd = `df', tags(type[`type'] race[`race'] model[1] year[chi2])	
			
		}
	}
}

collect layout (race#model#year#result) (type)

* row format
collect style header result, level(hide)
collect style autolevels model 1 2
collect style autolevels year 1950 1951 1952 1953 1950-53 chi2

collect label levels race 1 "Whites" 2 "Nonwhites", modify
collect label levels model 1 "Model 1" 2 "Model 2", modify

* column format
collect style autolevels type 2 1 3
collect label levels type 1 "Adjusted FICA Earnings" 2 "FICA Earnings" 3 "Total W-2 Earnings", modify

* cell format
collect style cell type, warn halign(center) valign(center) nformat(%9.1f)
collect style cell result[sd],sformat("(%s)") warn

* table format
collect style cell border_block, border(right, pattern(nil))
collect title "TABLE 4--TWO-STAGE INSTRUMENTAL VARIABLES ESTIMATES"

collect layout (race#model#year#result) (type)
collect export "table4", as(docx) replace	

*-----------------------------------------------------------------------------
* TABLE 5 EARNINGS-FUNCTION MODELS FOR THE VETERAN EFFECT, WHITES BORN 1950-52
*-----------------------------------------------------------------------------
use angrist1990_new, clear

* keep selected observations
keep if type == 2 & race  == 1
drop if byr == 53
drop if (byr==50 & year<75) | (byr==51 & year<76) | (byr==52 & year<77) 

table year byr

* UNCOMMENT TO USE OLD WEIGHTS
drop iweight
ren iweight_old iweight
	
* YEAR DUMMIES
qui tab year, ge(yrdum)

* initial estimates
gen lny = log(earnings)
gen xct = year - byr - 18
gen xct2 = xct^2
gen px = ps_r*xct 
gen const = 1
gen log_weight = earnings^2 * iweight


nl (lny = {beta0=0.1}*xct + {gamma=-0.02}*xct2 -({beta0}*{l}-{gamma}*{l}^2)*ps_r-2*{gamma}*{l}*px+{t1}*yrdum1+{t2}*yrdum2+{t3}*yrdum3+{t4}*yrdum4+{t5}*yrdum5+{t6}*yrdum6+{t7}*yrdum7+{t8}*yrdum8+{t9}*yrdum9+{t10}*yrdum10) [weight=log_weight]

nl (lny = {beta0=0.1}*xct + {gamma=-0.02}*xct2 -({beta0}*{l}-{gamma}*{l}^2+{beta1}*{l})*ps_r-(2*{gamma}*{l}-{beta1})*px+{t1}*yrdum1+{t2}*yrdum2+{t3}*yrdum3+{t4}*yrdum4+{t5}*yrdum5+{t6}*yrdum6+{t7}*yrdum7+{t8}*yrdum8+{t9}*yrdum9+{t10}*yrdum10) [weight=log_weight]

reg lny xct xct2 ps_r px i.year [weight=log_weight]

* 只计算第一步结果，不再按照表4继续计算了

*-----------------------------------------------------------------------------
* Table A1 (Appendix): DESCRIPTIVE STATISTICS FOR MEN BORN 1950-53
*-----------------------------------------------------------------------------
use angrist1990_data, clear

* keep cohorts born in 50-53
keep if inrange(byr,50,53) & year > 68
drop if mi(earn)

* 以每组总人数为加权的统计量
qui:table (race year) (type) [iweight=vnu1], stat(rawsum vnu1) stat(mean ltax vfin1) nototals

* 以每组收入大于0人数为加权的平均值
qui:table (race year) (type) [iweight=pop], stat(mean earn) nototals append

* 计算收入大于加权平均值的标准差，
* 注意var为方差，非均值方差，最终结果也是方差，非均值方差
collapse (mean) var [iweight=pop], by(year race type)
gen sd = sqrt(var)
qui:table (race year) (type), stat(mean sd) nototals append

* format 
collect dims
collect levelsof colname
collect levelsof result
collect levelsof type
collect layout (race#year#result) (type#colname)

* row format
collect recode result `"rawtotal"' = `"mean"'
collect recode colname `"sd"' = `"earn"'
collect recode cmdset `"2"' = `"1"'

collect style header race, title(hide)
collect style header year, title(hide)
collect style header cmdset, title(hide) level(hide)

* column format
collect style header type, title(hide)
collect label levels type 1 "FICA" 2 "W2", modify
collect label levels colname vnu1 "N" earn "Earnings" ltax "Limit" vfin1 "Zeros", modify
collect style autolevels colname vnu1 earn ltax vfin1

* cell format
collect style cell colname, warn halign(center) valign(center) 
collect style cell colname[vnu1 earn], nformat(%9.0fc)
collect style cell colname[ltax vfin1], nformat(%9.3fc)
collect style cell cmdset[3],sformat("(%s)") warn

* table format
collect style cell border_block, border(right, pattern(nil))
collect title "Table A1-DESCRIPTIVE STATISTICS FOR MEN BORN 1950-53"

collect layout (race#year#cmdset) (type[1]#colname type[2]#colname[earn ltax vfin1])
collect export "tableA1", as(docx) replace	




