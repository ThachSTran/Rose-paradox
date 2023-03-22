log using "C:\Thach\Research projects\Rose paradox\hipfx_roseparadox", replace

*****************************************
* Rose's paradox for hip fracture		*
*	Thach Tran		date: 5/4/2021		*
*****************************************

use "C:\Thach\Research projects\Rose paradox\hipfx_roseparadox.dta", clear

*************************************
* Table 1. Baseline characteristics	*
*************************************

*********
* Women *
*********

table1_mc if sex == 1, by(entry_cat) ///
vars(age contn %4.1f \ weight contn %4.1f \ height contn %4.1f \ bmi contn %4.1f \   ///
caintakeraw contn %5.1f \ caintake contn %4.2f \ pairaw contn %4.2f \ pai contn %4.2f \ ///
maxalcoraw contn %4.2f \ maxalco contn %4.2f \ smoking bin %4.1f \ packyearsraw contn %4.2f \ ///
packyears contn %4.2f \ prior_fx bin %4.1f \ fall bin %4.1f \ lrsfnbmd contn %4.2f \ lrsfnbmdadj contn %4.2f \ ///
cvd bin %4.1f \ diabetes bin %4.1f \ hypert bin %4.1f \ cancer bin %4.1f \ respi bin %4.1f \ /// 
neuro bin %4.1f \ rheum bin %4.1f \ fnbmd contn %4.2f \ fnbmdt contn %4.2f \ hip_10y contn %4.2f \) ///
nospace percent_n onecol missing total(before)

*******
* Men *
*******

table1_mc if sex == 0, by(entry_cat) ///
vars(age contn %4.1f \ weight contn %4.1f \ height contn %4.1f \ bmi contn %4.1f \   ///
caintakeraw contn %5.1f \ caintake contn %4.2f \ pairaw contn %4.2f \ pai contn %4.2f \ ///
maxalcoraw contn %4.2f \ maxalco contn %4.2f \ smoking bin %4.1f \ packyearsraw contn %4.2f \ ///
packyears contn %4.2f \ prior_fx bin %4.1f \ fall bin %4.1f \ lrsfnbmd contn %4.2f \ lrsfnbmdadj contn %4.2f \ ///
cvd bin %4.1f \ diabetes bin %4.1f \ hypert bin %4.1f \ cancer bin %4.1f \ respi bin %4.1f \ /// 
neuro bin %4.1f \ rheum bin %4.1f \ fnbmd contn %4.2f \ fnbmdt contn %4.2f \ hip_10y contn %4.2f \) ///
nospace percent_n onecol missing total(before)

*************************************************************************
* Table 2. Comparision of BMD, bone health and hip fracture incidence	*
*************************************************************************

*********
* Women *
*********

** FU time 
table entry_cat if sex == 1, c(n futime p25 futime median futime p75 futime)
** FNBMD
table entry_cat if sex == 1, c(n fnbmd mean fnbmd sd fnbmd)
glm fnbmd entry_cat age if sex == 1, nolog
glm fnbmd entry_cat age fall prior_fx caintake pai maxalco packyears bmi lrsfn_sd if sex == 1, nolog
** Bone health
table bone entry_cat if sex == 1, c(n fnbmd mean fnbmd sd fnbmd) col
tab bone entry_cat if sex == 1, co nokey
mlogit bone entry_cat age if sex == 1, base(0) rrr nolog
mlogit bone entry_cat age fall prior_fx caintake pai maxalco packyears bmi lrsfn_sd if sex == 1, base(0) rrr nolog
** Incidence hip fracture
ir hipfx entry_cat futime if sex == 1
ir hipfx entry_cat futime if sex == 1 & bone == 0
ir hipfx entry_cat futime if sex == 1 & bone == 1
ir hipfx entry_cat futime if sex == 1 & bone == 2

stset futime, f(hipfx)
stcox entry_cat age if sex == 1, nolog
	estat phtest, detail
stcox entry_cat##bone age if sex == 1, nolog
	estat phtest, detail
	lincom 1.entry_cat + 1.entry_cat#0.bone, hr
	lincom 1.entry_cat + 1.entry_cat#1.bone, hr	
	lincom 1.entry_cat + 1.entry_cat#2.bone, hr
stcox entry_cat age fall prior_fx caintake pai maxalco packyears bmi lrsfn_sd if sex == 1, nolog
	estat phtest, detail
stcox entry_cat##bone age fall prior_fx caintake pai maxalco packyears bmi lrsfn_sd if sex == 1, nolog
	estat phtest, detail
	lincom 1.entry_cat + 1.entry_cat#0.bone, hr
	lincom 1.entry_cat + 1.entry_cat#1.bone, hr	
	lincom 1.entry_cat + 1.entry_cat#2.bone, hr

*********
* Men	*
*********

** FU time 
table entry_cat if sex == 0, c(n futime p25 futime median futime p75 futime)
** FNBMD
table entry_cat if sex == 0, c(n fnbmd mean fnbmd sd fnbmd)
glm fnbmd entry_cat age if sex == 0, nolog
glm fnbmd entry_cat age fall prior_fx caintake pai maxalco packyears bmi lrsfn_sd if sex == 0, nolog
** Bone health
table bone entry_cat if sex == 0, c(n fnbmd mean fnbmd sd fnbmd) col
tab bone entry_cat if sex == 0, co nokey
mlogit bone entry_cat age if sex == 0, base(0) rrr nolog
mlogit bone entry_cat age fall prior_fx caintake pai maxalco packyears bmi lrsfn_sd if sex == 0, base(0) rrr nolog
** Incidence hip fracture
ir hipfx entry_cat futime if sex == 0
ir hipfx entry_cat futime if sex == 0 & bone == 0
ir hipfx entry_cat futime if sex == 0 & bone == 1
ir hipfx entry_cat futime if sex == 0 & bone == 2

stset futime, f(hipfx)
stcox entry_cat age if sex == 0, nolog
	estat phtest, detail
stcox entry_cat##bone age if sex == 0, nolog
	estat phtest, detail
	lincom 1.entry_cat + 1.entry_cat#0.bone, hr
	lincom 1.entry_cat + 1.entry_cat#1.bone, hr	
	lincom 1.entry_cat + 1.entry_cat#2.bone, hr
stcox entry_cat age fall prior_fx caintake pai maxalco packyears bmi lrsfn_sd if sex == 0, nolog
	estat phtest, detail
stcox entry_cat##bone age fall prior_fx caintake pai maxalco packyears bmi lrsfn_sd if sex == 0, nolog
	estat phtest, detail
	lincom 1.entry_cat + 1.entry_cat#0.bone, hr
	lincom 1.entry_cat + 1.entry_cat#1.bone, hr	
	lincom 1.entry_cat + 1.entry_cat#2.bone, hr

*************************
* Fig 1. BMD changes	*
*************************

twoway (kdensity fnbmd if sex == 1 & entry_cat == 0, lcolor(blue)) /// 
(kdensity fnbmd if sex == 1 & entry_cat == 1, lcolor(red)), /// 
legend(off) graphregion(color(white)) xtitle("Femoral neck BMD (g/cm2)") ///
ytitle("") ylabel(, angle(0)) name(women, replace) 

twoway (kdensity fnbmd if sex == 0 & entry_cat == 0, lcolor(blue)) /// 
(kdensity fnbmd if sex == 1 & entry_cat == 0, lcolor(red)), /// 
legend(off) graphregion(color(white)) xtitle("Femoral neck BMD (g/cm2)") ///
ytitle("") ylabel(, angle(0)) name(men, replace) 

gr combine women men

*************************************************************
* Fig 2. Incidence of hip fracture by bone health status	*
*************************************************************

clear
input sex group est lower upper
0 2 0.54 0.38 0.78 
0 3 0.42 0.18 0.999
0 4 0.60 0.36 1.00
0 5 0.56 0.33 0.98
0 7 0.84 0.67 1.05
0 8 0.65 0.47 0.89
1 2 0.39 0.19 0.80
1 3 0.11 0.01 0.84
1 4 0.56 0.25 1.28
1 5 0.56 0.15 2.06
1 7 0.89 0.68 1.18
1 8 0.61 0.35 1.07
end

lab def group 2 "All" 3 "Normal bone" 4 "Osteopenia" 5 "Osteporotic" 7 "Osteopenic" 8 "Osteoporotic"
lab val group group
lab def sex 0 "Women" 1 "Men"
lab val sex sex
lab var est "Hazard ratio (95% CI)"

eclplot est lower upper group, hori ///
estopts(msymbol(circle) msize(medium_small) mco(black)) ciopts(msize(medium_small) lco(black)) /// 
by(sex, legend(off) compact) yscale(range(1 8)) ylabel(1(1)8, nogrid) /// 
xline(1, lpattern(dash) lcolor(black)) scheme(lean1) graphregion(color(white)) bgcolor(white) ///
xscale(range(0 2)) xlab(0 0.5 1 2) yscale(range(1 9)) ylab(2 3 4 5 7 8)

*************************************************************
* Fig 3. Number of hip fractures potentially prevented		*
*************************************************************
clear
input sex group est lower upper
1 2 -2137 -4701 -424 
1 3 -3825 -6022 -1996
1 4 -13400 -19236 -8355 
0 2 -2672 -4621 -1415
0 3 -3086 -6113 -709
0 4 -6507 -16573 436
end

lab def group 2 "Normal" 3 "Osteopenia" 4 "Osteoporotic" 
lab val group group
lab def sex 0 "Women" 1 "Men"
lab val sex sex
lab var est "Number of fractures potentially prevented (95% CI)"

eclplot est lower upper group, hori ///
estopts(msymbol(circle) msize(medium_small) mco(black)) ciopts(msize(medium_small) lco(black)) /// 
by(sex, legend(off) compact) /// 
yscale(range(1.5 4.5)) ylabel(2 3 4, nogrid) xtitle ({bf: Number of hip fractures potentially prevented (95% CI)}) /// 
xline(0, lpattern(dash) lcolor(black)) scheme(lean1) graphregion(color(white)) bgcolor(white) /// 
xscale(range(-20000 500)) xlab(-20000 -10000 -5000 -1000 0 500)

*********************************
* Fig S1. Flowchart data		*
*********************************
use "C:\Thach\Research projects\Rose paradox\hipfx_roseparadox.dta", clear

ta entry_cat sex	
ta death hipfx if entry_cat == 0 & sex == 1, missing
ta death hipfx if entry_cat == 1 & sex == 1, missing
ta death hipfx if entry_cat == 0 & sex == 0, missing
ta death hipfx if entry_cat == 1 & sex == 0, missing

*****************************************************
* Table S1. BMD and hip fractures by entry periods	*
*****************************************************
stset futime, f(hipfx)

*********
* Women	*
*********

stcox fnbmd_sd age if sex == 1 & entry_cat == 0
	estat phtest, detail
stcox fnbmd_sd age bmi fall prior_fx caintake pai maxalco packyears lrsfn_sd if sex == 1 & entry_cat == 0, nolog
	estat phtest, detail
stcox fnbmd_sd age if sex == 1 & entry_cat == 1
	estat phtest, detail
stcox fnbmd_sd age bmi fall prior_fx caintake pai maxalco packyears lrsfn_sd if sex == 1 & entry_cat == 1, nolog
	estat phtest, detail

*********
* Men	*
*********

stcox fnbmd_sd age if sex == 0 & entry_cat == 0
	estat phtest, detail
stcox fnbmd_sd age bmi fall prior_fx caintake pai maxalco packyears lrsfn_sd if sex == 0 & entry_cat == 0, nolog
	estat phtest, detail
stcox fnbmd_sd age if sex == 0 & entry_cat == 1
	estat phtest, detail
stcox fnbmd_sd age bmi fall prior_fx caintake pai maxalco packyears lrsfn_sd if sex == 0 & entry_cat == 1, nolog
	estat phtest, detail

*************************************************************************
* Table S2. Sensitivity analysis excluding those with given BP anytime	*
*************************************************************************

drop if bp == 1

*********
* Women	*
*********

** FU time 
table entry_cat if sex == 1, c(n futime p25 futime median futime p75 futime)
** FNBMD
table entry_cat if sex == 1, c(n fnbmd mean fnbmd sd fnbmd)
glm fnbmd entry_cat age if sex == 1, nolog
glm fnbmd entry_cat age fall prior_fx caintake pai maxalco packyears bmi lrsfn_sd if sex == 1, nolog
** Bone health
table bone entry_cat if sex == 1, c(n fnbmd mean fnbmd sd fnbmd) col
tab bone entry_cat if sex == 1, co nokey
mlogit bone entry_cat age if sex == 1, base(0) rrr nolog
mlogit bone entry_cat age fall prior_fx caintake pai maxalco packyears bmi lrsfn_sd if sex == 1, base(0) rrr nolog
** Incidence hip fracture
ir hipfx entry_cat futime if sex == 1
ir hipfx entry_cat futime if sex == 1 & bone == 0
ir hipfx entry_cat futime if sex == 1 & bone == 1
ir hipfx entry_cat futime if sex == 1 & bone == 2

stset futime, f(hipfx)
stcox entry_cat age if sex == 1, nolog
	estat phtest, detail
stcox entry_cat##bone age if sex == 1, nolog
	estat phtest, detail
	lincom 1.entry_cat + 1.entry_cat#0.bone, hr
	lincom 1.entry_cat + 1.entry_cat#1.bone, hr	
	lincom 1.entry_cat + 1.entry_cat#2.bone, hr
stcox entry_cat age fall prior_fx caintake pai maxalco packyears bmi lrsfn_sd if sex == 1, nolog
	estat phtest, detail
stcox entry_cat##bone age fall prior_fx caintake pai maxalco packyears bmi lrsfn_sd if sex == 1, nolog
	estat phtest, detail
	lincom 1.entry_cat + 1.entry_cat#0.bone, hr
	lincom 1.entry_cat + 1.entry_cat#1.bone, hr	
	lincom 1.entry_cat + 1.entry_cat#2.bone, hr

*********
* Men	*
*********

** FU time 
table entry_cat if sex == 0, c(n futime p25 futime median futime p75 futime)
** FNBMD
table entry_cat if sex == 0, c(n fnbmd mean fnbmd sd fnbmd)
glm fnbmd entry_cat age if sex == 0, nolog
glm fnbmd entry_cat age fall prior_fx caintake pai maxalco packyears bmi lrsfn_sd if sex == 0, nolog
** Bone health
table bone entry_cat if sex == 0, c(n fnbmd mean fnbmd sd fnbmd) col
tab bone entry_cat if sex == 0, co nokey
mlogit bone entry_cat age if sex == 0, base(0) rrr nolog
mlogit bone entry_cat age fall prior_fx caintake pai maxalco packyears bmi lrsfn_sd if sex == 0, base(0) rrr nolog
** Incidence hip fracture
ir hipfx entry_cat futime if sex == 0
ir hipfx entry_cat futime if sex == 0 & bone == 0
ir hipfx entry_cat futime if sex == 0 & bone == 1
ir hipfx entry_cat futime if sex == 0 & bone == 2

stset futime, f(hipfx)
stcox entry_cat age if sex == 0, nolog
	estat phtest, detail
stcox entry_cat##bone age if sex == 0, nolog
	estat phtest, detail
	lincom 1.entry_cat + 1.entry_cat#0.bone, hr
	lincom 1.entry_cat + 1.entry_cat#1.bone, hr	
	lincom 1.entry_cat + 1.entry_cat#2.bone, hr
stcox entry_cat age fall prior_fx caintake pai maxalco packyears bmi lrsfn_sd if sex == 0, nolog
	estat phtest, detail
stcox entry_cat##bone age fall prior_fx caintake pai maxalco packyears bmi lrsfn_sd if sex == 0, nolog
	estat phtest, detail
	lincom 1.entry_cat + 1.entry_cat#0.bone, hr
	lincom 1.entry_cat + 1.entry_cat#1.bone, hr	
	lincom 1.entry_cat + 1.entry_cat#2.bone, hr

*************************************************************	
* Table S3- Sensitivity analysis 2: Competing risk of death	*
*************************************************************
use "C:\Thach\Research projects\Rose paradox\hipfx_roseparadox.dta", clear

gen fx_comp = .
	replace fx_comp = 1 if hipfx == 1
	replace fx_comp = 2 if hipfx == 0 & death == 1
	replace fx_comp = 0 if hipfx == 0 & death == 0

*********
* Women	*
*********

ta fx_comp entry_cat if sex == 1, co nokey
	ta fx_comp entry_cat if sex == 1 & bone == 0, co nokey
	ta fx_comp entry_cat if sex == 1 & bone == 1, co nokey
	ta fx_comp entry_cat if sex == 1 & bone == 2, co nokey

** Cause-specific regression for fracture risk	*
stset futime, f(fx_comp == 1)
stcox entry_cat age if sex == 1, nolog
stcox entry_cat##bone age if sex == 1, nolog
	lincom 1.entry_cat + 1.entry_cat#0.bone, hr
	lincom 1.entry_cat + 1.entry_cat#1.bone, hr	
	lincom 1.entry_cat + 1.entry_cat#2.bone, hr
stcox entry_cat age fall prior_fx caintake pai maxalco packyears bmi lrsfn_sd if sex == 1, nolog
stcox entry_cat##bone age fall prior_fx caintake pai maxalco packyears bmi lrsfn_sd if sex == 1, nolog
	lincom 1.entry_cat + 1.entry_cat#0.bone, hr
	lincom 1.entry_cat + 1.entry_cat#1.bone, hr	
	lincom 1.entry_cat + 1.entry_cat#2.bone, hr
** Cause-specific regression for risk of death wo fracture	*
stset futime, f(fx_comp == 2)
stcox entry_cat age if sex == 1, nolog
stcox entry_cat##bone age if sex == 1, nolog
	lincom 1.entry_cat + 1.entry_cat#0.bone, hr
	lincom 1.entry_cat + 1.entry_cat#1.bone, hr	
	lincom 1.entry_cat + 1.entry_cat#2.bone, hr
stcox entry_cat age fall prior_fx caintake pai maxalco packyears bmi lrsfn_sd if sex == 1, nolog
stcox entry_cat##bone age fall prior_fx caintake pai maxalco packyears bmi lrsfn_sd if sex == 1, nolog
	lincom 1.entry_cat + 1.entry_cat#0.bone, hr
	lincom 1.entry_cat + 1.entry_cat#1.bone, hr	
	lincom 1.entry_cat + 1.entry_cat#2.bone, hr
** Fine-Gray subdistribution regression	*
stset futime, f(fx_comp == 1)
stcrreg entry_cat age if sex == 1, compete(fx_comp == 2) nolog
stcrreg entry_cat##bone age if sex == 1, compete(fx_comp == 2) nolog
	lincom 1.entry_cat + 1.entry_cat#0.bone, hr
	lincom 1.entry_cat + 1.entry_cat#1.bone, hr	
	lincom 1.entry_cat + 1.entry_cat#2.bone, hr
stcrreg entry_cat age fall prior_fx caintake pai maxalco packyears bmi lrsfn_sd if sex == 1, compete(fx_comp == 2) nolog
stcrreg entry_cat##bone age fall prior_fx caintake pai maxalco packyears bmi lrsfn_sd if sex == 1, compete(fx_comp == 2) nolog
	lincom 1.entry_cat + 1.entry_cat#0.bone, hr
	lincom 1.entry_cat + 1.entry_cat#1.bone, hr	
	lincom 1.entry_cat + 1.entry_cat#2.bone, hr

*********
* Men	*
*********

ta fx_comp entry_cat if sex == 0, co nokey
	ta fx_comp entry_cat if sex == 0 & bone == 0, co nokey
	ta fx_comp entry_cat if sex == 0 & bone == 1, co nokey
	ta fx_comp entry_cat if sex == 0 & bone == 2, co nokey
** Cause-specific regression for fracture risk	*
stset futime, f(fx_comp == 1)
stcox entry_cat age if sex == 0, nolog
stcox entry_cat##bone age if sex == 0, nolog
	lincom 1.entry_cat + 1.entry_cat#0.bone, hr
	lincom 1.entry_cat + 1.entry_cat#1.bone, hr	
	lincom 1.entry_cat + 1.entry_cat#2.bone, hr
stcox entry_cat age fall prior_fx caintake pai maxalco packyears bmi lrsfn_sd if sex == 0, nolog
stcox entry_cat##bone age fall prior_fx caintake pai maxalco packyears bmi lrsfn_sd if sex == 0, nolog
	lincom 1.entry_cat + 1.entry_cat#0.bone, hr
	lincom 1.entry_cat + 1.entry_cat#1.bone, hr	
	lincom 1.entry_cat + 1.entry_cat#2.bone, hr
** Cause-specific regression for risk of death wo fracture	*
stset futime, f(fx_comp == 2)
stcox entry_cat age if sex == 0, nolog
stcox entry_cat##bone age if sex == 0, nolog
	lincom 1.entry_cat + 1.entry_cat#0.bone, hr
	lincom 1.entry_cat + 1.entry_cat#1.bone, hr	
	lincom 1.entry_cat + 1.entry_cat#2.bone, hr
stcox entry_cat age fall prior_fx caintake pai maxalco packyears bmi lrsfn_sd if sex == 0, nolog
stcox entry_cat##bone age fall prior_fx caintake pai maxalco packyears bmi lrsfn_sd if sex == 0, nolog
	lincom 1.entry_cat + 1.entry_cat#0.bone, hr
	lincom 1.entry_cat + 1.entry_cat#1.bone, hr	
	lincom 1.entry_cat + 1.entry_cat#2.bone, hr
** Fine-Gray subdistribution regression	*
stset futime, f(fx_comp == 1)
stcrreg entry_cat age if sex == 0, compete(fx_comp == 2) nolog
stcrreg entry_cat##bone age if sex == 0, compete(fx_comp == 2) nolog
	lincom 1.entry_cat + 1.entry_cat#0.bone, hr
	lincom 1.entry_cat + 1.entry_cat#1.bone, hr	
	lincom 1.entry_cat + 1.entry_cat#2.bone, hr
stcrreg entry_cat age fall prior_fx caintake pai maxalco packyears bmi lrsfn_sd if sex == 0, compete(fx_comp == 2) nolog
stcrreg entry_cat##bone age fall prior_fx caintake pai maxalco packyears bmi lrsfn_sd if sex == 0, compete(fx_comp == 2) nolog
	lincom 1.entry_cat + 1.entry_cat#0.bone, hr
	lincom 1.entry_cat + 1.entry_cat#1.bone, hr	
	lincom 1.entry_cat + 1.entry_cat#2.bone, hr

log close

*****************************************************
* Create a sample data set of 10% original dataset	*
*****************************************************

set seed 1234
sample 10
gen id = _n
drop garvanid
save "C:\Thach\Research projects\Rose paradox\Hipfx_Rose_sample.dta", replace

