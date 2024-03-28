*** This file shows data preparation and model estimates for the retrenchement period.
*** Discussed in Barrientos(2024) Social Protection in Latin America.

** open SPLAdataset

*** use "~/Downloads/SPLAdataset.dta", replace

sort country year

keep if year>1979 & year<2000


*** drop variables with no or v. few observations - mainly survey based data and CEPAL/Arenas de Mesa

keep country year v2pepwrses_mean v2catrauni_mean indus termstrade region lu hca emp indempsh ///
   gini_std gini a075 palma bottom5 bottom20 bottom40 top5 top20 middle50 ginia Population gdppc /// 
   presturnout presVAPturnout presneturnout parlturnout parlVAPturnout neturnout compulsoryvote  /// 
   povgap contrate contsowndata underfivemortalityrate realminwage p_h g_h coalition enph medmeand medmeanp ///
   demrss demtri demaclp dempolity popauthor repressauthor electoral formgov federal left p_polity2

gen emp2pop=(emp*1000000)/Population

*** contributors in EAP variable <contsowndata> has many gaps, and it is a mix of mainly administrative data
*** and survey data towards the very end of the period. Yet it is crucial to the analysis. 
*** Therefore I tried to fill in as many gaps as possible from several sources and checked for outliers and discarded them.

*  add MesaLago 1978/91 data points 1980 and 1985

gen contribeap=contsowndata
replace contribeap = 18.5 in 21
replace contribeap = 30.4 in 81
replace contribeap = 30.2 in 86
replace contribeap = 93 in 121
replace contribeap = 11.3 in 146
replace contribeap = 21.3 in 161
replace contribeap = 23.4 in 166
replace contribeap = 11.6 in 181
replace contribeap = 33.1 in 201
replace contribeap = 26.8 in 206
replace contribeap = 14.4 in 221
replace contribeap = 12.8 in 226
replace contribeap = 42 in 241
replace contribeap = 41.7 in 246
replace contribeap = 18.9 in 261
replace contribeap = 31.5 in 266
replace contribeap = 52.3 in 281
replace contribeap = 56.4 in 286
replace contribeap = 14 in 301
replace contribeap = 38 in 326
replace contribeap = 49.8 in 361
replace contribeap = 54.3 in 366

* add Dominican Rep 1970 datapoint from Mesa Lago 1978/91to ensure at least 2 datapoints per country
replace contribeap = 8.9 in 141

* add Arenas de Mesa using 2019 and 2000 datapoints frontloaded to 1999 where absent in recent period
replace contribeap = 18.5 in 160
replace contribeap = 22.9 in 220
replace contribeap = 17.1 in 240
replace contribeap = 17.3 in 280
replace contribeap = 52.5 in 300
replace contribeap = 11.9 in 340

**** drop Cuba b/c v. few observations

drop if country=="Cuba"


/*
	twoway (scatter contribeap  year, msize(vsmall) sort), ///
	ytitle("") ylabel(0(40)100) ///
	xlabel(1980(5)2000)  ///
	by(, title(Contributors as share of EAP)) ///
	by(country)
	
	twoway (scatter realminwage  year, msize(vsmall) sort), ///
	ytitle("") ylabel(0(200)500) ///
	xlabel(1980(5)2000)  ///
	by(, title(Real min wage index 100=2012)) ///
	by(country)
	
	twoway (scatter left  year, msize(vsmall) sort), ///
	ytitle("") ylabel(0(.2)1) ///
	xlabel(1980(5)2000)  ///
	by(, title(left) note(Huber et al. Politics dataset)) ///
	by(country)
	
	twoway (scatter neturnout  year, msize(vsmall) sort), ///
	ytitle("") ylabel(0(0.4)1) ///
	xlabel(1980(5)2000)  ///
	by(, title(turnout for parliamentary elections) note(IDEA dataset)) ///
	by(country)
	
	twoway (scatter dempolity  year, msize(vsmall) sort), ///
	ytitle("") ylabel(-10(5)10) ///
	xlabel(1980(5)2000)  ///
	by(, title(PolityIV) note(Huber dataset)) ///
	by(country)
	
	twoway (scatter emp2pop  year, msize(vsmall) sort), ///
	ytitle("") ylabel(0(0.1)0.5) ///
	xlabel(1980(5)2000)  ///
	by(, title(employment to population from penn tables)) ///
	by(country)
	
	twoway (scatter gdppc  year, msize(vsmall) sort), ///
	ytitle("") ylabel(800(8000)16000) ///
	xlabel(1980(5)2000)  ///
	by(, title(GDP per capita) note(Penn Tables)) ///
	by(country)
	
	twoway (scatter underfivemortalityrate  year, msize(vsmall) sort), ///
	ytitle("") ylabel(0(5)20) ///
	xlabel(1980(5)2000)  ///
	by(, title(Under five mortality rate)) ///
	by(country)
		
	twoway (scatter indus  year, msize(vsmall) sort), ///
	ytitle("") ylabel(0(1)5) ///
	xlabel(1980(5)2000)  ///
	by(, title(Manufacturing over agriculture value added) note(MOxLAD data)) ///
	by(country)
	
	twoway (scatter top20  year, msize(vsmall) sort), ///
	ytitle("") ylabel(30(10)50) ///
	xlabel(1980(5)2000)  ///
	by(, title(Top 20 % share of income)  ///
	note(WIID data)) ///
	by(country)
	
	twoway (scatter v2pepwrses_mean  year, msize(vsmall) sort), ///
	ytitle("") ylabel(0(2)4) ///
	xlabel(1980(5)2000)  ///
	by(, title(ec power dominates) note(Vdem data)) ///
	by(country)
	
	twoway (scatter v2catrauni_mean  year, msize(vsmall) sort), ///
	ytitle("") ylabel(0(1)4) ///
	xlabel(1980(5)2000)  ///
	by(, title(Share population engaged with independent trade unions) ///
	note(V-Dem v. 13 2023 data ; Index: 0=0; 1= >5%; 2= 5-15%; 3= 16-25%; 4 >25%)) ///
	by(country)
	
	
*/

** fill in missing data for indempsh (comes from Census data) and contribeap ; foward fill in.


foreach var of varlist contribeap indempsh {
                by country : replace `var' =`var'[_n-1] if missing(`var')
				}

** fill in missing data for WIID data - survey based


foreach var of varlist gini_std gini a075 palma bottom5 bottom20 bottom40 top5 top20 middle50 ginia medmeand medmeanp {
                by country : replace `var' =`var'[_n-1] if missing(`var')
				}

				
				
*** si-->(e(p,i))				
				

reg  indempsh contribeap neturnout underfivemortalityrate gdppc 
gen sample1=e(sample)
gen ytilde = indempsh - _b[neturnout] -_b[underfivemortalityrate]
reg ytilde contribeap gdppc if sample1==1

drop ytilde
drop sample1

reg  indus contribeap dempolity underfivemortalityrate region gdppc 
gen sample1=e(sample)
gen ytilde = indus - _b[dempolity] -_b[underfivemortalityrate]
reg ytilde contribeap  region gdppc if sample1==1

drop ytilde
drop sample1

reg  indus contribeap v2catrauni_mean underfivemortalityrate region gdppc 
gen sample1=e(sample)
gen ytilde = indus - _b[v2catrauni_mean] -_b[underfivemortalityrate]
reg ytilde contribeap  region gdppc if sample1==1

drop ytilde
drop sample1

reg a075 indus contribeap underfivemortalityrate region gdppc
gen sample1=e(sample)
gen ytilde = a075 -_b[underfivemortalityrate] -_b[indus]
reg ytilde contribeap region gdppc if sample1==1

drop ytilde
drop sample1

reg medmeanp indus contribeap underfivemortalityrate region gdppc
gen sample1=e(sample)
gen ytilde = medmeanp -_b[underfivemortalityrate] -_b[indus]
reg ytilde contribeap region gdppc if sample1==1


*** Correcting for standard errors 
** note to drop program: program drop deboot

drop ytilde
drop sample1

program define deboot, rclass
preserve
reg indus contribeap dempolity underfivemortalityrate region gdppc  
gen sample1=e(sample)
gen ytilde = indus
replace ytilde = indus - _b[dempolity] 
reg ytilde contribeap underfivemortalityrate region gdppc if sample1==1
return scalar deffect = _b[underfivemortalityrate]
drop sample1
restore
end
bootstrap deffect=r(deffect), reps(1000) seed(12345) : deboot				

*** dynamics??

* b/c country is string need to generate a numeric version to declare a panel
 
encode country, gen(cntry)	
xtset cntry year
			
* protection is underfivemortalityrate 
drop sample1
drop ytilde
reg indus L.contribeap dempolity underfivemortalityrate region L.gdppc 
gen sample1=e(sample)
gen ytilde = indus - _b[dempolity] - _b[underfivemortalityrate]
reg ytilde L.contribeap region L.gdppc if sample1==1


