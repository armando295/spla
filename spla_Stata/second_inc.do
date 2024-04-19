*** This file shows data preparation and model estimates for the second incorporation period.
*** Discussed in Barrientos (2024) Social Protection in Latin America


*** open SPLAdataset.dta

*** use "~/Downloads/SPLAdataset.dta", replace

sort country year

keep country year region allsapop oadipop citdipop realminwage termstrade ///
  allconts pensionrightworkers highlow hy_all laby_all xpension xtransfer ///
  gdppc leftturn largefirm smallfirm publicfirm largefirmsf publicsectorf ///
  wagedsmallfirmsi selfunskilledi zeroincomei neturnout presneturnout ///
  bottom40 top20 fgt0 fgt1 fgt2 medmeanp a075 gini_std 
  
keep if year>1999 & year<2021

* drop Cuba due to few valid observations
drop if country=="Cuba"

* generate income share of 'middle class'

gen middle40=100-(bottom40 +top20)
label variable middle40 "middle40 share of income"

*** generate employment vars 

gen formal = (largefirmsf + publicsectorf ) /100
gen informal = (wagedsmallfirmsi + selfunskilledi + zeroincomei) /100
gen largeemp = largefirmsf /100


** note: where surveys are not annual missing values can be assumed MCAR missing completely a random

** fill in missing data for the CEDLAS data - survey based
				
foreach var of varlist highlow laby_all hy_all fgt0 fgt1 fgt2 formal informal largeemp {
                by country : replace `var' =`var'[_n-1] if missing(`var')
				}						

** and for the WIID data - survey based
				
foreach var of varlist bottom40 top20 fgt0 fgt1 fgt2 medmeanp a075 gini_std {
                by country : replace `var' =`var'[_n-1] if missing(`var')
				}		

** clean up assistance and insurance data 
	
	**  nicaragua missing completely, b/c no socass - replace by zero
	** late years 2019 and 2020 have many missing due to COVID and administrative reportying delays.
	
	replace oadipop =0 if country=="Nicaragua"
	replace citdipop=0 if country=="Nicaragua"
	replace allsapop=0 if country=="Nicaragua"
	
	foreach var of varlist oadipop citdipop allsapop allconts xpension {
                by country : replace `var' =`var'[_n-1] if missing(`var')
				}

*** changes made can be checked visually before and after, for example contributors as share of the labour force

/*

twoway (scatter allconts year, msize(vsmall) sort), ///
	ytitle("") ylabel(0(40)100) ///
	xlabel(2000(5)2020)  ///
	by(, title(Contributors as share of EAP) note(Arenas de Mesa 2019 data)) ///
	by(country)	
*/			


******* Estimate Structural Nested Means Models SNMMs of second incorporation separate for assistance and insurance
**** SNMMs first estimate a regression with all variables, then strip the outcome variable of the mediators' coefficients ///
**** and in a second regression exclude the mediator variables.

**** Start with the simplest models and pooled data. The se errors need correcting, but this will be left for later.


*** sa-->(e(p,i)) 

*** read as: social assistance affects employment (informal), with protection (highlow) and incorporation (netturnout) as mediators. //
*** Growth per capita and region (Southern/Andean/Central America) as common cause confounders.  

* protection is highlow
reg informal allsapop highlow neturnout region gdppc 
gen sample1=e(sample)
gen ytilde = informal - _b[highlow] - _b[neturnout]
reg ytilde allsapop region gdppc if sample1==1
				
drop ytilde
drop sample1

* protection is fgt1
reg informal allsapop fgt1 neturnout region gdppc 
gen sample1=e(sample)
gen ytilde = informal - _b[fgt1] - _b[neturnout]
reg ytilde allsapop region gdppc if sample1==1
				
drop ytilde
drop sample1

* incorporation is leftturn
reg informal allsapop fgt1 leftturn region gdppc
gen sample1=e(sample) 
gen ytilde = informal - _b[fgt1] - _b[leftturn]
reg ytilde allsapop region gdppc if sample1==1

*** sa-->(p(e,i))

drop ytilde
drop sample1

* incorporation is leftturn
reg fgt1 allsapop leftturn highlow region gdppc
gen sample1=e(sample) 
gen ytilde = fgt1 - _b[leftturn] - _b[highlow]
reg ytilde allsapop region gdppc if sample1==1				

*** sa-->(i(e,p))

drop ytilde
drop sample1

* incorporation is leftturn protection is fgt1
reg leftturn allsapop fgt1 informal region gdppc
gen sample1=e(sample) 
gen ytilde = leftturn - _b[informal] - _b[fgt1]
reg ytilde allsapop region gdppc if sample1==1

drop ytilde
drop sample1

*** Reported standard errors need to be corrected to account for the two stage regression.
*** The model below bootstraps the se. See Acharya et al. (2016).
*** note:to drop program <program drop deboot> command



*** to check whether netturnout effect is significant

program define deboot, rclass
preserve
reg informal allsapop highlow neturnout region gdppc 
gen sample1=e(sample)
gen ytilde = informal
replace ytilde = informal - _b[highlow] 
reg ytilde allsapop neturnout region gdppc if sample1==1
return scalar deffect = _b[neturnout]
drop sample1
restore
end
bootstrap deffect=r(deffect), reps(1000) seed(12345) : deboot				

*** dynamics?

*** sa-->(e(p,i)) 

* b/c country is string need to generate a numeric version to declare a panel
 
encode country, gen(cntry)	
xtset cntry year
			
* protection is highlow
drop sample1
drop ytilde
reg informal L.allsapop highlow neturnout region L.gdppc 
gen sample1=e(sample)
gen ytilde = informal - _b[highlow] - _b[neturnout]
reg ytilde L.allsapop region L.gdppc if sample1==1


*** sa-->(i(e,p))

* incorporation is leftturn protection is fgt1 employment is highlow
drop ytilde
drop sample1
reg leftturn L.allsapop fgt1 highlow region L.gdppc
gen sample1=e(sample) 
gen ytilde = leftturn - _b[highlow] - _b[fgt1]
reg ytilde L.allsapop region L.gdppc if sample1==1	


**** Now focus on insurance (occupational insurance and individual retirement savings plans)

*** si-->(e(p,i))

drop ytilde
drop sample1

* protection is highlow
reg largefirm allconts highlow neturnout region gdppc 
gen sample1=e(sample)
gen ytilde = largefirm - _b[highlow] - _b[neturnout]
reg ytilde allconts region gdppc if sample1==1


drop ytilde
drop sample1

* incorporation is leftturn
reg largefirm allconts highlow leftturn region gdppc 
gen sample1=e(sample)
gen ytilde = largefirm - _b[highlow] - _b[leftturn]
reg ytilde allconts region gdppc if sample1==1

* protection is xpension

drop ytilde
drop sample1

reg largefirm allconts xpension neturnout region gdppc 
gen sample1=e(sample)
gen ytilde = largefirm - _b[xpension] - _b[neturnout]
reg ytilde allconts region gdppc if sample1==1

** Reported standard errors need to be corrected to account for the two stage regression.
*** The model below bootstraps the se. See Acharya et al. (2016).
*** note:to drop program <program drop deboot> command

drop ytilde
drop sample1

program drop deboot

program define deboot, rclass
preserve
reg reg largefirm allconts highlow neturnout region gdppc
gen sample1=e(sample)
gen ytilde = largefirm
replace ytilde = largefirm - _b[highlow] 
reg ytilde allconts neturnout region gdppc if sample1==1
return scalar deffect = _b[neturnout]
drop sample1
restore
end
bootstrap deffect=r(deffect), reps(1000) seed(12345) : deboot	

** dynamics

*** sa-->(e(p,i)) 

* b/c country is string need to generate a numeric version to declare a panel
 
* encode country, gen(cntry)	
* xtset cntry year				
drop ytilde
drop sample1

* protection is highlow

reg largefirm L.allconts highlow neturnout region L.gdppc 
gen sample1=e(sample)
gen ytilde = largefirm - _b[highlow] - _b[neturnout]
reg ytilde L.allconts region L.gdppc if sample1==1


** dynamics
				
drop ytilde
drop sample

* protection is xpension

reg largefirm L.allconts xpension neturnout region L.gdppc 
gen sample1=e(sample)
gen ytilde = largefirm - _b[xpension] - _b[neturnout]
reg ytilde L.allconts region L.gdppc if sample1==1


*** si-->(p(e,i))

drop ytilde
drop sample1
reg  xpension allconts largefirm neturnout region gdppc 
gen sample1=e(sample)
gen ytilde = xpension - _b[largefirm] - _b[neturnout]
reg ytilde allconts region gdppc if sample1==1

** dynamics
				
drop ytilde
drop sample1
reg xpension L.allconts largefirm neturnout L.region gdppc 
gen sample1=e(sample)
gen ytilde = xpension - _b[largefirm] - _b[neturnout]
reg ytilde L.allconts region L.gdppc if sample1==1
