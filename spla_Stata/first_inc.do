*** Industrialisation and Occupational Insurance Fund participation_ First Incorporation.

*** open first_inc_data

****use "~/Downloads/first_inc_data.dta", replace

*** Data Description
** dindus is decade average of industry to agriculture value added from MOxLAC dataset.
** expart is occupational insurance fund participation in EAP from Mesa-Lago (1991), data are single point observations.
** dtu is index of trade union engagement, expert estimate of share of population engaged with tu's from Vdem 2023 v13. Index: 0=0; 1= >5%; 2= 5-15%; 3= 16-25%; 4 >25%.
** dpolinq is index of the dominance of economic power in politics from Vdem. 0 if wealthy dominate 4 if political power is more or less equally distributed.
***** dtu and dpolinq can be used to explore potential mediators.

* Nonparametric regression of 1930s industrialisation on 1960 insurance fund participation. 

npregress kernel xpart60 dindus30 , vce(bootstrap , reps(100) seed(123))

set scheme lean2

npgraph , mlabel(country) mlabs(tiny) mlabp(pos) xtitle(Industry to agriculture value added 1930s) ///
	ytitle(Participation rates 1960s) title(Industrialisation on fund participation rate) 

	
* Nonparametric regression of 1930s industrialisation on 1970 insurance fund participation. 
	
npregress kernel xpart70 dindus30 , vce(bootstrap , reps(100) seed(123))

set scheme lean2

npgraph , mlabel(country) mlabs(tiny) mlabp(pos1) xtitle(Industry to agriculture value added 1930s) ///
	ytitle(Participation rates 1970s) title(Industrialisation on fund participation rate) 

* Nonparametric regression of 1930s industrialisation on 1980 insurance fund participation. 

npregress kernel xpart80 dindus30 , vce(bootstrap , reps(100) seed(123))

set scheme lean2

npgraph , mlabel(country) mlabs(tiny) xtitle(Industry to agriculture value added 1980s) ///
	ytitle(participation rates 1980s) title(Industrialisation on fund participation rate) 


