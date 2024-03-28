## The file estimates Structural Nested Means Models SNMMs of
## of social protection stratification effects.
## using data from the retrenchment period 1980-1999.

library(tidyverse)
library(dplyr) 
library(haven)
## place dataset in working directory: SPLAdataset <- read_dta("SPLAdataset.dta")

## extract a working data file with variables relevant to the 1980-2000 period;
## drop Cuba due to few observations 
## fill in gaps, locf last observation carried forward, where data comes from 
## surveys that are not annual.

## pension fund participation is the key variable; 'constowndata' collects mainly 
## administrative data from available pension fund reports and, towards the later 
## years, from survey data; 'contsowndata1' supplements it with data from research 
## studies (mainly Mesa-Lago); 'contribeap' fills in missing values (locf).

retrench <- SPLAdataset |> filter(year > 1979 & year < 2000 , country != "Cuba") |>
  arrange(country , year) |>
  select(country , year , v2pepwrses_mean , v2catrauni_mean , indus , termstrade ,
         region , lu , hca , emp , indempsh , gini_std , gini , a075 , palma , bottom5 ,
         bottom20 , bottom40 , top5 , top20 , middle50 , ginia , Population , gdppc , 
         presturnout , presVAPturnout , presneturnout , parlturnout , parlVAPturnout ,
         neturnout , compulsoryvote , povgap , contrate , contsowndata , contsowndata1 ,
         realminwage , p_h , g_h , coalition , enph , medmeand , medmeanp , demrss ,
         demtri , demaclp , dempolity , popauthor , repressauthor , electoral , formgov ,
         federal , left , p_polity2 , under5=underfivemortalityrate) |>
  mutate(emp2pop = (emp*1000000)/Population , middle40 = 100 - (bottom40 + top20) ,
      contribeap = contsowndata1 ) |>
  group_by(country) |>
  fill(contribeap , indempsh , medmeand , medmeanp , a075 , gini_std , ginia , palma ,
       bottom5 , bottom20 , bottom40 , top5 , top20 , middle50 , middle40)

## start with the simplest model and pooled data separately for insurance (si) and assistance (sa). 
## The se need correcting but this will be done later with bootstrapping.
## Dynamics models will follow.

# si-->(e(p,i))

## first stage regression (to get effect of protection and incorporation)
first1 <- lm(indempsh ~ contribeap + neturnout + under5 + gdppc ,
             data = retrench)
## second stage regression (employment net of protection and incorporation effects)
direct1 <- lm(I(indempsh - coef(first1)["under5"]*under5 
                - coef(first1)["neturnout"]*neturnout) ~contribeap + gdppc, data = retrench)

## replace indempsh with indus as treatment.
## first stage regression (to get effect of protection and incorporation)
first2 <- lm(indus ~ contribeap + neturnout + under5 + gdppc ,
             data = retrench)
## second stage regression (employment net of protection and incorporation effects)
direct2 <- lm(I(indus - coef(first2)["under5"]*under5 
                - coef(first2)["neturnout"]*neturnout) ~contribeap + gdppc, data = retrench)

## replace neturnout with trade union engagement as incorporation effect.
## first stage regression (to get effect of protection and incorporation)
first3 <- lm(indus ~ contribeap + v2catrauni_mean + under5 + gdppc ,
             data = retrench)
## second stage regression (employment net of protection and incorporation effects)
direct3 <- lm(I(indus - coef(first3)["under5"]*under5 
                - coef(first3)["v2catrauni_mean"]*v2catrauni_mean) ~contribeap + gdppc, data = retrench)

# si-->(p(e,i))

## using Atkinson 0.75 inequality index as protection effect.

## first stage regression (to get effect of protection and incorporation).
first4 <- lm(a075 ~ contribeap + v2catrauni_mean + indus + gdppc ,
             data = retrench)
## second stage regression (employment net of protection and incorporation effects).
direct4 <- lm(I(a075 - coef(first4)["indus"]*indus 
                - coef(first4)["v2catrauni_mean"]*v2catrauni_mean) ~contribeap + gdppc, data = retrench)

## replace a075 with medmeanp as the protection effect.

## first stage regression (to get effect of protection and incorporation).
first5 <- lm(medmeanp ~ contribeap + v2catrauni_mean + indus + gdppc ,
             data = retrench)
## second stage regression (employment net of protection and incorporation effects).
direct5 <- lm(I(medmeanp - coef(first5)["indus"]*indus 
                - coef(first5)["v2catrauni_mean"]*v2catrauni_mean) ~contribeap + gdppc, data = retrench)

## dynamics?

## si[t-1]-->(e(p,i))

## run the model with one year lagged treatment and pretreatment gdppc.
### generate lagged variables:
retrench |> arrange(country , desc(year)) 
retrench <- retrench |> mutate(l1gdppc = lag(gdppc , order_by=year))
retrench <- retrench |> mutate(l1contribeap = lag(contribeap , order_by=year))

## first stage regression (to get effect of protection and incorporation)
first6 <- lm(indus ~ l1contribeap + neturnout + under5 + l1gdppc ,
             data = retrench)
## second stage regression (employment net of protection and incorporation effects)
direct6 <- lm(I(indus - coef(first6)["under5"]*under5 
                - coef(first6)["neturnout"]*neturnout) ~ l1contribeap + l1gdppc, data = retrench)

## Correcting standard errors to account for the two stage regression
## for specific variables using bootstrap. See Acharya et al. (2016).
## this model returns corrected standard errors for neturnout in the first assistance model.

# sa-->(e(p,i))

boots <- 1000
fl.boots <- rep(NA, times = boots)
for (b in 1:boots) {
  retrench.star <- retrench[sample(1:nrow(retrench), replace = TRUE),]
  boot.first <- lm(indempsh ~ contribeap + neturnout + under5 + gdppc ,
                   data = retrench.star)
  boot.direct <- lm(I(indempsh - coef(boot.first)["under5"]*under5)
                    ~ contribeap + neturnout + gdppc , data = retrench.star)
  fl.boots[b] <- coef(boot.direct)["neturnout"]
}
sd(fl.boots)



