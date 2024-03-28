## The file estimates alternative Structural Nested Means Models SNMMs
## of social protection stratification effects.
## using data from the second incorporation period 2000-2020.


library(tidyverse)
library(dplyr) 
library(haven)
### place dataset in directory: SPLAdataset <- read_dta("SPLAdataset.dta")

## prepare dataset by extracting observation for 2000-2020 ; dropping Cuba b/c few obs ;
## selecting variables relevant to second incorporation ; and filling in some gaps due to 
## survey years.
## note: where data comes from surveys that are not annual, missing values 
## can be assumed MCAR missing completely at random and filled in with 
## locf last observation carried forward.

secondinc <- SPLAdataset |> filter(year > 1999 & year < 2021 , country != "Cuba") |>
  arrange(country , year) |>
  select(country , year, region, allsapop , oadipop , citdipop ,
         realminwage , termstrade , allconts , pensrightw = pensionrightworkers ,
         highlow , hy_all , laby_all , xpension , xtransfer , gdppc , leftturn ,
         largefirm , smallfirm , publicfirm , largefirmsf , publicsectorf ,
         wagedsmallfirmsi , selfunskilledi , zeroincomei , neturnout , 
         presneturnout , bottom40 , top20 , fgt0 , fgt1 , fgt2 , medmeanp , 
         a075 , gini_std) |>
  mutate(middle40 = 100 - (bottom40 + top20) ,
         formal = (largefirmsf + publicsectorf)/100 ,
         informal = (wagedsmallfirmsi + selfunskilledi + zeroincomei)/100 ,
         largeemp = largefirmsf/100 ) |>
  group_by(country) |>
  fill(highlow , hy_all , laby_all , formal , informal , largeemp ,
       bottom40 , top20 , fgt0 , fgt1 , fgt2 , medmeanp , a075 , gini_std ,
       oadipop , citdipop , allsapop, allconts , xpension)

## replace missing with 0 for all Nicaragua assistance programmes

secondinc <- secondinc|> mutate(oadipop =replace_na(oadipop , 0))
secondinc <- secondinc|> mutate(citdipop =replace_na(citdipop , 0))
secondinc <- secondinc|> mutate(allsapop =replace_na(allsapop , 0))

## start with the simplest model and pooled data separately for insurance (si) and assistance (sa). 
## The se need correcting but this will be done later with bootstrapping.
## Dynamics models will follow.

# sa-->(e(p,i))

## first stage regression (to get effect of protection and incorporation)
first1 <- lm(informal ~ allsapop + highlow + neturnout + region + gdppc ,
data = secondinc)
## second stage regression (employment net of protection and incorporation effects)
direct1 <- lm(I(informal - coef(first1)["highlow"]*highlow 
    - coef(first1)["neturnout"]*neturnout) ~ allsapop + region + gdppc, data = secondinc)


# sa-->(e(p,i))

## replace highlow with fgt1.
## first stage regression (to get effect of protection and incorporation)
first2 <- lm(informal ~ allsapop + fgt1 + neturnout + region + gdppc ,
            data = secondinc)
## second stage regression (employment net of protection and incorporation effects)
direct2 <- lm(I(informal - coef(first2)["fgt1"]*fgt1 
               - coef(first2)["neturnout"]*neturnout) ~ allsapop + region + gdppc, data = secondinc)

# sa-->(p(e,i))

## replace replace employment with protection as main effect.
## first stage regression (to get effect of employment and incorporation)
first3 <- lm(fgt1 ~ allsapop + highlow + leftturn + region + gdppc ,
             data = secondinc)
## second stage regression (employment net of employment and incorporation effects)
direct3 <- lm(I(fgt1 - coef(first3)["highlow"]*highlow 
                - coef(first3)["leftturn"]*leftturn) ~ allsapop + region + gdppc, data = secondinc)

# sa-->(i(p,e))

## replace protection with incorporation as main effect.
## first stage regression (to get effect of protection and employment)
first4 <- lm(leftturn ~ allsapop + fgt1 + informal + region + gdppc ,
             data = secondinc)
## second stage regression (employment net of protection and incorporation effects)
direct4 <- lm(I(leftturn - coef(first4)["fgt1"]*fgt1 
                - coef(first4)["informal"]*informal) ~ allsapop + region + gdppc, data = secondinc)


## dynamics??

## sa[t-1]-->(e(p,i))

## run the model with one year lagged treatment and pretreatment gdppc.
## generate lagged variables:

secondinc |> arrange(country , desc(year)) 
secondinc <- secondinc |> mutate(l1gdppc = lag(gdppc , order_by=year))
secondinc <- secondinc |> mutate(l1allsapop = lag(allsapop , order_by=year))
secondinc <- secondinc |> mutate(l1allconts = lag(allconts , order_by=year))


## first stage regression (to get effect of protection and incorporation)
first5 <- lm(informal ~ l1allsapop + highlow + neturnout + region + l1gdppc ,
            data = secondinc)
## second stage regression (employment net of protection and incorporation effects)
direct5 <- lm(I(informal - coef(first5)["highlow"]*highlow 
               - coef(first5)["neturnout"]*neturnout) ~ l1allsapop + region + l1gdppc, data = secondinc)


### now focus on insurance (occupational funds and savings plans).

## si-->e(p,i))

## first stage regression (to get effect of protection and incorporation).
first10 <- lm(largefirm ~ allconts + highlow + neturnout + region + gdppc ,
              data = secondinc)
esample10<-rownames(as.matrix(resid(first10)))
## second stage regression (employment net of protection and incorporation effects).
direct10 <- lm(I(informal - coef(first10)["highlow"]*highlow 
                 - coef(first10)["neturnout"]*neturnout) ~ allconts + region + gdppc, data = secondinc[esample10,])

## swap neturnout with leftturn as incorporation variable.

## first stage regression (to get effect of protection and incorporation)
first11 <- lm(largefirm ~ allconts + highlow + leftturn + region + gdppc ,
              data = secondinc)
esample11<-rownames(as.matrix(resid(first11)))
## second stage regression (employment net of protection and incorporation effects)
direct11 <- lm(I(informal - coef(first11)["highlow"]*highlow 
                 - coef(first11)["leftturn"]*leftturn) ~ allconts + region + gdppc, data = secondinc[esample11,])


## dynamics??

## si[t-1]-->(e(p,i))

## run the model with one year lagged treatment and pretreatment gdppc.
### generate lagged variables:

## first stage regression (to get effect of protection and incorporation).
first12 <- lm(largefirm ~ l1allconts + highlow + neturnout + region + l1gdppc ,
              data = secondinc)
esample12<-rownames(as.matrix(resid(first12)))
## second stage regression (employment net of protection and incorporation effects).
direct12 <- lm(I(informal - coef(first12)["highlow"]*highlow 
                 - coef(first12)["neturnout"]*neturnout) ~ l1allconts + region + l1gdppc, data = secondinc[esample12,])

## Correcting standard errors to account for the two stage regression
## for specific variables using bootstrap. See Acharya et al. (2016).
## this model returns corrected standard errors for neturnout in the first assistance model.

# sa-->(e(p,i))

boots <- 1000
fl.boots <- rep(NA, times = boots)
for (b in 1:boots) {
secinc.star <- secondinc[sample(1:nrow(secondinc), replace = TRUE),]
boot.first <- lm(informal ~ allsapop + highlow + neturnout + region + gdppc ,
                 data = secinc.star)
boot.direct <- lm(I(informal - coef(boot.first)["highlow"]*highlow)
                  ~ allsapop + neturnout + region + gdppc , data = secinc.star)
fl.boots[b] <- coef(boot.direct)["neturnout"]
}
sd(fl.boots)

