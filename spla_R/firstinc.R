## This file explores industrialisation effects on occupation insurance fund participation
## in the first incorporation period 1960-1979

## using first_inc_data.dta file containing decade averages:
## industrial to agriculture value added (MOxLAD); 
## trade union engagement and economic power in politics (Vdem) for 1920s to 1960s,
## and occupational insurance fund participation (Mesa-Lago) for 1960s, 1970s, 1980s and 1985.

library(haven)
firstinc <- read_dta("first_inc_data.dta")

library(ggplot2)

## using 1930s indus and 1960s participation

ggplot(firstinc, aes(dindus30 , xpart60 , label=(country))) +
  geom_point(color="black" , shape=21 , fill ="white", size = 1,  na.rm = TRUE) +
  geom_text(size=3 , color ="Red", vjust=-0.05 , nudge_y = 0.005) +
  geom_smooth(method=loess , color=("black") , se=FALSE) +
  labs(title = "Industrialisation and social protection in LA" ,
       x = "Industry to agriculture value added 1930s" ,
       y = "Occupational fund participation 1960s" ,
  )

## using 1930s indus and 1970s participation
ggplot(firstinc, aes(dindus30 , xpart70 , label=(country))) +
  geom_point(color="black" , shape=21 , fill ="white", size = 1,  na.rm = TRUE) +
  geom_text(size=3 , color ="Red", vjust=0 , nudge_y = 0.005) +
  geom_smooth(method=loess , color=("black") , se=FALSE) +
  labs(title = "Industrialisation and social protection in LA" ,
       x = "Industry to agriculture value added 1930s" ,
       y = "Occupational fund participation 1970s" ,
  )
     
## using 1940s indus and 1970s participation
ggplot(firstinc, aes(dindus40 , xpart70 , label=(country))) +
  geom_point(color="black" , shape=21 , fill ="white", size = 1,  na.rm = TRUE) +
  geom_text(size=3 , color ="Red", vjust=0 , nudge_y = 0.005 ) +
  geom_smooth(method=loess , color=("black") , se=FALSE) +
  labs(title = "Industrialisation and social protection in LA" ,
              x = "Industry to agriculture value added 1940s" ,
              y = "Occupational fund participation 1970s" ,
         )  