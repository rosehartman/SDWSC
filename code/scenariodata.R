#OK, which datasets do we need for each scenario?

library(tidyverse)
library(mgcv)

#1. Scenario 1 - Stratification - 2023 only ####
#data to start with

#Temperature - surface and bottom sondes from 2023 only
temp2023 = read_csv("data/Temperature_Data_2023_Imputed.csv")
turb2023 = read_csv("data/Turbidity_Data_2023_Imputed.csv")

#Zooplankton - average surface and bottom abundance by region. 
#this is the monthly mean surface and bottom from 2017-2021
#we only have surface and bottom trawls from DOP, others are oblique
load("outputs/zoopTopBottom.RData")

#Scenario 2 - Inter-annual differences - no stratification ####

#temperature and turbidity
load("data/DWSCallWQ.RData")

#zooplankton
load("data/zoopdataIBMR.RData")
ZoopAnnual_summerfall = filter(zoopIave2, Month %in% c(6:10)) %>%
  group_by(Year, Month, MonthYear, Region, IBMR) %>%
  summarize(CPUE = mean(CPUE), BPUE = mean(BPUE))

#Scenario 3 - Inter-annual differences w/stratificaiton ####

#we don't have surface and bottom sondes in all years, or even all
#three regoins in all years, so we may need to extrapolate based on 2023 patterns
temp2023 = mutate(temp2023, DOY = yday(Date))
tempmod = gam(Temp ~ s(DOY) + Station + Depth, data = temp2023)
summary(tempmod)
plot(tempmod, all.terms=TRUE)
gam.check(tempmod)

tempmodx = gam(Temp ~ s(DOY) + Station, data = temp2023)
summary(tempmodx)
plot(tempmodx, all.terms=TRUE)

turb2023 = mutate(turb2023, DOY = yday(Date))
turbmod = gam(Turbidity ~ s(DOY) + Station, data = turb2023)
summary(turbmod)
plot(turbmod, all.terms=TRUE)
gam.check(turbmod)

turbmod2 = lm(Turbidity ~ DOY + Station, data = turb2023)
summary(turbmod2)

library(effects)
plot(allEffects(turbmod2))

#so, the top of the ship channel is three degrees warmer than the bottom,
#and you loose 0.1 degree with each meter of depth (ish)

#we can apply those terms to the data I loaded in scenario 2 if we think it would be useful. 

#We also don't have surface bottom in all years for zoops, so I extrapolated
#based on the data I do have there too

load("data/zoop_expanded.RData")
