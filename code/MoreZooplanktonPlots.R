#I've got the zooplankton data all put together now, more exploritory plots!

library(tidyverse)
library(lubridate)
library(readxl)


load("data/shipchannelzoops.RData")
crosswalk = read_csv("data/zoopstaxa.csv") %>%
  select(Taxlifestage, IBMR, CarbonWeight_ug) %>%
  distinct()



zoops = left_join(shipchannel, crosswalk, relationship = "many-to-one") %>%
  mutate(BPUE = CPUE * CarbonWeight_ug)

ggplot(shipchannel, aes(x = Latitude, y = CPUE)) +geom_point(aes(color = TowType))


ggplot(zoops, aes(x = Latitude, y = BPUE)) +geom_point(aes(color = TowType))

#sum by IBMR group
zoopI = group_by(zoops, SampleID, IBMR, TowType, Date, Latitude, Longitude, Source, Station, DOY) %>%
  summarize(CPUE = sum(CPUE), BPUE = sum(BPUE))

#i'm not sure what the appropriate bins are for regions, but let's try this for now

zoopI = mutate(zoopI, Region = case_when(Latitude > 38.5 ~ "Top",
                                         Latitude > 38.3 & Latitude <= 38.5 ~ "Middle",
                                         Latitude <= 38.3 ~ "Lower"),
               Month = month(Date), 
               Year = year(Date),
               MonthYear = Year + (1-Month)/12)

ggplot(zoopI, aes(x = Region, y = BPUE, fill = IBMR)) +geom_col(position = "fill")+
  facet_wrap(~TowType)


zoopIwzeros = zoopI %>%
  pivot_wider(id_cols = c(SampleID, TowType, Date, Latitude, Longitude, Source, Station, DOY, Month, Year, MonthYear, Region),
              names_from = IBMR, values_from = BPUE, values_fill = 0) %>%
  pivot_longer(cols = c(allcopnaup:last_col()), names_to = "IBMR", values_to = "BPUE")
  
zoopIave = group_by(zoopIwzeros, Region, TowType, Month, Year, MonthYear, IBMR) %>%
  summarize(BPUE = mean(BPUE))

#Average BPUE by month, year, and region
ggplot(zoopIave, aes(x = Month, y = BPUE, fill = IBMR))+
  facet_grid(Region~Year)+ geom_col()


#Average BPUE by tow type, and region
ggplot(zoopIave, aes(x =TowType, y = BPUE, fill = IBMR))+
  facet_grid(Region~Month)+ geom_col()

ggplot(zoopIave, aes(x =Region, y = BPUE, fill = IBMR))+
  facet_grid(TowType~Month)+ geom_col()
  
# I guess we said we'd focus on summer-fall, so let's look at average biomass by 
#region and depth

#i'm taking out the vertical and oblique tows just so I can make this figure, i need to fiture out what to do with them later.
regave = filter(zoopIave, Month %in% c(6:11), TowType %in% c("Bottom", "Surface")) %>%
  group_by(Region, TowType, IBMR) %>%
  summarize(BPUE = mean(BPUE)) %>%
  mutate(TowType = factor(TowType, levels = c("Surface", "Bottom")))

#community composition
ggplot(regave, aes(x = Region, y = BPUE, fill = IBMR))+ geom_col(position = "fill")+
  facet_wrap(~TowType, ncol =1)

#BPUE
ggplot(regave, aes(x = Region, y = BPUE, fill = IBMR))+ geom_col()+
  facet_wrap(~TowType, ncol =1)
