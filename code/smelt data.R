#let's look at everything we have for the Sacramento Deep water ship channel

library(tidyverse)
library(lubridate)
library(deltafish)
library(discretewq)
library(zooper)
library(deltamapr)
library(sf)

# 
# create_fish_db()
# 
# 
# # open our two data files
# surv <- open_survey()
# fish <- open_fish()
# 
# fish_smelt <- fish %>% 
#   filter(Taxa == "Hypomesus transpacificus")
# 
# #ship channel sampling started in 2010, I think.
# 
# 
# # do a join and collect the resulting data frame
# # collect executes the sql query and gives you a table
# df <- left_join(surv, fish_smelt) %>% 
#   collect() %>%
#   filter(year(Date)>2000, !is.na(Latitude))
# 
# #Add some regions
# smeltsf = st_as_sf(df, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
#   st_transform(crs = st_crs(R_EDSM_Strata_1718P1))
# 
# smelt2 = st_join(smeltsf, R_EDSM_Strata_1718P1) %>%
#   st_drop_geometry()

#save(smelt2, smeltsf, df, file = "data/SmeltData.RData")
load("data/SmeltData.RData")
#check out just the ship channel

smeltSC = filter(smelt2, Stratum == "Sac Deep Water Shipping Channel") %>%
  mutate(Month = month(Date), Year = year(Date))

ggplot(smeltSC, aes(x = Date, y = Count)) + geom_point()
ggplot(smeltSC, aes(x = Secchi, y = Count)) + geom_point()
ggplot(smeltSC, aes(x = Sal_surf, y = Count)) + geom_point()
ggplot(smeltSC, aes(x = Temp_surf, y = Count, color = as.factor(Month))) + geom_point()
ggplot(smeltSC, aes(x = as.factor(Month), y = Count))+
  geom_boxplot()+
  facet_wrap(~Year, scales = "free_y")

#Monthly mean smelt CPUE

smeltSC2 = mutate(smeltSC, Month = month(Date), Year = year(Date), myear = Year + (Month-1)/12) %>%
  group_by(Station, Date, Month, Year, myear, Survey, SampleID, Sal_surf, Sal_bot, Secchi, Tow_volume, Source) %>%
  summarize(Count = sum(Count), CPUE = Count/Tow_volume) %>%
  ungroup() %>%
  distinct()

smeltSCsum = smeltSC2 %>%
  group_by(Month, Year, myear) %>%
  summarize(CPUE = mean(CPUE, na.rm =T))

ggplot(smeltSCsum, aes(x = myear, y = CPUE)) + geom_point()

#compare all the regions


#Monthly mean smelt CPUE

smelt3 = mutate(smelt2, Month = month(Date), Year = year(Date), myear = Year + (Month-1)/12) %>%
  group_by(Station, Date, Month, Year, Stratum, myear, Survey, SampleID, Sal_surf, Sal_bot, Secchi, Tow_volume, Source) %>%
  summarize(Count = sum(Count), CPUE = Count/Tow_volume) %>%
  ungroup() %>%
  distinct()

smelt3s = smelt3 %>%
  group_by(Month, Year, myear, Stratum) %>%
  summarize(CPUE = mean(CPUE, na.rm =T)) %>%
  filter(!is.na(Stratum))

#reorganize strata
smelt3s = mutate(smelt3s, Stratum = factor(Stratum, levels = c("Western Delta", "Suisun Bay", "Suisun Marsh", "Lower Sacramento",
                                                               "Lower San Joaquin", "Southern Delta", "Eastern Delta", 
                                                               "Cache Slough/Liberty Island", "Upper Sacramento", "Sac Deep Water Shipping Channel")))

ggplot(smelt3s, aes(x = myear, y = CPUE)) + geom_point() + 
  facet_wrap(~Stratum) + scale_y_log10()

ggplot(smelt3s, aes(x = Stratum, y = CPUE, fill = Stratum)) + geom_boxplot() + 
  facet_wrap(~Year)# + scale_y_log10()

ggplot(smelt3s, aes(x = Stratum, y = CPUE, fill = Stratum)) + geom_boxplot() + scale_y_log10()

library(ggridges)
ggplot(smelt3) + geom_ridgeline(aes(x =myear, y = Stratum, fill = Stratum, height = CPUE))+
  theme_bw()+
  theme(legend.position = "none")


ggplot(filter(smelt3s, Year >2014), aes(x = Stratum, y = CPUE, fill = Stratum)) + 
  geom_boxplot() + scale_y_log10()+
  theme(axis.text.x = element_text(angle = 45, vjust =1, hjust =1),
        legend.position = "none")

#maybe i need proportion of catch in the shipchannel

ggplot(smelt3s, aes(x = myear, y= CPUE, fill = Stratum))+ geom_col(position = "fill")+
  scale_fill_brewer(palette = "Set3")

#now average by year
yrssmelt = group_by(smelt3s, Year, Stratum) %>%
  summarize(CPUE = sum(CPUE, na.rm =T))

ggplot(smelt3s, aes(x = Year, y= CPUE, fill = Stratum))+ geom_col(position = "fill")+
  scale_fill_brewer(palette = "Set3")

ggplot(smelt3s, aes(x = Year, y= CPUE, fill = Stratum))+ geom_col()+
  scale_fill_brewer(palette = "Set3")

#percentage of smelt in the ship channel by water year

wysmelt = mutate(smelt3s, WY = case_when(Month %in% c(10,11,12) ~ Year +1,
                                         TRUE ~ Year)) %>%
  group_by(WY, Stratum) %>%
  summarize(CPUE = sum(CPUE, na.rm =T))

totsmelt = group_by(wysmelt, WY) %>%
  summarize(totsmelt = sum(CPUE, na.rm =T))

#attach totals to origional dataset
wysmelt = left_join(wysmelt, totsmelt) %>%
  mutate(percent = CPUE/totsmelt)

#load water year types
library(readr)
yearassignments <- read_csv("C:/Users/rhartman/OneDrive - California Department of Water Resources/Drought/DroughtSynthesis/data/yearassignments.csv")

yearassignments = rename(yearassignments, WY = Year)
wysmelt = left_join(wysmelt, yearassignments) %>%
  mutate(Yr_type = factor(Yr_type, c("Critical", "Dry", "Below Normal", "Above Normal", "Wet")))

ggplot(wysmelt, aes(x = WY, y = percent, fill = Stratum))+ geom_col()

ggplot(filter(wysmelt, Stratum == "Sac Deep Water Shipping Channel"), aes(x = Yr_type, y = percent)) + geom_boxplot()+
  ylab("percent of smelt catch in DWSC")

ggplot(filter(wysmelt, Stratum == "Sac Deep Water Shipping Channel"), aes(x = Index, y = percent)) + geom_point()+
  geom_smooth(method = "lm")+
  ylab("percent of smelt catch in DWSC")

ggplot(filter(wysmelt, Stratum == "Sac Deep Water Shipping Channel"), aes(x = WY, y = percent)) + geom_col()+
  ylab("percent of smelt catch in DWSC")


ggplot(filter(wysmelt, Stratum == "Sac Deep Water Shipping Channel"), aes(x = WY, y = CPUE)) + geom_col()+
  ylab("CPUE of smelt in DWSC")

ggplot(filter(wysmelt, Stratum == "Sac Deep Water Shipping Channel"), aes(x = Index, y = CPUE)) + geom_point()+
  geom_smooth(method = "lm")+
  ylab("CPUE of smelt catch in DWSC")

ggplot(filter(wysmelt, Stratum == "Sac Deep Water Shipping Channel"), aes(x = Yr_type, y = CPUE)) + geom_boxplot()+
  ylab("CPUE of smelt catch in DWSC")

#plot of smelt catch by location
pal = c(brewer.pal(11, "BrBG"), brewer.pal(9, "OrRd"), "black")
ggplot(filter(smeltSC, Count !=0), aes(x = Longitude, y = Latitude, color = as.factor(month(Date)), size = Count))+
  geom_point()+
  scale_size_binned(breaks = c(1,3,5,20,50), range = c(2,10))+
  scale_color_viridis_d(option = "turbo")+
  theme_bw()

pal = c(brewer.pal(11, "BrBG"), brewer.pal(9, "OrRd"), "black")
ggplot(filter(smeltSC, Count !=0), aes(x = Longitude, y = Count, color = as.factor(month(Date)), size = Count))+
  geom_point()+
  scale_size_binned(breaks = c(1,3,5,20,50), range = c(2,10))+
  scale_color_viridis_d(option = "turbo")+
  theme_bw()

#Look at difference in smelt distribution by sesaon.