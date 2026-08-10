#let's look at everything we have for the Sacramento Deep water ship channel

library(tidyverse)
library(lubridate)
library(deltafish)
library(discretewq)
library(zooper)
library(deltamapr)
library(sf)
library(readxl)
library(patchwork)
library(viridis)
library(EDIutils)

EDIkey = "l8sfres2dfOtnqglJUWSTELs1Uk"
login(key = "l8sfres2dfOtnqglJUWSTELs1Uk")
#
create_fish_db()
con = open_database()

# open our two data files
surv <- open_survey(con)
fish <- open_fish(con)
#
fish_smelt <- fish %>%
  filter(Taxa == "Hypomesus transpacificus")

#ship channel sampling started in 2010, I think.


# do a join and collect the resulting data frame
# collect executes the sql query and gives you a table
df <- left_join(surv, fish_smelt) %>%
  collect() %>%
  filter(year(Date)>2000, !is.na(Latitude))

#Add some regions
smeltsf = st_as_sf(df, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = st_crs(R_EDSM_Strata_1718P1))

smelt2 = st_join(smeltsf, R_EDSM_Strata_1718P1) %>%
  st_drop_geometry()
#
# save(smelt2, smeltsf, df, file = "data/SmeltData.RData")
load("data/SmeltData.RData")
smelt2 = mutate(smelt2, Date = ymd(Date))
#This has EDSM through the end of 2022


#load all the more recent smelt catch data
#oh, shoot. this doesn't have effort info
#or all the zeros, maybe not helpful after all. But interesting
library(readxl)
smeltcatch <- read_excel("C:/Users/rhartman/OneDrive - California Department of Water Resources/smelt cages/SmeltMap/Running Delta Smelt Catch_2023-09-05.xlsx",
                         sheet = "Delta Smelt Catch Data")
smeltrecent = smeltcatch %>%
  rename(Latitude = LatitudeStart, Longitude = LongitudeStart,
         Date = SampleDate, Length = ForkLength, Station = StationCode,
         Source = Survey) %>%
  mutate(Month = month(Date), Year = year(Date), Count =1, Length = as.numeric(Length)) %>%
  select(Latitude, Longitude, Station, Date, Length, Source, Count, Length) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = F) %>%
  st_transform(crs = st_crs(R_EDSM_Strata_1718P1)) %>%
  st_join(R_EDSM_Strata_1718P1) %>%
  st_drop_geometry()



#OK, but I really want the more recent EDSM data. Sigh.
read_data_entity_names("edi.415.11")
EDSM20mm = read_data_entity("edi.415.11", "d468c513fa69c4fc6ddc02e443785f28") %>%
  read_csv()
EDSMkdk = read_data_entity("edi.415.11", "4d7de6f0a38eff744a009a92083d37ae") %>%
  read_csv()

#add in zeros

EDSM = bind_rows(EDSM20mm, EDSMkdk) %>%
  filter(SampleDate > ymd("2022-11-30")) %>%
  pivot_wider(id_cols = c(Subregion, SampleDate, Stratum, StationCode, LatitudeStart, LongitudeStart, TowNumber,
                          Volume), names_from = IEPFishCode, values_from = Count, values_fill = 0, values_fn = sum) %>%
  select(Subregion, Stratum, SampleDate, StationCode, LatitudeStart, LongitudeStart, TowNumber,
         Volume, DELSME) %>%
  mutate(CPUE = DELSME/Volume, Source = "EDSM") %>%
  rename(Count = DELSME, Latitude = LatitudeStart, Longitude = LongitudeStart, Date = SampleDate) %>%
  group_by(Subregion, Stratum, StationCode, Latitude, Longitude, Date) %>%
  summarize(Count = sum(Count), CPUE = mean(CPUE), Tow_volume = sum(Volume))

#2024 data - I;m still missing data from 2023-12-1 through 2024-4-1

EDSM2024a = read_excel("data/90EDSMDailyReport24Nov29.xlsx") %>%
  select(SubRegion, SampleDate, Stratum, StationCode, LatitudeStart, LongitudeStart, TowNumber,
         VolumeFinal, OrganismCode, SumOfCatchCount)
EDSM2024b = read_excel("data/208EDSMDailyReport24Jul5.xlsx")%>%
  select(SampleDate, Stratum, StationCode, LatitudeStart, LongitudeStart, TowNumber,
         VolumeFinal, OrganismCode, SumOfCatchCount)
EDSM2024c = read_csv("data/45edsmdailyreport25feb14.csv") %>%
  select(SubRegion, SampleDate, Stratum, StationCode, LatitudeStart, LongitudeStart, TowNumber,
         VolumeFinal, OrganismCode, SumOfCatchCount) %>%
  mutate(SampleDate = mdy(SampleDate))

EDSM2024 = bind_rows(EDSM2024a, EDSM2024b, EDSM2024c) %>%
  pivot_wider(id_cols = c(SampleDate, Stratum, StationCode, LatitudeStart, LongitudeStart, TowNumber,
                          VolumeFinal), names_from = OrganismCode, values_from = SumOfCatchCount,
              values_fill = 0, values_fn = sum) %>%
  select(Stratum, SampleDate, StationCode, LatitudeStart, LongitudeStart, TowNumber,
         VolumeFinal, DSM) %>%
  mutate(CPUE = DSM/VolumeFinal, Source = "EDSM") %>%
  rename(Count = DSM, Latitude = LatitudeStart, Longitude = LongitudeStart, Date = SampleDate) %>%
  group_by(Stratum, StationCode, Latitude, Longitude, Date) %>%
  summarize(Count = sum(Count), CPUE = mean(CPUE), Tow_volume = sum(VolumeFinal))


#let's try getting the data from a different source
#https://www.fws.gov/media/all-gears-chn-pod-species-2012-2022
EDSM2024 = read_excel("data/edsm-trawls-chn-pod-species-2016-2025.xlsx")
EDSM2024x = EDSM2024%>%
  filter(Date > ymd("2022-11-30")) %>%
    pivot_wider(id_cols = c(SubRegion, Date, Stratum, Station, StartLat, StartLong, Tow,
                            Volume), names_from = OrganismCode, values_from = CatchCount, values_fill = 0, values_fn = sum) %>%
    select(SubRegion, Stratum, Date, Station, StartLat, StartLong, Tow,
           Volume, DSM) %>%
    mutate(CPUE = DSM/Volume, Source = "EDSM") %>%
    rename(Count = DSM, Latitude = StartLat, Longitude = StartLong) %>%
    group_by(SubRegion, Stratum, Station, Latitude, Longitude, Date, Source) %>%
    summarize(Count = sum(Count), CPUE = mean(CPUE), Tow_volume = sum(Volume))
#
# chips = read_excel("data/chipps-island-trawls-chn-pod-species-2012-2025.xlsx")%>%
#   filter(Date > ymd("2023-07-28")) %>%
#   pivot_wider(id_cols = c(Date, Station, TowNumber,
#                           Volume), names_from = Species, values_from = Catch,
#               values_fill = 0, values_fn = sum) %>%
#   select(Date, Station, TowNumber,
#          Volume, DSM) %>%
#   mutate(CPUE = DSM/Volume, Source = "DJFMP", Stratum == "Lower Sacramento") %>%
#   rename(Count = DSM) %>%
#   group_by(SubRegion, Stratum, Station,Date) %>%
#   summarize(Count = sum(Count), CPUE = mean(CPUE), Tow_volume = sum(Volume))
# #Huh, I thought they got a delta smelt in 2023, but maybe I was wrong



#of course the one seine where we caught smelt doesn't have lat longs. Sigh.


smelt2.5 = bind_rows(mutate(smelt2, Date = ymd(Date)), EDSM2024x)  %>%
  mutate(Stratum = case_match(Stratum,
                              "Sac Deep Water Shipping Channel" ~ "Sac DW Ship Channel",
                              c("Cache Slough/Liberty Island", "Cache Slough/LI") ~"Cache Slough LI",
                              .default = Stratum),
         Stratum = case_when(Source == "Salvage" ~ "Salvage",
                             TRUE ~ Stratum))



#check out just the ship channel

smeltSC = filter(smelt2.5, Stratum == "Sac DW Ship Channel", Tow_volume <100000) %>%
  mutate(Month = month(Date), Year = year(Date))

save(smeltSC, smelt2.5, file = "data/smeltSC.RData")
load("data/smeltSC.RData")

ggplot(smeltSC, aes(x = round(Latitude,2), y = Count)) + geom_col()


ggplot(smeltSC, aes(x = Date, y = Count)) + geom_point()
ggplot(smeltSC, aes(x = Secchi, y = Count)) + geom_point()
ggplot(smeltSC, aes(x = Sal_surf, y = Count)) + geom_point()
ggplot(smeltSC, aes(x = Temp_surf, y = Count, color = as.factor(Month))) + geom_point()
ggplot(smeltSC, aes(x = as.factor(Month), y = Count))+
  geom_boxplot()+
  facet_wrap(~Year, scales = "free_y")

###########smelt catch heatmap#####################################################################################
#average catch by latitue category

avecatchLat = smeltSC %>%
  filter(Month %in% c(6:10), Source != "DJFMP") %>%
  mutate(LatCat = round(Latitude, 2), CPUE = Count/Tow_volume) %>%
  group_by(LatCat) %>%
  summarize(Count = mean(Count, na.rm =T), CPUE = mean(CPUE, na.rm =T), N = n())

ggplot(avecatchLat, aes(x = LatCat,y = Count)) + geom_col()+
  geom_label(aes(label = N, y =0))

ggplot(avecatchLat, aes(x = LatCat,y = CPUE*10000)) + geom_col(aes(fill = N))+
  ylab("Catch per 10000 cubic meters")+ xlab("Latitude")+ 
  scale_fill_viridis_c(option = "C", name = "Number of\nTrawls")+
  theme_bw()+
  theme(legend.position = "inside", legend.position.inside = c(.1, .8))

stations_sf = read_excel("data/stations.xlsx") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = F)

fig1 <- ggplot()+
  geom_sf(data=WW_Canals,color="slategray1")+
  geom_sf(data=WW_Watershed,color="slategray1",fill="slategray1")+
  geom_sf(data=WW_Watershed%>%
            filter(HNAME%in%c("Sacto. R Deep Water Sh Chan")),
          color="slategray3",fill="slategray3")+
  annotate("text",label="Sacramento Deep Water Ship Channel",fontface="italic",
           x=-121.58,y=38.42,color="slategray4",hjust=0.5,size=2,angle=70)+
  geom_sf(data=WW_Canals,color="slategray1")+
  geom_sf(data=stations_sf,size=2)+
  geom_sf_text(data=stations_sf,aes(label=Site),
               nudge_y=0.01,size=3)+
  coord_sf(xlim=c(-121.85,-121.5),ylim=c(38.2,38.6))+
  labs(x=NULL,y=NULL)+
  scale_x_continuous(breaks=seq(-121.8,-121.5,by=0.1))+
  theme_bw()+
  theme(axis.text=element_text(size=7))
fig1

dwsc_gradients = read.csv("data/DWSC_gradients.csv")

fig_1g <- ggplot(data=dwsc_gradients)+
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = -Inf, ymax = Inf), 
            color = NA, fill = NA, linewidth = 1)+
  geom_point(aes(x=1,y=normalized.channel.length,fill=100-concentration,color=100-concentration), 
             shape=22,size=20,alpha=0.8)+
  geom_abline(aes(intercept=Boundary,slope=0),linetype="dashed")+
  geom_abline(aes(intercept=Excursion,slope=0))+
  geom_text(data=dwsc_gradients%>%
              filter(Color==1),aes(x=1.02,y=Marker,label=Station),size=2,color="white")+
  geom_text(data=dwsc_gradients%>%
              filter(Color==2),aes(x=1.02,y=Marker,label=Station),size=2)+
  geom_point(data=dwsc_gradients%>%
               filter(Color==1),aes(x=0.92,y=Marker),color="white")+
  geom_point(data=dwsc_gradients%>%
               filter(Color==2),aes(x=0.92,y=Marker))+
  xlim(c(0.875,1.13))+
  #guides(color=FALSE,
  #       fill=guide_legend(keywidth = 0.75, keyheight = 0.75))+
  guides(color=FALSE)+
  labs(fill="Exchange",color=NULL)+
  scale_fill_viridis(option="plasma",begin=0,direction=-1)+
  #scale_color_manual(values=c("gray80","black"))+
  scale_color_viridis(option="plasma",begin=0,direction=-1)+
  theme_void()+
  theme(panel.border=element_rect(color="black",fill=NA),
        panel.background=element_rect(color=NA,fill="white"))+
  theme(legend.position="bottom",
        legend.title.position="top",
        legend.title.align = 0.5,
        legend.key.width = unit(4, "mm"),   
        legend.text=element_text(size=7),
        legend.title=element_text(size=7))
fig_1g


avecatchLat2 = smeltSC %>%
  filter(Month %in% c(6:10), Source != "DJFMP") %>%
  mutate(LatCat = round(Latitude, 1), CPUE = Count/Tow_volume) %>%
  group_by(LatCat) %>%
  summarize(Count = mean(Count, na.rm =T), CPUE = mean(CPUE, na.rm =T), N = n())

fig_1ds <- ggplot(data=filter(avecatchLat, LatCat > 38.22))+
  geom_tile(aes(x = 1, y = LatCat, fill = CPUE*10000))+
  guides(color=FALSE)+
  labs(fill="Mean Smelt CPUE",color=NULL)+
  scale_fill_viridis(option="B",begin=0,direction=-1)+
  geom_point(data = filter(stations_sf, !Site %in% c("HTE", "DIX")),
             aes(x = 0.6,y = Latitude), inherit.aes = F)+
  geom_text(data = filter(stations_sf, !Site %in% c("HTE", "DIX")),
             aes(x = 0.6,y = Latitude, label = Site), inherit.aes = F, nudge_x = .5,
            size =2)+
  #scale_color_manual(values=c("gray80","black"))+
  theme_void()+
  theme(panel.border=element_rect(color="black",fill=NA),
        panel.background=element_rect(color=NA,fill="white"))+
  theme(legend.position="bottom",
        legend.title.position="top",
        legend.title.align = 0.5,
        legend.key.width = unit(4, "mm"),   
        legend.text=element_text(size=7),
        legend.title=element_text(size=7))
fig_1ds

fig1 + inset_element(fig_1ds, 0.1, 0, .2, .4)+
  inset_element(fig_1g, 0.7, 0, 0.8, 0.4)
###########################################################################################
#Monthly mean smelt CPUE######################

smeltSC2 = mutate(smeltSC, Month = month(Date), Year = year(Date), myear = Year + (Month-1)/12) %>%
  group_by(Station, Date, Month, Year, myear, Survey, SampleID, Sal_surf, Sal_bot, Secchi, Tow_volume, Source) %>%
  summarize(Count = sum(Count)) %>%
  mutate(CPUE = Count/Tow_volume) %>%
  ungroup() %>%
  distinct()

summary(smeltSC2$Tow_volume)
#well, that's not right.


smeltSCsum = smeltSC2 %>%
  group_by(Month, Year, myear) %>%
  summarize(CPUE = mean(CPUE, na.rm =T))

ggplot(smeltSCsum, aes(x = myear, y = CPUE)) + geom_point()

#maybe we should just do summer and fall, since there is larval smelt ID issues.

smeltSC3 = smeltSC %>%
  mutate(Month = month(Date), Year = year(Date), myear = Year + (Month-1)/12) %>%
  filter(Month %in% c(6:10)) %>%
  group_by(Station, Date, Month, Year, myear, Survey, SampleID, Sal_surf, Sal_bot, Secchi, Tow_volume, Source) %>%
  summarize(Count = sum(Count)) %>%
  mutate(CPUE = Count/Tow_volume) %>%
  ungroup() %>%
  distinct()

smeltSCsum3 = smeltSC3 %>%
  group_by(Year) %>%
  summarize(CPUE = mean(CPUE, na.rm =T))


ggplot(smeltSCsum3, aes(x = Year, y = CPUE)) + geom_point()+ geom_line()


ggplot(smeltSCsum3, aes(x = Year, y = CPUE)) + geom_col()


#compare all the regions




#Monthly mean smelt CPUE

smelt3 = mutate(smelt2.5, Month = month(Date), Year = year(Date), myear = Year + (Month-1)/12) %>%
  group_by(Station, Date, Month, Year, Stratum, myear, Survey, SampleID, Sal_surf, Sal_bot, Secchi, Tow_volume, Source) %>%
  summarize(Count = sum(Count)) %>%
  mutate(CPUE = Count/Tow_volume) %>%
  ungroup() %>%
  distinct()

smelt3s = smelt3 %>%
  group_by(Month, Year, myear, Stratum) %>%
  summarize(CPUE = mean(CPUE, na.rm =T), Count = mean(Count, na.rm =T)) %>%
  filter(!is.na(Stratum))

#reorganize strata
smelt3s = mutate(smelt3s, Stratum = factor(Stratum, levels = c("Western Delta", "Suisun Bay", "Suisun Marsh", "Lower Sacramento",
                                                               "Lower San Joaquin", "Southern Delta", "Eastern Delta", 
                                                               "Cache Slough LI", "Upper Sacramento","Salvage", "Sac DW Ship Channel")))

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
  summarize(CPUE = sum(CPUE, na.rm =T), Count = sum(Count, na.rm =T))

ggplot(smelt3s, aes(x = Year, y= Count, fill = Stratum))+ geom_col(position = "fill")+
  scale_fill_brewer(palette = "Set3")

ggplot(smelt3s, aes(x = Year, y= CPUE, fill = Stratum))+ geom_col(position = "fill")+
  scale_fill_brewer(palette = "Set3")

ggplot(smelt3s, aes(x = Year, y= Count, fill = Stratum))+ geom_col()+
  scale_fill_brewer(palette = "Set3")

ggplot(smelt3s, aes(x = Year, y= CPUE, fill = Stratum))+ geom_col()+
  scale_fill_brewer(palette = "Set3")
#huh, there might be something off with the salvage CPUE. oh well.

#percentage of smelt in the ship channel by water year

wysmelt = mutate(smelt3s, WY = case_when(Month %in% c(10,11,12) ~ Year +1,
                                         TRUE ~ Year)) %>%
  group_by(WY, Stratum) %>%
  summarize(CPUE = mean(CPUE, na.rm =T))

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

ggplot(filter(wysmelt, Stratum == "Sac DW Ship Channel"), aes(x = Yr_type, y = percent)) + geom_boxplot()+
  ylab("percent of smelt catch in DWSC")

ggplot(filter(wysmelt, Stratum == "Sac DW Ship Channel"), aes(x = Index, y = percent)) + geom_point()+
  geom_smooth(method = "lm")+
  ylab("percent of smelt catch in DWSC")

ggplot(filter(wysmelt, Stratum == "Sac DW Ship Channel"), aes(x = WY, y = percent)) + geom_col()+
  ylab("percent of annual smelt catch in DWSC")+
  scale_y_continuous(labels = scales::percent)


ggplot(filter(wysmelt, Stratum == "Sac DW Ship Channel"), aes(x = WY, y = CPUE)) + geom_col()+
  ylab("CPUE of smelt in DWSC")

ggplot(filter(wysmelt, Stratum == "Sac DW Ship Channel"), aes(x = Index, y = CPUE)) + geom_point()+
  geom_smooth(method = "lm")+
  ylab("CPUE of smelt catch in DWSC")

ggplot(filter(wysmelt, Stratum == "Sac DW Ship Channel"), aes(x = Yr_type, y = CPUE)) + geom_boxplot()+
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

#percentage of smelt in the ship channel by water year - summer-fall only

sumfallsmelt = mutate(smelt3s, Month %in% c(6:10)) %>%
  group_by(Year, Stratum) %>%
  summarize(CPUE = mean(CPUE, na.rm =T))

totsmeltsf = group_by(sumfallsmelt, Year) %>%
  summarize(totsmelt = sum(CPUE, na.rm =T))

#attach totals to origional dataset
sumfsllsmelt3 = left_join(sumfallsmelt, totsmeltsf) %>%
  mutate(percent = CPUE/totsmelt)

#THIS IS THE FIGURE IN THE PAPER ##################################
space = ggplot(filter(sumfsllsmelt3 , Stratum == "Sac DW Ship Channel", Year != 2025), aes(x = Year, y = percent)) + 
  geom_col(fill = "skyblue", color = "grey23")+
  ylab("Percent of June-October \nDelta Smelt catch in SDWSC")+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  annotate("text", x = 2001, y = 0.9, label = "A", size =10)+
  theme_bw()

space

ggsave("plots/shipchannelPercent.tiff", device = "tiff", width = 6, 
       height =4)

mean(filter(sumfsllsmelt3 , Stratum == "Sac DW Ship Channel")$percent)
#####two panel - space and teme ################################################################
stations = st_drop_geometry(stations_sf) %>%
  filter(!Site %in% c("HTE", "DIX"))
timesmelt = ggplot(avecatchLat, aes(x = LatCat,y = CPUE*10000)) + geom_col(fill = "skyblue", color = "grey23")+
  ylab("Catch per 10000 cubic meters")+ xlab("Latitude")+ 
  geom_vline(data = stations, aes(xintercept = Latitude))+
  geom_text(data = stations, aes(x= Latitude, y =8, label = Site), angle =90, vjust =0, nudge_x = -0.003)+
  
  annotate("text", x = 38.18, y = 8, label = "B", size =10)+
  theme_bw()

timesmelt

space/timesmelt
ggsave("plots/Figure2_twopanel.tiff", device = "tiff", width = 8, height =7)

timesmelt = ggplot(avecatchLat, aes(x = LatCat,y = CPUE*10000)) + 
  geom_smooth()+
  ylab("Catch per 10000 cubic meters")+ xlab("Latitude")+ 
  geom_vline(data = stations, aes(xintercept = Latitude))+
  geom_text(data = stations, aes(x= Latitude, y =5.5, label = Site), angle =90, vjust =0, nudge_x = -0.003)+
  
  annotate("text", x = 38.18, y = 5.5, label = "B", size =10)+
  theme_bw()+ coord_cartesian(ylim = c(0, 6))

timesmelt

save(stations, avecatchLat, sumfsllsmelt3, file = "data/dataforFig2.RData")
##############################################################


#Look at difference in smelt distribution by sesaon.

#Especially summer-fall

#also try and standardize effort if I can.
smeltplus = filter(smelt3, Count !=0) %>%
  group_by(Source, Year) %>%
  summarize(N = n(), catch = sum(Count))

smelt3a = mutate(smelt3, Season = case_when(Month %in% c(6,7,8) ~ "Summer",
                                           Month %in% c(9,10,11) ~ "Fall",
                                           Month %in% c(12,1,2) ~ "Winter",
                                           Month %in% c(3, 4,5) ~ "Spring"),
                Season2 = case_when(Month %in% c(6:10) ~ "Summer-Fall"),
                Stratum = factor(Stratum, levels = c("Western Delta", "Suisun Bay", "Suisun Marsh", "Lower Sacramento",
                                                     "Lower San Joaquin", "Southern Delta", "Eastern Delta", 
                                                     "Cache Slough LI", "Upper Sacramento","Salvage", "Sac DW Ship Channel"))) %>%
  filter(!Source %in% c("Salvage", "Bay Study", "Suisun"))

ggplot(smelt3a, 
       aes(x = Year, y = CPUE, fill = Stratum)) +
  geom_col(position = "fill") +
  facet_wrap(~Season)+
  scale_fill_brewer(palette = "Set3")

ggplot(smelt3a,  
       aes(x = Year, y = Count, fill = Stratum)) +
  geom_col(position = "fill") +
  facet_wrap(~Season)+
  theme_bw()+
  scale_fill_brewer(palette = "Set3")

ggsave("plots/seasonalcatch.tiff", device = "tiff", width = 8, height =6)

smeltsumfall = filter(smelt3, Season2 == "Summer-Fall", !Source %in% c("SLS", "SKT", "Salvage", "Bay Study", "Suisun"))

ggplot(smeltsumfall, aes(x = Year, y = Count, fill = Stratum)) +
  geom_col(position = "fill") +
  scale_fill_brewer(palette = "Set3")

ggplot(smeltsumfall, aes(x = Year, y = Count, fill = Stratum)) +
  geom_col() +
  scale_fill_brewer(palette = "Set3")

ggplot(smeltsumfall, aes(x = Year, y = CPUE, fill = Stratum)) +
  geom_col() +
  scale_fill_brewer(palette = "Set3")
