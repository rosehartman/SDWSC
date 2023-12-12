# #Matt thinks it's a combination, of temperature, turbidity, and food
#I think I want to do 2005-2021. Maybe look by EDSM subregion and see where things plot out?

library(discretewq)
library(zooper)
library(tidyverse)
library(lubridate)
library(deltamapr)
library(sf)
library(vegan)

#Maybe this set of regions?
ggplot(R_EDSM_Strata_1718P1)+
  geom_sf(aes(fill=Stratum))+
  theme_bw()

#Or this set of regions?
ggplot(R_EDSM_Subregions_1718P1)+
  geom_sf(aes(fill=SubRegion))+
  geom_sf_text(aes(label = SubRegion))+
  theme_bw()+
  theme(legend.position="none")

#I'll probably start with the subregions and combine them if it doesn't work out well

wq1 = wq(Sources = c("EMP", "STN", "NCRO", "FMWT", "EDSM", "DJFMP", "SDO", "SKT", "SLS", "20mm", "Suisun", "Baystudy", "USBR", "USGS_SFBS", "YBFMP", "USGS_CAWSC"),
         Start_year = 2005, End_year = 2021) %>%
  filter(!is.na(Latitude))

#Attach subregions and average by month and year.


wq1r = st_as_sf(wq1, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = st_crs(R_EDSM_Subregions_1718P1)) %>%
  st_join(R_EDSM_Subregions_1718P1) %>%
  st_drop_geometry() %>%
  mutate(Month = month(Date),
         SubRegion = case_when(SubRegion == "Holland Cut" ~ "Mildred Island",
                               SubRegion == "Middle River" ~ "Old River",
                               SubRegion == "Sacramento River near Ryde" ~ "Upper Sacramento River",
                               TRUE ~ SubRegion)) 

wq4 = group_by(wq1r, Year, Month, SubRegion) %>%
  summarize(Chl = mean(Chlorophyll, na.rm = T), Secchi = mean(Secchi, na.rm =T), 
            Turbididity = mean(TurbidityNTU, na.rm =T), Temp = mean(Temperature, na.rm = T),
            Temp_bottom = mean(Temperature_bottom, na.rm =T), Conductivity = mean(Conductivity, na.rm =T),
                               Salinity = mean(Salinity, na.rm =T))


#zooplankton
zoop1 = Zoopsynther(Data_type = "Community", Sources = c("EMP", "FMWT", "STN", "20mm", "DOP"),
                    Size_class = c("Meso", "Micro"), Years = 2005:2021)

#Calculate CPUE of copepods and cladocera
#Convert to BPUE
#add ship channel samples


zoop2 = zoop1 %>%
  filter(!Undersampled, Order %in% c("Calanoida", "Cyclopoida", "Cladocera"), Lifestage %in% c("Juvenile", "Adult")) %>%
  group_by(SampleID, Year, Date, Source, Volume, Genus, Order, Class, Lifestage, Chl, Station, Latitude, Longitude) %>%
    summarize(CPUE = sum(CPUE))

#total copepods and total cladocera (no nauplii)  
zoop3 = zoop2 %>%
  group_by(SampleID, Order, Source, Year, Date, Chl, Station, Latitude, Longitude) %>%
  summarize(CPUE = sum(CPUE)) %>%
  filter(!is.na(Latitude))

##############################################################################
#just the ones in the ship channel

zoopSC = st_as_sf(zoop3, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = st_crs(R_EDSM_Subregions_1718P1)) %>%
  st_join(R_EDSM_Subregions_1718P1) %>%
  st_drop_geometry() %>%
  filter(SubRegion %in% c("Upper Sacramento River Ship Channel", "Lower Sacramento River Ship Channel"))


ggplot(zoopSC, aes(x = Date, y = CPUE))+
  
  geom_point()+
  facet_wrap(Order~SubRegion, scales = "free_y")

#number of samples by subregion and year

sampsSC = group_by(zoopSC, SampleID, SubRegion, Date, Year, Source) %>%
  summarize(CPUE = sum(CPUE, na.rm =T), N = n()) %>%
  mutate(Month = month(Date))

ggplot(sampsSC, aes(x = Year, y = CPUE, color = Source)) +
  geom_point()+
  facet_wrap(~SubRegion)
ggplot(sampsSC, aes(x = Year, y = N, fill = Source)) +
  geom_col()+
  facet_grid(SubRegion~Month)

############################################################################
#Attach subregions and average by month and year.

zoop3r = st_as_sf(zoop3, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = st_crs(R_EDSM_Subregions_1718P1)) %>%
  st_join(R_EDSM_Subregions_1718P1) %>%
  st_drop_geometry() %>%
  mutate(Month = month(Date),
         SubRegion = case_when(SubRegion == "Holland Cut" ~ "Mildred Island",
                               SubRegion == "Middle River" ~ "Old River",
                               SubRegion == "Sacramento River near Ryde" ~ "Upper Sacramento River",
                               TRUE ~ SubRegion)) 

zoop4 = group_by(zoop3r, Year, Month, SubRegion, Order) %>%
  summarize(Chl = mean(Chl, na.rm = T), CPUE = mean(CPUE, na.rm =T)) %>%
  mutate(logCPUE = log(CPUE+1))

ggplot(zoop4, aes(x = as.factor(Month), y = logCPUE, fill = Order)) + geom_boxplot()+
  facet_wrap(~SubRegion)

#Integrate water qulaity and zoops in wide format and see about an NMDS

zoopwide = pivot_wider(zoop4,id_cols = c(Year, Month, SubRegion), 
                       names_from = Order, values_from = logCPUE, values_fill = 0) %>%
  filter(!is.na(SubRegion)) 

alldata = left_join(zoopwide, wq4) %>%
  ungroup() %>%
  mutate(SubRegion = as.factor(SubRegion))


#OK, let's look at an NMDS
#we don't ahve a lot of turbidity data, or chlorophyll data, so start without those
alldata2 = filter(alldata, !is.na(Secchi), !is.na(Temp), !is.na(Conductivity))

alldatmat = alldata2 %>%
  select(Calanoida, Cladocera, Cyclopoida, Secchi, Temp, Conductivity)

mds1 = metaMDS(alldatmat, autotransform =TRUE)
 #oof, this is taking forever, maybe seasonal averages? Or a shorter time scale?
#but it converged nicely in the end. And then I forgot to save it (DOODF)
source("plotNMDS.R")
PlotNMDS2(mds1, alldata2, group = "SubRegion", lines = "Month")
###############################################################################
#https://portal.edirepository.org/nis/mapbrowse?packageid=edi.1319.1


#Try a shorter timescale first
alldata3 = filter(alldata, !is.na(Secchi), !is.na(Temp), !is.na(Conductivity), Year >2013)

alldatmat2 = alldata3 %>%
  select(Calanoida, Cladocera, Cyclopoida, Secchi, Temp, Conductivity)

mds2 = metaMDS(alldatmat2, autotransform =TRUE)
#oof, this is taking forever, maybe seasonal averages? Or a shorter time scale?
#but it converged nicely in the end. And then I forgot to save it (DOODF)

PlotNMDS2(mds2, alldata3, group = "SubRegion", lines = "Month")
PlotNMDS(mds2, alldata3, group = "SubRegion")

#I should probably take out the saltier regions, it's going to throw me off.



#Try a shorter timescale first
alldata4 = filter(alldata, !is.na(Secchi), !is.na(Temp), !is.na(Conductivity), Year >2013,
                  !SubRegion %in% c("Upper Napa River", "Lower Napa River",
                                    "East San Pablo Bay", "Carquinez Strait"))

alldatmat3 = alldata4 %>%
  select(Calanoida, Cladocera, Cyclopoida, Secchi, Temp, Conductivity)

mds3 = metaMDS(alldatmat3, autotransform =TRUE, try = 100, trymax = 100)
#huh, didnt converge

PlotNMDS2(mds3, alldata4, group = "SubRegion", lines = "Month")
PlotNMDS(mds3, alldata4, group = "SubRegion")
foo = mds3$points
foo = bind_cols(foo, alldata4)


#what about average by month and region without year?

allmean = filter(alldata, !is.na(Secchi), !is.na(Temp), !is.na(Conductivity),
                 !SubRegion %in% c("Upper Napa River", "Lower Napa River",
                                   "East San Pablo Bay", "Carquinez Strait")) %>%
  group_by(Month, SubRegion) %>%
  summarize(Chl = mean(Chl, na.rm = T), Secchi = mean(Secchi, na.rm =T), 
            Turbididity = mean(Turbididity, na.rm =T), Temp = mean(Temp, na.rm = T),
            Temp_bottom = mean(Temp_bottom, na.rm =T), Conductivity = mean(Conductivity, na.rm =T),
            Salinity = mean(Salinity, na.rm =T), Calanoida = mean(Calanoida, na.rm =T), 
            Cladocera = mean(Cladocera, na.rm =T), Cyclopoida =mean(Cyclopoida, na.rm =T)) %>%
  ungroup()

alldatmat4 = allmean%>%
  select(Calanoida, Cladocera, Cyclopoida, Secchi, Temp, Conductivity)

mds4 = metaMDS(alldatmat4, autotransform =TRUE, try = 100, trymax = 100)

PlotNMDS2(mds4, allmean, group = "SubRegion", lines = "Month")
PlotNMDS(mds3, alldata4, group = "SubRegion")



####################################################################
#maybe try again with smaller groups
#matt suggested breaking the ship channel up a big mor ethough

zoop3r2 = st_as_sf(zoop3, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = st_crs(R_EDSM_Subregions_1718P1)) %>%
  st_join(R_EDSM_Subregions_1718P1) %>%
  st_join(R_EDSM_Strata_1718P1) %>%
  st_drop_geometry() %>%
  mutate(Month = month(Date),
         Stratum = case_when(Stratum == "Sac Deep Water Shipping Channel" ~ SubRegion,
                             TRUE ~ Stratum),
         Stratum = case_when(Stratum == "Lower Sacramento River Ship Channel" & Latitude > 38.337~  "Mid Ship Channel",
                             TRUE ~ Stratum))

zoop42 = group_by(zoop3r2, Year, Month, Stratum, Order) %>%
  summarize(Chl = mean(Chl, na.rm = T), CPUE = mean(CPUE, na.rm =T)) %>%
  mutate(logCPUE = log(CPUE+1))


wq1r2 = st_as_sf(wq1, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = st_crs(R_EDSM_Subregions_1718P1)) %>%
  st_join(R_EDSM_Subregions_1718P1) %>%
  st_join(R_EDSM_Strata_1718P1) %>%
  st_drop_geometry() %>%
  mutate(Month = month(Date),
         Stratum = case_when(Stratum == "Sac Deep Water Shipping Channel" ~ SubRegion,
                             TRUE ~ Stratum),
         Stratum = case_when(Stratum == "Lower Sacramento River Ship Channel" & Latitude > 38.337~  "Mid Ship Channel",
                             TRUE ~ Stratum))

wq42 = group_by(wq1r2, Year, Month, Stratum) %>%
  summarize(Chl = mean(Chlorophyll, na.rm = T), Secchi = mean(Secchi, na.rm =T), 
            Turbididity = mean(TurbidityNTU, na.rm =T), Temp = mean(Temperature, na.rm = T),
            Temp_bottom = mean(Temperature_bottom, na.rm =T), Conductivity = mean(Conductivity, na.rm =T),
            Salinity = mean(Salinity, na.rm =T))

#put them all together
zoopwide2 = pivot_wider(zoop42,id_cols = c(Year, Month, Stratum), 
                       names_from = Order, values_from = logCPUE, values_fill = 0) %>%
  filter(!is.na(Stratum)) 

#put the strata in order
alldataX = left_join(zoopwide2, wq42) %>%
  ungroup() %>%
  filter(Stratum != "Western Delta", !is.na(Stratum)) %>%
  mutate(Stratum = factor(Stratum, levels = c("Upper Sacramento", "Lower Sacramento River Ship Channel", "Mid Ship Channel",
                                              "Upper Sacramento River Ship Channel",
                                              "Cache Slough/Liberty Island", "Eastern Delta",
                                              "Lower Sacramento", "Lower San Joaquin", "Southern Delta",
                                              "Suisun Bay", "Suisun Marsh"),
                          labels = c("Upper Sac", "Lower Sac Ship Channel", "Mid Ship Channel", "Upper Sac Ship Channel",
                                     "Cache/Liberty", "East Delta", "Lower Sac", "Lower SJ", "South Delta",
                                     "Suisun Bay", "Suisun Marsh")))
  

alldata2X = filter(alldataX, !is.na(Secchi), !is.na(Temp), !is.na(Conductivity))

alldatmatX = alldata2X %>%
  select(Calanoida, Cladocera, Cyclopoida, Secchi, Temp, Conductivity)

#plot each parameter
ggplot(alldata2X, aes(x = Stratum, y = Calanoida, fill = Stratum)) + geom_boxplot()+
  facet_wrap(~Month)+ylab("Log-transformed Calanoid Copepod CPUE")+scale_fill_brewer(palette = "Set3")
ggplot(alldata2X, aes (x = Stratum, y = Cladocera, fill = Stratum)) + geom_boxplot()+
  facet_wrap(~Month)+ylab("Log-transformed Cladocera CPUE")+scale_fill_brewer(palette = "Set3")
ggplot(alldata2X, aes(x = Stratum, y = Cyclopoida, fill = Stratum)) + geom_boxplot()+
  facet_wrap(~Month)+scale_fill_brewer(palette = "Set3")+ylab("Log-transformed cyclopoid copepod CPUE")
ggplot(alldata2X, aes(x = Stratum, y = Temp, fill = Stratum)) + geom_boxplot()+
  facet_wrap(~Month, scales = "free_y")+scale_fill_brewer(palette = "Set3")+ylab("Surface temperature (C)")
ggplot(alldata2X, aes(x = Stratum, y = Secchi, fill = Stratum)) + geom_boxplot()+
  facet_wrap(~Month)+scale_fill_brewer(palette = "Set3")+ylab("Secchi Depth (cm)")
ggplot(alldata2X, aes(x = Stratum, y = Temp_bottom, fill = Stratum)) + geom_boxplot()+
  facet_wrap(~Month, scales = "free_y")+scale_fill_brewer(palette = "Set3")+ylab("Bottom temperature")
ggplot(alldata2X, aes(x = Stratum, y = abs(Temp-Temp_bottom), fill = Stratum)) + geom_boxplot()+
  facet_wrap(~Month, scales = "free_y")+scale_fill_brewer(palette = "Set3")+ylab("temperature difference")



mds1X = metaMDS(alldatmatX, autotransform =TRUE, try = 100, trymax = 50)
#didn't coverge But stress is pretty good. 

#############################################################################################
PlotNMDS2(mds1X, alldata2X, group = "Stratum", lines = "Month")


#what if i try one NMDS per month
mdses = as.list(NA)

for(i in 1:12) {
  alldata2i = filter(alldata2X, Month ==i)
  
  alldatmatX = alldata2i %>%
    select(Calanoida, Cladocera, Cyclopoida, Secchi, Temp, Conductivity)
  mdses[[i]] = metaMDS(alldatmatX, autotransform =TRUE, try = 100, trymax = 50)
}
save(mdses, file = "NMDSes.RData")



for(i in 1:12){
  foo = mdses[[i]]
PlotNMDS(foo, filter(alldata2X, Month == i), group = "Stratum")
title(paste("Month = ", i))
}
