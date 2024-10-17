#I've got the zooplankton data all put together now, more exploritory plots!

library(tidyverse)
library(lubridate)
library(readxl)
library(sf)
library(deltamapr)
library(RColorBrewer)

load("data/shipchannelzoops.RData")
crosswalk = read_csv("data/zoopstaxa.csv") %>%
  select(Taxlifestage, IBMR, IBMR2, CarbonWeight_ug) %>%
  distinct()

scregions = filter(R_EDSM_Subregions_19P3, SubRegion %in% c("Upper Sacramento River Ship Channel",
                                                            "Lower Sacramento River Ship Channel"))
ggplot()+
  geom_sf(data = R_EDSM_Subregions_19P3) 
zoopssfx = st_as_sf(shipchannelall, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = st_crs(scregions)) %>%
  st_join(scregions) %>%
  st_drop_geometry() %>%
  filter(!is.na(SubRegion))

zoops = left_join(zoopssfx, crosswalk, relationship = "many-to-one") %>%
  mutate(BPUE = CPUE * CarbonWeight_ug, DOY = yday(Date))

ggplot(zoops, aes(x = Latitude, y = CPUE)) +geom_point(aes(color = TowType))+
  facet_wrap(~IBMR, scales = "free")


zoopmonths = mutate(zoops, Region = case_when(SubRegion == "Upper Sacramento River Ship Channel" ~ "Top",
                                              SubRegion == "Lower Sacramento River Ship Channel" & Latitude > 38.36  ~ "Middle",
                                              Latitude <= 38.36 ~ "Lower"),
                    Month = month(Date)) %>%
  group_by(Region, Taxlifestage, Month) %>%
  summarize(CPUE = mean(CPUE, na.rm =T), BPUE = mean(BPUE, na.rm =T))

mypal = c(brewer.pal(8, "Dark2"), brewer.pal(8, "Set3"), brewer.pal(8, "Set2"), "black", "purple", "tomato")
ggplot(zoopmonths, aes(x = Month, y = CPUE, fill= Taxlifestage)) + geom_col(position = "fill")+
  facet_wrap(~Region, nrow =3)+
  scale_fill_manual(values = mypal)


ggplot(zoopmonths, aes(x = Month, y = BPUE, fill= Taxlifestage)) + geom_col(position = "fill")+
  facet_wrap(~Region, nrow =3)+
  scale_fill_manual(values = mypal)


ggplot(zoops, aes(x = Latitude, y = CPUE)) +geom_point(aes(color = TowType))+
  facet_wrap(~IBMR, scales = "free")


ggplot(zoops, aes(x = Latitude, y = BPUE)) +geom_point(aes(color = TowType))

#sum by IBMR group
zoopI = group_by(zoops, SampleID, SubRegion, IBMR, TowType, Date, Latitude, Longitude, Source, Station, DOY) %>%
  summarize(CPUE = sum(CPUE), BPUE = sum(BPUE)) %>%
  mutate(Region = case_when(SubRegion == "Upper Sacramento River Ship Channel" ~ "Top",
                            SubRegion == "Lower Sacramento River Ship Channel" & Latitude > 38.36  ~ "Middle",
                            Latitude <= 38.36 ~ "Lower"),
         Month = month(Date), 
         Year = year(Date),
         MonthYear = Year + (1-Month)/12)

#sum by IBMR group - with sino
zoopI2 = group_by(zoops, SampleID, SubRegion, IBMR2, TowType, Date, Latitude, Longitude, Source, Station, DOY) %>%
  summarize(CPUE = sum(CPUE), BPUE = sum(BPUE)) %>%
  mutate(Region = case_when(SubRegion == "Upper Sacramento River Ship Channel" ~ "Top",
                                   SubRegion == "Lower Sacramento River Ship Channel" & Latitude > 38.36  ~ "Middle",
                                   Latitude <= 38.36 ~ "Lower"),
         Month = month(Date), 
         Year = year(Date),
         MonthYear = Year + (1-Month)/12)


#i'm not sure what the appropriate bins are for regions, but let's try this for now

ggplot(zoopI, aes(x = Region, y = BPUE, fill = IBMR)) +geom_col(position = "fill")+
  facet_wrap(~TowType)


ggplot(zoopI, aes(x = Region, y = CPUE, fill = IBMR)) +geom_col(position = "fill")+
  facet_wrap(~Month)

write.csv(zoopI, "data/DWSC_zoops_wIBMR.csv")
save(zoopI, file = "data/DWSC_zoops_wIBMR.RData")


zoopsf = st_as_sf(zoopI, coords = c("Longitude", "Latitude"), crs = 4326)
ggplot()+
  geom_sf(data = WW_Delta)+
  geom_sf(data = zoopsf, aes(color = Region))+
  coord_sf(ylim = c(38.2, 38.6), xlim = c(-121.5, -121.7))

zoopIwzeros = zoopI %>%
  pivot_wider(id_cols = c(SampleID, TowType, Date, Latitude, Longitude, Source, Station, DOY, Month, Year, MonthYear, Region),
              names_from = IBMR, values_from = BPUE, values_fill = 0) %>%
  pivot_longer(cols = c(allcopnaup:last_col()), names_to = "IBMR", values_to = "BPUE")
  
zoopIave = group_by(zoopIwzeros, Region, TowType, Month, Year, MonthYear, IBMR) %>%
  summarize(BPUE = mean(BPUE))

#Average BPUE by month, year, and region
ggplot(zoopIave, aes(x = Month, y = BPUE, fill = IBMR))+
  facet_grid(Region~Year)+ geom_col()



zoopI2wzeros2 = zoopI2 %>%
  pivot_wider(id_cols = c(SampleID, TowType, Date, Latitude, Longitude, Source, Station, DOY, Month, Year, MonthYear, Region),
              names_from = IBMR2, values_from = BPUE, values_fill = 0) %>%
  pivot_longer(cols = c(allcopnaup:last_col()), names_to = "IBMR", values_to = "BPUE")

zoopIave2 = group_by(zoopI2wzeros2, Region, TowType, Month, Year, MonthYear, IBMR) %>%
  summarize(BPUE = mean(BPUE))


#Average BPUE by tow type, and region
ggplot(zoopIave, aes(x =TowType, y = BPUE, fill = IBMR))+
  facet_grid(Region~Month)+ geom_col()+ theme(axis.text.x = element_text(angle = 90))

ggplot(zoopIave, aes(x =Region, y = BPUE, fill = IBMR))+
  facet_grid(TowType~Month)+ geom_col(position = "fill")+
  scale_fill_brewer(palette = "Set3")
  
# I guess we said we'd focus on summer-fall, so let's look at average biomass by 
#region and depth

#i'm taking out the vertical and oblique tows just so I can make this figure, i need to fiture out what to do with them later.
regave = filter(zoopIave2, Month %in% c(6:11), TowType %in% c("Bottom", "Surface")) %>%
  group_by(Region, TowType, IBMR) %>%
  summarize(BPUE = mean(BPUE)) %>%
  mutate(TowType = factor(TowType, levels = c("Surface", "Bottom")))

#community composition
ggplot(regave, aes(x = Region, y = BPUE, fill = IBMR))+ geom_col(position = "fill")+
  facet_wrap(~TowType, ncol =1)

#BPUE
ggplot(regave, aes(x = Region, y = BPUE, fill = IBMR))+ geom_col()+
  facet_wrap(~TowType, ncol =1)

ggplot(regave, aes(x = TowType, y = BPUE, fill = IBMR))+ geom_col()+
  facet_wrap(~Region, ncol =1, scales = "free_y")

ggplot(regave, aes(x = IBMR, y = log(BPUE+1), fill = Region))+ geom_col(position = "dodge")+
  facet_wrap(~TowType, ncol =1)

#export just surface/bottom for brock. 
write.csv(regave, "outputs/zoopTopBottom.csv")
##############################################################################
#if we're lining things up with diet studies, get rid of tow type

zoopIave3 = group_by(zoopI2wzeros2, Region,  Month, Year, MonthYear, IBMR) %>%
  summarize(BPUE = mean(BPUE))
#oh, they don't seperate sinocal juv, i shouldn't do that with the diets either.



#################################
#combine with water quality
library(discretewq)
wq1x = wq(Sources = c("EMP", "STN", "NCRO", "FMWT", "EDSM", "DJFMP", "SKT", "SLS", "20mm","USBR", "USGS_SFBS", "YBFMP", "USGS_CAWSC"),
         Start_year = 2011, End_year = 2021) %>%
  filter(!is.na(Latitude), Latitude > min(zoops$Latitude), Latitude < max(zoops$Latitude),
         Longitude > min(zoops$Longitude),Longitude < max(zoops$Longitude))

wq2x = mutate(wq1x,  Region = case_when(Latitude > 38.5 ~ "Top",
                                      Latitude > 38.3 & Latitude <= 38.5 ~ "Middle",
                                      Latritude <= 38.3 ~ "Lower"),
             Month = month(Date), Year = year(Date))

wqavex = group_by(wq2x, Region, Month, Year) %>%
  summarize(Secchi = mean(Secchi, na.rm =T), TurbidityNTU = mean(TurbidityNTU, na.rm =T),
            Conductivity = mean(Conductivity, na.rm =T), Chlorophyll = mean(Chlorophyll, na.rm =T),
            Temperature = mean(Temperature, na.rm =T))

zoopwq = left_join(zoopIave, wqave)

ggplot(zoopwq, aes(x = TurbidityNTU, y = log(BPUE+1)))+
  geom_point(aes(color = TowType))+ geom_smooth()+facet_wrap(~IBMR, scales = "free")+
  coord_cartesian(xlim = c(0,100))

ggplot(zoopwq, aes(x = TurbidityNTU, y = log(BPUE+1)))+
  geom_point(aes(color = Region))+ geom_smooth()+facet_wrap(~IBMR, scales = "free")+
  coord_cartesian(xlim = c(0,100))

ggplot(zoopwq, aes(x = log(TurbidityNTU), y = log(BPUE+1)))+
  geom_point(aes(color = TowType))+ geom_smooth()+facet_grid(Region~IBMR, scales = "free")+
  coord_cartesian(xlim = c(0,6), ylim = c(0,12))


#################################################################
#now add mysid data


load("data/mysidsmonth.Rdata")
load("data/dietbymonth.RData")

totIBMR = rename(mysidsmonth, BPUE = bpue) %>%
  bind_rows( zoopIave3) %>%
  dplyr::select(-MonthYear) %>%
  rename(ZooplanktonBPUE = BPUE)

table(totIBMR$IBMR, totIBMR$Year)

table(totIBMR$IBMR, totIBMR$Month)




#bind diet and zooplankton data together
IBMRdietzoops = left_join(totIBMR, diet_mon) %>%
  rename(DietBiomass = mean_mon)

write.csv(IBMRdietzoops, "outputs/IBMRdietmatrix.csv", row.names = FALSE)

IBMR2 = pivot_longer(IBMRdietzoops, cols = c(ZooplanktonBPUE, DietBiomass), 
                     names_to = "Metric", values_to = "Biomass")

ggplot(filter(IBMR2, Month %in% c(6:10)), aes(x = Metric, y = Biomass, fill = IBMR))+ geom_col(position = "fill")+
  facet_grid(Region~Year)+
  scale_fill_manual(values = mypal, name = NULL)

ggplot(filter(IBMR2, Month %in% c(6:10)), aes(x = Metric, y = Biomass, fill = IBMR))+ geom_col(position = "fill")+
  facet_wrap(~Region)+
  scale_fill_manual(values = mypal, name = NULL)

ggplot(IBMR2, aes(x = Month, y = Biomass, fill = IBMR))+ geom_col(position = "fill")+
  facet_grid(Metric~Region)+
  scale_fill_manual(values = mypal)+
  theme_bw()

#zooplankton data in wide format for will

zoops_ibmr_wide = pivot_wider(IBMRdietzoops, id_cols = c(Month, Year, Region),
                              values_from = ZooplanktonBPUE, names_from = IBMR)

write.csv(zoops_ibmr_wide, "outputs/IBMRzoopswide.csv", row.names = FALSE)

zoops_ibmr_conversion = group_by(zoopI2, Month, IBMR2) %>%
  summarize(totcatch = sum(CPUE), totmass = sum(BPUE)) %>%
  mutate(Biomass_C = totmass/totcatch)
mys_ibmr_convsersions = group_by(Mysidstots, Month, Year,  IBMR) %>%
  summarize(bpue = sum(bpue), cpue = sum(CPUE)) %>%
  mutate(Biomass_C = bpue/cpue)

IBM_biomass = bind_rows(zoops_ibmr_conversion, mys_ibmr_convsersions)

write.csv(IBM_biomass, "outputs/IBMR_ave_biomass.csv")
