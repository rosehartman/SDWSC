#Let's do a clean version of organizing the zooplankton data

library(tidyverse)
library(lubridate)
library(zooper)
library(readxl)
library(sf)
library(deltamapr)
library(RColorBrewer)

#this is all the mezozooplaknton data
load("data/shipchannelzoops.RData")
names(shipchannelall)
unique(shipchannelall$Source)
unique(shipchannelall$SizeClass)

#this loads which taxa are in which IBMR group and waht their carbon weight conversion is
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

#attach IBMR groups and calculate BPUE
zoops = left_join(zoopssfx, crosswalk, relationship = "many-to-one") %>%
  mutate(BPUE = CPUE * CarbonWeight_ug, DOY = yday(Date))

ggplot(zoops, aes(x = Latitude, y = CPUE)) +geom_point(aes(color = TowType))+
  facet_wrap(~IBMR, scales = "free")

ggplot(zoops, aes(x = Latitude, y = BPUE)) +geom_point(aes(color = TowType))+
  facet_wrap(~IBMR, scales = "free")

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

#add in zeros and calculate average
zoopIwzeros = zoopI %>%
  pivot_wider(id_cols = c(SampleID, TowType, Date, Latitude, Longitude, Source, Station, DOY, Month, Year, MonthYear, Region),
              names_from = IBMR, values_from = BPUE, values_fill = 0) %>%
  pivot_longer(cols = c(allcopnaup:last_col()), names_to = "IBMR", values_to = "BPUE")

zoopIave = group_by(zoopIwzeros, Region, TowType, Month, Year, MonthYear, IBMR) %>%
  summarize(BPUE = mean(BPUE))

#Average BPUE by month, year, and region
ggplot(zoopIave, aes(x = Month, y = BPUE, fill = IBMR))+
  facet_grid(Region~Year)+ geom_col()


#do it again with sino

zoopI2wzeros2 = zoopI2 %>%
  pivot_wider(id_cols = c(SampleID, TowType, Date, Latitude, Longitude, Source, Station, DOY, Month, Year, MonthYear, Region),
              names_from = IBMR2, values_from = BPUE, values_fill = 0) %>%
  pivot_longer(cols = c(allcopnaup:last_col()), names_to = "IBMR", values_to = "BPUE")

zoopI2wzeros2.1 = zoopI2 %>%
  pivot_wider(id_cols = c(SampleID, TowType, Date, Latitude, Longitude, Source, Station, DOY, Month, Year, MonthYear, Region),
              names_from = IBMR2, values_from = CPUE, values_fill = 0) %>%
  pivot_longer(cols = c(allcopnaup:last_col()), names_to = "IBMR", values_to = "CPUE")

zoopI2zerosall = left_join(zoopI2wzeros2, zoopI2wzeros2.1)

zoopIave2 = left_join(zoopI2wzeros2, zoopI2wzeros2.1) %>%
  group_by(Region, TowType, Month, Year, MonthYear, IBMR) %>%
  summarize(BPUE = mean(BPUE), CPUE = mean(CPUE))


#Average BPUE by tow type, and region
ggplot(zoopIave2, aes(x =TowType, y = BPUE, fill = IBMR))+
  facet_grid(Region~Month)+ geom_col()+ theme(axis.text.x = element_text(angle = 90))

ggplot(zoopIwzeros, aes(x =Region, y = BPUE, fill = IBMR))+
  facet_grid(Source~Month)+ geom_col()+ theme(axis.text.x = element_text(angle = 90))

ggplot(zoopIwzeros, aes(x =Region, y = BPUE, fill = IBMR))+
  facet_grid(Source~Month)+ geom_point()+ theme(axis.text.x = element_text(angle = 90))


#OK, now mysids
#####################################################################################




#FMWT mysids
fmwtmys = Zoopsynther(Data_type = "Community", Sources = "FMWT", Size_class = "Macro", Years = c(2010:2020)) 

#load("data/shipchannelzoops.RData")
#filter to stations in the ship channel
fmwtsats = filter(shipchannelall, Source == "FMWT") %>%
  select(Station) %>%
  distinct

FMWTmys = filter(fmwtmys, Station %in% fmwtsats$Station) %>%
  mutate(Taxname = str_remove(Taxname, "_UnID")) %>%
  mutate(Taxname2 = case_when(Taxname %in% c("Gammarus daiberi", "Unidentified Gammarus", 
                                             "Crangonyx", "Grandidierella japonica", 
                                             "Amithoe", "Hyalella")~ "Gammaridae",
                              Taxname == "Unidentified Amphipod" ~ "Amphipoda",
                              Taxname %in% c("Corophium", "Monocorophium") ~ "Corophiidae",
                              Taxname %in% c("Deltamysis holmquistae", "Acanthomysis hwanhaiensis", 
                                             "Mysida", "Alienacanthomysis macropsis") ~ "Neomysis mercedis",
                              TRUE ~ Taxname)) %>%
  group_by(Taxname2, Station, Date) %>%
  summarise(CPUE = sum(CPUE))

#Now add lengths
mysidlenghts = read_excel("data/MysidAmphipodLengths.xlsx") 
myslookup =  read_excel("data/MysidAmphipodSpeciesLookUp.xlsx")

#@@@ Come back to this

lw_conversions = read_xlsx("Data/Mesomicromacro Biomass conversions.xlsx", sheet = "Macro-zooplankton") %>% 
  #filter(Preservative != "Formalin"& Weight_type != "Dry") %>%  #filtering so its just wet and not formalin---- used this when we were using my conversions and not Orsi's. Now just using mine for amphipods
  filter(MacroCategory == "amphipod" & Preservative == "Ethanol" & Weight_type == "Wet" | Preservative == "Formalin" & Weight_type == "Dry") %>%  #filtering so have orsi for the mysids, which is in dry weight and the others in wet
  rename("PreyConversion" = Taxname) %>% 
  select(PreyConversion, MacroCategory, Preservative, Weight_type, a_grams, a_mg, b)

conversions = read_excel("data/ZoopSynth_biomass_CEBupdated.xlsx", sheet = "Macro-zooplankton")

#####################################
testlengths = data.frame(Length = c(.1, .2, .5, 1,2,3,4,5,6,7))

con = merge(conversions, testlengths)

con = mutate(con, BPUE = (a2*(Length^b))) %>%
  #filter(Preservative == "Formalin", Weight_type == "Dry") %>%
  mutate(BPUEdryC = case_when(Weight_type == "Wet" ~ BPUE* .25*.4,
                              Weight_type == "Dry"~ BPUE*.4,
                              TRUE ~ BPUE),
         #multiply by 1000 to convert 
         bpue = BPUEdryC*1000,
         Source = str_sub(Reference, 1,8)) 

ggplot(con, aes(x = Length, y = bpue, color = Taxname, linetype = Source)) + geom_point()+geom_line()+
  facet_wrap(Preservative~Weight_type)
ggplot(con, aes(x = Length, y = BPUE, color = Taxname, linetype = Preservative)) + geom_point()+geom_line()+
  facet_wrap(~Weight_type)
  
ggplot(con, aes(x = Length, y = bpue, linetype = Weight_type, color = Source)) + geom_point()+geom_line()+
  facet_wrap(Preservative~Taxname)

#############################################################
#add species names, summarize by sample and size, calculate frequency of occurance

mysids = left_join(mysidlenghts, myslookup) %>%
  filter(Station %in% fmwtsats$Station)  


Mysidsb = mysids %>%
  mutate(Taxname = case_when(SpeciesName %in% c("Gammarus daiberi", "Unidentified Gammarus", "Crangonyx sp", 
                                                "Grandidierella japonica", "Amithoe sp", "Hyalella sp")~ "Gammarus",
                             SpeciesName == "Unidentified Amphipod" ~ "Amphipoda",
                             SpeciesName %in% c("Unidentified Corophium", "Monocorophium sp") ~ "Corophiidae",
                             SpeciesName %in% c("Deltamysis holmquistae", "Acanthomysis hwanhaiensis", 
                                                "Unidentified Mysid", "Alienacanthomysis macropsis") ~ "Neomysis mercedis",
                             TRUE ~ SpeciesName)) %>%
  group_by(Taxname, Station, SampleDate, TowRep, Size) %>%
  summarize(N = n()) %>%
  rename(Date = SampleDate) %>%
  mutate(unique = paste(Station, Date, Taxname))


mysidstots = group_by(Mysidsb, Station, Date, Taxname) %>%
  summarise(all = sum(N))

mysidstot = FMWTmys %>%
  mutate(unique = paste(Station, Date,Taxname2)) %>%
  ungroup() %>%
  select(unique, CPUE) %>%
  left_join(Mysidsb, by = "unique") %>%
  left_join(mysidstots) %>%
  mutate(FO = N/all, CPUE2 = FO*CPUE)



mysids2 = mysidstot %>%
  left_join(lw_conversions, by = c("Taxname" = "PreyConversion"))



#assume dry weight is 25% wet weight
#assume carbon weight is 0.4 times dry weight
#then covert to micro-grams to match the mesozoo[s]

FMWTvols = filter(fmwtmys, Station %in% fmwtsats$Station) %>%
  select(Station, Date, Volume) %>%
  mutate(Date = as.Date(Date)) %>%
  distinct()


Mysidsbc = mysids2 %>%
  left_join(FMWTvols) %>%
  mutate(BPUE = (a_grams*(Size^b))*CPUE2) %>%
  #filter(Preservative == "Formalin", Weight_type == "Dry") %>%
  mutate(BPUEdryC = case_when(Weight_type == "Wet" ~ BPUE* .25*.4,
                              Weight_type == "Dry"~ BPUE*.4,
                              TRUE ~ BPUE),
         #multiply by 1000000 to convert from g to ug
         bpue = BPUEdryC*1000000) %>%
  select(-BPUE, -BPUEdryC)

Mysidsbc2 = mutate(Mysidsbc, Month = month(Date),
                   include = case_when(Month ==6 & Size <=4 ~ "Yes",
                                       Month ==7 & Size <=4.5 ~ "Yes",
                                       Month ==8 & Size <=5 ~ "Yes",
                                       Month ==9 & Size <=5.5 ~ "Yes",
                                       Month ==10 & Size <=6 ~ "Yes",
                                       Month ==11 & Size <=6 ~ "Yes",
                                       Month ==12 & Size <=6.5 ~ "Yes",
                                       TRUE ~ "No")) %>%
  filter(include == "Yes") %>%
  mutate(MacroCode = case_when(Taxname %in% c("Neomysis mercedis", "Hyperacanthomysis longirostris") ~ "mysid",
                               Taxname %in% c( "Americorophium spinicorne" , "Americorophium stimpsoni" ,
                                               "Corophiidae",  "Gammarus", "Amphipoda" )~ "amphipod" ))


Mysidstot = group_by(Mysidsbc2, Month, Date, Station,MacroCode) %>%
  summarize(CPUE = sum(CPUE2), bpue = sum(bpue, na.rm =T)) %>%
  mutate(Year = year(Date))

ggplot(Mysidstot, aes(x = CPUE, y = bpue, color = MacroCode))+ geom_point()

mysidstot0 = pivot_wider(Mysidstot, names_from = MacroCode, values_from = bpue, values_fill = 0) %>%
  pivot_longer(cols = c(mysid, amphipod), names_to = "IBMR", values_to = "bpue")

ggplot(Mysidstot, aes(x = as.factor(Month), y = log(bpue), fill = MacroCode)) + geom_boxplot() +
  facet_wrap(~Year)

#add regions
library(zooper)
mysids = Zoopsynther(Data_type = "Community", Sources = c("FMWT", "DOP"), Size_class = "Macro",
                     Years = c(2011:2022)) %>%
  filter(!is.na(Latitude))


mysidssfx = st_as_sf(mysids, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = st_crs(scregions)) %>%
  st_join(scregions) %>%
  st_drop_geometry() %>%
  filter(!is.na(SubRegion)) %>%
  mutate( Region = case_when(SubRegion == "Upper Sacramento River Ship Channel" ~ "Top",
                             SubRegion == "Lower Sacramento River Ship Channel" & Latitude > 38.36  ~ "Middle",
                             Latitude <= 38.36 ~ "Lower"))


FMWTstas = mysidssfx %>%
  filter(Source == "FMWT") %>%
  select(Source, Station, Latitude, Longitude, Region, SubRegion) %>%
  distinct()

Mysidstots = left_join(mysidstot0, FMWTstas)  %>%
  filter(!is.na(SubRegion))

###################################
#dop mysids

DOPmysenv = read_excel("data/MysidDataSSC_2018-2022.xlsx", sheet = "Water Quality", na = "NA") %>%
  select(Date, Start_Time, Station_Code, Latitude, Longitude, Macrozooplankton_Volume, unique_id)
DOPmys = read_excel("data/MysidDataSSC_2018-2022.xlsx", sheet = "Mysid", guess_max = 10000)


lookup = read_csv("data/DOPspecies.csv")
DOPmysx = left_join(DOPmys, lookup) %>%
  left_join(DOPmysenv) %>%
  ungroup() %>%
  mutate(Station = str_replace_all(site_id, "-", ""))


#Just use zooper CPUE since I seem to be getting it way wrong?
DOP2 = Zoopsynther(Data_type = "Community", Sources = c("DOP"), Size_class = "Macro") %>%
  filter(Year %in% c(2015:2022)) %>%
  select(SampleID, Station, Date, Datetime, Latitude, Longitude, Taxname, Taxlifestage, CPUE) %>%
  mutate(Station = str_replace_all(Station, "-", ""))

#OK there are some cases where ther eare two enteries per species, but I'm not going to worry aobut that 
DOPmys3 = left_join(DOPmysx, DOP2, by = c("Station",  "Taxname", "Latitude", "Longitude"))

DOPmyslong = pivot_longer(DOPmys3, cols = c(m_1:m_100), names_to = "number", values_to = "Length") %>%
  filter(!is.na(Length))  %>%
  
  mutate(Taxname = case_when(Species %in% c("Gammarus_daiberi","Crangonyx_spp",
                                            "Grandidierella_japonica",
                                            "gammarus_daiberi", "Hyalella_spp", "Incisocalliope_derzhavini")~ "Gammaridae",
                             Species == "Amphipod_UNID"  ~ "Amphipoda",
                             Species %in% c("Neomysis_mercedes", "Neomysis_kadiakensis") ~ "Neomysis mercedis",
                             Species %in% c("Corophiidae_UNID" , "Monocorophium_acherusicum","Americorophium_spp",
                                            "Sinocorophium_alienense","Corophium_alienense") ~ "Corophiidae",
                             Species %in% c("Deltamysis holmquistae", "Acanthomysis hwanhaiensis", 
                                            "Unidentified Mysid", "Alienacanthomysis macropsis", "Mysid_UNID",
                                            "Orientomysis_hwanhaiensis", "Orientomysis_aspera",
                                            "Acanthomysis_aspera") ~ "Neomysis mercedis",
                             TRUE ~ str_replace(Species, "_", " ")))

DOPmys2 = group_by(DOPmyslong, Taxname, Species, Length, tow_date, CPUE, tow_time, unique_id, site_id, habitat_, Macrozooplankton_Volume,
                   Latitude, Longitude, total_count) %>%
  summarize(count = n()) %>%
  mutate(CPUEx = count/Macrozooplankton_Volume, tot_cpue = total_count/Macrozooplankton_Volume) 


#group by length, relative lenght abundance

DOPtots = group_by(DOPmys, Species, tow_date, tow_time, site_id, unique_id, habitat_, total_count) %>%
  summarise(all = sum(count)) %>%
  left_join(DOPmysenv) 

#filter to just sizes smelt will eat
mysidstotd = DOPmys2 %>%
  ungroup() %>%
  left_join(DOPtots) %>%
  mutate(FO = count/total_count, CPUE2 = FO*CPUE)%>%
  left_join(lw_conversions, by = c("Taxname"= "PreyConversion")) %>%
  mutate(BPUE = (a_grams*(Length^b))*CPUE2) %>%
  #filter(Preservative == "Formalin", Weight_type == "Dry") %>%
  mutate(BPUEdryC = case_when(Weight_type == "Wet" ~ BPUE* .25*.4,
                              Weight_type == "Dry"~ BPUE*.4,
                              TRUE ~ BPUE),
         #multiply by 1000000 to convert from g to ug
         bpue = BPUEdryC*1000000) %>%
  select(-BPUE, -BPUEdryC) %>%
  mutate(Month = month(tow_date),
         include = case_when(Month ==6 & Length <=4 ~ "Yes",
                             Month ==7 & Length <=4.5 ~ "Yes",
                             Month ==8 & Length <=5 ~ "Yes",
                             Month ==9 & Length <=5.5 ~ "Yes",
                             Month ==10 & Length <=6 ~ "Yes",
                             Month ==11 & Length <=6 ~ "Yes",
                             Month ==12 &Length <=6.5 ~ "Yes",
                             TRUE ~ "No")) %>%
  filter(include == "Yes") 

#now add up by taxname, i guess
DOPmystot = mysidstotd%>%
  mutate(MacroCode = case_when(Taxname %in% c("Neomysis mercedis", "Hyperacanthomysis longirostris") ~ "mysid",
                               Taxname %in% c( "Americorophium spinicorne" , "Americorophium stimpsoni" ,
                                               "Corophiidae",  "Gammaridae", "Amphipoda" )~ "amphipod" ))



#add zeros and regions
DOPmysall = left_join(DOPmysenv, DOPmystot) %>%
  pivot_wider(id_cols = c(unique_id, Latitude, Longitude, tow_date, Date, Month),
              names_from = MacroCode, values_from = bpue, values_fn = sum, values_fill = 0) %>%
  pivot_longer(
    cols = c(amphipod, mysid), names_to = "IBMR", values_to = "bpue") %>%
  select(c(Date, Latitude, Longitude, unique_id, Month, IBMR, bpue)) %>%
  filter(!is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = st_crs(scregions)) %>%
  st_join(scregions) %>%
  st_drop_geometry() %>%
  filter(!is.na(SubRegion)) %>%
  mutate( Region = case_when(SubRegion == "Upper Sacramento River Ship Channel" ~ "Top",
                             SubRegion == "Lower Sacramento River Ship Channel" & Latitude > 38.36  ~ "Middle",
                             Latitude <= 38.36 ~ "Lower")) %>%
  mutate(Year = year(Date))

DOPmysallc = left_join(DOPmysenv, DOPmystot) %>%
  pivot_wider(id_cols = c(unique_id, Latitude, Longitude, tow_date, Date, Month), 
              names_from = MacroCode, values_from = CPUE2, values_fn = sum, values_fill = 0) %>%
  pivot_longer(
    cols = c(amphipod, mysid), names_to = "IBMR", values_to = "CPUE") 

DOPmysalld = left_join(DOPmysall, DOPmysallc) %>%
  mutate(Year = year(Date), Month = month(Date), Source = "DOP")
#add all mysids together and group by month and summarize

mysidsall = bind_rows(mutate(Mysidstots, Source = "FMWT"), DOPmysalld)

mysidsmonth = bind_rows(Mysidstots, DOPmysalld) %>%
  group_by( Month, Year, Region, IBMR) %>%
  summarize(bpue = mean(bpue, na.rm =T), cpue = mean(CPUE, na.rm =T)) %>%
  mutate(Biomass_C = bpue/cpue)

save(mysidsmonth, file = "data/mysidsmonth.Rdata")




####################################################################################

load("data/mysidsmonth.Rdata")
load("data/dietmonth.RData")


##############################################################################
#if we're lining things up with diet studies, get rid of tow type

zoopIave3 = group_by(zoopI2zerosall, Region,  Month, Year, MonthYear, IBMR) %>%
  summarize(BPUE = mean(BPUE, na.rm = T), CPUE = mean(CPUE, na.rm = T))
#oh, they don't seperate sinocal juv, i shouldn't do that with the diets either.


totIBMR = rename(mysidsmonth, BPUE = bpue, CPUE = cpue) %>%
  bind_rows( zoopIave3) %>%
  dplyr::select(-MonthYear)

table(totIBMR$IBMR, totIBMR$Year)

table(totIBMR$IBMR, totIBMR$Month)


ggplot(totIBMR, aes(x = CPUE, y = BPUE, color = IBMR))+geom_point()+
  geom_point(data = filter(totIBMR, IBMR == "mysid"), size =5)+
  coord_cartesian(ylim = c(0,1000), xlim = c(0,1000))+
  facet_wrap(~IBMR, scales = "free_y")


ggplot(totIBMR, aes(x = Month, y = BPUE, color = IBMR))+geom_point()+
  geom_point(data = filter(totIBMR, IBMR == "mysid"), size =5)+
  facet_wrap(~IBMR, scales = "free_y")


ggplot(totIBMR, aes(x = Month, y = CPUE, color = IBMR))+geom_point()+
  geom_point(data = filter(totIBMR, IBMR == "mysid"), size =5)+
  facet_wrap(~IBMR, scales = "free_y")


#now bind diet and zoops together
allbugs = bind_rows(mutate(rename(dietmonth, BPUE = bpue), Metric = "Diet"),
                    mutate(totIBMR, Metric = "Zoops")) %>%
  mutate(IBMR = case_when(IBMR == "mysids" ~ "mysid",
                          TRUE ~ IBMR))


mypal = c(brewer.pal(8, "Set2"), brewer.pal(8, "Dark2"), "pink")

ggplot(filter(allbugs, Month %in% c(6:10)), aes(x = Metric, y = BPUE, fill = IBMR))+ geom_col(position = "fill")+
  facet_grid(Region~Year)+
 scale_fill_manual(values = mypal, name = NULL)



#zooplankton data in wide format for will

zoops_ibmr_wide = pivot_wider(totIBMR, id_cols = c(Month, Year, Region),
                              values_from = BPUE, names_from = IBMR)

write.csv(zoops_ibmr_wide, "outputs/IBMRzoopswide_June12.csv", row.names = FALSE)

zoops_ibmr_catch = pivot_wider(totIBMR, id_cols = c(Month, Year, Region),
                               values_from = BPUE, names_from = IBMR)


write.csv(zoops_ibmr_catch, "outputs/IBMRzoopsCPUE_June12.csv", row.names = FALSE)



ggplot(IBMR2, aes(x = Month, y = Biomass, color = IBMR)) + geom_point()


zoops_ibmr_conversion = group_by(zoopI2, Month, IBMR2) %>%
  summarize(totcatch = sum(CPUE), totmass = sum(BPUE)) %>%
  mutate(Biomass_C = totmass/totcatch)
mys_ibmr_convsersions = group_by(Mysidstots, Month, Year,  IBMR) %>%
  summarize(bpue = sum(bpue), cpue = sum(CPUE)) %>%
  mutate(Biomass_C = bpue/cpue)

IBM_biomass = bind_rows(zoops_ibmr_conversion, mys_ibmr_convsersions)

write.csv(IBM_biomass, "outputs/IBMR_ave_biomass.csv")

#####################################

diettest = read_csv("data/shipchannel_dietbiomass_new.csv")

dietlong = pivot_longer(diettest, cols = c(acartela:mysids), names_to = "IBMR", values_to = "Biomass")

ggplot(dietlong, aes(x = SampleID, y = Biomass, fill = IBMR))+ geom_col()+ 
  theme(axis.text.x = element_blank())

ggplot(dietlong, aes(x = as.factor(month(Date)), y = Biomass, fill = IBMR))+ geom_col(position = "fill")+ 
  theme(axis.text.x = element_blank()) +
  facet_wrap(~Region) +
  scale_fill_manual(values = mypal)


#now by number, just to compare

dietnum = read_csv("data/shipchannel_dietbynumb.csv")



bm_conversions = read_xlsx("Data/Diet and IBMR Conversions.xlsx") %>% 
  mutate("DietCategory" = PostCategory) %>% 
  select(-PostCategory)

#need to convert the diet by number file to long format so can multiple the carbon conversions to the numbers
#use the meso file first, then will calc the macro from the lengths to add to this

sdwscdiet_long = pivot_longer(dietnum, 
                              cols = `Acanthocyclops spp`:`Nippoleucon hinumensis`, 
                              names_to = "DietCategory", 
                              values_to = "DietbyNumber" )

sdwscdietbm = left_join(sdwscdiet_long, bm_conversions, by = "DietCategory" ) %>% 
 group_by(Year, Month, latitude, longitude, LogNumber, SampleID, Station, Region, IBMR) %>%
  summarize(Count = sum(DietbyNumber)) %>%
  mutate(IBMR = case_when(IBMR == "mysid" ~ "mysids",
                          TRUE ~ IBMR))

foo = left_join(sdwscdietbm, dietlong)

ggplot(filter(foo, Count !=0), aes(x = log(Count), y = log(Biomass), color = IBMR)) + geom_point()+
  scale_color_manual(values = mypal)


ggplot(filter(foo, Count !=0), aes(x = Count, y =Biomass, color = IBMR)) + geom_point()+
  scale_color_manual(values = mypal)+ coord_cartesian(ylim = c(0,100), xlim = c(0,20))+
  geom_point(data = foom, aes(x = Count, y =Biomass, color = IBMR), shape = 10, size =5)

ggplot(filter(foo, Count !=0, IBMR %in% c("amphipod", "mysids")), aes(x = Count, y =Biomass, color = IBMR)) + 
  geom_point()
  scale_color_manual(values = mypal)+ coord_cartesian(ylim = c(0,100), xlim = c(0,20))

foom = filter(foo, IBMR == "mysids")
