#Mysid biomass

library(tidyverse)
library(lubridate)
library(readxl)
library(zooper)


#FMWT mysids
fmwtmys = Zoopsynther(Data_type = "Community", Sources = "FMWT", Size_class = "Macro", Years = c(2010:2020)) 

load("data/shipchannelzoops.RData")
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
conversions = read_excel("data/ZoopSynth_biomass_CEBupdated.xlsx", sheet = "Macro-zooplankton")
conversions = filter(conversions, Preservative == "Formalin") %>%
  filter(!(Taxname == "Hyperacanthomysis longirostris" & Weight_type == "Wet")) %>%
  select(Taxname, a2, b, Weight_type)

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
  left_join(conversions)



#assume dry weight is 10% wet weight
#assume carbon weight is 0.4 times dry weight
#then covert to micro-grams to match the mesozoo[s]

FMWTvols = filter(fmwtmys, Station %in% fmwtsats$Station) %>%
  select(Station, Date, Volume) %>%
  mutate(Date = as.Date(Date)) %>%
  distinct()


Mysidsbc = mysids2 %>%
  left_join(FMWTvols) %>%
  mutate(BPUE = (a2*(Size^b))*CPUE2) %>%
  #filter(Preservative == "Formalin", Weight_type == "Dry") %>%
  mutate(BPUEdryC = case_when(Weight_type == "Wet" ~ BPUE* .1*.4,
                              Weight_type == "Dry"~ BPUE*.4,
                              TRUE ~ BPUE),
         #multiply by 1000 to convert 
         bpue = BPUEdryC*1000) %>%
  select(-BPUE, -BPUEdryC)

##################
#ok, now how big of a mysid can a smelt eat?

dietlengths = read_csv("data/shipchannel_avglengths.csv") %>%
  filter(Month %in% c(6:10), MacroCategory != "fish")



ggplot(dietlengths, aes(x = Month, y = mean_withest, color = MacroCategory))+ geom_point()+
  geom_smooth(method = "lm")+
  ylim(0,7.5)



dietlengths2 = read_csv("data/shipchannel_lengths.csv") %>%
  filter(Month %in% c(6:10), MacroCategory != "fish")

ggplot(dietlengths2, aes(x = Month, y = PreyLength, color = MacroCategory))+ geom_point()+
  geom_smooth(method = "lm")

hist(dietlengths2$PreyLength)

ggplot(Mysidsb, aes(x = month(Date), y = Size, color = Taxname))+ geom_point(position = "jitter")
hist(Mysidsb$Size)

#so, in June 4mm is about the max, incresaes throught the season.
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
##############################################################
#DOP mysid data
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


DOPmysx$Station %in%DOP2$Station

test = select(DOP2, Station, Taxlifestage)

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


mysidstotd = DOPmys2 %>%
  ungroup() %>%
  left_join(DOPtots) %>%
  mutate(FO = count/total_count, CPUE2 = FO*CPUE)%>%
  left_join(conversions) %>%
  mutate(BPUE = (a2*(Length^b))*CPUE2) %>%
  #filter(Preservative == "Formalin", Weight_type == "Dry") %>%
  mutate(BPUEdryC = case_when(Weight_type == "Wet" ~ BPUE* .1*.4,
                              Weight_type == "Dry"~ BPUE*.4,
                              TRUE ~ BPUE),
         #multiply by 1000 to convert 
         bpue = BPUEdryC*1000) %>%
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
  summarize(bpue = mean(bpue), cpue = mean(CPUE, na.rm =T)) %>%
  mutate(Biomass_C = bpue/cpue)

save(mysidsmonth, file = "data/mysidsmonth.Rdata")

ggplot(mysidsmonth, aes(x = Month, y = bpue, fill = IBMR))+ geom_col()

ggplot(mysidsmonth, aes(x = Month, y = bpue, fill = IBMR))+ geom_col() +
  facet_wrap(~Year)

ggplot(mysidsall, aes(x = Month, y = CPUE, fill = IBMR))+ geom_col() +
  facet_grid(Source~Year)

ggplot(DOPmysalld, aes(x = Month, y = bpue, fill = IBMR))+ geom_col()

ggplot(FMWTmys, aes(x = month(Date), y = CPUE))+ geom_point()

ggplot(DOPmys2, aes(x = month(tow_date), y = tot_cpue))+ geom_point()

