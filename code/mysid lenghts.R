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
#AND DIVIDE BY VOLUME
FMWTvols = filter(fmwtmys, Station %in% fmwtsats$Station) %>%
  select(Station, Date, Volume)

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

mysidsmonth = group_by(Mysidstots, Month, Year, Region, IBMR) %>%
  summarize(bpue = mean(bpue))

save(mysidsmonth, file = "data/mysidsmonth.Rdata")
