#looking at the delta smelt diet data to figure out how to group zoop

library(tidyverse)
library(readxl)
library(deltamapr) #need the deltamapr package for the SDWSC regions
library(sf)

# install.packages("devtools")
#devtools::install_github("InteragencyEcologicalProgram/deltamapr")


#read in the dop and flash diet by number. 
#2023 hasn't been QC'd yet
#made a category in the databases for the zoop categories and then ran the matrix queries

#DOP & Flash Diet Data-------

dop= read_xlsx("data/DOP Diet by Number 2017 to 2022.xlsx") %>% 
  select(-c(Time, Region)) 

flash2020= read_xlsx("~/Data/SDWSC Synthesis/SDWSC Zoop/Analysis/Data/FLaSH 2011 to 2020 Delta Smelt Diet by Number to Post.xlsx") %>% 
  select(-c(Time, BottomTemperature, BottomConductivity, BottomPPT)) %>% 
  mutate(Station = as.character(Station))

#i don't know why but flash queries don't put out empties, so need to add them separately

flashempty = read_xlsx("~/Data/SDWSC Synthesis/SDWSC Zoop/Analysis/Data/FLaSH 2011 to 2022 Empties.xlsx") %>% 
  mutate(Station = as.character(Station)) %>% 
  mutate(SerialNumber = as.character(SerialNumber))

#join empties to other flash matrix
#is this correct? Shouldn't it be 'bind_rows'?
flash2 = full_join(flash2020, flashempty)

flash2022 = read_xlsx("~/Data/SDWSC Synthesis/SDWSC Zoop/Analysis/Data/FLaSH 2021 to 2022 Delta Smelt Diet by Number to Post.xlsx") %>% select(-c(HatcheryReleased, TowCount, Time)) %>% 
  mutate(Station = as.character(Station))

#This shold probably also be bind_rows
flash_all = full_join(flash2, flash2022)

all = full_join(flash_all, dop) %>% 
  select(-'Worm pieces') %>% 
  mutate(SampleID = paste(Database, LogNumber, Project, Date, Station, SerialNumber, sep = " ")) %>% #create sample ID
  select(Project, Year, Month, Date, Station, SampleID, Species, 'Acanthocyclops spp' :'Nippoleucon hinumensis')

#Stations & Regions------

#need to add in the coordinates to be able to add region

stations2020 = read.csv("~/Data/SDWSC Synthesis/SDWSC Zoop/Analysis/Data/Delta Smelt Diet Station Lookup.csv") %>% 
  rename(Project = project, Station = station)

stations2022 = read_xlsx("~/Data/SDWSC Synthesis/SDWSC Zoop/Analysis/Data/DOP Stations 2021 to 2023.xlsx")

stations = full_join(stations2020, stations2022) %>% 
  select(-region)

allwithstations = merge(all, stations, by = c('Project', 'Station'))

#adding in the delta mapr package regions for the ship channel

sdwscregions = filter(R_EDSM_Subregions_19P3, SubRegion %in% c("Upper Sacramento River Ship Channel","Lower Sacramento River Ship Channel"))

#use this if want to plot the regions but don't need to here.
#ggplot()+
  #geom_sf(data = R_EDSM_Subregions_19P3) 

sdwsc_diet = st_as_sf(allwithstations, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = st_crs(sdwscregions)) %>%
  st_join(sdwscregions) %>%
  st_drop_geometry() %>%
  filter(!is.na(SubRegion)) %>% 
  mutate(Region = case_when(SubRegion == "Upper Sacramento River Ship Channel" ~ "Top", SubRegion == "Lower Sacramento River Ship Channel" & latitude > 38.36  ~ "Middle", latitude <= 38.36 ~ "Lower")) %>% 
  select(-c(SubRegion, SQM))


#convert to long format so can convert prey categories

sdwscdiet_long = pivot_longer(sdwsc_diet, 
                        cols = `Acanthocyclops spp`:`Nippoleucon hinumensis`, 
                        names_to = "DietCategory", 
                        values_to = "DietbyNumber") %>% 
  select(Project:SampleID, latitude, longitude, Region, Species, DietCategory, DietbyNumber) %>% 
  mutate(DietbyNumber = ifelse(is.na(DietbyNumber), 0, DietbyNumber)) #replace any NAs in the prey numbers

#move back to wide for csv

sdwscdiet_wide = pivot_wider(sdwscdiet_long, id_cols = Project:Species, 
                              names_from = DietCategory, 
                              values_from = DietbyNumber, 
                              values_fill = 0)


write.csv(sdwsc_diet_wide, file = "shipchannel_dietbynumb.csv", row.names = FALSE )

#########################################################################################
#start here

sdwsc_diet_wide = read_csv("data/shipchannel_dietbynumb.csv")

#look at averages and sample size to see how the distributions come out
#need to move back to long to be able to group

sdwscdiet_long = sdwsc_diet_wide %>% 
  pivot_longer(cols = `Acanthocyclops spp`:`Nippoleucon hinumensis`, 
               names_to = "DietCategory", 
               values_to = "DietbyNumber" )

avg_diet = sdwsc_diet_wide %>% 
  filter(Month>5 & Month<11) %>% #filter for summer fall
  group_by(Year, Month, Region) %>% 
  summarize(n= length (SampleID))

#want to make a plot with sample sizes by region

n1 = ggplot(avg_diet, aes(x = Month, y = n))
n2 = n1+ geom_bar(stat = "identity") + 
  facet_grid(Region~Year) + 
  scale_fill_brewer(palette = "Set3", name = NULL) +
  ylab("sample size") +
  theme(text = element_text(family = "sans", size = 10),
        legend.text = element_text(face = "italic"), 
        axis.text.x = element_text(angle=45, hjust=1)) #angles x axis so the years fit
n2

#Prey Lengths------

#need to look at average prey lengths of macrozoop in order to calc biomass
#queried all prey lengths 
# 
# lengths = read_xlsx("~/Data/SDWSC Synthesis/SDWSC Zoop/Analysis/Data/Diet Prey Lengths to 2022.xlsx") %>% 
#   merge(., stations, by= c("Project", "Station")) %>% 
#   mutate(SampleID = paste(Database, LogNumber, Project, Date, Station, SerialNumber, sep = " ")) #create sample ID
# 
# sdwsc_lengths = st_as_sf(lengths, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
#   st_transform(crs = st_crs(sdwscregions)) %>%
#   st_join(sdwscregions) %>%
#   st_drop_geometry() %>%
#   filter(!is.na(SubRegion)) %>% 
#   mutate(Month = month(Date)) %>% 
#   mutate(Region = case_when(SubRegion == "Upper Sacramento River Ship Channel" ~ "Top", SubRegion == "Lower Sacramento River Ship Channel" & latitude > 38.36  ~ "Middle", latitude <= 38.36 ~ "Lower")) %>% 
#   select(Year, Month, Date, latitude, longitude, Region, SampleID, PreyType:Estimate)

#write.csv(sdwsc_lengths, file = "shipchannel_lengths.csv", row.names = FALSE)
sdwsc_lengths = read_csv("data/shipchannel_lengths.csv")

#want to find an average for each larger macro category

avglength = sdwsc_lengths %>% 
  filter(is.na(Estimate)) %>% #filter it so only look at actual measurements and not those that use estimates
  group_by(Year, Month, Region, MacroCategory) %>% 
  summarise(mean_length = mean(PreyLength), 
            n= length(Year))


#Biomass-------------

#want to look at biomass in diets in order to compare to the zoop data
#first want to convert to biomass via the conversions that we use for zooplankton, then we can combine them for the IBMR categories

biomass = read_xlsx("Diet and IBMR Conversions.xlsx") %>% 
  mutate("DietCategory" = PostCategory) %>% 
  select(-PostCategory)

dietbiomass = left_join(sdwscdiet_long, biomass, by = "DietCategory" ) %>% 
  filter(CarbonWeight_ug !="NA") %>%  #filter out what doesn't have a carbon weight for now, which is mostly macrozoop
  mutate(CarbonWeight_ug = as.numeric(CarbonWeight_ug )) %>% 
  mutate(DietBiomass = DietbyNumber*CarbonWeight_ug) #create new column which converts diet by number to a measure of biomass using the zoop conversions

lw_conversions = read_xlsx("Data/Mesomicromacro Biomass conversions.xlsx", sheet = "Macro-zooplankton") %>% 
  filter(Preservative != "Formalin", Weight_type != "Dry") %>%  #filtering so its just dry and ethanol
  rename("PreyConversion" = Taxname) %>% 
  select(PreyConversion, Preservative, Weight_type, a, b)

#combine files to convert length to weight 
cross = read_excel("Data/Mesomicromacro Biomass conversions.xlsx", sheet = "crosswalk")

lw = left_join(sdwsc_lengths, cross) %>%
               left_join(lw_conversions, by= "PreyConversion") %>% 
  filter(MacroCategory != "fish" & MacroCategory != "isopods") %>%  #only keeping mysids and amphipods
  mutate(Weight = (a*(PreyLength^b)) *1000000)  #find weight and then convert to ug

#find biomass for the macrozoop
#dry weight = 10% of wet weight
#carbon weight .4 times dry weight
#so multiply by .04 to go from wet weight to biomass

macrobm = lw %>% 
  mutate(DietBiomass = Weight*.04) %>% 
  select(Year, Month, Region, SampleID, MacroCategory, Estimate, DietBiomass, IBMR) 

#sum macro by sample 
totmacro = macrobm %>% 
  group_by(Year, Month, Region, SampleID, MacroCategory, IBMR) %>% 
  summarize(totibmr = sum(DietBiomass
                          )) 

#er, I htink we need to add in zeros here.
totmacrozeros = pivot_wider(totmacro, names_from = IBMR, values_from = totibmr, values_fill = 0) %>%
  pivot_longer(cols = c(amphipod, mysid), names_to = "IBMR", values_to = "totibmr")
#get mean per year, month, region and then will add together with meso
macro_mon = totmacrozeros %>% 
  group_by(Year, Month, Region, IBMR) %>% 
  summarize(mean_mon = mean(totibmr))

#Biomass-------------

#want to look at biomass in diets in order to compare to the zoop data
#first want to convert to biomass via the conversions that we use for zooplankton, then we can combine them for the IBMR categories

biomass = read_xlsx("Diet and IBMR Conversions.xlsx") %>% 
  mutate("DietCategory" = PostCategory) %>% 
  select(-PostCategory)

dietbiomass = left_join(sdwscdiet_long, biomass, by = "DietCategory" ) %>% 
  filter(CarbonWeight_ug !="NA") %>%  #filter out what doesn't have a carbon weight for now, which is mostly macrozoop
  mutate(CarbonWeight_ug = as.numeric(CarbonWeight_ug )) %>% 
  mutate(DietBiomass = DietbyNumber*CarbonWeight_ug) #create new column which converts diet by number to a measure of biomass using the zoop conversions

allbiomass = bind_rows(dietbiomass, macrobm) %>%
  mutate(IBMR = case_when(DietCategory == "Sinocalanus spp" ~ "sino",
                          TRUE ~ IBMR))





#Summarizing for IBMR-----

#want to group by year, month and region and then add up all the biomasses for the IBMR categories 

#combining by sample first for the ibmr categories in each fish stomach
#################################
####Sum first!! Then mean!!
diet_ibmr= allbiomass %>% 
  group_by(Year, Month, Region, SampleID, IBMR) %>% 
  summarize(tot_samp = sum(DietBiomass))

#ugh, i didn't put in the zeros for mysids right

diet_ibmr0s = pivot_wider(diet_ibmr, names_from = IBMR, 
                          id_cols = c(SampleID, Month, Year, Region),
                             values_from = tot_samp, values_fill = 0) %>%
  pivot_longer(cols = c(acartela:mysid), names_to = "IBMR", values_to = "tot_samp") %>%
  group_by(SampleID) %>%
  mutate(propdiet = tot_samp/(sum(tot_samp))) %>%
  ungroup()


write.csv(diet_ibmr0s, "outputs/diet_ibmr.csv")

#it sounds like Will need it in wide format, rather than long

diet_ibmr_wide = pivot_wider(diet_ibmr0s, id_cols = c(SampleID, Month, Year, Region),
                             names_from = IBMR, values_from = propdiet)

write.csv(diet_ibmr_wide, "outputs/diet_ibmr_wide.csv")

#now month and year
diet_mon = diet_ibmr0s %>% 
  group_by(Year, Month, Region, IBMR) %>% 
  summarize(mean_mon = mean(tot_samp))
  
save(diet_mon, file = "data/dietbymonth.RData")
#Plots--------

#make stacked plots of the diet categories by year, month, region
#eventually want to filter for months wanted but just curious about this now
library(RColorBrewer)
mypal = c(brewer.pal(12, "Set3"), "tan", "olivedrab2", "violetred")
db1 = ggplot(diet_mon, aes(x = Year, y = mean_mon))
db2 = db1+ geom_bar(stat = "identity", aes(fill = IBMR), position = "fill") + 
  facet_wrap(~Region) + 
  scale_fill_manual(values = mypal, name = NULL) +
  ylab("diet biomass") +
  theme(text = element_text(family = "sans", size = 10),
                      legend.text = element_text(face = "italic"), 
                      axis.text.x = element_text(angle=45, hjust=1)) #angles x axis so the years fit

db2

ggsave(db2, filename = "sdwscdietbiomass_all.tiff", device = "tiff", width = 6, height = 4.5, units = "in", dpi = 300)

#now filtered by months

sumfall = diet_mon %>% 
  filter(Month>5 & Month <11)

dbsf1 = ggplot(sumfall, aes(x = Year, y = mean_mon))
dbsf2 = dbsf1+ geom_bar(stat = "identity", aes(fill = IBMR), position = "fill") + 
  facet_wrap(~Region) + 
  scale_fill_manual(values = mypal, name = NULL) +
  ylab("diet biomass") +
  theme(text = element_text(family = "sans", size = 10),
        legend.text = element_text(face = "italic"), 
        axis.text.x = element_text(angle=45, hjust=1)) #angles x axis so the years fit

dbsf2

ggsave(dbsf2, filename = "sdwscdietbiomass_sumfall.tiff", device = "tiff", width = 6, height = 4.5, units = "in", dpi = 300)


ggplot(sumfall, aes(x = Month, y = mean_mon, fill = IBMR))+ geom_col(position = "fill")+
  facet_wrap(~Region)

##################################################################################
#now break out sinocalanus seperately

diet_ibmr2= dietbiomass %>% 
  mutate(IBMR = case_when(DietCategory == "Sinocalanus spp" ~ "sino",
                          TRUE ~ IBMR)) %>%
  group_by(Year, Month, Region, SampleID, IBMR) %>% 
  summarize(tot_samp = sum(DietBiomass))

#now month and year
diet_mon2 = diet_ibmr2 %>% 
  group_by(Year, Month, Region, IBMR) %>% 
  summarize(mean_mon = mean(tot_samp))


sumfall2 = diet_mon2 %>% 
  filter(Month>5 & Month <11)

dbsf1.1 = ggplot(sumfall2, aes(x = Year, y = mean_mon))
dbsf2.1 = dbsf1.1+ geom_bar(stat = "identity", aes(fill = IBMR), position = "fill") + 
  facet_wrap(~Region) + 
  scale_fill_brewer(palette = "Set3", name = NULL) +
  ylab("diet biomass") +
  theme(text = element_text(family = "sans", size = 10),
        legend.text = element_text(face = "italic"), 
        axis.text.x = element_text(angle=45, hjust=1)) #angles x axis so the years fit
dbsf2.1
