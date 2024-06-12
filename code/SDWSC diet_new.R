#code for the sac deep water ship channel diets and synthesis
#had some issues with the previous script and lost where errors were so going through and starting over to clean up


#looking at the delta smelt diet data to figure out how to group zoop


library(tidyverse)
library(readxl)
library(deltamapr) #need the deltamapr package for the SDWSC regions
library(sf)
library(lubridate)

# install.packages("devtools")
#devtools::install_github("InteragencyEcologicalProgram/deltamapr")


#read in the dop and flash diet by number. 
#2023 hasn't been QC'd yet
#made a category in the databases for the zoop categories and then ran the matrix queries
#needed to add environmentals too

#Diet Data from Access----------------------------------------------------

#########DOP Data-------


dop = read.csv("~/Data/SDWSC Synthesis/SDWSC Zoop/Analysis/Data/DOP Qry_Diet By Number Matrix All Prey.csv", check.names = FALSE) %>% #adding in the check names argument is needed for when reading a csv so it doesn't remove the spaces in the prey columns
  mutate(Date2 = mdy_hm(Date)) %>% #need to convert to date, but also has the time in there which is why use _hm
  mutate(Date = date(Date2), 
         Database = "DOP", 
         Year = as.numeric(Year), 
         Month = as.numeric(Month)) %>% #now move it to just the date, no time
  rename(LogNumber = DOPLogNumber) %>% 
    select(-c(Time, Region, Date2))

#for some reason the late 2021 and early 2022 enviro data aren't talking to each other in the database and don't pop up in the queries. Copying the enviro data from the database and hoping to merge it to the diet by number file

dop_enviro = read.csv("~/Data/SDWSC Synthesis/SDWSC Zoop/Analysis/Data/DOP Qry_Enviro for SDWSC.csv", check.names = FALSE) %>% 
  select(-c(SpecialStudyID, SerialNumber)) %>% 
  mutate(Date2 = mdy_hms(Date)) %>% #need to convert to date, but also has the time in there which is why use _hm
  mutate(Date = date(Date2)) %>% 
  select(-Date2) %>% 
  filter(Year == 2022| Year == 2021)

dop2 = left_join(dop, dop_enviro, by = c('Project', 'Year', 'Month', 'Station', 'Date')) %>% 
  mutate(SurfaceTemperature = coalesce(SurfaceTemperature.y, SurfaceTemperature.x), #use coalesce to replace the NA enviro values in the dop diet matrix with the values in the dop enviro matrix
         SurfaceConductivity = coalesce(SurfaceConductivity.y, SurfaceConductivity.x), 
         SurfacePPT = coalesce(SurfacePPT.y, SurfacePPT.x), 
         Secchi = coalesce(Secchi.y, Secchi.x), 
         Turbidity = coalesce(Turbidity.y, Turbidity.x)) %>% 
  select(-c(SurfaceTemperature.y, SurfaceTemperature.x, SurfaceConductivity.y, SurfaceConductivity.x, 
            SurfacePPT.y, SurfacePPT.x, Secchi.y, Secchi.x, Turbidity.y, Turbidity.x )) %>%  #then remove all the extra columns
  distinct() #have some duplicates but this doesn't get rid of all of them. The dups have slightly different enviros. May be from multiple tows, or maybe different data/ rounding differences. 

#make a new dataframe which averages the slightly different enviros of the duplicated fish
#unsure why there may be duplicates. Different tows, slightly different versions of the data, rounding weirdness? No idea but they're close enough

dop3 = dop2 %>% 
  group_by(Project, Year, Month, Station, Date, Database, LogNumber, SerialNumber, Species, across(TotalBodyWeight:`Unid mysids`)) %>% 
  summarize(SurfaceConductivity = mean(SurfaceConductivity), 
            SurfacePPT = mean(SurfacePPT), 
            Secchi = mean(Secchi), 
            Turbidity = mean(Turbidity))

#numbers look good, but just checking to make sure there's no duplicates

dups = dop3 %>% 
  group_by(LogNumber) %>% 
  summarise(n = n()) %>%
  filter(n>1)

#######Flash Data--------


#have flash data that has been formatted to post from 2011 to 2020. Taking this file since i know it has the correct GES samples which pops up as duplicates when run it again.... something weird with the tow

flash2020 = read.csv("~/Data/SDWSC Synthesis/SDWSC Zoop/Analysis/Data/FLaSH 2011 to 2020 Delta Smelt Diet by Number to Post.csv", check.names = FALSE) %>% 
  mutate(Station = as.character(Station)) %>% 
  mutate(Date2= mdy(Date)) %>% #this doesn't have the time, so just mdy
  mutate(Date = Date2) %>% 
  select(-c(Time, Date2))

#flash queries don't put out empties for some reason so need to add them

flashempty = read.csv("~/Data/SDWSC Synthesis/SDWSC Zoop/Analysis/Data/FLaSH Qry_Empties to Post with Enviro.csv", check.names = FALSE) %>% 
  select(-Time) %>% 
  mutate(SerialNumber = as.character(SerialNumber)) %>% 
  mutate(Date2= mdy(Date)) %>% 
  mutate(Date = Date2) %>% 
  select(-Date2)

#combine the two files so far

flash2 = bind_rows(flash2020, flashempty)

#add the recent data after 2020. Only fish in 2022 were caught

flash2022 = read.csv("~/Data/SDWSC Synthesis/SDWSC Zoop/Analysis/Data/Flash 2021 to 2022 Qry_Diet by Number All Prey to Post with Enviro.csv", check.names = FALSE) %>% 
  select(-Time) %>% 
  mutate(Station = as.character(Station)) %>% 
  mutate(Date2= mdy_hm(Date)) %>% 
  mutate(Date = date(Date2)) %>% 
  select(-Date2)

#Combine all of flash files

flash_all = bind_rows(flash2, flash2022)

all = bind_rows(flash_all, dop3) %>% 
  select(-c("Worm pieces", FullnessRank, DigestionRank, GearType, Depth, BottomTemperature, BottomConductivity, BottomPPT, TotalNumberofPrey, "Gut Fullness")) %>% 
  mutate_at(vars('Acanthocyclops spp' :'Other rotifer'), ~replace(., is.na(.), 0)) %>%  #need to replace the NAs with 0s in some of the prey columns that are in one df but not the other
  #mutate(SampleID = paste(Database, LogNumber, Project, Date, Station, SerialNumber, sep = " ")) %>% 
  select(Year, Month, Database, LogNumber, Project, Date, Station, SerialNumber, Species: Turbidity, ForkLength, TotalBodyWeight, GutContents, 'Acanthocyclops spp' :'Other rotifer') #keeping all the columns used to creat the sample ID since somthing funky is happening when trying to join this plus the lengths)
#also removing the sampleID for now to connect everything and then add sample id later


#Stations & Regions------

#need to add in the coordinates to be able to add region

stations2020 = read.csv("~/Data/SDWSC Synthesis/SDWSC Zoop/Analysis/Data/Delta Smelt Diet Station Lookup.csv") %>% 
  rename(Project = project, Station = station)

stations2022 = read_xlsx("~/Data/SDWSC Synthesis/SDWSC Zoop/Analysis/Data/DOP Stations 2021 to 2023.xlsx")

stations = full_join(stations2020, stations2022) %>% 
  select(-region)

allwithstations = merge(all, stations, by = c('Project', 'Station')) #when merging lose 3 records, need to figure out which ones and why

lost = filter(all, !Station %in% stations$Station) #from ges station 704/705 which should actually be 706 according to coordinate check. Keep how is here since not applying to this dataset. Will need to change and update when publish full dataset.
#also a salvage fish in here


#adding in the delta mapr package regions for the ship channel

sdwscregions = filter(R_EDSM_Subregions_19P3, SubRegion %in% c("Upper Sacramento River Ship Channel","Lower Sacramento River Ship Channel"))

#use this if want to plot the regions but don't need to here.
#ggplot()+
#geom_sf(data = R_EDSM_Subregions_19P3) 

#SDWSC Diet----------------

#filtering out so only have a diet by number data frame with ship channel critters

sdwscdiet = st_as_sf(allwithstations, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = st_crs(sdwscregions)) %>%
  st_join(sdwscregions) %>%
  st_drop_geometry() %>%
  filter(!is.na(SubRegion)) %>% 
  mutate(Region = case_when(SubRegion == "Upper Sacramento River Ship Channel" ~ "Top", SubRegion == "Lower Sacramento River Ship Channel" & latitude > 38.36  ~ "Middle", latitude <= 38.36 ~ "Lower")) %>% 
  select(-c(SubRegion, SQM)) %>% 
  select(Year, Month, Region, latitude, longitude, Database, LogNumber, Project, Date, Station, SerialNumber, Species: Turbidity, ForkLength, TotalBodyWeight, GutContents, 'Acanthocyclops spp' :'Other rotifer')


write.csv(sdwscdiet, "Outputs/shipchannel_dietbynumb.csv", row.names = FALSE)

#want to look at sample sizes to see how things are distributed

sdwsc_n = sdwscdiet %>% 
  filter(Month>5 & Month<11) %>% #filter for summer fall
  group_by(Year, Month, Region) %>% 
  summarize(n= n())

#want to make a plot with sample sizes by region
#hardly any fish in the "top" region. July 2019 is considerably higher than the other years/months

n1 = ggplot(sdwsc_n, aes(x = Month, y = n))
n2 = n1+ geom_bar(stat = "identity") + 
  facet_grid(Region~Year) + 
  scale_fill_brewer(palette = "Set3", name = NULL) +
  ylab("sample size") +
  theme(text = element_text(family = "sans", size = 10),
        legend.text = element_text(face = "italic"), 
        axis.text.x = element_text(angle=45, hjust=1)) #angles x axis so the years fit
n2


#Prey Lengths-----------

#need to add in prey lengths in order to calc biomass of macro critters. 
#did queries in access for all prey lengths and then converted to csv

#need to add in the basic ibmr macro category and which prey conversion equation I'll use for the length conversions.
#could do this in r with if statements but want to save time and just did it with the csv from access

dop_lengths = read.csv("~/Data/SDWSC Synthesis/SDWSC Zoop/Analysis/Data/DOP SDWSC Qry_Prey Lengths.csv", check.names = FALSE) %>%
  mutate(Database = "DOP",   #adding a database column
         Date2 = mdy_hm(Date),  #need to do the same thing with the dates like i did the diet by number files
         Date = date(Date2)) %>%  #now move it to just the date, no time 
  rename(LogNumber = DOPLogNumber) %>% 
  select(-c(Date2, ForkLength, TotalBodyWeight, Region, GutContents))

#using flash prepared files to 2020 again since GES gives duplicates

f2020_lengths = read.csv("~/Data/SDWSC Synthesis/SDWSC Zoop/Analysis/Data/FLaSH Delta Smelt 2011-2020 Prey Lengths to Post.csv", check.names = FALSE) %>% 
  rename(LogNumber = FLaSHLogNumber) %>% 
  mutate(Database = "FLaSH", 
         Station = as.character(Station), 
         Date2 = mdy_hm(Date), 
         Date = date(Date2), 
         Year = year(Date), 
         Month = month(Date)) %>% 
  select(-Date2) 

 
#the fish processed since 2020 had no macro prey in the SDWSC region. Only ones found in SM

#combine length files and add on station coordinates + sampleID

lengths = rbind(dop_lengths, f2020_lengths) %>% 
  merge(., stations, by= c("Project", "Station")) %>% 
  mutate(Estimate = na_if(Estimate, ""))  #replace the blanks in the estimate column with NA
  #mutate(SampleID = paste(Database, LogNumber, Project, Date, Station, SerialNumber, sep = " ")) 

#filter to sdwsc only

sdwsc_lengths = st_as_sf(lengths, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = st_crs(sdwscregions)) %>%
  st_join(sdwscregions) %>%
  st_drop_geometry() %>%
  filter(!is.na(SubRegion)) %>% 
  mutate(Region = case_when(SubRegion == "Upper Sacramento River Ship Channel" ~ "Top", SubRegion == "Lower Sacramento River Ship Channel" & latitude > 38.36  ~ "Middle", latitude <= 38.36 ~ "Lower")) %>% 
  select(Year, Month, Region, latitude, longitude, Database, LogNumber, Project, Date, Station, SerialNumber, Species, PreyType:Estimate)


sdwsc_lengths2 = sdwsc_lengths %>% 
  select(-c(PreyConversion, MacroCategory))

write.csv(sdwsc_lengths2, file = "Outputs/shipchannel_lengths.csv", row.names = FALSE)

#want to find an average length for each larger macro category so Rosie can use it to use for the zoop biomass
#would normally total by fish, but just want to know which lengths are being eaten, so act like each individual length is from one fish, and ignore the fact that multiple may have been found in one fish

avg_length = sdwsc_lengths %>% 
  filter(is.na(Estimate)) %>%  #filter it so only look at actual measurements and not those that use estimates
  group_by(Year, Month, Region, MacroCategory) %>% 
  summarize(mean_length = mean(PreyLength))


#now do it with estimates just to see how it changes

avg_lengthwithest = sdwsc_lengths %>% 
  group_by(Year, Month, Region, MacroCategory) %>% 
  summarize(mean_withest = mean(PreyLength))

avglengths_all = right_join(avg_length, avg_lengthwithest)#merge the two so we have a value for the measured lengths, and those with the estimates includes

write.csv(avglengths_all, file = "Outputs/shipchannel_avglengths.csv", row.names = FALSE )

#length graph
#some lengths are removed because  we are missing lengths if they dont have any estimate

l1 = ggplot(avglengths_all, aes(x = Month, y = mean_length))
l2 = l1+ geom_bar(stat = "identity", aes(fill = MacroCategory))+ 
  facet_grid(Region~Year) + 
  scale_fill_brewer(palette = "Set3", name = NULL) +
  ylab("sample size") +
  scale_x_continuous(breaks = c(1, 4, 8, 12))+
  theme(text = element_text(family = "sans", size = 10),
        legend.text = element_text(face = "italic"), 
        axis.text.x = element_text(angle=45, hjust=1)) #angles x axis so the years fit
l2


#Macro Length/Weight-------

#need to calc the biomass for the macro prey
#need to convert the lengths to weight to then get biomass


#read in LW conversions. use the conversion file from my newsletter article
#the mysid lw conversions are funky, so will use Orsi for now until we figure it out. Since these are in dry weights, and the 

lw_conversions = read_xlsx("Data/Mesomicromacro Biomass conversions.xlsx", sheet = "Macro-zooplankton") %>% 
  #filter(Preservative != "Formalin"& Weight_type != "Dry") %>%  #filtering so its just wet and not formalin---- used this when we were using my conversions and not Orsi's. Now just using mine for amphipods
  filter(MacroCategory == "amphipod" & Preservative == "Ethanol" & Weight_type == "Wet" | Preservative == "Formalin" & Weight_type == "Dry") %>%  #filtering so have orsi for the mysids, which is in dry weight and the others in wet
  rename("PreyConversion" = Taxname) %>% 
  select(PreyConversion, MacroCategory, Preservative, Weight_type, a_grams, a_mg, b)

lw = left_join(sdwsc_lengths, lw_conversions, by= c("PreyConversion", "MacroCategory")) %>% 
  filter(MacroCategory != "fish" & MacroCategory != "isopods") %>%  #only keeping mysids and amphipods
  mutate(Weight = (a_grams*(PreyLength^b)*100000)) %>% #find weight and then convert to ug
  rename(IBMR = MacroCategory) %>% 
  select(-c(Species, PreyConversion, PreyLengthSpecies, Estimate:b))


#Biomass-------------

#want to look at biomass in diets in order to compare to the zoop data
#first want to convert to biomass via the conversions that we use for zooplankton, then we can combine them for the IBMR categories

###Meso Biomass--------

bm_conversions = read_xlsx("~/Data/SDWSC Synthesis/SDWSC Zoop/Analysis/Data/Diet and IBMR Conversions.xlsx") %>% 
  mutate("DietCategory" = PostCategory) %>% 
  select(-PostCategory)

#need to convert the diet by number file to long format so can multiple the carbon conversions to the numbers
#use the meso file first, then will calc the macro from the lengths to add to this

sdwscdiet_long = pivot_longer(sdwscdiet, 
                              cols = `Acanthocyclops spp`:`Other rotifer`, 
                              names_to = "DietCategory", 
                              values_to = "DietbyNumber" )

sdwscdietbm = left_join(sdwscdiet_long, bm_conversions, by = "DietCategory" ) %>% 
  filter(CarbonWeight_ug !="NA") %>%  #filter out what doesn't have a carbon weight for now, which is mostly macrozoop
  mutate(CarbonWeight_ug = as.numeric(CarbonWeight_ug )) %>% 
  mutate(DietBiomass = DietbyNumber*CarbonWeight_ug) %>%  #create new column which converts diet by number to a measure of biomass using the zoop conversions
  select(-c(Species, CarbonWeight_ug, latitude, longitude)) %>% 
  mutate(Year = as.numeric(Year), 
         Month = as.numeric(Month))


#probably need to total up the ibmr categories for each fish since these are now listed by the individual categories

totmeso_ibmr = sdwscdietbm %>% 
  group_by(across(Year:GutContents), IBMR) %>% #across helps grab a list of columns instead of having to name them all out, need the paratheses inbetween because the IBMR is separate
  summarize(totbm = sum(DietBiomass))

  
###Macro Biomass-------

#find biomass for the macrozoop
#dry weight = 10% of wet weight
#carbon weight .4 times wet weight
#so multiply by .04 to go from wet weight to biomass

#need to calc the mysids and amphipods separately since the mysids are in dry weight and amphipods in wet weight. 
#do in 2 separate steps so I make sure I don't mess it up. 
#first convert amphipods to dry weight

macrobm = lw %>% 
  mutate(DryWeight= (case_when(IBMR == "amphipod" ~ Weight*0.1, IBMR == "mysids" ~ Weight))) %>% 
  mutate(DietBiomass = (DryWeight*0.4)) %>% 
  select(-c(latitude, longitude, PreyType, PreyLength, Weight, DryWeight))

#want to get the sum of the biomass for each fish. Need to make sure I put zeros in here too

macsum = macrobm %>% 
  group_by(Year, Month, Region, Database, LogNumber, Project, Date, Station, SerialNumber, IBMR) %>% 
  summarize(totbm = sum(DietBiomass))

macsum_wide = macsum %>% 
  pivot_wider(id_cols = `Year`: `SerialNumber`, 
              names_from = IBMR, 
              values_from = totbm, 
              values_fill = 0)

macdups = macsum_wide %>% 
  group_by(LogNumber, Project, Date, Station) %>% 
  summarize(n = n()) %>% 
  filter(n>1)

#now pivot the total macro biomass to wide format so I have 0s before adding to the meso biomass

#####Combined biomass------

#now need to combine the two files so have all the ibmr categories
#had issues with this combining from the two files before so fingers crossed

#having issues with the long formats, move to wide before combining

totmeso_ibmr_wide = totmeso_ibmr %>% 
  pivot_wider(id_cols = Year:GutContents, 
              names_from = IBMR, 
              values_from = totbm, 
              values_fill = 0)

#combining for total biomass file

allbiomass = totmeso_ibmr_wide %>%  
  full_join(., macsum_wide, by = c("Year", "Month", "Region",  "Database", "LogNumber", "Project", "Date", "Station", "SerialNumber")) %>% 
  mutate_at(vars('acartela' :'mysids'), ~replace(., is.na(.), 0)) %>% 
  mutate(SampleID = paste(Database, LogNumber, Project, Date, Station, SerialNumber, sep = " ")) #create sample ID 

#remove other columns to clean up, but it keeps adding back those variables so, whatever

allbiomass2 = allbiomass %>% 
  select(Year, Month, Region, SampleID, SurfaceTemperature, SurfaceConductivity, SurfacePPT, Secchi, Turbidity, ForkLength, TotalBodyWeight, acartela:mysids)

#check for duplicates

alldups = allbiomass %>% 
  group_by(LogNumber, Project, Date, Station) %>% 
  summarize(n = n()) %>%
  filter(n>1)


write.csv(allbiomass, "Outputs/ shipchannel_dietbiomass.csv", row.names = FALSE)




