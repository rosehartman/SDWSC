# Code to compile and clean turbidity data at the Ship Channel
# Brian Mahardja, May 2024

setwd("~/GitHub/SDWSC")

library(tidyverse)
library(lubridate)
library(sharpshootR)
library(dataRetrieval)
library(readxl)
library(caTools)
library(TTR)

#########################################################
# Load datasets

# Station 66
Turb_CM66 <- read_excel(file.path("data/2024-03-13 - Data from Steve Sadro/DWSC Turbidity data corrected.xlsx"),sheet="Site 66",
                        skip=1) %>%
  rename(DateTime=3,Turbidity=4) %>% dplyr::select(Deployment,DateTime,Turbidity) %>% mutate(Station="CM66") %>%
  mutate(DateTime=as.POSIXct(DateTime,format="%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"))

# Station 74
Turb_CM74 <- read_excel(file.path("data/2024-03-13 - Data from Steve Sadro/DWSC Turbidity data corrected.xlsx"),sheet="Site 74",
                        skip=1) %>%
  rename(DateTime=2,Turbidity=3) %>% dplyr::select(Deployment,DateTime,Turbidity) %>% mutate(Station="CM74") %>%
  mutate(DateTime=as.POSIXct(DateTime,format="%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"))

# Station 84
Turb_CM84 <- read_excel(file.path("data/2024-03-13 - Data from Steve Sadro/DWSC Turbidity data corrected.xlsx"),sheet="Site 84",
                        skip=1) %>%
  rename(DateTime=2,Turbidity=3) %>% dplyr::select(Deployment,DateTime,Turbidity) %>% mutate(Station="CM84") %>%
  mutate(DateTime=as.POSIXct(DateTime,format="%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"))

#####################
# Get DWS/CM51 data from USGS website
# Set date here
start_date<-"2022-08-17"
end_date<-"2024-01-24"

# Parameter 63680 is for Turbidity, water, unfiltered, monochrome near infra-red LED light, 780-900 nm, detection angle 90 +-2.5 degrees, formazin nephelometric units (FNU)
Turb_CM51 <- readNWISuv(siteNumbers = "11455338",
                        parameterCd = "63680",
                        startDate = start_date,
                        endDate = end_date)

# Note data gap in November of 2023
Turb_CM51_edit <- Turb_CM51 %>% rename(DateTime=dateTime,Turbidity=X_63680_00000) %>% mutate(Station="CM51",Deployment=NA) %>%
  select(Deployment,DateTime,Turbidity,Station)

# Combine datasets into one
Turb_all<-rbind(Turb_CM51_edit,Turb_CM66,Turb_CM74,Turb_CM84) %>% mutate(Date=as.Date(DateTime)) %>%
  # Remove deployment column since it's not used
  dplyr::select(-Deployment) %>%
  # There are some NA for DateTime column from station 84
  # Remove them
  dplyr::filter(!is.na(DateTime))


### Follow the 6 QC steps similar to Catarina Pien's turberature screening
## See original script here: https://github.com/catarfish/climatechange/blob/master/CDEC_QA_4.Rmd
#########################################################

## QC1) Flag data outside of reasonable range 

# Plot data overall
#ggplot(data=Turb_all) + geom_point(aes(x=DateTime,y=Turbidity)) + facet_grid(~Station)

# Remove zero values and replace with the minimum non-zero value
turb_nonzero <- Turb_all %>% filter(Turbidity >0)
min(turb_nonzero$Turbidity)

Turb_all$Turbidity <- ifelse(Turb_all$Turbidity <0, min(turb_nonzero$Turbidity),Turb_all$Turbidity)

# Unclear as to what is reasonable range for Turbidity, leave as is
turb_q1 <- Turb_all %>% mutate(Flag_QC1 = "N")

#########################################################

## QC2) Missing values: Flag days with less than n values (missing more than an hour's worth of data)

#1. Count the number of rows per station, date group. (There should only be one row per date)
#2. Flag days where there are less than complete values (every 10 mins for Steve's data = 144, every 15 mins for CM51/DWS = 96)
#3. Use leftjoin to add flags to the original data.

# This data frame contains all the dates with less than 138 or 92 values. 
turb_q2_a <- turb_q1 %>%
  filter(Flag_QC1 == "N") %>% # See next comment about removing QC1="Y" values
  group_by(Station, Date) %>%
  arrange(Station, Date, DateTime) %>%
  summarise(total = n()) %>%
  mutate(Flag_QC2 = case_when(Station=="CM51"&total < 92 ~ "Y",
                              Station=="CM51"&total >= 92 ~ "N",
                              Station!="CM51"&total < 138 ~ "Y",
                              Station!="CM51"&total >= 138 ~ "N")) %>%
  select(-total)

# Flagged values
turb_q2_b <- turb_q2_a %>%
  filter(Flag_QC2 == "Y")

#Join original dataframe with flagged values based on values NOT in common. 
#based on station, depth, and date
turb_q2 <- turb_q1 %>%
  left_join(turb_q2_a, by = c("Station", "Date")) %>%
  filter(Flag_QC1 == "N") 
# From Cat: This part is important for QC5 and QC6. Basically removes all values that are not within range (QC1) 


#########################################################
## QC3) Flag if there are 18+ repeating values in a row

#1. Create new columns indicating whether the turbidity at x time is the same as that of x-1 timepoint
#2. Take a cumulative sum of all the rows where turbidity values are different
#3. Group by the sum and count up the number of rows where the turbidity is the same.
#4. Flag the rows where number of repeated values is above our cut-off

# Function to determine whether values are repeating by each station
# Inputs are data frame and x (number of repeating values you want to check for)
# Check if number is same as previous number. If yes, 1. If no, 0.
# Cumulative sum so each time a value repeats, cumulative sum goes up
# Count the number in a row that are the same
# Flag if that number > threshold 

repeating_vals = function(df, x){
  df$same = ifelse(df$Turbidity == lag(df$Turbidity, 1, default = 0), 1L, 0L)
  df = df %>%
    mutate(issame = cumsum(df$same == 0L)) %>%
    group_by(Station, issame) %>%
    mutate(flag = sum(same)+1 ) %>%
    ungroup() %>%
    mutate(Flag_repeats = ifelse(flag > x, "Y", "N"))
  return(df)
}


# Run function repeating values and get rid of the columns we don't need
turb_q3 <- repeating_vals(df = turb_q2, x = 18) %>%
  select(-flag, -issame, -same) %>%
  rename(Flag_QC3 = Flag_repeats) 

# Flagged values
turb_q3_b <- turb_q3 %>%
  filter(Flag_QC3 == "Y")

# Remove unnecessaries
rm(turb_q2_a)
gc()

###########################################################
## QC4) Use the anomalize package to flag anomalies
#* Twitter + GESD is for more for highly seasonal data (however, GESD is extremely slow because it is iterative)
#* STL + IQR if seasonality is not a major factor
#* Trend period depends on personal knowledge of data

library(anomalize)
library(tibbletime)
# see https://business-science.github.io/anomalize/articles/anomalize_methods.html

turb_q4_a <- as_tbl_time(turb_q3, index = DateTime)

# Anomaly Detection
# time_decompose: separates time series into seasonal, trend, and remainder components
# stl: loess works well when long term trend is present
# twitter: (removes median rather than fitting smoother) - when long-term trend is less dominant than short-term seasonal component
# anomalize: applies anomaly detection methods to remainder component
# time_recompose: calculate limits to separate "normal" data from anomalies
turb_q4_c <- turb_q4_a %>%
  group_by(Station) %>%
  time_decompose(Turbidity, method = "stl", trend = "3 months") %>%
  # Make the alpha very low to adjust for the fact that turbidity values are a lot more variable
  anomalize(remainder, method = "iqr", alpha = 0.005) %>%
  time_recompose() %>% 
  select(c(DateTime, anomaly)) %>%
  as_tibble() 

# Join "anomaly" with rest of the data
turb_q4_d <- inner_join(turb_q3, turb_q4_c, by = c( "DateTime", "Station"))

# Rename "anomaly" Flag_QC4 for consistency, change No to N and Yes to Y
turb_q4 <- turb_q4_d %>%
  mutate(anomaly = factor(anomaly)) %>%
  mutate(anomaly = recode(anomaly, No = "N", Yes = "Y"))  %>%
  rename(Flag_QC4 = anomaly)

# Flagged values
turb_q4_b <- turb_q4 %>%
  filter(Flag_QC4 == "Y")


###########################################################
### QC5) Spike test

#Anomalize is pretty good, but there are a few single points here and there that don't get detected. 
#1. If difference between value and the value before it > X amount, it would be flagged.

## Unclear what value would be unreasonable for turbidity change in 10-15 minutes
# This may be a true event when cargo ship passes by

# Skip

turb_q5 <- turb_q4 %>%
  mutate(Flag_QC5 = "N")


###########################################################
### QC6) Rate of Change Test
#2. Define standard deviation threshold
#3. If difference between value and the value before it > threshold, it is flagged.

## Unclear what value would be unreasonable for turbidity change in 10-15 minutes
# This may be a true event when cargo ship passes by

# Skip

turb_q6 <- turb_q5 %>%
  mutate(Flag_QC6 = "N")


#####################################
## Filter final dataset

# Merge back in q1 data
# Data that were filtered out were not subsequently run under other QC tests, so should be NA, but this messes up other flags, so
# flagged as "N"
turb_q1_table <- turb_q1 %>%
  filter(Flag_QC1 == "Y") %>%
  mutate(Flag_QC2 = "N",
         Flag_QC3 = "N", 
         Flag_QC4 = "N",
         Flag_QC5 = "N",
         Flag_QC6 = "N")

# Combine Flags 
turb_flags <- rbind(turb_q6, turb_q1_table) %>%
  ungroup() %>%
  mutate(Flag1_6 = paste0(Flag_QC1, ",", Flag_QC2, ",", Flag_QC3, ",", Flag_QC4, ",", Flag_QC5, ",", Flag_QC6))
head(turb_flags)

#Flag any day with flag
turb_flags <- turb_flags %>% mutate(any_flag=ifelse(Flag1_6=="N,N,N,N,N,N","N","Y"))

#Create final flag (7) to remove any day with more than 1 hour worth of flagged data
turb_flags_day <- turb_flags %>% filter(any_flag=="N") %>% group_by(Station,Date) %>%   
  arrange(Station, Date, DateTime) %>%
  summarise(total = n()) %>%
  mutate(Flag_QC7 = case_when(Station=="CM51"&total < 92 ~ "Y",
                              Station=="CM51"&total >= 92 ~ "N",
                              Station!="CM51"&total < 138 ~ "Y",
                              Station!="CM51"&total >= 138 ~ "N")) 

#Rejoin data
turb_flags <- turb_flags %>% left_join(turb_flags_day)
turb_flags$Flag_QC7 <- ifelse(is.na(turb_flags$Flag_QC7),"Y",turb_flags$Flag_QC7)

#Plot all figures to check removed dataset
ggplot(data=turb_flags) + 
  geom_point(aes(x=DateTime,y=Turbidity,color=Flag_QC7)) +
  facet_grid(~Station)

#Remove flagged dataset to finalize clean dataset
turb_final <- turb_flags %>%
  filter(Flag_QC7=="N") %>%
  select(Turbidity, Station, Date, DateTime) 

#Plot daily summarized data
turb_day <- turb_final %>% group_by(Station, Date) %>%
  summarise(Turbidity=mean(Turbidity)) %>% ungroup()

ggplot(data=turb_day) + 
  geom_point(aes(x=Date,y=Turbidity)) +
  facet_grid(~Station)


#####################################
## Impute data using linear interpolation
library(imputeTS)

# Create complete dataset to impute data
# Check turb_day data and it seems like we should always start from April 1st and end at September 24th
turb_day_edit <- expand.grid(Date=seq.Date(from=as.Date(start_date),to=as.Date("2024-01-01"),by="day"),
                             Station=c("CM51","CM66","CM74","CM84"))

# Add data into turb_day_edit
turb_day_edit_fill <- turb_day_edit %>% left_join(turb_day) %>%
  mutate(Imputed=ifelse(is.na(Turbidity),"Yes","No"))

# Split data into list
turb_day_split <- turb_day_edit_fill %>% 
  arrange(Station, Date)

turb_day_split <- split(turb_day_split, turb_day_split$Station)

# Impute data using linear interpolation
for(i in seq_along(turb_day_split)){ 
  turb_day_split[[i]]$Turbidity <- na_interpolation(turb_day_split[[i]]$Turbidity)
}

# Rejoin into a single data frame
turb_day_imputed<-bind_rows(turb_day_split, .id = "Station")

# Visualize imputed data
ggplot(data=turb_day_imputed) + 
  geom_point(aes(x=Date,y=Turbidity,color=Imputed)) +
  facet_grid(~Station)

######
# Export out data
write.csv(turb_day_imputed,file.path("data/Turbidity_Data_2023_Imputed.csv"))

