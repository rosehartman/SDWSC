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

Temp_CM66_1<- read.csv(file.path("data/2023-11-28 - Data from Adrianne/Temp_data_forBrian_March_June2023/20174654.csv"),
                       skip=1,col.names = c("X","DateTime","Temp")) %>% select(-X) %>%
  mutate(Station="CM66",Depth=1.5)

Temp_CM66_2<- read.csv(file.path("data/2023-11-28 - Data from Adrianne/Temp_data_forBrian_March_June2023/20174618.csv"),
                       skip=1,col.names = c("X","DateTime","Temp")) %>% select(-X) %>%
  mutate(Station="CM66",Depth=2.5)

Temp_CM66_3<- read.csv(file.path("data/2023-11-28 - Data from Adrianne/Temp_data_forBrian_March_June2023/20174666.csv"),
                       skip=1,col.names = c("X","DateTime","Temp")) %>% select(-X) %>%
  mutate(Station="CM66",Depth=3.5)

Temp_CM66_4<- read.csv(file.path("data/2023-11-28 - Data from Adrianne/Temp_data_forBrian_June_Sept2023/20174654.csv"),
                       skip=1,col.names = c("X","DateTime","Temp")) %>% select(-X) %>%
  mutate(Station="CM66",Depth=1.5)

Temp_CM66_5<- read.csv(file.path("data/2023-11-28 - Data from Adrianne/Temp_data_forBrian_June_Sept2023/20174618.csv"),
                       skip=1,col.names = c("X","DateTime","Temp")) %>% select(-X) %>%
  mutate(Station="CM66",Depth=2.5)

Temp_CM66_6<- read.csv(file.path("data/2023-11-28 - Data from Adrianne/Temp_data_forBrian_June_Sept2023/20868837.csv"),
                       skip=1,col.names = c("X","DateTime","Temp")) %>% select(-X) %>%
  mutate(Station="CM66",Depth=5)

Temp_CM66 <- rbind(Temp_CM66_1,Temp_CM66_2,Temp_CM66_3,Temp_CM66_4,Temp_CM66_5,Temp_CM66_6) %>% 
  mutate(DateTime=as.POSIXct(DateTime,format="%m/%d/%y %I:%M:%S %p", tz="America/Los_Angeles")) %>%
  mutate(Date=as.Date(DateTime))

# Station 74

Temp_CM74_1<- read.csv(file.path("data/2023-11-28 - Data from Adrianne/Temp_data_forBrian_March_June2023/20481361.csv"),
                       skip=1,col.names = c("X","DateTime","Temp")) %>% select(-X) %>%
  mutate(Station="CM74",Depth=1.5)

Temp_CM74_2<- read.csv(file.path("data/2023-11-28 - Data from Adrianne/Temp_data_forBrian_March_June2023/20174628.csv"),
                       skip=1,col.names = c("X","DateTime","Temp")) %>% select(-X) %>%
  mutate(Station="CM74",Depth=2.5)

Temp_CM74_3<- read.csv(file.path("data/2023-11-28 - Data from Adrianne/Temp_data_forBrian_March_June2023/20481358.csv"),
                       skip=1,col.names = c("X","DateTime","Temp")) %>% select(-X) %>%
  mutate(Station="CM74",Depth=5)

Temp_CM74_4<- read.csv(file.path("data/2023-11-28 - Data from Adrianne/Temp_data_forBrian_June_Sept2023/20481361.csv"),
                       skip=1,col.names = c("X","DateTime","Temp")) %>% select(-X) %>%
  mutate(Station="CM74",Depth=1.5)

Temp_CM74_5<- read.csv(file.path("data/2023-11-28 - Data from Adrianne/Temp_data_forBrian_June_Sept2023/20174628.csv"),
                       skip=1,col.names = c("X","DateTime","Temp")) %>% select(-X) %>%
  mutate(Station="CM74",Depth=2.5)

Temp_CM74_6<- read.csv(file.path("data/2023-11-28 - Data from Adrianne/Temp_data_forBrian_June_Sept2023/20481358.csv"),
                       skip=1,col.names = c("X","DateTime","Temp")) %>% select(-X) %>%
  mutate(Station="CM74",Depth=5)


Temp_CM74 <- rbind(Temp_CM74_1,Temp_CM74_2,Temp_CM74_3,Temp_CM74_4,Temp_CM74_5,Temp_CM74_6) %>% 
  mutate(DateTime=as.POSIXct(DateTime,format="%m/%d/%y %I:%M:%S %p", tz="America/Los_Angeles")) %>%
  mutate(Date=as.Date(DateTime))

# Station 84

Temp_CM84_1<- read.csv(file.path("data/2023-11-28 - Data from Adrianne/Temp_data_forBrian_March_June2023/20174630.csv"),
                       skip=1,col.names = c("X","DateTime","Temp")) %>% select(-X) %>%
  mutate(Station="CM84",Depth=1.5)

Temp_CM84_2<- read.csv(file.path("data/2023-11-28 - Data from Adrianne/Temp_data_forBrian_March_June2023/20174616.csv"),
                       skip=1,col.names = c("X","DateTime","Temp","D","F","G","H","I")) %>% select("DateTime","Temp") %>%
  mutate(Station="CM84",Depth=2.5)

Temp_CM84_3<- read.csv(file.path("data/2023-11-28 - Data from Adrianne/Temp_data_forBrian_March_June2023/20174629.csv"),
                       skip=1,col.names = c("X","DateTime","Temp","D","F","G","H","I")) %>% select("DateTime","Temp") %>%
  mutate(Station="CM84",Depth=5)

Temp_CM84_4<- read.csv(file.path("data/2023-11-28 - Data from Adrianne/Temp_data_forBrian_June_Sept2023/20174630.csv"),
                       skip=1,col.names = c("X","DateTime","Temp")) %>% select(-X) %>%
  mutate(Station="CM84",Depth=1.5)

Temp_CM84_5<- read.csv(file.path("data/2023-11-28 - Data from Adrianne/Temp_data_forBrian_June_Sept2023/20174616.csv"),
                       skip=1,col.names = c("X","DateTime","Temp")) %>% select(-X) %>%
  mutate(Station="CM84",Depth=2.5)

Temp_CM84_6<- read.csv(file.path("data/2023-11-28 - Data from Adrianne/Temp_data_forBrian_June_Sept2023/20174629.csv"),
                       skip=1,col.names = c("X","DateTime","Temp")) %>% select(-X) %>%
  mutate(Station="CM84",Depth=5)


Temp_CM84 <- rbind(Temp_CM84_1,Temp_CM84_2,Temp_CM84_3,Temp_CM84_4,Temp_CM84_5,Temp_CM84_6) %>% 
  mutate(DateTime=as.POSIXct(DateTime,format="%m/%d/%y %I:%M:%S %p", tz="America/Los_Angeles")) %>%
  mutate(Date=as.Date(DateTime))


#########################################################

# Get DWS/CM51 data from USGS website
# Set date here
start_date<-"2023-03-31"
end_date<-"2023-09-25"
# DWS_temp <- CDECquery(id='DWS', sensor=25, interval='E', start=start_date, end=end_date)
# Data stopped in April 2021, started in Jan 2011

Temp_CM51 <- readNWISuv(siteNumbers = "11455338",
                       parameterCd = "00010",
                       startDate = start_date,
                       endDate = end_date)

Temp_CM51_edit <- Temp_CM51 %>% rename(DateTime=dateTime,Temp=X_00010_00000) %>% mutate(Station="CM51",Depth=1.5,Date=as.Date(DateTime)) %>%
  select(DateTime,Temp,Station,Depth,Date)

# Combine datasets into one
Temp_all<-rbind(Temp_CM51_edit,Temp_CM66,Temp_CM74,Temp_CM84)

#########################################################
#Message from Adrianne Smits [2/13 9:47 AM] 
#Sensors were removed from water on June 7
#Assume that this means that the spring data that ends on June 13 should have
#all data from 6/7 to 6/13 removed
Temp_all_anti <- Temp_all %>% filter(Station %in% c("CM66","CM74","CM84")&
                                  as.Date(Date)>=as.Date("2023-06-07")&
                                  as.Date(Date)<=as.Date("2023-06-13"))

Temp_all <- Temp_all %>% anti_join(Temp_all_anti)
remove(Temp_all_anti)

### Follow the 6 QC steps from Catarina Pien
## See original script here: https://github.com/catarfish/climatechange/blob/master/CDEC_QA_4.Rmd
#########################################################

## QC1) Flag data outside of reasonable temperature range (1-40)

# Data including flags
temp_q1 <- Temp_all %>% mutate(Flag_QC1 = ifelse(Temp<1 | Temp>40, "Y", "N"))  %>%
  mutate(Flag_QC1)

# Flagged values only
temp_q1_b <- temp_q1 %>%
  filter(Flag_QC1 == "Y")

#########################################################

## QC2) Missing values: Flag days with less than n values (missing more than an hour's worth of data)

#1. Count the number of rows per station, date group. (There should only be one row per date)
#2. Flag days where there are less than complete values (every 10 mins for Adrianne's data = 144, every 15 mins for CM51/DWS = 96)
#3. Use leftjoin to add flags to the original data.

# This data frame contains all the dates with less than 138 or 92 values. 
temp_q2_a <- temp_q1 %>%
  filter(Flag_QC1 == "N") %>% # See next comment about removing QC1="Y" values
  group_by(Station, Date, Depth) %>%
  arrange(Station, Date, Depth, DateTime) %>%
  summarise(total = n()) %>%
  mutate(Flag_QC2 = case_when(Station=="CM51"&total < 92 ~ "Y",
                               Station=="CM51"&total >= 92 ~ "N",
                               Station!="CM51"&total < 138 ~ "Y",
                               Station!="CM51"&total >= 138 ~ "N")) %>%
  select(-total)

# Flagged values
temp_q2_b <- temp_q2_a %>%
  filter(Flag_QC2 == "Y")

#Join original dataframe with flagged values based on values NOT in common. 
#based on station, depth, and date
temp_q2 <- temp_q1 %>%
  left_join(temp_q2_a, by = c("Station", "Date","Depth")) %>%
  filter(Flag_QC1 == "N") 
# From Cat: This part is important for QC5 and QC6. Basically removes all values that are not within range (QC1) 

#########################################################
## QC3) Flag if there are 18+ repeating values in a row

#1. Create new columns indicating whether the temperature at x time is the same as that of x-1 timepoint
#2. Take a cumulative sum of all the rows where temperatures are different
#3. Group by the sum and count up the number of rows where the temperature is the same.
#4. Flag the rows where number of repeated values is above our cut-off

# Function to determine whether values are repeating by each station
# Inputs are data frame and x (number of repeating values you want to check for)
# Check if number is same as previous number. If yes, 1. If no, 0.
# Cumulative sum so each time a value repeats, cumulative sum goes up
# Count the number in a row that are the same
# Flag if that number > threshold 

repeating_vals = function(df, x){
  df$same = ifelse(df$Temp == lag(df$Temp, 1, default = 0), 1L, 0L)
  df = df %>%
    mutate(issame = cumsum(df$same == 0L)) %>%
    group_by(Station,Depth, issame) %>%
    mutate(flag = sum(same)+1 ) %>%
    ungroup() %>%
    mutate(Flag_repeats = ifelse(flag > x, "Y", "N"))
  return(df)
}


# Run function repeating values and get rid of the columns we don't need
temp_q3 <- repeating_vals(df = temp_q2, x = 18) %>%
  select(-flag, -issame, -same) %>%
  rename(Flag_QC3 = Flag_repeats) 

# Flagged values
temp_q3_b <- temp_q3 %>%
  filter(Flag_QC3 == "Y")

# Remove unnecessaries
rm(temp_q2_a)
gc()

###########################################################
## QC4) Use the anomalize package to flag anomalies
#* Twitter + GESD is for more for highly seasonal data (however, GESD is extremely slow because it is iterative)
#* STL + IQR if seasonality is not a major factor
#* Trend period depends on personal knowledge of data

library(anomalize)
library(tibbletime)
# see https://business-science.github.io/anomalize/articles/anomalize_methods.html

# Subset data that can use this method (some data have too short of a period)
# temp_test <- temp_H %>% filter(station %in% c("OAD"))
# Convert data frame to table 

temp_q4_a <- as_tbl_time(temp_q3, index = DateTime)

# Anomaly Detection
# time_decompose: separates time series into seasonal, trend, and remainder components
# stl: loess works well when long term trend is present
# twitter: (removes median rather than fitting smoother) - when long-term trend is less dominant than short-term seasonal component
# anomalize: applies anomaly detection methods to remainder component
# time_recompose: calculate limits to separate "normal" data from anomalies
temp_q4_c <- temp_q4_a %>%
  group_by(Station,Depth) %>%
  time_decompose(Temp, method = "stl", trend = "3 months") %>%
  anomalize(remainder, method = "iqr", alpha = 0.05) %>%
  time_recompose() %>% 
  select(c(DateTime, anomaly)) %>%
  as_tibble() 

# Join "anomaly" with rest of the data
temp_q4_d <- inner_join(temp_q3, temp_q4_c, by = c( "DateTime", "Station","Depth"))

# Rename "anomaly" Flag_QC4 for consistency, change No to N and Yes to Y
temp_q4 <- temp_q4_d %>%
  mutate(anomaly = factor(anomaly)) %>%
  mutate(anomaly = recode(anomaly, No = "N", Yes = "Y"))  %>%
  rename(Flag_QC4 = anomaly)

# Flagged values
temp_q4_b <- temp_q4 %>%
  filter(Flag_QC4 == "Y")

###########################################################
### QC5) Spike test

#Anomalize is pretty good, but there are a few single points here and there that don't get detected. 
#1. If difference between value and the value before it > 2, it is flagged.


### ---------------------------------------------
# Q5: Temp - Temp@time-1
# Additionally, if Q5 > 2 (2 degree change in 10-15 mins), flag. 

temp_q5 <- temp_q4 %>%
  group_by(Station,Depth) %>%
  arrange(Station,Depth, DateTime) %>%
  mutate(QC5 = abs(Temp- 0.5 * (lag(Temp, n = 1, default = 0) + lead(Temp, n=1, default = 0))))%>%
  mutate(Flag_QC5 = ifelse((QC5 > 2), "Y", "N"))  %>%
      mutate(Flag_QC5 = replace(Flag_QC5, is.na(Flag_QC5), "N")) %>%
  select(-QC5) %>%
  ungroup()

# Flagged values
temp_q5_b <- temp_q5 %>%
  filter(Flag_QC5 == "Y")

###########################################################
### QC6) Rate of Change Test
#2. Define standard deviation threshold
#3. If difference between value and the value before it > threshold, it is flagged.


# Q6 = Temp - Temp@time-1
# sdev_th: Determined threshold for too high of a rate of change (5 * SD(Temp) over 200 15-min intervals/50 hours or 2 tidal cycles)
# If Q6 > sdev_th, flag.

temp_q6 <- temp_q5 %>%
  group_by(Station,Depth) %>%
  arrange(Station,Depth, DateTime) %>%
  mutate(QC6 = abs(Temp- lag(Temp, n = 1, default = 0)))%>%
  mutate(sdev_th = 5 * runSD(Temp, 200))%>%
  mutate(Flag_QC6 = ifelse((QC6 > sdev_th), "Y", "N"))  %>%
  mutate(Flag_QC6 = replace(Flag_QC6, is.na(Flag_QC6), "N")) %>%
  select(-c(QC6, sdev_th)) %>%
  ungroup()

# Flagged values
temp_q6_b <- temp_q6 %>%
  filter(Flag_QC6 == "Y")

#####################################
## Filter final dataset

# Merge back in q1 data
# Data that were filtered out were not subsequently run under other QC tests, so should be NA, but this messes up other flags, so
# flagged as "N"
temp_q1_table <- temp_q1 %>%
  filter(Flag_QC1 == "Y") %>%
  mutate(Flag_QC2 = "N",
         Flag_QC3 = "N", 
         Flag_QC4 = "N",
         Flag_QC5 = "N",
         Flag_QC6 = "N")

# Combine Flags 
temp_flags <- rbind(temp_q6, temp_q1_table) %>%
  ungroup() %>%
  mutate(Flag1_6 = paste0(Flag_QC1, ",", Flag_QC2, ",", Flag_QC3, ",", Flag_QC4, ",", Flag_QC5, ",", Flag_QC6))
head(temp_flags)

#Flag any day with flag
temp_flags <- temp_flags %>% mutate(any_flag=ifelse(Flag1_6=="N,N,N,N,N,N","N","Y"))

#Create final flag (7) to remove any day with more than 1 hour worth of flagged data
temp_flags_day <- temp_flags %>% filter(any_flag=="N") %>% group_by(Station,Depth,Date) %>%   
  arrange(Station, Date, Depth, DateTime) %>%
  summarise(total = n()) %>%
  mutate(Flag_QC7 = case_when(Station=="CM51"&total < 92 ~ "Y",
                              Station=="CM51"&total >= 92 ~ "N",
                              Station!="CM51"&total < 138 ~ "Y",
                              Station!="CM51"&total >= 138 ~ "N")) 

#Rejoin data
temp_flags <- temp_flags %>% left_join(temp_flags_day)
temp_flags$Flag_QC7 <- ifelse(is.na(temp_flags$Flag_QC7),"Y",temp_flags$Flag_QC7)

#Plot all figures to check removed dataset
ggplot(data=temp_flags) + 
  geom_point(aes(x=DateTime,y=Temp,color=Flag_QC7)) +
  facet_grid(Depth~Station)

#Remove flagged dataset to finalize clean dataset
temp_final <- temp_flags %>%
  filter(Flag_QC7=="N") %>%
  select(Temp, Station, Depth, Date, DateTime) 

#Plot daily summarized data
temp_day <- temp_final %>% group_by(Station, Depth, Date) %>%
  summarise(Temp=mean(Temp)) %>% ungroup()

ggplot(data=temp_day) + 
  geom_point(aes(x=Date,y=Temp)) +
  facet_grid(Depth~Station)
  
#Reconfigure data to wide
#temp_day_final <- temp_final %>% group_by(Station, Depth, Date) %>%
#  summarise(Temp=mean(Temp)) %>% ungroup() %>%
#  mutate(StationCode=paste(Station,Depth,sep="_")) %>%
#  select(-Station, -Depth) %>%
#  spread(StationCode,Temp)


#####################################
## Impute data using linear interpolation
library(imputeTS)

# Check start and end date for the deepest CM66 stations
temp_day_isolate_CM66_3.5 <- temp_day %>% filter(Station=="CM66",Depth==3.5)
temp_day_isolate_CM66_5 <- temp_day %>% filter(Station=="CM66",Depth==5)

# Create complete dataset to impute data
# Check temp_day data and it seems like we should always start from April 1st and end at September 24th
temp_day_edit <- expand.grid(Date=seq.Date(from=as.Date("2023-04-01"),to=as.Date("2023-09-24"),by="day"),
                             Station=c("CM51","CM66","CM74","CM84"),Depth=c(1.5,2.5,3.5,5)) %>%
  mutate(StationCode=paste(Station,Depth,sep="_"))

# Remove combinations that don't exist
temp_day_edit <- temp_day_edit %>% filter(!(StationCode %in% c("CM51_2.5","CM51_3.5","CM51_5","CM74_3.5","CM84_3.5"))) %>%
  # Remove CM66 station data that don't exist
  filter(!(Station=="CM66"&Depth==3.5&Date>max(temp_day_isolate_CM66_3.5$Date))) %>%
  filter(!(Station=="CM66"&Depth==5&Date<min(temp_day_isolate_CM66_5$Date)))

# Add data into template
temp_day_edit_fill <- temp_day_edit %>% left_join(temp_day) %>%
  mutate(Imputed=ifelse(is.na(Temp),"Yes","No"))

# Split data into list
temp_day_split <- temp_day_edit_fill %>% 
  arrange(StationCode, Date)

temp_day_split <- split(temp_day_split, temp_day_split$StationCode)

# Impute data using linear interpolation
for(i in seq_along(temp_day_split)){ 
  temp_day_split[[i]]$Temp <- na_interpolation(temp_day_split[[i]]$Temp)
}

# Rejoin into a single data frame
temp_day_imputed<-bind_rows(temp_day_split, .id = "StationCode")

# Visualize imputed data
ggplot(data=temp_day_imputed) + 
  geom_point(aes(x=Date,y=Temp,color=Imputed)) +
  facet_grid(Depth~Station)

######
# Export out data
write.csv(temp_day_imputed,file.path("data/Temperature_Data_2023_Imputed.csv"))
