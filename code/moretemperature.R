#Let's look for other temperature datasets in the SDWSC
#also turbidity

library(cder)
library(dataRetrieval)
library(tidyverse)

#CDEC has one near Liberty Island

tc = cdec_query("DWS", sensors = 25,
                start.date = ymd("2010-01-01"), end.date = today(),
                durations = "E")



#NWIS has two stations, one near the junvtion with Cache (mile 51) 
#and one about two thirds of the way up

t1 = readNWISdata(sites = c("11455095", "11455338", "11455142"), parameterCd = "00010", 
                  startDate = "2010-01-01T00:00Z", endDate = "2024-10-01T00:00Z",
                  service = "iv")



#USGS are in UTC and C, CDEC are in PST and F. Gerrr.

tc2 = mutate(tc, DateTimeUTC = with_tz(DateTime, tzone = "UTC"),
             Temperature = (Value-32)*5/9,
             Date = date(DateTimeUTC)) %>%
  select(StationID, DateTimeUTC, Date, Temperature) %>%
  filter(Temperature >1)

t1a = rename(t1, StationID = site_no, DateTimeUTC = dateTime) %>%
mutate(Temperature = case_when(!is.na(X_00010_00000) ~ X_00010_00000,
                               !is.na(X_BGC.PROJECT...BGC.PROJECT._00010_00000) ~ X_BGC.PROJECT...BGC.PROJECT._00010_00000,
                               !is.na(X_DWS.BOR...HYDRO.PROJECT._00010_00000) ~ X_DWS.BOR...HYDRO.PROJECT._00010_00000))%>%
  mutate(Date = date(DateTimeUTC)) %>%
  select(StationID, DateTimeUTC, Date, Temperature)

DWSCtemps = bind_rows(t1a, tc2) %>%
  mutate(StationID = factor(StationID, levels = c("11455338", "DWS", "11455142", "11455095"),
                            labels = c("M51-11455338", "M56-DWS", "M62-11455142", "M70-11455095"))) 

ggplot(DWSCtemps, aes(x = DateTimeUTC, y = Temperature, color = StationID))+
  geom_line()


ggplot(DWSCtemps, aes(x = DateTimeUTC, y = Temperature, color = StationID))+
  geom_line()+ facet_wrap(~StationID, nrow =4)


latlongs = data.frame(StationID = c("11455095", "11455142", "11455338", "DWS"),
                      Latitude = c(38.47686944, 38.34166667, 	38.23739167, 38.25611),
                      Longitude = c(-121.5836417, -121.6438889, -121.6739556, 	-121.66667),
                      Marker = c(70,62,51, 56))
DWSCtemps = left_join(DWSCtemps, latlongs)

save(DWSCtemps, file = "outputs/longTermTemps.RData")

######################################
#all the turbidity

#does it have turbitity?

turbc = cdec_query("DWS", sensors = c(27, 221),
                   start.date = ymd("2010-01-01"), end.date = today(),
                   durations = "E")

#I can never figure out which turbitiyd value people use

test = filter(parameterCdFile, str_detect(parameter_nm, "Turbidity"))
turb1 = readNWISdata(sites = c("11455095", "11455338", "11455142"), parameterCd = test$parameter_cd, 
                     startDate = "2010-01-01T00:00Z", endDate = "2024-10-01T00:00Z",
                     service = "iv")



#USGS are in UTC and FNU, CDEC are in PST and NTU Gerrr.

turbc2 = mutate(turbc, DateTimeUTC = with_tz(DateTime, tzone = "UTC"),
             Turbidity = Value,
             Date = date(DateTimeUTC)) %>%
  select(StationID, DateTimeUTC, Date, Turbidity) #%>%
  #filter(Turbidity >0, Turbidity <700)

#NWIS has three different entrys for turbidity, depending on the project. I'm not super clear on whether there are any actual differnces.
turb1a = rename(turb1, StationID = site_no, DateTimeUTC = dateTime) %>%
   mutate(Turbidity = case_when(!is.na(X_63680_00000) ~ X_63680_00000),
          Turbidity = case_when(is.na(Turbidity) & !is.na(X_BGC.PROJECT...BGC.PROJECT.TS213.YSI.EXO._63680_00000) ~ X_BGC.PROJECT...BGC.PROJECT.TS213.YSI.EXO._63680_00000,
                                TRUE ~ Turbidity))%>%
  mutate(Date = date(DateTimeUTC)) %>%
  select(StationID, DateTimeUTC, Date, Turbidity) %>%
 filter(!is.na(Turbidity))#%>%
 # filter(Turbidity >0, Turbidity <700)

DWSCturb = bind_rows(turb1a, turbc2) %>%
  mutate(StationID = factor(StationID, levels = c("11455338", "DWS", "11455142", "11455095"),
                            labels = c("M51-11455338", "M56-DWS", "M62-11455142", "M70-11455095"))) 

ggplot(DWSCturb, aes(x = DateTimeUTC, y = Turbidity, color = StationID))+
   geom_line()+
  facet_wrap(StationID~.)
  # geom_line(data = turb1, aes(x = dateTime, y = X_DWS.BOR...HYDRO.PROJECT.TS213..YSI.EXO._63680_00000),
  #           inherit.aes = FALSE, color = "red")


ggplot(turb1, aes(x = dateTime, y = X_BGC.PROJECT...BGC.PROJECT.TS213.YSI.EXO._63680_00000,
                  color = site_no))+geom_line()

DWSCallWQ = full_join(DWSCturb, DWSCtemps)

DWSCturbmean = group_by(DWSCturb, Date, StationID) %>%
  summarise(Turb = mean(Turbidity, na.rm =T))


ggplot(DWSCturbmean, aes(x = Date, y = Turb, color = StationID))+
  geom_line()

