#Let's look for other temperature datasets in the SDWSC

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
                            labels = c("M51", "M56", "M62", "M70"))) 

ggplot(DWSCtemps, aes(x = DateTimeUTC, y = Temperature, color = StationID))+
  geom_line()


ggplot(DWSCtemps, aes(x = DateTimeUTC, y = Temperature, color = StationID))+
  geom_line()+ facet_wrap(~StationID, nrow =4)


latlongs = data.frame(StationID = c("11455095", "11455142", "11455338", "DWS"),
                      Latitude = c(38.47686944, 38.34166667, 	38.23739167, 38.25611),
                      Longitude = c(-121.5836417, -121.6438889, -121.6739556, 	-121.66667),
                      Marker = c(70,62,51, 56))
