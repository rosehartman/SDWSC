#Let's look for other temperature datasets in the SDWSC
#also turbidity

library(cder)
library(dataRetrieval)
library(tidyverse)
library(sf)
library(lme4)
library(lmerTest)
library(effects)
library(emmeans)

#CDEC has one near Liberty Island

tc = cdec_query("DWS", sensors = 25,
                start.date = ymd("2000-01-01"), end.date = today(),
                durations = "E")



#NWIS has two stations, one near the junvtion with Cache (mile 51) 
#and one about two thirds of the way up

t1 = readNWISdata(sites = c("11455095", "11455338", "11455142"), parameterCd = "00010", 
                  startDate = "2000-01-01T00:00Z", endDate = "2024-10-01T00:00Z",
                  service = "iv")



#USGS are in UTC and C, CDEC are in PST and F. Gerrr.

tc2 = mutate(tc, DateTimeUTC = with_tz(DateTime, tzone = "UTC"),
             Temperature = (Value-32)*5/9,
             Date = date(DateTimeUTC)) %>%
  select(StationID, DateTimeUTC, Date, Temperature) %>%
  filter(Temperature >1)

ggplot(tc2, aes(x = DateTimeUTC, y = Temperature)) + geom_line()


#Look at trends in max, min, mean summer temperatures

tcsum = tc2 %>%
  mutate(Year = year(Date), Month = month(Date), DOY = yday(Date)) %>%
  filter(Month %in% c(6,7,8,9)) %>%
  group_by(Year, Month, Date, DOY) %>%
  summarize(Min = min(Temperature, na.rm =T), Max = max(Temperature, na.rm =T), Mean = mean(Temperature, na.rm = T))

ggplot(tcsum, aes(x = Date, y = Min)) + geom_smooth(method = "lm") + geom_point()
ggplot(tcsum, aes(x = Date, y = Max)) + geom_smooth(method = "lm") + geom_point()
ggplot(tcsum, aes(x = Date, y = Mean)) + geom_smooth(method = "lm") + geom_point()


t1a = rename(t1, StationID = site_no, DateTimeUTC = dateTime) %>%
mutate(Temperature = case_when(!is.na(X_00010_00000) ~ X_00010_00000,
                               !is.na(X_BGC.PROJECT...BGC.PROJECT._00010_00000) ~ X_BGC.PROJECT...BGC.PROJECT._00010_00000,
                               !is.na(X_DWS.BOR...HYDRO.PROJECT._00010_00000) ~ X_DWS.BOR...HYDRO.PROJECT._00010_00000))%>%
  mutate(Date = date(DateTimeUTC)) %>%
  select(StationID, DateTimeUTC, Date, Temperature)

DWSCtemps = bind_rows(t1a, tc2) %>%
  mutate(StationID2 = factor(StationID, levels = c("11455338", "DWS", "11455142", "11455095"),
                            labels = c("M51-11455338", "M56-DWS", "M62-11455142", "M70-11455095"))) 

ggplot(DWSCtemps, aes(x = DateTimeUTC, y = Temperature, color = StationID2))+
  geom_line()


ggplot(DWSCtemps, aes(x = DateTimeUTC, y = Temperature, color = StationID))+
  geom_line()+ facet_wrap(~StationID, nrow =4)


latlongs = data.frame(StationID = c("11455095", "11455142", "11455338", "DWS"),
                      Latitude = c(38.47686944, 38.34166667, 	38.23739167, 38.25611),
                      Longitude = c(-121.5836417, -121.6438889, -121.6739556, 	-121.66667),
                      Marker = c(70,62,51, 56),
                      Region = c("Top", "Middle", "Lower", "Lower"))

########################################################
#Which region are each of these in?
library(deltamapr)

scregions = filter(R_EDSM_Subregions_19P3, SubRegion %in% c("Upper Sacramento River Ship Channel",
                                                            "Lower Sacramento River Ship Channel"))

latlongssf = st_as_sf(latlongs, coords = c("Longitude", "Latitude"), crs = 4326)

ggplot(latlongssf) +
  geom_sf(data = scregions, aes(fill = SubRegion))+
  geom_sf()+
  geom_sf_label(aes(label = StationID))
  


##########################################################


DWSCtemps = left_join(DWSCtemps, latlongs)

save(DWSCtemps, file = "outputs/longTermTemps.RData")

################all the turbidity######################
#

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
  mutate(StationID2 = factor(StationID, levels = c("11455338", "DWS", "11455142", "11455095"),
                            labels = c("M51-11455338", "M56-DWS", "M62-11455142", "M70-11455095"))) %>%
  left_join(latlongs)

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


save(DWSCallWQ, file = "Data/DWSCallWQ.RData")

wqsummary = group_by(DWSCallWQ, StationID) %>%
  filter(!is.na(Temperature)) %>%
  summarize(start = min(Date), end = max(Date))

#################################################\
#what's the oldest water temperature data we have? ####
#https://portal.edirepository.org/nis/mapbrowse?packageid=edi.591.2
watertemp = read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.591.2&entityid=fb147771773b8354667a0b43e3f8e0e4")

watertempdaily = watertemp %>%
  group_by(Date, Station, StationName) %>%
  summarise(Min = min(Temp, na.rm =T), Max = max(Temp, na.rm =T), MeanTemp = mean(Temp, na.rm =T))

summs = watertempdaily %>%
  mutate(Year = year(Date)) %>%
  group_by(Station, StationName, Year) %>%
  summarise(N = n())

SRH = filter(watertempdaily, Station == "SRH")
ggplot(SRH, aes(x = Date, y = MeanTemp)) + geom_point()

#get the more recent data
SRH2 = cdec_query("SRH", 25, start.date = ymd("2020-01-01"), end.date = today())
SRH2b = mutate(SRH2, Date =date(ObsDate), Temp = (Value-32)*5/9) %>%
  rename(Station = StationID) %>%
  group_by(Date, Station) %>%
  summarise(Min = min(Temp, na.rm =T), Max = max(Temp, na.rm =T), MeanTemp = mean(Temp, na.rm =T))

SRHx = bind_rows(SRH, SRH2b)

ggplot(SRHx, aes(x = Date, y = MeanTemp)) + geom_point()

ggplot(SRHx, aes(x = Date, y = Max)) + geom_point()

#just summer and fall

ggplot(filter(SRHx, month(Date) %in% c(6:9)), aes(x = Date, y = MeanTemp)) + geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~month(Date))

ggplot(filter(SRHx, month(Date) %in% c(6:9)), aes(x = Date, y = Max)) + geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~month(Date))+
  ylab("Daily Mean Temperature (degrees C)")+
  theme_bw()


#I'm not sure if the autocorrelation stuff is working, so let's domonthly means

SRHx2 = SRHx %>%
  mutate(Month = month(Date), Year = year(Date)) %>%
  filter(Month %in% c(6:9)) %>%
  group_by(Month, Year) %>%
  summarize(MeanTemp = mean(MeanTemp, na.rm =T))

lmmeans = lm(MeanTemp ~ Year*Month, data = SRHx2)
summary(lmmeans)
#Whelp, I guess not. 

#how about days above a 22 C or 24 C? ##############

SRHdays = SRHx %>%
  mutate(Month = month(Date), Monthf = as.factor(Month), Year = year(Date),
         above22 = case_when(MeanTemp >22 ~ 1,
                             TRUE ~0),
         above24 = case_when(MeanTemp >24 ~ 1,
                             TRUE ~0)) %>%
  group_by(Month, Monthf, Year) %>%
  summarize(above22 = sum(above22, na.rm =T), above24 = sum(above24, na.rm =T)) %>%
  filter(Month %in% c(6:9))

ggplot(SRHdays, aes(x = Year, y = above22)) +
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Month)+
  ylab("Number of days with mean temperature above 22 C")

ggplot(SRHdays, aes(x = Year, y = above24)) +
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Month)

lmdays = lm(above22 ~ Year*Monthf, data = SRHdays)
summary(lmdays)
plot(allEffects(lmdays))
emtrends(lmdays, pairwise ~ Monthf, "Year")

####now for the whole summer #####

SRHdaysY = SRHx %>%
  mutate(Month = month(Date), Monthf = as.factor(Month), Year = year(Date),
         above22 = case_when(MeanTemp >22 ~ 1,
                             TRUE ~0),
         above24 = case_when(MeanTemp >24 ~ 1,
                             TRUE ~0)) %>%
  filter(Month %in% c(6:9)) %>%
  group_by(Year) %>%
  summarize(above22 = sum(above22, na.rm =T), above24 = sum(above24, na.rm =T)) 

lmdaysY = lm(above22 ~ Year, data = SRHdaysY)
summary(lmdaysY)
plot(allEffects(lmdaysY))

#Air temperature  #####
SRHair = cdec_query("SRH", 4, start.date = ymd("2000-01-01"), end.date = today())

SRHair2 = mutate(SRHair, Date =date(ObsDate), Temp = (Value-32)*5/9) %>%
  rename(Station = StationID) %>%
  group_by(Date, Station) %>%
  summarise(MinAir = min(Temp, na.rm =T), MaxAir = max(Temp, na.rm =T), MeanTempAir = mean(Temp, na.rm =T))
#oh, only has data 2010-2024

ggplot(filter(SRHair2, month(Date) %in% c(6:10)), aes(x = Date, y = MeanTempAir)) + geom_point()+
  geom_smooth(method = "lm")

#how correlated is the ship channel and SRH? ###################

tc2sum = mutate(tc, Date =date(ObsDate), Temp = (Value-32)*5/9) %>%
  rename(Station = StationID) %>%
  filter(Temp >5) %>%
  group_by(Date, Station) %>%
  summarise(MinDWS = min(Temp, na.rm =T), MaxDWS = max(Temp, na.rm =T), MeanTempDWS = mean(Temp, na.rm =T))

compareSRHDWS = left_join(tc2sum, select(ungroup(SRHx), -Station)) %>%
  filter(!is.infinite(MaxDWS))

ggplot(compareSRHDWS, aes(x = MeanTemp, y = MeanTempDWS)) + geom_point(alpha = 0.5, color = "grey")+ geom_smooth()+
  geom_abline(slope = 1, intercept =0, size =1) + ylab("Mean Temperature at DWSC mile 56")+
  xlab("mean temperature at station Sac River at Hood")

ggplot(compareSRHDWS, aes(x = Max, y = MaxDWS)) + geom_point(alpha = 0.5, color = "grey")+ geom_smooth()+
  geom_abline(slope = 1, intercept =0, size =1) + ylab("Max Temperature at DWSC mile 56")+
  xlab("max temperature at station Sac River at Hood")

ggplot(compareSRHDWS, aes(x = Max, y = MaxDWS)) + geom_point(alpha = 0.5, color = "grey")+ geom_smooth()+
  geom_abline(slope = 1, intercept =0, size =1) + ylab("Max Temperature at DWSC mile 56")+
  xlab("max temperature at station Sac River at Hood")+
  facet_wrap(~year(Date))


#So the ship channel is a little warmer, particularly in wet years when the river has higher flow and so probably cooler. 
#this is actually in interesting way of extracting the impact of flow versus air tem pon the temperature.

#air temperature at the sacrmento executive airport
# SacAir = read_csv("data/AirTempinSacramento3923532.csv")
# 
# SacAir2 = mutate(SacAir, Month = month(DATE), Year = year(DATE)) %>%
#   rename(Date = DATE) %>%
#   mutate(TempAir = (TAVG-32)*5/9)%>%
#   filter(Month %in% c(6:9))
# 
# ggplot(SacAir2, aes(x = Date, y = TempAir)) + geom_point()+
#   geom_smooth(method = "lm")
# #darn, we're missing a bunch of data. Much larger ranges too
# 
# comparesac2 = left_join(tc2sum, SacAir2) %>%
#   filter(!is.na(TempAir))
# 
# 
# ggplot(comparesac2, aes(x = MeanTempDWS, y = TempAir)) + geom_point(alpha = 0.5, color = "grey")+ geom_smooth()+
#   geom_abline(slope = 1, intercept =0, size =1) + ylab("mean air temperature in Sacramento")+
#   xlab("Mean water temperature in ship channel")+
#   facet_wrap(~year(Date))



#OK, I give up. Let's do trends in water temperature at hood
SRHx = mutate(SRHx, Month = month(Date), Monthf = as.factor(Month), Year = year(Date), DOY = yday(Date)) %>%
  group_by(Year) %>%
  mutate(Lagtemp = lag(MeanTemp), test = Lagtemp -MeanTemp) %>%
  ungroup()

foo = filter(SRHx, abs(test) >2)


library(mgcv)
tempgam = gam(MeanTemp ~ Year + s(DOY, bs = "cc"), data = SRHx)
summary(tempgam)
plot(tempgam, all.terms = T)

templm = lm(MeanTemp ~ Year*Monthf + Lagtemp, data = SRHx)
summary(templm)
plot(templm)
plot(allEffects(templm))

#Let's try just the summer months

SRHsummer = filter(SRHx, Month %in% c(6:9)) %>%
  mutate(Yearf = as.factor(Year))
templm = lm(MeanTemp ~ Year*Monthf + Lagtemp, data = SRHsummer)
summary(templm)
plot(templm)
plot(allEffects(templm))
emtrends(templm, pairwise ~ Monthf, "Year")

#Let's try just the summer months

templmJ = lm(MeanTemp ~ Year + Lagtemp, data = filter(SRHsummer, Month ==7))
summary(templmJ)

#ok, so July has gotten significantly hotter


#now year as a factor
templmyf = lm(MeanTemp ~ Yearf + Lagtemp, data = SRHsummer)
summary(templmyf)
plot(templmyf)
test = summary(emmeans(templmyf, pairwise ~ Yearf))$contrasts
#no. no significatn differences. Am i doing somethign wrong?

templmyf = lm(MeanTemp ~ Yearf*Monthf + Lagtemp, data = SRHsummer)
summary(templmyf)
plot(templmyf)
test = summary(emmeans(templmyf, pairwise ~ Yearf, by = "Monthf"))$contrasts
plot(emmeans(templmyf, pairwise ~ Yearf, by = "Monthf"), comparisons = T)
#Ech, really not working


#maybe the AR1 term instead?
library(itsadug)
#tempgam = gamm(MeanTemp ~ Year*Month + s(DOY),correlation = corAR1(form = ~ Date | Yearf), data = SRHsummer)
tempgam2 = gam(MeanTemp ~ Year*Month + s(DOY), data = SRHsummer)
summary(tempgam)
plot(tempgam2, all.terms = T)
acf(residuals(tempgam2))
rho1 = start_value_rho(tempgam2)

tempgamAR1 = bam(MeanTemp ~ s(Year, by = Month) + s(DOY, bs = "cc"), rho = rho1, data = SRHx)
summary(tempgamAR1)
acf_resid(tempgamAR1)
plot(tempgamAR1, all.terms =T)

###############################################
#Matt mentioned degree dayts ###############
library(pollen)

tempDD = SRHsummer %>%
  group_by(Year) %>%
mutate(DD = gdd(tmax = Max, tmin = Min, tbase = 20, type = "B")) %>%
  ungroup()

ggplot(tempDD, aes(x = DOY, y = DD, color = Yearf)) + geom_line()

DDend = tempDD %>%
  group_by(Year) %>%
  summarize(DDmax = max(DD))

ggplot(DDend, aes(x = Year, y = DDmax)) + geom_point()+ geom_smooth(method = "lm")+
  ylab("Degree Days")

lmdd = lm(DDmax~ Year, data = DDend)
summary(lmdd)

#######bioenergetic thresholds## #######
#I'm not entirely sure what tehse parameters are, but I think theya re temperature related

CTO <- 20 # Rose et al. 2013 
CTM <- 23 #Estimate from Smith and Nobriga 2023 (20.5, 23.6), Rose 23
CTL <- 27  # Rose et al. 2013 
CQ <- 10 # Rose et al. 2013 
CK1 <- 0.40 # Rose et al. 2013
CK4 <- 0.01 # Rose et al. 2013


G1 <- (1 / (CTO - CQ)) * log((0.98 * (1 - CK1)) / (CK1 * 0.02))
L1 <- exp(G1 * (22- CQ)) #see what happens if I put 22 in as temperature
KA <- (CK1 * L1) / (1 + CK1 * (L1 - 1))
G2 <- (1 / (CTL - CTM)) * log((0.98 * (1 - CK4)) / (CK4 * 0.02))
L2 <- exp(G2 * (CTL - 22))
KB <- (CK4 * L2) / (1 + CK4 * (L2 - 1))
ft.con <- KA * KB 

Consumption <- (CA * W0^CB) * P * ft.con #Units --> Specific (g/g/d).

Temps = c(18:27)
Ls = exp(G1 * (Temps-CQ))
foo = data.frame(Temps = Temps, Ls = Ls)
ggplot(foo, aes(x = Temps, y = Ls)) + geom_point()

#OK, let's try doing this. 

SRHx = mutate(SRHx, Tempwieghted = exp(G1*(MeanTemp)))

ggplot(SRHx, aes(x = Date, y = Tempwieghted)) + geom_point()+ geom_smooth(method = "lm")

#just summer
ggplot(filter(SRHx, month(Date) %in% c(6:9)), aes(x = Date, y = Tempwieghted)) + geom_point()+ geom_smooth()


#quick demo for Michelle

testdata = data.frame(SampleID = 1:20, organism.count = rnorm(20, 16, 10))

dataframe = dataframe %>%
  mutate(NewCount = case_when(organism.count < 16 ~ organism.count,
                              organism.count >= 16 ~ organism.count/16))


#save the tempareature data from for Brock

SRHtemp = select(SRHx,Station, Year, DOY, Month, Date, MeanTemp, Min, Max) %>%
rename(MinTemp = Min, MaxTemp = Max)

save(SRHtemp, file = "data/SRHtemp.RData")
