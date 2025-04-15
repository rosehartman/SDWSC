#Maps for the publication with sampling locations

library(tidyverse)
library(deltamapr)
library(sf)
library(ggspatial)
library(pdftools)
library(pdftables)
library(ggrepel)
library(readxl)

load("data/zoopstas.RData")

scregions = filter(R_EDSM_Subregions_19P3, SubRegion %in% c("Upper Sacramento River Ship Channel",
                                                            "Lower Sacramento River Ship Channel"))

#split the lower, middle, upper regions

# lights = pdf_text('data/LightList_V6_2025.pdf') %>%
#   bind_rows()
#bleh

lights = read_excel("data/lights.xlsx") %>%
  filter(!is.na(Light)) 

lights2 = matrix(lights$Light, ncol = 4, byrow = T) %>%
  as.data.frame() 

names(lights2) = c("Number", "Light", "Latitude", "Longitude")

lights2 = mutate(lights2, Latdeg = as.numeric(str_sub(Latitude, 1,2)), 
                Latmin = as.numeric(str_sub(Latitude, 4,5)), 
                Latsec =  as.numeric(str_sub(Latitude, 7,12)),
                longdeg = as.numeric(str_sub(Longitude, 1,3)), 
                longmin = as.numeric((str_sub(Longitude, 5,6))), longsec =  as.numeric(str_sub(Longitude, 8,13)),
                LatitudeDD = Latdeg + Latmin/60+Latsec/3600,
                LongitudeDD = (longdeg+ longmin/60+longsec/3600)*-1) %>%
  mutate(Lightnum = str_extract(Light,"\\d+" )) %>%
  st_as_sf(coords = c("LongitudeDD", "LatitudeDD"), crs = 4326, remove = F)


DWSCpoints = filter(deltamapr::P_Stations, Source %in% c("FMWT", "STN")) %>%
  st_transform(crs = st_crs(scregions)) %>%
  st_join(scregions) %>%
  filter(!is.na(SubRegion))

ggplot()+
  geom_sf(data = WW_Delta, color = "grey", fill = "skyblue")+
  geom_sf(data = scregions, aes(fill = SubRegion))+
  geom_sf(data = zoopssfstas, aes(shape = Source, size = N, fill = Source), alpha = 0.5, color = "grey30")+
  scale_shape_manual(values = c(21,22,23,24))+
  coord_sf(ylim = c(38.2, 38.6), xlim = c(-121.7, -121.5))


ggplot()+
  geom_sf(data = WW_Delta, color = "grey", fill = "skyblue")+
  geom_sf(data = scregions, aes(fill = SubRegion))+
  geom_sf(data = filter(lights2, Lightnum >50))+
  #geom_text_repel(data = filter(lights2, Lightnum >50), aes(label = Lightnum, x = LongitudeDD, y = LatitudeDD))+
  geom_text_repel(data = filter(lights2, Lightnum >50), aes(label = Lightnum, x = LongitudeDD, y = LatitudeDD),
                  label.padding = 0.1, box.padding = 0.1, max.overlaps = 20, size =3)+
  coord_sf(ylim = c(38.2, 38.6), xlim = c(-121.7, -121.5))


#now the regions
cm50 = filter(lights2, Lightnum == 50)
cm60 = filter(lights2, Lightnum == 60)
cm70 = filter(lights2, Lightnum == 70)
cm74 = filter(lights2, Lightnum == 75)
cm85 = filter(lights2, Lightnum == 85)

SClower = filter(scregions,  SubRegion == "Lower Sacramento River Ship Channel") %>%
  st_transform(crs = 4326) %>%
  st_crop(xmin = cm50$LongitudeDD-0.01, xmax = cm60$LongitudeDD+0.01, ymin = cm50$LatitudeDD, ymax = cm60$LatitudeDD) %>%
  mutate(Region = "Lower")

SCmid = filter(scregions,  SubRegion == "Lower Sacramento River Ship Channel") %>%
  st_transform(crs = 4326) %>%
  st_crop(xmin = cm70$LongitudeDD+0.01, xmax = cm60$LongitudeDD-0.01, ymax = cm70$LatitudeDD, ymin = cm60$LatitudeDD)%>%
  mutate(Region = "Middle")


SCmid2 = filter(scregions,  SubRegion == "Lower Sacramento River Ship Channel") %>%
  st_transform(crs = 4326) %>%
  st_crop(xmin = cm70$LongitudeDD+0.01, xmax = cm74$LongitudeDD-0.01, ymax = cm74$LatitudeDD, ymin = cm70$LatitudeDD)%>%
  mutate(Region = "Middle2")
SCmid2v = filter(scregions,  SubRegion == "Upper Sacramento River Ship Channel") %>%
  st_transform(crs = 4326) %>%
  st_crop(xmin = cm70$LongitudeDD+0.01, xmax = cm74$LongitudeDD-0.01, ymax = cm74$LatitudeDD, ymin = cm70$LatitudeDD)%>%
  mutate(Region = "Middle2")


SCtop = filter(scregions,  SubRegion == "Upper Sacramento River Ship Channel") %>%
  st_transform(crs = 4326) %>%
  st_crop(xmin = cm70$LongitudeDD-0.01, xmax = cm85$LongitudeDD+0.01, ymax = cm85$LatitudeDD+0.01, ymin = cm74$LatitudeDD)%>%
  mutate(Region = "Upper")

SCregs2 = bind_rows(SClower, SCmid, SCmid2, SCmid2v, SCtop)

ggplot()+
  geom_sf(data = WW_Delta, color = "grey", fill = "skyblue")+
  geom_sf(data = SCregs2, aes(fill = Region))+
  geom_sf(data = filter(lights2, Lightnum >50))+
  #geom_text_repel(data = filter(lights2, Lightnum >50), aes(label = Lightnum, x = LongitudeDD, y = LatitudeDD))+
  geom_text_repel(data = filter(lights2, Lightnum >50), aes(label = Lightnum, x = LongitudeDD, y = LatitudeDD),
                   box.padding = 0.1, max.overlaps = 20, size =3)+
  coord_sf(ylim = c(38.2, 38.6), xlim = c(-121.7, -121.5))


save(SCregs2, file = "data/SCregs.RData")

#Not sure if I want DOP on there. 


latlongs = data.frame(StationID = c("11455095", "11455142", "11455338", "DWS", "CM66", "CM74", "CM84"),
                      Latitude = c(38.47686944, 38.34166667, 	38.23739167, 38.25611, 38.4048, 38.5064, 38.5593),
                      Longitude = c(-121.5836417, -121.6438889, -121.6739556, 	-121.66667, -121.616, -121.585, -121.568),
                      Marker = c(70,62,51, 56, 66, 74, 84),
                      Region = c("Top", "Middle", "Lower", "Lower", "Middle", "Upper", "Upper"))


latlongssf = st_as_sf(latlongs, coords = c("Longitude", "Latitude"), crs = 4326)

ggplot(latlongssf) +
  geom_sf(data = WW_Delta, color = "grey", fill = "skyblue")+
  geom_sf()+
  geom_sf_text( aes(label = StationID), hjust =0, nudge_x = 0.005)+
  coord_sf(ylim = c(38.2, 38.6), xlim = c(-121.7, -121.5))+
  ylab(NULL)+ xlab(NULL)+
  annotation_scale()+
  annotation_north_arrow(location = "tl")+
  theme_bw()

ggsave("plots/wqmap.tiff", device = "tiff", width = 4, height =8)  


