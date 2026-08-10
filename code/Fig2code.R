#figure 2

library(tidyverse)
library(patchwork)

load("data/dataforFig2.RData")

#THIS IS THE FIGURE IN THE PAPER ##################################
space = ggplot(filter(sumfsllsmelt3 , Stratum == "Sac DW Ship Channel", Year != 2025), aes(x = Year, y = percent)) + 
  geom_col(fill = "skyblue", color = "grey23")+
  ylab("Percent of June-October \nDelta Smelt catch in SDWSC")+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  annotate("text", x = 2001, y = 0.9, label = "A", size =10)+
  theme_bw()

space

ggsave("plots/shipchannelPercent.tiff", device = "tiff", width = 6, 
       height =4)

mean(filter(sumfsllsmelt3 , Stratum == "Sac DW Ship Channel")$percent)
#####two panel - space and teme ################################################################

timesmelt = ggplot(avecatchLat, aes(x = LatCat,y = CPUE*10000)) + geom_col(fill = "skyblue", color = "grey23")+
  ylab("Catch per 10000 cubic meters")+ xlab("Latitude")+ 
  geom_vline(data = stations, aes(xintercept = Latitude))+
  geom_text(data = stations, aes(x= Latitude, y =8, label = Site), angle =90, vjust =0, nudge_x = -0.003)+
  
  annotate("text", x = 38.18, y = 8, label = "B", size =10)+
  theme_bw()

timesmelt

space/timesmelt
ggsave("plots/Figure2_twopanel.tiff", device = "tiff", width = 8, height =7)

