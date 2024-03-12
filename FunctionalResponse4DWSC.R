# Script to test whether Kenny/Wim's multi-species functional response will work for DWSC
# (and how to make it work if it doesn't)

rm(list=ls())
library(ggplot2)
library(tidyverse)
library(dplyr)

dwsc.zp <- read.csv("data/DWSC_zoops_wIBMR.csv", header=TRUE)

head(dwsc.zp)

df <- dwsc.zp[c("SampleID", "IBMR", "DOY", 'Date', "BPUE", 'Region')]
head(df)

#Hm. IT looks like matt just did this by day of year, not year or region or anything.

d <- data.frame(df %>% 
  group_by(IBMR) %>% 
  mutate(index = row_number()) %>%
  pivot_wider(names_from = "IBMR", values_from = "BPUE", values_fill = 0))
#I'm confused about the index thing here. 


#This is how I would have done it

d2 <- data.frame(df %>% 
                  pivot_wider(names_from = "IBMR", id_cols = c(SampleID, DOY, Date, Region),
                              values_from = "BPUE", values_fill = 0))

# Wim's IBM appendix versions also IBMR versions; only in here to show my work
#naup.VK <- c(1, 75) # not used in TAFS paper
#limno.VK <- c(1, 1.5)
#juv.cal.VK <- c(1, 7.5)
#juv.Pf.VK <- c(1, 1.5) #This taxon isn't in the zp file
#o.cyc.VK <- c(1, 1.5)
#eury.VK <- c(1, 0.375)
#pseud.VK <- c(1, 0.375)
#calan.VK <- c(1, 0.75)
#acart.VK <- c(1, 0.75)
#o.clad.VK <- c(1, 4.5)
#daph.VK <- c(1, 4.5)
#other.VK <- c(1, 7.5) # not used in TAFS paper

# Remove redundancy
vuln1.K <- 0.375
vuln2.K <- 0.75
vuln3.K <- 1.5
vuln4.K <- 4.5
vuln5.K <- 7.5
vuln6.K <- 75

d$allcopnaup.K <- d$allcopnaup/vuln6.K
d$daphnia.K <- d$daphnia/vuln4.K
d$limno.K <- d$limno/vuln3.K
d$othcalad.K <- d$othcalad/vuln2.K
d$othcaladjuv.K <- d$othcaladjuv/vuln5.K
d$othclad.K <- d$othclad/vuln4.K
d$othcyc.K <- d$othcyc/vuln3.K
d$pdiapfor.K <- d$pdiapfor/vuln1.K
d$other.K <- d$other/vuln5.K
d$acartela.K <- d$acartela/vuln2.K
d$eurytem.K <- d$eurytem/vuln1.K

d$Ksum <- 0
for(i in 1:15521){
  d[i,28] <- sum(d[i, 17:27])
}

#Rosie's version
d2 = mutate(d2, allcopnaup.K = allcopnaup/vuln6.K,
            daphnia.K = daphnia/vuln4.K,
            limno.K = limno/vuln3.K,
            othcalad.K = othcalad/vuln2.K,
            othcaladjuv.K = othcaladjuv/vuln5.K,
            othclad.K = othclad/vuln4.K,
            othcyc.K = othcyc/vuln3.K,
            pdiapfor.K = pdiapfor/vuln1.K,
            other.K  = other/vuln5.K,
            acartela.K = acartela/vuln2.K,
            eurytem.K = eurytem/vuln1.K,
            pdiapjuv.K = pdiapjuv/vuln3.K)

d2$Ksum = rowSums(select(d2, daphnia.K:pdiapjuv.K))



# In the "real" BEM, Cmax and fish weight are dynamic, but we're just testing the fxl response so...
# these predict the diet fractions
d$naup <- d$allcopnaup.K/(1 + d$Ksum)
d$daph <- d$daphnia.K/(1 + d$Ksum)
d$limn <- d$limno.K/(1 + d$Ksum)
d$ocal <- d$othcalad.K/(1 + d$Ksum)
d$ojuv <- d$othcaladjuv.K/(1 + d$Ksum)
d$clad <- d$othclad.K/(1 + d$Ksum)
d$cycl <- d$othcyc.K/(1 + d$Ksum)
d$pfor <- d$pdiapfor.K/(1 + d$Ksum)
d$othr <- d$other.K/(1 + d$Ksum)
d$acar <- d$acartela.K/(1 + d$Ksum)
d$eury <- d$eurytem.K/(1 + d$Ksum)

head(d)


#Check to make sure I'm getting something that resembles a Type-II functional response
ggplot(d, aes(log(pdiapfor+1), pfor)) + 
  geom_point(pch=21, size=3, fill='firebrick') +
  theme_bw() +
  theme(text=element_text(size=15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position="none", legend.title=element_blank()) +
  labs(x="log density of P. forbesi", y="Predicted smelt diet composition")

# now I've made a big annoyance of a file so to get to something ggplot will accept...
dgg <- d[c("DOY", "naup", "daph", "limn", "ocal", "ojuv", "clad", "cycl", "pfor", "othr", "acar", "eury")]

eat.gg <- dgg %>% pivot_longer(c('naup', 'daph', 'limn', 'ocal', 'ojuv', 'clad',
                               'cycl', 'pfor', 'othr', 'acar', 'eury'), names_to = "Species", values_to = "Diet")
eat.gg.df <- data.frame(eat.gg); head(eat.gg.df)

# can't make heads or tails of it this way
ggplot(eat.gg.df, aes(DOY, Diet, fill=as.factor(Species))) + 
  geom_area() +
  labs(x="Day of year", y="Diet composition") +
  theme_bw() + labs(color=NULL) + 
  theme(legend.title=element_blank(), legend.position="right", text=element_text(size=15), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# Compute percentages with dplyr
eat.gg.df <- eat.gg.df  %>%
  group_by(DOY, Species) %>%
  summarise(n = sum(Diet)) %>%
  mutate(percentage = n / sum(n))

ggplot(eat.gg.df, aes(DOY, percentage, fill=as.factor(Species))) + 
  geom_area(alpha=0.6 , size=.6, colour="black") +
  labs(x="Day of year", y="Diet composition") +
  theme_bw() + labs(color=NULL) + 
  theme(legend.title=element_blank(), legend.position="right", text=element_text(size=15), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

#Rosie's version

d2long = pivot_longer(d2, cols = c(allcopnaup.K:pdiapjuv.K), names_to = "IBMR", values_to = "DietProportion") %>%
  mutate(Diet = DietProportion/Ksum, Month = month(Date))

#summaryize by month and region

d2longave = group_by(d2long, Month, Region, IBMR) %>%
  summarize(Diet = mean(Diet))

ggplot(d2longave, aes(x = Month, y = Diet, fill = IBMR))+
  geom_col()+
  facet_wrap(~Region)

##############################################################
#load the diet data

diet = read_csv("data/SDWSC diets fo.csv")
