#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                             ##
# Dropcam Metadata Extraction                                                 ##
# Script created 2023-10-10                                                   ##
# Data source: Holderied, K; Hondolero, D; Renner, M -  NOAA                  ##
# R code prepared by Ross Whippo                                              ##
# Last updated 2023-11-17                                                     ##
#                                                                             ##
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:

# Data taken from Environmental Drivers: Oceanographic monitoring in Cook Inlet 
# and Kachemak Bay - longterm monitoring project by Kasitsna Bay Lab

# Required Files (check that script is loading latest version):
# CookInletKachemakBay_CTD_2017.csv
# CookInletKachemakBay_CTD_2016.csv
# CookInletKachemakBay_CTD_2015.csv
# CookInletKachemakBay_CTD_2014.csv
# CookInletKachemakBay_CTD_2013.csv
# CookInletKachemakBay_CTD_2012.csv

# Associated Scripts:
# FILE.R

# TO DO 

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE OF CONTENTS                                                         ####
#                                                                              +
# RECENT CHANGES TO SCRIPT                                                     +
# LOAD PACKAGES                                                                +
# READ IN AND PREPARE DATA                                                     +
# MANIPULATE DATA                                                              +
#                                                                              +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RECENT CHANGES TO SCRIPT                                                  ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                             ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(viridis)
library(GGally)
# library(sf)
# library(leaflet)
# library(leafem)
# library(av)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                  ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Import CTD data and join files
CTD2012 <- read_csv("data/CTD/CookInletKachemakBay_CTD_2012.csv", skip = 1)
CTD2013 <- read_csv("data/CTD/CookInletKachemakBay_CTD_2013.csv", skip = 1)
CTD2014 <- read_csv("data/CTD/CookInletKachemakBay_CTD_2014.csv", skip = 1)
CTD2015 <- read_csv("data/CTD/CookInletKachemakBay_CTD_2015.csv", skip = 1)
CTD2016 <- read_csv("data/CTD/CookInletKachemakBay_CTD_2016.csv", skip = 1)
CTD2017 <- read_csv("data/CTD/CookInletKachemakBay_CTD_2017.csv", skip = 1)

AllCTD <- CTD2012 %>%
  bind_rows(list(CTD2013, CTD2014, CTD2015, CTD2016, CTD2017))



# CTD2016 <- read_csv("data/CTD/2016_LowerCookInlet_ProcessedCTD.csv")
# CTD2017 <- read_csv("data/CTD/2017_Aggregatedfiles.csv", 
#                        col_types = cols(latitude_DDN = col_double(), 
#                                         longitude_DDW = col_double(), `Bottom Depth (m)` = col_double()))
# AllCTD$Date <- mdy(AllCTD$Date)

AllCTD$Month <- month(AllCTD$Date, label= TRUE)
AllCTD$year <- year(AllCTD$Date)
AllCTD <- AllCTD %>%
  mutate(temperature = `Temperature_ITS90_DegC`) %>%
  mutate(salinity = `Salinity_PSU`) %>%
  mutate(turbidity = `Turbidity`) 

CTDsummaryUngrouped <- AllCTD %>%
  filter(Station %in% c("4_3", "9_6", "AlongBay_POGI") | grepl("AlongBay_\\d", Station)) %>%
  filter(Station != "AlongBay_14") %>%
  filter(`Depth` %in% c(1:40))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                           ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# reduced to KB01 - KB13 along bay only and only top 40m
CTDsummary <- AllCTD %>%
  filter(Transect == "AlongBay") %>%
  filter(Station %in% c("4_3", "9_6") | grepl("^KB", Station)) %>%
  filter(Station != "KB04east") %>%
  filter(`Depth (m)` %in% c(1:40)) %>%
  group_by(Station, Month) %>%
  summarise(across(latitude_DDN:turbidity, \(x) mean(x, na.rm = TRUE)))

# turbidity by month
CTDsummary %>%
  ggplot(aes(x = Month, y = turbidity, color = Station, group = Station)) +
  scale_color_viridis(discrete = TRUE) +
  geom_point() +
  geom_smooth(aes(x = Month, y = turbidity), method = 'loess', se = FALSE) 

# Station turbidity by month
CTDsummary %>%
  ggplot(aes(x = Month, y = turbidity)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Station)

# annual mean station turbidity
CTDsummary %>%
  group_by(Station) %>%
  mutate(meanTurb = mean(turbidity)) %>%
  ggplot(aes(x = Station, y = meanTurb)) +
  geom_point()

# annual mean station temp
CTDsummary %>%
  group_by(Station) %>%
  mutate(meanTemp = mean( `Temperature (deg C)`)) %>%
  ggplot(aes(x = Station, y = meanTemp)) +
  geom_point()

# annual mean station sal
CTDsummary %>%
  group_by(Station) %>%
  mutate(meanSal = mean(`Salinity, Practical [PSU]`)) %>%
  ggplot(aes(x = Station, y = meanSal)) +
  geom_point()

# correlations of abiotics

CTDsummaryUngrouped %>%
  ggplot(aes(x = temperature, y = salinity)) +
  geom_point() +
  geom_smooth(method = "lm")

CTDsummaryUngrouped %>%
  ggplot(aes(x = temperature, y = log(turbidity))) +
  geom_point() +
  geom_smooth(method = "lm")

CTDsummaryUngrouped %>%
  ggplot(aes(x = salinity, y = log(turbidity))) +
  geom_point() +
  geom_smooth(method = "lm")

CTDsummaryUngrouped %>%
  filter(year %in% c(2012, 2013)) %>%
  filter(salinity > 28) %>%
  filter(turbidity < 4) %>%
ggpairs(columns = 26:28, aes(color = as.character(year), alpha = 0.4),
        lower = list(continuous = "smooth")) +
  theme_bw() +
  scale_color_viridis(discrete = TRUE, option = "D", begin = 0.2, end = 0.7) +
  scale_fill_viridis(discrete = TRUE, option = "D", begin = 0.2, end = 0.7)


############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####

# where are the stations at?
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot()

AvgStations <- CTDsummaryUngrouped %>%
  group_by(Station) %>%
  summarise(across(Latitude_DD:Longitude_DD, mean, na.rm = TRUE))
ggplot(data = world) +
  geom_sf() +
  geom_text(data= AvgStations,aes(x=Longitude_DD, Latitude_DD, label=Station),
            color = "darkblue", fontface = "bold", check_overlap = TRUE, size = 2) +
  coord_sf(xlim = c(-150.5, -152.5), ylim = c(59.2, 59.8), expand = FALSE)


checkstation <- AllCTD %>%
  filter(Station %in% c("PtGr"))
checkstation1 <- AllCTD %>%
  filter(Station %in% c("KB04"))
checkstation2 <- AllCTD %>%
  filter(Station %in% c("KB04east"))


##### OUTLIERS

# NO TEMP OUTLIERS

quartiles <- quantile(CTDsummaryUngrouped$temperature, probs=c(.25, .75), na.rm = TRUE)
IQR <- IQR(CTDsummaryUngrouped$temperature, na.rm = TRUE, type = 8)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier_temp <- subset(CTDsummaryUngrouped, CTDsummaryUngrouped$temperature > Lower & CTDsummaryUngrouped$temperature < Upper)

dim(data_no_outlier_temp)


# SALINITY

quartiles <- quantile(CTDsummaryUngrouped$salinity, probs=c(.25, .75), na.rm = TRUE)
IQR <- IQR(CTDsummaryUngrouped$salinity, na.rm = TRUE, type = 8)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier_sal <- subset(CTDsummaryUngrouped, CTDsummaryUngrouped$salinity > Lower & CTDsummaryUngrouped$salinity < Upper)

dim(data_no_outlier_sal)

# TURBIDITY

quartiles <- quantile(CTDsummaryUngrouped$turbidity, probs=c(.25, .75), na.rm = TRUE)
IQR <- IQR(CTDsummaryUngrouped$turbidity, na.rm = TRUE, type = 8)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier_turb <- subset(CTDsummaryUngrouped, CTDsummaryUngrouped$turbidity > Lower & CTDsummaryUngrouped$turbidity < Upper)

dim(data_no_outlier_turb)

data_no_outlier_sal %>%
  group_by(Station) %>%
  tally()
  
data_no_outlier_turb %>%
  group_by(Station) %>%
  tally()

CTDsummaryUngrouped %>%
  group_by(Station) %>%
  tally()

CTDsummaryUngrouped %>%
ggplot() +
  geom_histogram(aes(x = Station), stat = "count")

alongDates <- AllCTD %>%
  filter(grepl("AlongBay_\\d", Station)) %>%
  select(Date) %>%
  distinct(Date)

alongDates$Date <- ymd(alongDates$Date) 

alongUnion <- CTDsummaryUngrouped %>%
  filter(Date %in% alongDates$Date)

alongUnion %>%
  ggplot() +
  geom_histogram(aes(x = Station), stat = "count")
