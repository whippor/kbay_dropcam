#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                             ##
# Dropcam Metadata Extraction                                                 ##
# Script created 2023-10-10                                                   ##
# Data source: Holderied, K; Hondolero, D; Renner, M -  NOAA                  ##
# R code prepared by Ross Whippo                                              ##
# Last updated 2023-10-10                                                     ##
#                                                                             ##
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:

# Data taken from Environmental Drivers: Oceanographic monitoring in Cook Inlet 
# and Kachemak Bay - longterm monitoring project by Kasitsna Bay Lab

# Required Files (check that script is loading latest version):
# 2016_LowerCookInlet_ProcessedCTD.csv
# 2017_Aggregatedfiles.csv

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
# library(sf)
# library(leaflet)
# library(leafem)
# library(av)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                  ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Import CTD data and join files
CTD2016 <- read_csv("data/CTD/2016_LowerCookInlet_ProcessedCTD.csv")
CTD2017 <- read_csv("data/CTD/2017_Aggregatedfiles.csv", 
                       col_types = cols(latitude_DDN = col_double(), 
                                        longitude_DDW = col_double(), `Bottom Depth (m)` = col_double()))

AllCTD <- CTD2016 %>%
  bind_rows(CTD2017) 
AllCTD$Date <- mdy(AllCTD$Date)
AllCTD$Month <- month(AllCTD$Date, label= TRUE)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                           ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

CTDsummary <- AllCTD %>%
  filter(Transect == "AlongBay") %>%
  filter(`Depth (m)` %in% c(1:40)) %>%
  group_by(Station, Month) %>%
  summarise(across(latitude_DDN:`Oxygen Saturation, Garcia & Gordon [mg/l]`, mean))

CTDsummary %>%
  ggplot(aes(x = Month, y = `PAR/ Irradiance, Biospherical/ Licor`, color = Station)) +
  scale_color_viridis(discrete = TRUE) +
  geom_point() +
  geom_smooth(aes(x = Month, y = `PAR/ Irradiance, Biospherical/ Licor`), method = 'loess') 

# Station by month
CTDsummary %>%
  ggplot(aes(x = Month, y = `PAR/ Irradiance, Biospherical/ Licor`)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Station)
  
############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####

# where are the stations at?
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot()

AvgStations <- AllCTD %>%
  filter(Transect == "AlongBay") %>%
  group_by(Station) %>%
  summarise(across(longitude_DDW:latitude_DDN, mean, na.rm = TRUE))
ggplot(data = world) +
  geom_sf() +
  geom_text(data= AvgStations,aes(x=longitude_DDW, latitude_DDN, label=Station),
            color = "darkblue", fontface = "bold", check_overlap = TRUE, size = 2) +
  coord_sf(xlim = c(-150.5, -152.5), ylim = c(59.2, 59.8), expand = FALSE)


checkstation <- AllCTD %>%
  filter(Station %in% c("PtGr"))
