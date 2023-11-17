#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                             ##
# Dropcam Metadata Extraction                                                 ##
# Script created 2023-10-02                                                   ##
# Data source: Field, D; Malhotra, A; Holderied, K; Taylor, C -  NOAA         ##
# R code prepared by Ross Whippo                                              ##
# Last updated 2023-11-16                                                     ##
#                                                                             ##
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:

# Extraction of metadata from dropcam imagery around Kachemak Bay derived from
# https://www.nodc.noaa.gov/archive/arc0152/0209109/2.2/data/0-data/NCCOS-Kachemak-Bay-Mapping_ArchiveDataPackage/

# Required Files (check that script is loading latest version):
# Kachemak_underwater_dropcamera_shapes
# Kachemak_Subtidal_Benthic_Habitats_shapes

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
library(sf)
library(leaflet)
library(leafem)
library(av)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                  ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Import shapefile of GPS points for camera drops
drops <- read_sf(dsn = "data/Kachemak_underwater_dropcamera_shapes")

# Import csv of file names and depths and remove unneeded columns
KB_dropBathy <- read_csv("data/KB_dropBathy.csv")
depth <- KB_dropBathy %>%
  select(Video_file, Merged_1) %>%
  mutate(depth = round(Merged_1*-1, 2), .keep = "unused")

# Import csv of coralnet annotations
coralnetdata <- read_csv("data/photoAnalysis/coralnetdata.csv")

# add depths to drops file
drops <- drops %>%
  left_join(depth, by = "Video_file")

# separate out all drops by year
dropyear <- drops %>%
  separate_wider_delim(Site_ID, delim = "_", names = c("year", "ID"),
                       too_many = "drop")
drop16 <- dropyear %>%
  filter(year == 2016)
drop17 <- dropyear %>%
  filter(year == 2017)

# subset for algal presence
algae <- drops %>%
  filter(grepl("macroalgae", Class)) 
  
# subset for algal presence and GOPRO camera
goproalgae <- drops %>%
  filter(grepl("macroalgae", Class)) %>%
  filter(grepl("GOPRO", Video_file))

# subset for algal presence, GoPRO camera, and depth < 40
shallowgoproalgae <- drops %>%
  filter(depth < 40) %>%
  filter(grepl("macroalgae", Class)) %>%
  filter(grepl("GOPRO", Video_file))

# identify drops that have been used in analysis
usedvideo <- str_sub(coralnetdata$Name, end = -20)
usedvideo <- unique(usedvideo)
abbysites <- drops %>%
  filter(Video_file %in% usedvideo) %>%
  separate_wider_delim(Site_ID, delim = "_", names = c("year", "ID"))
abby16 <- abbysites %>%
  filter(year == 2016)
abby17 <- abbysites %>%
  filter(year == 2017)

# Import habitat type shapes
habs <- read_sf("data/Kachemak_Subtidal_Benthic_Habitats_shapes") %>%
  st_transform("+proj=longlat +datum=WGS84")

# Load basemap of KBAY
kbay_basemap <- leaflet() %>% setView(lng = -151.45, lat = 59.55, zoom = 10)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                           ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# I've coded the points on the map so that:
# blue = all drop cam points
# yellow = algae detected
# red = algae detected via GoPro
# green = algae detected via GoPro shallower than 40 m depth

n <- length(unique(habs$Class))
factorPal <- colorFactor(viridis(n, option = "D"), levels = unique(habs$Class))
kbay_basemap %>% 
  addTiles() %>%
  addPolygons(data = habs,
              fillColor = ~factorPal(Class),
              fillOpacity = 0.4,
              label = ~Class,
              stroke = FALSE) %>%
  addCircles(lng = algae$POINT_X, lat = algae$POINT_Y, 
             radius = 150,
             color = "limegreen",
             opacity = 1,
             fillOpacity = 1,
             popup = paste("File:", algae$Video_file, "<br>",
                           "Depth:", round(algae$depth,2), "m")) %>%
  addCircles(lng = drops$POINT_X, lat = drops$POINT_Y, 
             radius = 1,
             color = "blue",
             popup = paste("File:", algae$Video_file, "<br>",
                           "Depth:", round(drops$depth,2), "m")) %>%
  addCircles(lng = abby16$POINT_X, lat = abby16$POINT_Y, 
             radius = 200,
             color = "yellow",
             opacity = 1,
             fillOpacity = 1,
             popup = paste("File:", abby16$Video_file, "<br>",
                           "Depth:",  round(abby16$depth,2), "m")) %>%
  addCircles(lng = abby17$POINT_X, lat = abby17$POINT_Y,
             radius = 200,
             color = "red",
             opacity = 1,
             fillOpacity = 1,
             popup = paste("File:", abby17$Video_file, "<br>",
                           "Depth:",  round(abby17$depth,2), "m")) 

# for all drop 2016 vs 2017

addCircles(lng = drop16$POINT_X, lat = drop16$POINT_Y, 
           radius = 200,
           color = "yellow",
           opacity = 1,
           fillOpacity = 1,
           popup = paste("File:", drop16$Video_file, "<br>",
                         "Depth:",  round(drop16$depth,2), "m")) %>%
  addCircles(lng = drop17$POINT_X, lat = drop17$POINT_Y,
             radius = 200,
             color = "red",
             opacity = 1,
             fillOpacity = 1,
             popup = paste("File:", drop17$Video_file, "<br>",
                           "Depth:",  round(drop17$depth,2), "m")) 
 
  # write_csv(algae, "algaeGOPRO.csv")

############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####
  
#  color_practice <- read_csv("~/Documents/color_practice.csv") #inputting my own data set 

# addLegend("bottomright", pal = factorPal, values = unique(habs$Class),
# title = "Substrate Type",
# opacity = 1)
