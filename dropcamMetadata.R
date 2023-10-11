#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                             ##
# Dropcam Metadata Extraction                                                 ##
# Script created 2023-10-02                                                   ##
# Data source: Field, D; Malhotra, A; Holderied, K; Taylor, C -  NOAA         ##
# R code prepared by Ross Whippo                                              ##
# Last updated 2023-10-10                                                     ##
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
  
# subset for algal presence and GOPRO camera
algae <- drops %>%
  filter(grepl("macroalgae", Class)) %>%
  filter(grepl("GOPRO", Video_file))

# Import habitat type shapes
habs <- read_sf("data/Kachemak_Subtidal_Benthic_Habitats_shapes") %>%
  st_transform("+proj=longlat +datum=WGS84")

# Import bathymetry rasters
bath1 <- read_sf("C:/Users/Ross.Whippo/Desktop/test.grd")

# Load basemap of KBAY
kbay_basemap <- leaflet() %>% setView(lng = -151.45, lat = 59.55, zoom = 10)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                           ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


n <- length(unique(habs$Class))
factorPal <- colorFactor(viridis(n, option = "D"), levels = unique(habs$Class))
kbay_basemap %>% 
  addTiles() %>%
  addPolygons(data = habs,
              fillColor = ~factorPal(Class),
              fillOpacity = 0.4,
              label = ~Class,
              stroke = FALSE) %>%
  addCircles(lng = drops$POINT_X, lat = drops$POINT_Y, 
             radius = 1,
             color = "blue",
             popup = paste("Substrate:", drops$Class, "<br>",
                           "File:", drops$Video_file)) %>%
  # addRasterRGB(data = )
  addCircles(lng = algae$POINT_X, lat = algae$POINT_Y, 
             radius = 1,
             color = "red",
             popup = paste("Substrate:", algae$Class, "<br>",
                           "File:", algae$Video_file)) 
  
 
# write_csv(algae, "algaeGOPRO.csv")

############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####
  
#  color_practice <- read_csv("~/Documents/color_practice.csv") #inputting my own data set 

# addLegend("bottomright", pal = factorPal, values = unique(habs$Class),
# title = "Substrate Type",
# opacity = 1)
