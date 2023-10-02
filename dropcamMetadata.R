#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                             ##
# Dropcam Metadata Extraction                                                 ##
# Script created 2023-10-02                                                   ##
# Data source: Field, D; Malhotra, A; Holderied, K; Taylor, C -  NOAA         ##
# R code prepared by Ross Whippo                                              ##
# Last updated 2023-10-02                                                     ##
#                                                                             ##
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:

# Extraction of metadata from dropcam imagery around Kachemak Bay derived from
# https://www.nodc.noaa.gov/archive/arc0152/0209109/2.2/data/0-data/NCCOS-Kachemak-Bay-Mapping_ArchiveDataPackage/

# Required Files (check that script is loading latest version):
# Kachemak_underwater_dropcamera.SHP

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

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                  ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Import a point shapefile, only keep GOPRO video points
drops <- read_sf(dsn = "Kachemak_underwater_dropcamera")
  

algae <- drops %>%
  filter(grepl("macroalgae", Class)) %>%
  filter(grepl("GOPRO", Video_file))
habs <- read_sf(dsn = "Kachemak_Subtidal_Benthic_Habitats")

kbay_basemap <- leaflet() %>% setView(lng = -151.45, lat = 59.55, zoom = 10)

kbay_basemap %>% 
  addTiles() %>%
  addCircles(lng = drops$POINT_X, lat = drops$POINT_Y, 
                   radius = 1,
             color = "blue",
                   popup = paste("Substrate:", drops$Class, "<br>",
                                 "File:", drops$Video_file)) %>%
  addCircles(lng = algae$POINT_X, lat = algae$POINT_Y, 
              radius = 1,
              color = "red",
              popup = paste("Substrate:", algae$Class, "<br>",
                           "File:", algae$Video_file))
  
write_csv(algae, "algaeGOPRO.csv")


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                           ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

plot(shape)

############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####



library('leaflet')

# Fake data
df <- data.frame(lng = c(-5, -10, -15, -20, 25),
                 lat = c(8, 12, 33, 4, 18),
                 size = c(200000, 100000, 800000, 250000, 350000),
                 popup = c('A', 'B', 'C', 'D', 'E'),
                 type = c('A', 'B', 'C', 'D', 'E'),
                 stringsAsFactors = FALSE)

# If you want to set your own colors manually:
pal <- colorFactor(
  palette = c('red', 'blue', 'green', 'purple', 'orange'),
  domain = df$type
)

# If you want to use predefined palettes in the RColorBrewer package:
# Call RColorBrewer::display.brewer.all() to see all possible palettes
pal <- colorFactor(
  palette = 'Dark2',
  domain = df$type
)

leaflet(df) %>%
  addTiles() %>%
  addCircles(lng = ~lng, lat = ~lat, weight = 1, 
             radius = ~size, popup = ~popup, color = ~pal(type))
