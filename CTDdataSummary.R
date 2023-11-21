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


# filter out non-Along Bay transect values
CTDsummaryUngrouped <- AllCTD %>%
  filter(Station %in% c("9_6", "AlongBay_POGI") | grepl("AlongBay_\\d", Station)) %>%
  filter(Station != "AlongBay_14") %>%
  filter(Transect != "9") %>%
  filter(`Depth` %in% c(1:40))
# create list of Along Bay transect dates
alongDates <- AllCTD %>%
  filter(grepl("AlongBay_\\d", Station)) %>%
  select(Date) %>%
  distinct(Date)
alongDates$Date <- ymd(alongDates$Date) 
# filter out shared transects that didn't occur during Along Bay
alongBay <- CTDsummaryUngrouped %>%
  filter(Date %in% alongDates$Date)

# order Stations from west to east
alongBay <- alongBay %>%
  mutate(Station = factor(Station, levels = c("AlongBay_POGI",
                                                 "AlongBay_1",
                                                 "AlongBay_2",
                                                 "AlongBay_3",
                                                 "AlongBay_4",
                                                 "AlongBay_5",
                                                 "9_6",
                                                 "AlongBay_7",
                                                 "AlongBay_8",
                                                 "AlongBay_9",
                                                 "AlongBay_10",
                                                 "AlongBay_11",
                                                 "AlongBay_12",
                                                 "AlongBay_13")))


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                           ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# histogram of number of measurements per station
alongBay %>%
  ggplot() +
  geom_histogram(aes(x = Station), stat = "count")

# summarise means per month per Station
CTDsummary <- alongBay %>%
  group_by(Station, Month) %>%
  summarise(across(temperature:turbidity, \(x) mean(x, na.rm = TRUE)))

# turbidity by month line
CTDsummary %>%
  ggplot(aes(x = Month, y = turbidity, color = Station, group = Station)) +
  scale_color_viridis(discrete = TRUE) +
  geom_point() +
  geom_smooth(aes(x = Month, y = turbidity), method = 'loess', se = FALSE) 

# Station turbidity by month individual
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
  mutate(meanTemp = mean(temperature)) %>%
  ggplot(aes(x = Station, y = meanTemp)) +
  geom_point()

# annual mean station sal
CTDsummary %>%
  group_by(Station) %>%
  mutate(meanSal = mean(salinity)) %>%
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


# pairs 
alongBay %>%
ggpairs(columns = 26:28, aes(color = as.character(year), alpha = 0.4),
        lower = list(continuous = "smooth")) +
  theme_bw() +
  scale_color_viridis(discrete = TRUE, option = "D", begin = 0.2, end = 0.7) +
  scale_fill_viridis(discrete = TRUE, option = "D", begin = 0.2, end = 0.7)



##### OUTLIERS

# NO TEMP OUTLIERS

quartiles <- quantile(alongBay$temperature, probs=c(.25, .75), na.rm = TRUE)
IQR <- IQR(alongBay$temperature, na.rm = TRUE, type = 8)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier_temp <- subset(alongBay, alongBay$temperature > Lower & alongBay$temperature < Upper)

temp_outliers <- anti_join(alongBay, data_no_outlier_temp)


# SALINITY

quartiles <- quantile(alongBay$salinity, probs=c(.25, .75), na.rm = TRUE)
IQR <- IQR(alongBay$salinity, na.rm = TRUE, type = 8)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier_sal <- subset(alongBay, alongBay$salinity > Lower & alongBay$salinity < Upper)

sal_outliers <- anti_join(alongBay, data_no_outlier_sal)

# TURBIDITY

quartiles <- quantile(alongBay$turbidity, probs=c(.25, .75), na.rm = TRUE)
IQR <- IQR(alongBay$turbidity, na.rm = TRUE, type = 8)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier_turb <- subset(alongBay, alongBay$turbidity > Lower & alongBay$turbidity < Upper)

turb_outliers <- anti_join(alongBay, data_no_outlier_turb)

# no outliers by Station
data_no_outlier_sal %>%
  group_by(Station) %>%
  tally()

data_no_outlier_turb %>%
  group_by(Station) %>%
  tally()

alongBay %>%
  group_by(Station) %>%
  tally()



##################### ALL ABOVE WITH OUTLIERS REMOVED

alongBay_nooutliers <- alongBay
alongBay_nooutliers <- alongBay_nooutliers %>%
  mutate(temperature_no = temperature) %>%
  mutate(salinity_no = replace(salinity, salinity <= 29.6187, NA)) %>%
  mutate(turbidity_no = replace(turbidity, turbidity >= 1.63320, NA))


# histogram of number of measurements per station
alongBay_nooutliers %>%
  ggplot() +
  geom_histogram(aes(x = Station), stat = "count")

# summarise means per month per Station
CTDsummary <- alongBay_nooutliers %>%
  group_by(Station, Month) %>%
  summarise(across(temperature_no:turbidity_no, \(x) mean(x, na.rm = TRUE)))

# turbidity by month line
CTDsummary %>%
  ggplot(aes(x = Month, y = turbidity_no, color = Station, group = Station)) +
  scale_color_viridis(discrete = TRUE) +
  geom_point() +
  geom_smooth(aes(x = Month, y = turbidity_no), method = 'loess', se = FALSE) 

# Station turbidity by month individual
CTDsummary %>%
  ggplot(aes(x = Month, y = turbidity_no)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Station)

# annual mean station turbidity
CTDsummary %>%
  group_by(Station) %>%
  mutate(meanTurb = mean(turbidity_no)) %>%
  ggplot(aes(x = Station, y = meanTurb)) +
  geom_point()

# annual mean station temp
CTDsummary %>%
  group_by(Station) %>%
  mutate(meanTemp = mean(temperature_no)) %>%
  ggplot(aes(x = Station, y = meanTemp)) +
  geom_point()

# Station temp by month individual
CTDsummary %>%
  ggplot(aes(x = Month, y = temperature_no)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Station)

# annual mean station sal
CTDsummary %>%
  group_by(Station) %>%
  mutate(meanSal = mean(salinity_no)) %>%
  ggplot(aes(x = Station, y = meanSal)) +
  geom_point()

# Station sal by month individual
CTDsummary %>%
  ggplot(aes(x = Month, y = salinity_no)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Station)

# correlations of abiotics

alongBay_nooutliers %>%
  ggplot(aes(x = temperature_no, y = salinity_no)) +
  geom_point() +
  geom_smooth(method = "lm")

alongBay_nooutliers %>%
  ggplot(aes(x = temperature_no, y = turbidity_no)) +
  geom_point() +
  geom_smooth(method = "lm")

alongBay_nooutliers %>%
  ggplot(aes(x = salinity_no, y = turbidity_no)) +
  geom_point() +
  geom_smooth(method = "lm")


# pairs 
alongBay_nooutliers %>%
  ggpairs(columns = 29:31, aes(color = as.character(year), alpha = 0.4),
          lower = list(continuous = "smooth")) +
  theme_bw() +
  scale_color_viridis(discrete = TRUE, option = "D", begin = 0.2, end = 0.7) +
  scale_fill_viridis(discrete = TRUE, option = "D", begin = 0.2, end = 0.7)




###############################################################################
# 2016-2017 ONLY                                                              #
###############################################################################

CTD2016 <- read_csv("data/CTD/CookInletKachemakBay_CTD_2016.csv", skip = 1)
CTD2017 <- read_csv("data/CTD/CookInletKachemakBay_CTD_2017.csv", skip = 1)

AllCTD <- CTD2016 %>%
  bind_rows(list(CTD2017))

AllCTD$Month <- month(AllCTD$Date, label= TRUE)
AllCTD$year <- year(AllCTD$Date)
AllCTD <- AllCTD %>%
  mutate(temperature = `Temperature_ITS90_DegC`) %>%
  mutate(salinity = `Salinity_PSU`) %>%
  mutate(turbidity = `Turbidity`) 


# filter out non-Along Bay transect values
CTDsummaryUngrouped <- AllCTD %>%
  filter(Station %in% c("9_6", "AlongBay_POGI") | grepl("AlongBay_\\d", Station)) %>%
  filter(Station != "AlongBay_14") %>%
  filter(Transect != "9") %>%
  filter(`Depth` %in% c(1:40))
# create list of Along Bay transect dates
alongDates <- AllCTD %>%
  filter(grepl("AlongBay_\\d", Station)) %>%
  select(Date) %>%
  distinct(Date)
alongDates$Date <- ymd(alongDates$Date) 
# filter out shared transects that didn't occur during Along Bay
alongBay <- CTDsummaryUngrouped %>%
  filter(Date %in% alongDates$Date)

# order Stations from west to east
alongBay <- alongBay %>%
  mutate(Station = factor(Station, levels = c("AlongBay_POGI",
                                              "AlongBay_1",
                                              "AlongBay_2",
                                              "AlongBay_3",
                                              "AlongBay_4",
                                              "AlongBay_5",
                                              "9_6",
                                              "AlongBay_7",
                                              "AlongBay_8",
                                              "AlongBay_9",
                                              "AlongBay_10",
                                              "AlongBay_11",
                                              "AlongBay_12",
                                              "AlongBay_13")))


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                           ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# histogram of number of measurements per station
alongBay %>%
  ggplot() +
  geom_histogram(aes(x = Station), stat = "count")

# summarise means per month per Station
CTDsummary <- alongBay %>%
  group_by(Station, Month) %>%
  summarise(across(temperature:turbidity, \(x) mean(x, na.rm = TRUE)))

# turbidity by month line
CTDsummary %>%
  ggplot(aes(x = Month, y = turbidity, color = Station, group = Station)) +
  scale_color_viridis(discrete = TRUE) +
  geom_point() +
  geom_smooth(aes(x = Month, y = turbidity), method = 'loess', se = FALSE) 

# Station turbidity by month individual
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
  mutate(meanTemp = mean(temperature)) %>%
  ggplot(aes(x = Station, y = meanTemp)) +
  geom_point()

# annual mean station sal
CTDsummary %>%
  group_by(Station) %>%
  mutate(meanSal = mean(salinity)) %>%
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


# pairs 
alongBay %>%
  ggpairs(columns = 26:28, aes(color = as.character(year), alpha = 0.4),
          lower = list(continuous = "smooth")) +
  theme_bw() +
  scale_color_viridis(discrete = TRUE, option = "D", begin = 0.2, end = 0.7) +
  scale_fill_viridis(discrete = TRUE, option = "D", begin = 0.2, end = 0.7)



##### OUTLIERS

# TEMP OUTLIERS

quartiles <- quantile(alongBay$temperature, probs=c(.25, .75), na.rm = TRUE)
IQR <- IQR(alongBay$temperature, na.rm = TRUE, type = 8)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier_temp <- subset(alongBay, alongBay$temperature > Lower & alongBay$temperature < Upper)

temp_outliers <- anti_join(alongBay, data_no_outlier_temp)


# SALINITY

quartiles <- quantile(alongBay$salinity, probs=c(.25, .75), na.rm = TRUE)
IQR <- IQR(alongBay$salinity, na.rm = TRUE, type = 8)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier_sal <- subset(alongBay, alongBay$salinity > Lower & alongBay$salinity < Upper)

sal_outliers <- anti_join(alongBay, data_no_outlier_sal)

# TURBIDITY

quartiles <- quantile(alongBay$turbidity, probs=c(.25, .75), na.rm = TRUE)
IQR <- IQR(alongBay$turbidity, na.rm = TRUE, type = 8)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier_turb <- subset(alongBay, alongBay$turbidity > Lower & alongBay$turbidity < Upper)

turb_outliers <- anti_join(alongBay, data_no_outlier_turb)

# no outliers by Station
data_no_outlier_temp %>%
  group_by(Station) %>%
  tally()

data_no_outlier_sal %>%
  group_by(Station) %>%
  tally()

data_no_outlier_turb %>%
  group_by(Station) %>%
  tally()

alongBay %>%
  group_by(Station) %>%
  tally()



##################### ALL ABOVE WITH OUTLIERS REMOVED

alongBay_nooutliers <- alongBay
alongBay_nooutliers <- alongBay_nooutliers %>%
  mutate(temperature_no = replace(temperature, temperature >= 15.2467, NA)) %>%
  mutate(salinity_no = replace(salinity, salinity <= 29.4569, NA)) %>%
  mutate(turbidity_no = replace(turbidity, turbidity >= 1.5847, NA))


# histogram of number of measurements per station
alongBay_nooutliers %>%
  ggplot() +
  geom_histogram(aes(x = Station), stat = "count")

# summarise means per month per Station
CTDsummary <- alongBay_nooutliers %>%
  group_by(Station, Month) %>%
  summarise(across(temperature_no:turbidity_no, \(x) mean(x, na.rm = TRUE)))

# turbidity by month line
CTDsummary %>%
  ggplot(aes(x = Month, y = turbidity_no, color = Station, group = Station)) +
  scale_color_viridis(discrete = TRUE) +
  geom_point() +
  geom_smooth(aes(x = Month, y = turbidity_no), method = 'loess', se = FALSE) 

# Station turbidity by month individual
CTDsummary %>%
  ggplot(aes(x = Month, y = turbidity_no)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Station)

# annual mean station turbidity
CTDsummary %>%
  group_by(Station) %>%
  mutate(meanTurb = mean(turbidity_no)) %>%
  ggplot(aes(x = Station, y = meanTurb)) +
  geom_point()

# annual mean station temp
CTDsummary %>%
  group_by(Station) %>%
  mutate(meanTemp = mean(temperature_no)) %>%
  ggplot(aes(x = Station, y = meanTemp)) +
  geom_point()

# Station temp by month individual
CTDsummary %>%
  ggplot(aes(x = Month, y = temperature_no)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Station)

# annual mean station sal
CTDsummary %>%
  group_by(Station) %>%
  mutate(meanSal = mean(salinity_no)) %>%
  ggplot(aes(x = Station, y = meanSal)) +
  geom_point()

# Station sal by month individual
CTDsummary %>%
  ggplot(aes(x = Month, y = salinity_no)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Station)

# correlations of abiotics

alongBay_nooutliers %>%
  ggplot(aes(x = temperature_no, y = salinity_no)) +
  geom_point() +
  geom_smooth(method = "lm")

alongBay_nooutliers %>%
  ggplot(aes(x = temperature_no, y = turbidity_no)) +
  geom_point() +
  geom_smooth(method = "lm")

alongBay_nooutliers %>%
  ggplot(aes(x = salinity_no, y = turbidity_no)) +
  geom_point() +
  geom_smooth(method = "lm")


# pairs 
alongBay_nooutliers %>%
  select(temperature_no, salinity_no, turbidity_no, year) %>%
  mutate(`Temperature (C)` = temperature_no) %>%
  mutate(`Salinity (ppt)` = salinity_no) %>%
  mutate(`Turbidity (NTU)` = turbidity_no) %>%
  ggpairs(columns = 5:7, aes(color = as.character(year), alpha = 0.5,), 
          lower = list(continuous = "smooth")) +
  theme_bw() +
  scale_color_viridis(discrete = TRUE, option = "D", begin = 0.2, end = 0.7) +
  scale_fill_viridis(discrete = TRUE, option = "D", begin = 0.2, end = 0.7)


############### WRITE SUMMARY CSV MEANS FOR PHOTO ANALYSIS

# all stations
abioticMeans <- alongBay_nooutliers %>%
  select(Station, Date, Transect, 
         Bottom.Depth, Depth, Latitude_DD, Longitude_DD, temperature_no, salinity_no, turbidity_no) %>%
  mutate(year = year(Date)) %>%
  group_by(Station, year) %>%
  summarise(across(Latitude_DD:turbidity_no, list(mean = mean, sd = sd), na.rm = TRUE))
  
write_csv(abioticMeans, "data/abioticMeans.csv")

# 'area' stations
areaMeans <- alongBay_nooutliers %>%
  mutate(area = case_when(Longitude_DD < -151.8 ~ "1",
                          Longitude_DD > -151.79 & Longitude_DD < -151.6 ~ "2",
                          Longitude_DD > -151.59 & Longitude_DD < -151.4 ~ "3",
                          Longitude_DD > -151.3 & Longitude_DD < -151.23 ~ "4")) %>%
  select(Station, Date, Transect, Bottom.Depth, Depth, area, 
         Latitude_DD, Longitude_DD, temperature_no, salinity_no, turbidity_no) %>%
  mutate(Year = year(Date)) %>%
  group_by(area, Year) %>%
  summarise(across(temperature_no:turbidity_no, list(mean = mean, sd = sd), na.rm = TRUE)) %>%
  filter(area %in% c(1:4))

write_csv(areaMeans, "data/areaMeans.csv")


###############################################################################
# MAP OF ABIOTIC GRADIENTS                                                    #
###############################################################################

library(sf)
library(leaflet)
library(leafem)
library(av)

# Load basemap of KBAY
kbay_basemap <- leaflet() %>% setView(lng = -151.45, lat = 59.55, zoom = 10)

# make map

# factorPal <- colorNumeric(viridis(n, option = "D"), domain = NULL)
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
                           "Depth:",  round(abby17$depth,2), "m")) %>%
  addCircles(lng = abiotic$Longitude_DD_mean, lat = abiotic$Latitude_DD_mean,
             radius = 200,
             color = "white",
             opacity = 1,
             fillOpacity = 1,
             popup = paste("Station:", abiotic$Station, "<br>",
                           "Long:", abiotic$Longitude_DD_mean))





world <- map_data("world")
options(ggrepel.max.overlaps = Inf)
ggplot() +
  geom_map(data = world, map = world,aes(long, lat, map_id = region),color = "white", fill = "gray", size = 0.1)+
  #scale_fill_continuous(data=abioticMeans, values=colorRampPalette(brewer.pal(9, 'Reds'))(length(abioticMeans$turbidity_no)))+
  labs(title = "")+
  geom_point(data = abioticMeans, aes(fill = turbidity_no)) +
  scale_color_viridis_d(option = "plasma")+
  xlab("Longitude") + ylab("Latitude")+
  theme(legend.title= element_blank())

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####

# where are the stations at?
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot()

AvgStations <- alongBay %>%
  group_by(Station) %>%
  summarise(across(Latitude_DD:Longitude_DD, mean, na.rm = TRUE))
ggplot(data = world) +
  geom_sf() +
  geom_text(data= AvgStations,aes(x=Longitude_DD, Latitude_DD, label=Station),
            color = "darkblue", fontface = "bold", check_overlap = FALSE, size = 2) +
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

temp_outliers <- anti_join(CTDsummaryUngrouped, data_no_outlier_temp)


# SALINITY

quartiles <- quantile(CTDsummaryUngrouped$salinity, probs=c(.25, .75), na.rm = TRUE)
IQR <- IQR(CTDsummaryUngrouped$salinity, na.rm = TRUE, type = 8)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier_sal <- subset(CTDsummaryUngrouped, CTDsummaryUngrouped$salinity > Lower & CTDsummaryUngrouped$salinity < Upper)

sal_outliers <- anti_join(CTDsummaryUngrouped, data_no_outlier_sal)

# TURBIDITY

quartiles <- quantile(CTDsummaryUngrouped$turbidity, probs=c(.25, .75), na.rm = TRUE)
IQR <- IQR(CTDsummaryUngrouped$turbidity, na.rm = TRUE, type = 8)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier_turb <- subset(CTDsummaryUngrouped, CTDsummaryUngrouped$turbidity > Lower & CTDsummaryUngrouped$turbidity < Upper)

turb_outliers <- anti_join(CTDsummaryUngrouped, data_no_outlier_turb)

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
