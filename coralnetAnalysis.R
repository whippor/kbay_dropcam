#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                             ##
# Coralnet Analysis                                                           ##
# Script created 202311-16                                                    ##
# Data source: Kasitsna Bay Lab - NOAA                                        ##
# R code prepared by Ross Whippo                                              ##
# Last updated 2023-11-16                                                     ##
#                                                                             ##
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:
# Code to analyze drop photo video still extracted from two years of footage
# in Kachemak Bay Alaska. Work for Abby Mish's Semester By the Bay Internship 

# Required Files (check that script is loading latest version):
# coralnetdata.csv

# Associated Scripts:
# FILE.R

# TO DO 

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE OF CONTENTS                                                         ####
#                                                                              +
# LOAD PACKAGES                                                                +
# READ IN AND PREPARE DATA                                                     +
# MANIPULATE DATA                                                              +
#                                                                              +
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                             ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(viridis)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                  ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Import csv of coralnet annotations
coralnetdataraw <- read_csv("data/photoAnalysis/coralnetdata.csv")

# remove NA columns
coralnetdata <- coralnetdataraw %>%
  select_if(~!all(is.na(.))) %>%
  mutate(drop = str_sub(coralnetdata$Name, end = -20)) %>%
  mutate(area = case_when(drop == "KBAY_2017_07_13_GOPRO_193_clip_8.mp4" ~ "1",
                          drop == "KBAY_2017_07_13_GOPRO_194_clip_9.mp4" ~ "1",
                          drop == "KBAY_GOPR0_2016_07_20_325_clip-5.mp4" ~ "1",
                          drop == "KBAY_2016_07_14_GOPRO_341_clip-9.mp4" ~ "2",
                          drop == "KBAY_2016_07_13_512_clip-13.mp4" ~ "2",
                          drop == "KBAY_2016_07_13_634_clip-22.mp4" ~ "2",
                          drop == "KBAY_2016_07_10_191_clip-8.mp4" ~ "3",
                          drop == "KBAY_2016_07_10_275_clip-12.mp4" ~ "3",
                          drop == "KBAY_2016_07_10_150_clip-3.mp4" ~ "3",
                          drop == "KBAY_2016_07_11_181_clip-9.mp4" ~ "4",
                          drop == "KBAY_2016_07_11_143_clip-7.mp4" ~ "4",
                          drop == "KBAY_2017_07_15_GOPRO_094_clip_13.mp4" ~ "4",
                          drop == "KBAY_2017_07_15_GOPRO_083_clip_15.mp4" ~ "4",
                          drop == "KBAY_2017_07_15_GOPRO_082_clip_16.mp4" ~ "4"))

# Import labelset
# labelset from CoralNet
labelset <- read_csv("data/photoAnalysis/KBAY_labelset.csv")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                           ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# total number of each category observed across all photos
ggplot(coralnetdata, aes(x = forcats::fct_infreq(Label))) +
  geom_bar() +
  theme_classic() +
  labs(x="label", y="count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# mean amount of algae detected
coralnetdata %>%
  filter(Label %in% c("split_KE", "coland_KE", "sugar_KE", "broad_KE", "3rib_KE", "ribbn_KE")) %>%
  group_by(Name, Label) %>%
  count(Label) %>%
ggplot(aes(x = Label, y = n)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, shape = 5) +
  labs(x = "algal group", y = "count") +
  theme_classic() 

# total algal cover by area
coralnetdata %>%
  filter(Label %in% c("split_KE", "coland_KE", "sugar_KE", "broad_KE", "3rib_KE", "ribbn_KE")) %>%
  group_by(Name, Label, area) %>%
  count(Label) %>%
  summarise(kelp = sum(n)) %>%
  ggplot(aes(x = Label, y = kelp)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, shape = 5) +
  labs(x = "algal group", y = "count") +
  theme_classic() +
  facet_wrap(.~area, ncol = 1, nrow = 4)
# add year and do year ~ area


############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####