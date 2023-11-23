#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                             ##
# Coralnet Analysis                                                           ##
# Script created 202311-16                                                    ##
# Data source: Kasitsna Bay Lab - NOAA                                        ##
# R code prepared by Ross Whippo                                              ##
# Last updated 2023-11-23                                                     ##
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
library(vegan)
library(ggfortify)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                  ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Import csv of coralnet annotations
coralnetdataraw <- read_csv("data/photoAnalysis/coralnetdata.csv", 
                                         col_types = cols(Latitude = col_number(), 
                                                          Longitude = col_number()))

# remove NA columns
coralnetdata <- coralnetdataraw %>%
  select_if(~!all(is.na(.))) %>%
  mutate(drop = str_sub(coralnetdataraw$Name, end = -20)) %>%
  mutate(area = case_when(drop == "KBAY_2017_07_13_GOPRO_193_clip_8.mp4" ~ 4,
                          drop == "KBAY_2017_07_13_GOPRO_194_clip_9.mp4" ~ 4,
                          drop == "KBAY_GOPR0_2016_07_20_325_clip-5.mp4" ~ 4,
                          drop == "KBAY_2016_07_14_GOPRO_341_clip-9.mp4" ~ 3,
                          drop == "KBAY_2016_07_13_512_clip-13.mp4" ~ 3,
                          drop == "KBAY_2016_07_13_634_clip-22.mp4" ~ 3,
                          drop == "KBAY_2017_07_12_GOPRO_148_clip_9.mp4" ~ 3,
                          drop == "KBAY_2017_07_12_GOPRO_146_clip_12.mp4" ~ 3,
                          drop == "KBAY_2017_07_12_GOPRO_145_clip_11.mp4" ~ 3,
                          drop == "KBAY_2016_07_10_191_clip-8.mp4" ~ 2,
                          drop == "KBAY_2016_07_10_275_clip-12.mp4" ~ 2,
                          drop == "KBAY_2016_07_10_150_clip-3.mp4" ~ 2,
                          drop == "KBAY_2017_07_18_GOPRO_964_clip_10.mp4" ~ 2,
                          drop == "KBAY_2017_07_19_GOPRO_253_clip_13.mp4" ~ 2,
                          drop == "KBAY_2017_07_19_GOPRO_251_clip_11.mp4" ~ 2,
                          drop == "KBAY_2016_07_11_181_clip-9.mp4" ~ 1,
                          drop == "KBAY_2016_07_11_143_clip-7.mp4" ~ 1,
                          drop == "KBAY_2017_07_15_GOPRO_094_clip_13.mp4" ~ 1,
                          drop == "KBAY_2017_07_15_GOPRO_083_clip_15.mp4" ~ 1,
                          drop == "KBAY_2017_07_15_GOPRO_082_clip_16.mp4" ~ 1))



# labelset from CoralNet
labelset <- read_csv("data/photoAnalysis/KBAY_labelset.csv")

# Import abiotic data
abiotic <- read_csv("data/areaMeans.csv")

# pull out year
coralnetdata$Year <- year(coralnetdata$Date)

# reshape dataset for kelp only PCA and add abiotics
coraltemp <- coralnetdata %>%
  filter(Label %in% c("split_KE", "coland_KE", "sugar_KE", "broad_KE", "3rib_KE", "ribbn_KE"))
coraltemp$seen <- 1
coralnetwide <- coraltemp %>%
  select(Name, drop, Label, area:seen) %>%
  group_by(Name, drop, Label, area, Year) %>%
  summarise(seen = sum(seen)) %>%
  ungroup() %>%
  left_join(abiotic, by = c("area", "Year")) %>%
  select(Name, drop, area, Year, Label, seen, temperature_no_mean:turbidity_no_sd) %>%
  pivot_wider(names_from = Label, values_from = seen, values_fill = 0)

# reshape for all algae and inverts
coraltemp <- coralnetdata
  coraltemp$seen <- 1
coralnetwideALL <- coraltemp %>%
    select(Name, drop, Label, area:seen) %>%
    group_by(Name, drop, Label, area, Year) %>%
    summarise(seen = sum(seen)) %>%
    ungroup() %>%
    left_join(abiotic, by = c("area", "Year")) %>%
    select(Name, drop, area, Year, Label, seen, temperature_no_mean:turbidity_no_sd) %>%
    pivot_wider(names_from = Label, values_from = seen, values_fill = 0)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                           ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# total number of each category observed across all photos
ggplot(coralnetdata, aes(x = forcats::fct_infreq(Label))) +
  geom_bar() +
  theme_classic() +
  labs(x="label", y="count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# mean amount of kelp detected per photo
coralnetdata %>%
  filter(Label %in% c("split_KE", "coland_KE", "sugar_KE", "broad_KE", "3rib_KE", "ribbn_KE")) %>%
  group_by(Name, Label) %>%
  count(Label) %>%
ggplot(aes(x = Label, y = n)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, shape = 5) +
  labs(x = "algal group", y = "count") +
  theme_classic() 

# total kelp cover by area
coralnetdata %>%
  filter(Label %in% c("split_KE", "coland_KE", "sugar_KE", "broad_KE", "3rib_KE", "ribbn_KE")) %>%
  group_by(Name, Label, area, Year) %>%
  count(Label) %>%
  summarise(kelp = sum(n)) %>%
  ggplot(aes(x = Label, y = kelp)) +
  geom_boxplot(aes(fill = Label)) +
  scale_fill_viridis(discrete = TRUE, option = "D", begin = 0.2, end = 0.9) +
  geom_jitter(width = 0.1, shape = 5) +
  labs(x = "algal group", y = "count") +
  theme_bw() +
  facet_grid(area~Year)




############### PCA OF KELP PREVALENCE AND ABIOTIC PARAMETERS

kelpValues <- coralnetwide[11:16]

# drop site and area
adonis2(kelpValues ~ drop + area, data = coralnetwide, method = 'altGower', na.rm = TRUE)


# run PCA
PCA_results <-  rda(coralnetwide[,c(11:16, 5, 7, 9)], scale = TRUE)
# check that axes are above the mean (per Numerical Ecology)
ev <- PCA_results$CA$eig

# extract PCA coordinates
uscores <- data.frame(PCA_results$CA$u)
uscores1 <- inner_join(rownames_to_column(coralnetwide), rownames_to_column(data.frame(uscores)), by = "rowname")
vscores <- data.frame(PCA_results$CA$v)
# extract explanatory percentages
PCA_summary <- summary(PCA_results)
PCA_import <- as.data.frame(PCA_summary[["cont"]][["importance"]])
var_explained <- PCA_import[2, 1:2]

rownames(vscores) <- c("split kelp", "sugar kelp", "broad kelp",
                       "ribbon kelp", "colander kelp", "three-rib kelp",
                       "temperature", "salinity", "turbidity")

# make final ggplot figure (Points scaled by 1.5)
ggplot(uscores1) + 
  scale_fill_viridis(discrete = TRUE, option = "G", begin = 0.3, end = 0.8, guide = guide_legend(title = "Year"), alpha = 0.5) +
  scale_color_viridis(discrete = TRUE, option = "G", begin = 0.2, end = 0.9, guide = guide_legend(title = "Year"))  +
  geom_segment(data = vscores, aes(x = 0, y = 0, xend = PC1, yend = PC2), arrow=arrow(length=unit(0.2,"cm")),
               alpha = 0.75, color = 'grey30') +
  geom_text(data = vscores, aes(x = PC1, y = PC2, label = rownames(vscores)),
            col = 'red') +
  geom_text(data = vscores[1:6,], aes(x = PC1, y= PC2, label = rownames(vscores[1:6,])),
            col = "black") +
  geom_point(aes(x = PC1*1.5, y = PC2*1.5, fill = as.character(Year), color = as.character(Year),
                 shape = as.character(area)), size = 4) +
  scale_shape_manual(values = c(21, 24, 25, 22)) +
  labs(shape = "Area") +
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0)) +
  labs(x=paste0("PC1: ",round(var_explained[1]*100,1),"%"),
       y=paste0("PC2: ",round(var_explained[2]*100,1),"%"))


################### nMDS

# run the nMDS
kelp_mds <- metaMDS(kelpValues, distance = "altGower")
# extract the 'points' from the nMDS that you will plot in ggplot2
kelp_mds_points <- kelp_mds$points
# turn those plot points into a dataframe that ggplot2 can read
kelp_mds_points <- data.frame(kelp_mds_points)
# join your plot points with your summed species observations from each habitat type
plot_data <- data.frame(kelp_mds_points, coralnetwide[,c(3,4)])



# run the ggplot
ggplot(plot_data, aes(x=MDS1, y=MDS2, 
                            color = as.character(Year))) +  
  labs(x = "nMDS1", y = "nMDS2") +
  theme_classic() + 
  geom_point(size = 5, aes(shape = as.character(area)), alpha = 0.5) + 
  scale_color_viridis(discrete = TRUE, begin = 0.2, end = 0.9, option = "G", name = "Year") 


# MAKE CSV FOR ARCMAP
dropArc <- coralnetdataraw %>%
  select_if(~!all(is.na(.))) %>%
  mutate(drop = str_sub(coralnetdataraw$Name, end = -20)) %>%
  select(drop, Date, Latitude, Longitude)
dropArc$YY <- year(dropArc$Date)
dropArc <- dropArc %>%
  distinct()

write_csv(dropArc, "data/drops.csv")



############### PCA OF ALL LABEL PREVALENCE AND ABIOTIC PARAMETERS

allValues <- coralnetwideALL[11:40]

# drop site and area
adonis2(allValues ~ drop + area, data = coralnetwideALL, method = 'altGower', na.rm = TRUE)


# run PCA
PCA_results <-  rda(coralnetwideALL[,c(11:40, 5, 7, 9)], scale = TRUE)
# check that axes are above the mean (per Numerical Ecology)
ev <- PCA_results$CA$eig

# extract PCA coordinates
uscores <- data.frame(PCA_results$CA$u)
uscores1 <- inner_join(rownames_to_column(coralnetwide), rownames_to_column(data.frame(uscores)), by = "rowname")
vscores <- data.frame(PCA_results$CA$v)
# extract explanatory percentages
PCA_summary <- summary(PCA_results)
PCA_import <- as.data.frame(PCA_summary[["cont"]][["importance"]])
var_explained <- PCA_import[2, 1:2]

# make final ggplot figure (Points scaled by 1.5)
ggplot(uscores1) + 
  scale_fill_viridis(discrete = TRUE, option = "G", begin = 0.3, end = 0.8, guide = guide_legend(title = "Year"), alpha = 0.5) +
  scale_color_viridis(discrete = TRUE, option = "G", begin = 0.2, end = 0.9, guide = guide_legend(title = "Year"))  +
  geom_segment(data = vscores, aes(x = 0, y = 0, xend = PC1, yend = PC2), arrow=arrow(length=unit(0.2,"cm")),
               alpha = 0.75, color = 'grey30') +
  geom_text(data = vscores, aes(x = PC1, y = PC2, label = rownames(vscores)),
            col = 'red') +
  geom_point(aes(x = PC1*1.5, y = PC2*1.5, fill = as.character(Year), color = as.character(Year),
                 shape = as.character(area)), size = 4) +
  scale_shape_manual(values = c(21, 24, 25, 22)) +
  labs(shape = "Area") +
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0)) +
  labs(x=paste0("PC1: ",round(var_explained[1]*100,1),"%"),
       y=paste0("PC2: ",round(var_explained[2]*100,1),"%"))


################### nMDS

# run the nMDS
kelp_mds <- metaMDS(kelpValues, distance = "altGower")
# extract the 'points' from the nMDS that you will plot in ggplot2
kelp_mds_points <- kelp_mds$points
# turn those plot points into a dataframe that ggplot2 can read
kelp_mds_points <- data.frame(kelp_mds_points)
# join your plot points with your summed species observations from each habitat type
plot_data <- data.frame(kelp_mds_points, coralnetwide[,c(3,4)])



# run the ggplot
ggplot(plot_data, aes(x=MDS1, y=MDS2, 
                      color = as.character(Year))) +  
  labs(x = "nMDS1", y = "nMDS2") +
  theme_classic() + 
  geom_point(size = 5, aes(shape = as.character(area)), alpha = 0.5) + 
  scale_color_viridis(discrete = TRUE, begin = 0.2, end = 0.9, option = "G", name = "Year") 


####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####


coralnetdata$drop %>%
  setdiff(dropArc$drop)
unique(coralnetdata$drop)

coraltemp %>%
  select(area, Year) %>%
  unique()


test <- coralnetdata %>%
  filter(drop == "KBAY_2017_07_12_GOPRO_148_clip_9.mp4")
unique(test$Label)

test <- coralnetdata %>%
  filter(area %in% c(2,3) & Year == "2017")
unique(test$Label)
