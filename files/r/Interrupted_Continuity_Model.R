library(tidyverse)
library(dplyr)
library(readxl)
library(writexl)

# Reading in Feeder, Blender and Tablet Press OG Files

Feeders = read_xlsx("Feeders.xlsx", sheet = "Feeders_Full")
Blenders = read_xlsx("Blenderz.xlsx", sheet = "Blenders_Full")
Tablet_Press = read_xlsx("Tablet_Prezz.xlsx", sheet = "Tablet_Prezz")

# Right Joining Blenders and Tablet_Press into Feedrs
Interruptions_DF_WIP = Feeders %>%
  right_join(Blenders, by = "3-Dex")

Interruptions_DF_WIP = Interruptions_DF_WIP %>%
  right_join(Tablet_Press, by = "3-Dex")


write_xlsx(Interruptions_DF_WIP, "Interruptions.xlsx")

# Subsetting Interruptions to Key Parameters

feed_cols  = c("Screw RPM PD1","Screw RPM PD2","Screw RPM PD3"," Screw RPM PD4","Screw RPM PD5","Screw RPM PD7","Massflow PD 1","Massflow PD 2","Massflow PD 3","Massflow PD 4","Massflow PD 5","Massflow PD 7","%  PD1","%  PD2","%  PD3", "LiW Feeders 2.%  PD4","LiW Feeders 2.%  PD5","LiW Feeders 2.%  PD7")
blend_cols = c("Massflow Blender 1","Massflow Blender 2")
press_cols = c("Filling Shoe M20M13 speed","Filling Shoe M20M23 speed") 

Interruptions_WIP = Interruptions_DF_WIP %>%
  select(TimeStamp,any_of(feed_cols), any_of(blend_cols), any_of(press_cols))
  

# Creating Flags based on Stops



df_state <- df %>%
  arrange(TimeStamp) %>%
  mutate(
    F_stop = if_all(all_of(feed_cols),  ~ !is.na(.x) & .x == 0),
    B_stop = if_all(all_of(blend_cols), ~ !is.na(.x) & .x == 0),
    P_stop = if_all(all_of(press_cols), ~ !is.na(.x) & .x == 0)
  )

