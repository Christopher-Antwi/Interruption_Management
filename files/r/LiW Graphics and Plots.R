library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)


# All Feed Factors Plot

LiW_long = LiW_Feeders %>%
  pivot_longer(
    cols = starts_with("Feed Factor"),
    names_to = "Feeder",
    values_to = "FeedFactor")

All_Feed_Factors_OT = ggplot(LiW_long, aes(x = TimeStamp, y = FeedFactor, color = Feeder)) +
  geom_line(linewidth = 0.4) +
  labs(title = "All Feed Factors Over Time",
       x = "Time",
       y = "Feed Factor") +
  theme_minimal()

ggsave(
  filename = "Feed_Factors_Over_Time.png",
  plot = All_Feed_Factors_OT,
  width = 12,
  height = 6,
  dpi = 300
)


# Based on All, need to take closer look at PD3, PD4, PD1, PD7 especially


LiW_Feed_Factor_PD1 = ggplot(LiW_Feeders, aes(x = TimeStamp, y = `Feed Factor PD1`)) +
  geom_line() +
  labs(title = "Feed Factor PD1 Over Time (Microcrystalline Cellulose (Diluent))",
       x = "Time",
       y = "Feed Factor PD1") +
  theme_minimal()


LiW_Feed_Factor_PD3 = ggplot(LiW_Feeders, aes(x = TimeStamp, y = `Feed Factor PD3`)) +
  geom_line() +
  labs(title = "Feed Factor PD3 Over Time (Acetaminophen Powder, APAP_P)",
       x = "Time",
       y = "Feed Factor PD3") +
  theme_minimal()


LiW_Feed_Factor_PD4 = ggplot(LiW_Feeders, aes(x = TimeStamp, y = `Feed Factor PD4`)) +
  geom_line() +
  labs(title = "Feed Factor PD4 Over Time (Microcrystalline Cellulose, Avicel)",
       x = "Time",
       y = "Feed Factor PD4") +
  theme_minimal()

LiW_Feed_Factor_PD7 = ggplot(LiW_Feeders, aes(x = TimeStamp, y = `Feed Factor PD7`)) +
  geom_line() +
  labs(title = "Feed Factor PD7 Over Time (Magnesium Stearate, Ligamed MF-2-V (Lubricant) ",
       x = "Time",
       y = "Feed Factor PD7") +
  theme_minimal()

ggsave(
  filename = "LiW_Feedfactor_PD7.png",
  plot = LiW_Feed_Factor_PD7,
  width = 12,
  height = 6,
  dpi = 300
)


# All Screw RPM Plot

LiW_long_ScrewRPM = LiW_Feeders %>%
  pivot_longer(
    cols = starts_with("Screw RPM"),
    names_to = "Screw Speed",
    values_to = "ScrewRPM")

All_ScrewRPM_OT = ggplot(LiW_long_ScrewRPM, aes(x = TimeStamp, y = ScrewRPM, color = `Screw Speed`)) +
  geom_line(linewidth = 0.4) +
  labs(title = "All Screw RPM  Over Time",
       x = "Time",
       y = "Screw RPM") +
  theme_minimal()

ggsave(
  filename = "All_ScrewRPM_OT.png",
  plot = All_ScrewRPM_OT,
  width = 12,
  height = 6,
  dpi = 300
)





# Massflow Rate Over Time

LiW_long_Massflow = LiW_Feeders %>%
  pivot_longer(
    cols = starts_with("Massflow"),
    names_to = "Mass Flow PD",
    values_to = "massflow")

All_Massflow_OT = ggplot(LiW_long_Massflow, aes(x = TimeStamp, y = massflow, color = `Mass Flow PD`)) +
  geom_line(linewidth = 0.4) +
  labs(title = "All LiW Massflow  Over Time",
       x = "Time",
       y = "Mass Flow") +
  theme_minimal()


ggsave(
  filename = "Massflow_OT.png",
  plot = All_Massflow_OT,
  width = 12,
  height = 6,
  dpi = 300
)
