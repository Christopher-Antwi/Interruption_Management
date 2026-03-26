
# Clean Data in the Work space

rm(list = ls())

# Attaching Required Packages

source(paste(getwd(),"/Code/","ipak.R",sep = ""))

#### Reading Data sets ####

blender = read_rds(paste(getwd(),"/Data/", "blenders.rds", sep = ""))

feeder1 = read_rds(paste(getwd(),"/Data/", "fdr1.rds", sep = ""))

feeder2 = read_rds(paste(getwd(),"/Data/", "fdr2.rds", sep = ""))

tablet_press = read_rds(paste(getwd(),"/Data/", "tablet_press.rds", sep = ""))

temperature = read_rds(paste(getwd(),"/Data/", "temperature.rds", sep = ""))

humidity = read_rds(paste(getwd(),"/Data/", "humidity.rds", sep = ""))



temp = blender %>% pivot_longer(cols = c(massflow_blender1:oos_conc_blender2_inlet), names_to = "measure", values_to = "value") %>%
  arrange(measure, timestamp) %>% mutate(status = if_else(value == 0, "interruption", "running")) %>% 
  mutate(slot_30sec = if_else(sec <= 29, 0, 30), slot_30min = if_else(min <= 29, 0, 30)) %>%
  group_by(measure) %>% mutate(status_chg = if_else(status == lag(status,1), 0, 1)) %>% 
  replace_na(list(status_chg = 0)) %>% mutate(seq = cumsum(status_chg)) %>% ungroup()

temp1 = temp %>% group_by(measure, seq, status) %>% summarise(start = min(timestamp), end = max(timestamp), dur_secs = n()) %>% ungroup() %>%
  arrange(measure, seq, start)

summary(temp1 %>% filter(status == "interruption"))

hist(temp1 %>% filter(status == "interruption") %>% pull(dur_secs), breaks = 1000)

temp2 = temp %>% group_by(measure, date, hour, slot_30min, seq, status) %>% 
  summarise(start = min(timestamp), end = max(timestamp), dur_secs = n()) %>% ungroup() %>%
  arrange(measure, seq, start)

temp2 = temp %>% group_by(measure, date, hour, min, slot_30sec, seq, status) %>% 
  summarise(start = min(timestamp), end = max(timestamp), dur_secs = n()) %>% ungroup() %>%
  arrange(measure, seq, start)

