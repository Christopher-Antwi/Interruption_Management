library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(writexl)

# Reading in Feeder, Blender and Tablet Press OG Files

Feeders = read_xlsx("C:/Interruption Management/Interruption_Management/data/raw/Machine data/Feeders.xlsx", sheet = "Feeders_Full")
Blenders = read_xlsx("C:/Interruption Management/Interruption_Management/data/raw/Machine data/Blenderz.xlsx", sheet = "Blenders_Full")
Tablet_Press = read_xlsx("C:/Interruption Management/Interruption_Management/data/raw/Machine data/Tablet_Prezz.xlsx", sheet = "Tablet_Prezz")

# Right Joining Blenders and Tablet_Press into Feeders
Interruptions_DF_WIP = Feeders %>%
  right_join(Blenders, by = "3-Dex")

Interruptions_DF_WIP = Interruptions_DF_WIP %>%
  right_join(Tablet_Press, by = "3-Dex")


write_xlsx(Interruptions_DF_WIP, "Interruptions.xlsx")

# Subsetting Interruptions to Key Parameters

feed_cols  = c("Screw RPM PD1","Screw RPM PD2","Screw RPM PD3","Screw RPM PD4","Screw RPM PD5","Screw RPM PD7","Massflow PD 1","Massflow PD 2","Massflow PD 3","Massflow PD 4","Massflow PD 5","Massflow PD 7","%  PD1","%  PD2","%  PD3", "LiW Feeders 2.%  PD4","LiW Feeders 2.%  PD5","LiW Feeders 2.%  PD7")
blend_cols = c("Massflow Blender 1","Massflow Blender 2")
press_cols = c("Filling Shoe M20M13 speed","Filling Shoe M20M23 speed") 

Interruptions_WIP = Interruptions_DF_WIP %>%
  select(TimeStamp,any_of(feed_cols), any_of(blend_cols), any_of(press_cols))

Interruptions_WIP = Interruptions_WIP %>%
  mutate(
    row_idx = row_number() - 1,
    seconds_elapsed = row_idx %% 60,
    minute_block = row_idx %/% 60,
    TimeStamp_corrected = TimeStamp + seconds(seconds_elapsed)
  ) %>%
  ungroup()

Interruptions_Full = Interruptions_WIP %>%
  mutate(
    TimeStamp = TimeStamp_corrected
  )

#Interruptions = Interruptions %>%
  #filter(TimeStamp_corrected >= as.POSIXct("2018-01-12 08:42:31") &
           #TimeStamp_corrected <= as.POSIXct("2018-01-17 03:40:00"))


# This takes into account the prior line must be running for it to be considered an Interruption.
#Interruptions = Interruptions %>%
 # arrange(TimeStamp_corrected) %>%
  #mutate(
   # machine_running = if_any(any_of(c(feed_cols, blend_cols, press_cols)), ~ . > 0),
    #machine_interruption = !machine_running & lag(machine_running, default = TRUE)
  #)

# This takes is simply checking to see if there is a zero value in one of the key process parameters
Interruptions_Full = Interruptions_Full %>%
  arrange(TimeStamp_corrected) %>%
  mutate(
    machine_running = if_any(any_of(c(feed_cols, blend_cols, press_cols)), ~ . > 0),
    machine_interruption = !machine_running 
  )



# Adding 30 Second Intervals
Interruptions_Full = Interruptions_Full %>%
  mutate(
    interval_30s = floor_date(TimeStamp_corrected, unit = "30 seconds")
  )



# Calculating Duration of Interruptions
time_diff_duration = Interruptions_Full %>%
  arrange(TimeStamp_corrected) %>%
  mutate(
    duration_s = as.numeric(difftime(lead(TimeStamp_corrected), 
                                     TimeStamp_corrected, units = "secs")),
    duration_s = ifelse(machine_interruption, duration_s, 0)
  ) %>%
  group_by(interval_30s) %>%
  mutate(
    interruption_duration_sec = sum(duration_s, na.rm = TRUE)
  ) %>%
  ungroup()



# Start & Stop Duration Time

Interruptions = Interruptions_Full %>%
  arrange(TimeStamp_corrected) %>%
  mutate(
   
    duration_s = as.numeric(difftime(lead(TimeStamp_corrected), 
                                     TimeStamp_corrected, units = "secs")),
    duration_s = ifelse(machine_interruption, duration_s, 0),
    
  
    interruption_start = machine_interruption == TRUE & 
      (lag(machine_interruption, default = FALSE) == FALSE),
    interruption_end   = machine_interruption == TRUE & 
      (lead(machine_interruption, default = FALSE) == FALSE),
    interruption_id    = cumsum(interruption_start)
  ) %>%
  group_by(interval_30s) %>%
  mutate(
    interruption_duration_sec = sum(duration_s, na.rm = TRUE)
  ) %>%
  ungroup()



# Interruption Events as a Linking Table/Index
interruption_events = Interruptions %>%
  filter(machine_interruption == TRUE) %>%
  mutate(
    TimeStamp_corrected = as.POSIXct(TimeStamp_corrected, origin = "1970-01-01", tz = "UTC")
  ) %>%
  group_by(interruption_id) %>%
  summarise(
    interruption_start = min(TimeStamp_corrected),
    interruption_end   = max(TimeStamp_corrected),
    duration_sec  = as.numeric(difftime(max(TimeStamp_corrected), min(TimeStamp_corrected), units = "secs")),
    .groups = "drop"
  ) %>%
  mutate(
    time_between_sec = as.numeric(difftime(interruption_start, 
                                           lag(interruption_end), 
                                           units = "secs"))
  )

interruption_duration = Interruptions %>%
  filter(machine_interruption == TRUE) %>%
  mutate(
    interval_30s = floor_date(as.POSIXct(TimeStamp_corrected), "30 seconds")
  ) %>%
  group_by(interval_30s) %>%
  mutate(
    interruption_duration_sec = sum(duration_s, na.rm = TRUE)
  ) %>%
  ungroup()

interval_lookup = interruption_duration %>%
  distinct(interval_30s, interruption_duration_sec) %>%
  arrange(interval_30s) %>%
  mutate(interval_seq = row_number())

# Finding all intervals each interruption event touches
interruption_intervals = interruption_events %>%
  rowwise() %>%
  mutate(
    intervals_touched = list(
      interval_lookup %>%
        filter(interval_30s >= floor_date(interruption_start, "30 seconds")) %>%
        filter(interval_30s <= floor_date(interruption_end, "30 seconds")) %>%
        select(interval_30s, interval_seq, interruption_duration_sec)
    )
  ) %>%
  unnest(intervals_touched) %>%
  ungroup() %>%
  group_by(interruption_id) %>%
  mutate(
    interval_within_interruption = row_number(), 
    n_intervals_touched = n()
  ) %>%
  ungroup()


# Joining 
interruptions_merge = Interruptions %>%
  select(-interruption_start, -interruption_end)

interruptions_w_duration = interruption_intervals %>%
  left_join(interruptions_merge,
            by = "interruption_id")

duration_merge = interruptions_w_duration %>%
  select(c(interruption_id,TimeStamp,interruption_start,interruption_end, duration_s, duration_sec, interval_30s.x,interval_30s.y, interruption_duration_sec.x, interval_seq, interval_within_interruption, n_intervals_touched,any_of(feed_cols), any_of(blend_cols), any_of(press_cols) ))

write_csv(duration_merge, "Interruption_Events_With_Duration_V1.csv")

library(readxl)

logbook = read_xlsx("data/processed/long_logbook_data.xlsx") %>%
  rename(time = `Observations:`, date = Date, operation = Operation, note = note) %>%
  mutate(
    
    TimeStamp = as.POSIXct(paste0("2018-01-", date, " ", time)),
    
    interval_30s = floor_date(TimeStamp, unit = "30 seconds"),
    
    exogenous = !(operation %in% c("RTD/PAT", "RTD", "PAT", "Quality"))
  )












