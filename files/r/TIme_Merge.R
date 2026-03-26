library(tidyverse)
library(dplyr)
library(readxl)
library(writexl)

#Interruptions Only
interruptions_only = Interruptions_Full %>% group_by(machine_running = FALSE, machine_interruption = TRUE)

blend_interruptions = interruptions_only %>%
  select(TimeStamp_corrected, interval_30s, all_of(blend_cols))  %>%
  group_by(`Massflow Blender 1` <= 0,`Massflow Blender 2` <= 0)


feeder_interruptions <- interruptions_only %>%
  select(TimeStamp_corrected, interval_30s,
         `Screw RPM PD1`, `Screw RPM PD2`, `Screw RPM PD3`,
         `Screw RPM PD4`, `Screw RPM PD5`, `Screw RPM PD7`,
         `Massflow PD 1`, `Massflow PD 2`, `Massflow PD 3`, `Massflow PD 4`,
         `Massflow PD 5`, `Massflow PD 7`,`%  PD1`, `%  PD2`, `%  PD3`,  `LiW Feeders 2.%  PD4`, `LiW Feeders 2.%  PD5`, `LiW Feeders 2.%  PD7`)

tablet_interruptions = interruptions_only %>%
  select(TimeStamp_corrected, interval_30s, all_of(press_cols))  %>%
  group_by(`Filling Shoe M20M13 speed` <= 0,`Filling Shoe M20M23 speed` <= 0)

write_xlsx(tablet_interruptions, "tablet_interruptions.xlsx")
write_xlsx(feeder_interruptions, "feeder_interruptions.xlsx")
write_xlsx(blend_interruptions, "blend_interruptions.xlsx")
ptime_merge = interruptions_only %>% select(TimeStamp) %>% mutate(TimeStamp = ymd_hms(TimeStamp))

time_merge = interruptions_only %>% mutate(date = as.Date(TimeStamp), day = day(TimeStamp), 
                       hour = hour(TimeStamp), minute = minute(TimeStamp), second = second(TimeStamp)) %>%
  replace_na(list(hour = 0, minute = 0, second = 0))

time_merge = time_merge %>%
  mutate(tm_dt = as.POSIXct(paste(date, sprintf("%02d:%02d:%02d", hour, minute, second)),
                            format = "%Y-%m-%d %H:%M:%S"))


time_merge = time_merge %>% mutate(slot = if_else(minute <= 29, 30, 0))


#Reference Table Creation

interruption_events = interruptions_only %>%
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
interruption_intervals = interruption_intervals %>% mutate(date = as.Date(interval_30s), day = day(interval_30s), 
       hour = hour(interval_30s), minute = minute(interval_30s), second = second(interval_30s)) %>%
  replace_na(list(hour = 0, minute = 0, second = 0))

interruption_intervals = interruption_intervals %>% mutate(slot = if_else(minute <= 29, 30, 0))
write_xlsx(interruption_intervals, "interruption_intervals.xlsx")

# I joined tables in Excel Using PowerQuery to match the logbook entries to the interruption events
reference = read_xlsx("interruption_intervals.xlsx", sheet = "Logbook_Matched")

reference = reference %>% select(matched_interruption_start,logbook_datetime, note, operation, interruption_duration_sec, interval_30s)

# I was not able to get the first two matches so I enforced them here
first_interruption = min(interruption_intervals$interruption_start)

reference = reference %>%
  mutate(matched_interruption_start = replace_na(matched_interruption_start, first_interruption))


reference <- reference %>%
  left_join(
    interruption_intervals %>% 
      select(interruption_start, interval_30s) %>%
      distinct(interruption_start, .keep_all = TRUE),
    by = c("matched_interruption_start" = "interruption_start")
  )

interruption_instances = interruption_intervals %>%
  left_join(
    reference %>%
      select(interval_30s.y, note, operation) %>%
      distinct(interval_30s.y, .keep_all = TRUE),
    by = c("interval_30s" = "interval_30s.y")
  )

write_xlsx(interruption_instances, "interruption_instances.xlsx")

# Logbook
logbook = logbook %>% mutate(
  index = row_number()
)

logbook_merge = logbook %>% mutate(date = as.Date(TimeStamp), day = day(TimeStamp), 
                                   hour = hour(TimeStamp), minute = minute(TimeStamp), second = second(TimeStamp),slot = if_else( minute <= 29, 0, 30)) %>%
  replace_na(list(hour = 0, minute = 0, second = 0))

logbook_merge <- logbook_merge %>%
  mutate(logbook_dt = as.POSIXct(paste(date, sprintf("%02d:%02d:%02d", hour, minute, second)),
                                 format = "%Y-%m-%d %H:%M:%S"))

logbook_merge = logbook_merge %>% mutate(slot = if_else(minute <= 29, 30, 0))




# Joining to find matches for each logbook event I am enforcing each logbook value to find a match, if there are multiple events per entry I am forcing it to the find the next match.
# Several of the matches in the matched_results seem to be off

# Sort both by time
logbook_sorted <- reference %>%
  arrange(logbook_datetime)

interruptions_sorted <- interruption_events %>%
  arrange(interruption_start)

# Greedy match - each logbook entry claims the nearest unmatched interruption
matched_results <- logbook_sorted %>%
  mutate(matched_interruption_id = NA_integer_)

available_interruptions <- interruptions_sorted

for(i in 1:nrow(matched_results)) {
  lb_time <- matched_results$logbook_datetime[i]
  
  if(nrow(available_interruptions) == 0) break
  
  # Find closest interruption by absolute time difference
  diffs <- abs(difftime(available_interruptions$interruption_start, 
                        lb_time, units = "secs"))
  best_idx <- which.min(diffs)
  
  matched_results$matched_interruption_id[i] <- 
    available_interruptions$interruption_id[best_idx]
  
  # Remove claimed interruption so next logbook entry can't use it
  available_interruptions <- available_interruptions[-best_idx, ]
}


#Here is the file 
write_xlsx(matched_results, "logbook_interruptions_matched.xlsx")


