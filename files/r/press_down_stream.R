library(tidyverse)
library(dplyr)

library(tidyverse)



# Shared cols across all three ─
shared_cols <- c("timestamp", "date", "hour", "min", "sec",
                 "measure", "value", "status", "status_chg")

# Press-specific KPP cols to keep
press_kpp_cols <- c("maincomp_force", "comp_cyclefill_depth",
                    "matinlet_hopperlevel_detection", "eje_force_tablet")
# Add any others you want from tablet_press_long here

# ── Tag each source ───────────────────────────────────────────
blenderz_tagged <- blenderz_long %>%
  select(all_of(shared_cols)) %>%
  mutate(source = "blender")

feederz_tagged <- feederz_long %>%
  select(all_of(shared_cols)) %>%
  mutate(source = "feeder")

press_tagged <- tablet_press_long %>%
  select(all_of(shared_cols), all_of(press_kpp_cols)) %>%
  mutate(source = "press")

# ── Append — bind_rows fills NA for press KPP cols in blender/feeder rows ──
combined_long <- bind_rows(blenderz_tagged, feederz_tagged, press_tagged) %>%
  arrange(timestamp)


# Pull upstream interruption events (feeder or blender)
upstream_interruptions <- combined_long %>%
  filter(source %in% c("feeder", "blender"), status == "interruption") %>%
  group_by(measure, source) %>%
  # Collapse consecutive seconds into events with start/end
  mutate(event_id = cumsum(status_chg != 0 | row_number() == 1)) %>%
  group_by(measure, source, event_id) %>%
  summarise(
    interruption_start = min(timestamp),
    interruption_end   = max(timestamp),
    dur_secs           = as.numeric(difftime(max(timestamp), min(timestamp), units = "secs")),
    .groups = "drop"
  ) %>%
  mutate(
    impact_start   = interruption_start + 136,
    impact_end     = interruption_start + 150,
    baseline_start = interruption_start - 150,
    baseline_end   = interruption_start
  )



# Pull filling shoe interruptions from all_interruptions
filling_interruptions <- all_interruptions %>%
  filter(grepl("fillingshoe", measure, ignore.case = TRUE),
         status == "interruption") %>%
  mutate(
    start = as.POSIXct(start, tz = "UTC"),
    end = as.POSIXct(end, tz= "UTC")
  ) %>%
  select(measure, start, end, dur_secs, seq, slot_30min, hour)

# cascade_check has timestamp — create a matching key
# For each cascade-confirmed second, find if it falls within a filling shoe interruption window
cascade_with_filling <- cascade_check %>%
  filter(cascade_confirmed == TRUE) %>%
  cross_join(
    filling_interruptions %>%
      rename(fill_start = start, fill_end = end)
  ) %>%
  filter(timestamp >= fill_start, timestamp <= fill_end) %>%
  select(timestamp, blender, feeder, press,
         upstream_hit, cascade_confirmed,
         measure, fill_start, fill_end, 
         dur_secs, seq, slot_30min, hour)

nrow(cascade_with_filling)
count(cascade_with_filling, measure)



cascade_with_filling_2 <- cascade_check %>%
  filter(cascade_confirmed == TRUE) %>%
  left_join(
    filling_interruptions %>%
      rename(fill_start = start, fill_end = end),
    by = join_by(timestamp >= fill_start, timestamp <= fill_end)
  ) %>%
  select(timestamp, blender, feeder, press,
         upstream_hit, cascade_confirmed,
         measure, fill_start, fill_end,
         dur_secs, seq, slot_30min, hour)

nrow(cascade_with_filling_2)
count(cascade_with_filling_2, measure)

