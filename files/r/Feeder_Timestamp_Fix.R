library(lubridate)
library(tidyverse)

# First, let's see what those 2 NA rows actually contain
Feeders %>%
  filter(is.na(TimeStamp_fixed)) %>%
  select(TimeStamp)

# Now fix the whole column properly
Feeders <- Feeders %>%
  mutate(TimeStamp_fixed = case_when(
    # Format 1: "1/12/2018 ..." (month/day/year)
    str_detect(TimeStamp, "^1/12/2018") ~ mdy_hms(TimeStamp),
    # Format 2: "17/01/2018 ..." (day/month/year)  
    str_detect(TimeStamp, "^[0-9]{2}/01/2018") ~ dmy_hms(TimeStamp),
    # Catch anything else
    TRUE ~ as.POSIXct(NA)
  ))

# Check the range - should now be 2018-01-12 to 2018-01-17
range(Feeders$TimeStamp_fixed, na.rm = TRUE)

# Count rows per date - should have 5 days (Jan 12-17, 2018)
Feeders %>%
  mutate(Date = as.Date(TimeStamp_fixed)) %>%
  count(Date)

# How many NAs?
sum(is.na(Feeders$TimeStamp_fixed))
