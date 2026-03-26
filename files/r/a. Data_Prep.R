
# Clean Data in the Work space

rm(list = ls())

# Attaching Required Packages

source(paste(getwd(),"/Code/","ipak.R",sep = ""))

#### Blenders Data ####

file = "Blenders.csv"

loc = paste(getwd(),"/Data/Machine data/", file, sep = "")

blndrs = read_csv(loc) %>% as_tibble()

rm(file, loc)

# Separating timestamp into date and time

temp = blndrs %>% separate_wider_delim(time_stamp, delim = " ", names = c("date", "time"), too_few = "debug")

# Abnormal Records on row 73014 and 353384 : Dropping these two records

temp = temp %>% drop_na() %>% select(-time_stamp_ok, -time_stamp_pieces, -time_stamp_remainder)

# Separating date into three different variables and converting into numeric type

temp = temp %>% separate_wider_delim(date, delim = "/", names = paste0(c("var"), 1:3)) %>%
  mutate(across(.cols = c(var1, var2, var3), .fns = ~as.numeric(.x)))

### Creating cleaned date column

# Correcting day column
temp %>% count(var1)
temp = temp %>% rename(day = var1) %>% mutate(day = if_else(day == 1, 12, day))
temp %>% count(day)

# Correcting month column
temp %>% count(var2)
temp = temp %>% rename(month = var2) %>% mutate(month = if_else(month == 12, 1, month))
temp %>% count(month)

# Renaming var3 to year
temp = temp %>% rename(year = var3)

# Creating cleaned date column
temp = temp %>% mutate(date = make_date(year = year, month = month, day = day)) 

### Creating cleaned hour, minute and second columns

# Separating date into three different variables and converting into numeric type

temp = temp %>% separate_wider_delim(time, delim = ":", names = c("hour", "min", "sec"), too_few = "debug")  %>%
  mutate(across(.cols = c(hour, min, sec), .fns = ~as.numeric(.x)))

temp = temp %>% select(-time_ok, -time_pieces, -time_remainder)

# Parting the dataset into the ones with and without second information and filling missing second information

temp1 = temp %>% filter(is.na(sec))
temp1 = temp1 %>% group_by(time_stamp) %>% mutate(sec = 0:(n()-1)) %>% ungroup() 
temp1 = temp1 %>% filter(sec <= 59)

temp2 = temp %>% drop_na(sec)

temp = temp1 %>% bind_rows(temp2) %>% mutate(timestamp = make_datetime(year = year, month = month, day = day, hour = hour, min = min, sec = sec))
temp = temp %>% select(timestamp, date, hour, min, sec, massflow_blender1:oos_conc_blender2_inlet) %>% arrange(timestamp)

temp = temp %>% mutate(across(.cols = c(massflow_blender1:oos_conc_blender2_inlet), .fns = ~round(.x, 5)))

blndrsc = temp

write_rds(blndrsc, paste(getwd(),"/Data/", "blenders.rds", sep = ""))

rm(temp, temp1, temp2, blndrs, blndrsc)


#### LIW Feeder 1 Data ####

file = "LiW Feeders 1.csv"

loc = paste(getwd(),"/Data/Machine data/", file, sep = "")

fdr1 = read_csv(loc) %>% as_tibble()

rm(file, loc)

# Separating timestamp into date and time

temp = fdr1 %>% separate_wider_delim(time_stamp, delim = " ", names = c("date", "time"), too_few = "debug")

# Abnormal Records on row 73014 and 353384 : Dropping these two records

temp = temp %>% drop_na() %>% select(-time_stamp_ok, -time_stamp_pieces, -time_stamp_remainder)

# Separating date into three different variables and converting into numeric type

temp = temp %>% separate_wider_delim(date, delim = "/", names = paste0(c("var"), 1:3)) %>%
  mutate(across(.cols = c(var1, var2, var3), .fns = ~as.numeric(.x)))

### Creating cleaned date column

# Correcting day column
temp %>% count(var1)
temp = temp %>% rename(day = var1) %>% mutate(day = if_else(day == 1, 12, day))
temp %>% count(day)

# Correcting month column
temp %>% count(var2)
temp = temp %>% rename(month = var2) %>% mutate(month = if_else(month == 12, 1, month))
temp %>% count(month)

# Renaming var3 to year
temp = temp %>% rename(year = var3)

# Creating cleaned date column
temp = temp %>% mutate(date = make_date(year = year, month = month, day = day)) 

### Creating cleaned hour, minute and second columns

# Separating date into three different variables and converting into numeric type

temp = temp %>% separate_wider_delim(time, delim = ":", names = c("hour", "min", "sec"), too_few = "debug")  %>%
  mutate(across(.cols = c(hour, min, sec), .fns = ~as.numeric(.x)))

temp = temp %>% select(-time_ok, -time_pieces, -time_remainder)

# Parting the dataset into the ones with and without second information and filling missing second information

temp1 = temp %>% filter(is.na(sec))
temp1 = temp1 %>% group_by(time_stamp) %>% mutate(sec = 0:(n()-1)) %>% ungroup() 
temp1 = temp1 %>% filter(sec <= 59)

temp2 = temp %>% drop_na(sec)

temp = temp1 %>% bind_rows(temp2) %>% mutate(timestamp = make_datetime(year = year, month = month, day = day, hour = hour, min = min, sec = sec))
temp = temp %>% select(timestamp, date, hour, min, sec, feedfactor_pd1:per_pd3) %>% arrange(timestamp)

temp = temp %>% mutate(across(.cols = c(feedfactor_pd1:per_pd3), .fns = ~round(.x, 5)))

fdr1 = temp

write_rds(fdr1, paste(getwd(),"/Data/", "fdr1.rds", sep = ""))

rm(temp, temp1, temp2, fdr1)


#### LIW Feeder 2 Data ####

file = "LiW Feeders 2.csv"

loc = paste(getwd(),"/Data/Machine data/", file, sep = "")

fdr2 = read_csv(loc) %>% as_tibble()

rm(file, loc)

# Separating timestamp into date and time

temp = fdr2 %>% separate_wider_delim(time_stamp, delim = " ", names = c("date", "time"), too_few = "debug")

# Abnormal Records on row 73014 and 353384 : Dropping these two records

temp = temp %>% drop_na() %>% select(-time_stamp_ok, -time_stamp_pieces, -time_stamp_remainder)

# Separating date into three different variables and converting into numeric type

temp = temp %>% separate_wider_delim(date, delim = "/", names = paste0(c("var"), 1:3)) %>%
  mutate(across(.cols = c(var1, var2, var3), .fns = ~as.numeric(.x)))

### Creating cleaned date column

# Correcting day column
temp %>% count(var1)
temp = temp %>% rename(day = var1) %>% mutate(day = if_else(day == 1, 12, day))
temp %>% count(day)

# Correcting month column
temp %>% count(var2)
temp = temp %>% rename(month = var2) %>% mutate(month = if_else(month == 12, 1, month))
temp %>% count(month)

# Renaming var3 to year
temp = temp %>% rename(year = var3)

# Creating cleaned date column
temp = temp %>% mutate(date = make_date(year = year, month = month, day = day)) 

### Creating cleaned hour, minute and second columns

# Separating date into three different variables and converting into numeric type

temp = temp %>% separate_wider_delim(time, delim = ":", names = c("hour", "min", "sec"), too_few = "debug")  %>%
  mutate(across(.cols = c(hour, min, sec), .fns = ~as.numeric(.x)))

temp = temp %>% select(-time_ok, -time_pieces, -time_remainder)

# Parting the dataset into the ones with and without second information and filling missing second information

temp1 = temp %>% filter(is.na(sec))
temp1 = temp1 %>% group_by(time_stamp) %>% mutate(sec = 0:(n()-1)) %>% ungroup() 
temp1 = temp1 %>% filter(sec <= 59)

temp2 = temp %>% drop_na(sec)

temp = temp1 %>% bind_rows(temp2) %>% mutate(timestamp = make_datetime(year = year, month = month, day = day, hour = hour, min = min, sec = sec))
temp = temp %>% select(timestamp, date, hour, min, sec, per_pd4:totalizer_pd7) %>% arrange(timestamp)

temp = temp %>% mutate(across(.cols = c(per_pd4:totalizer_pd7), .fns = ~round(.x, 5)))

fdr2 = temp

write_rds(fdr2, paste(getwd(),"/Data/", "fdr2.rds", sep = ""))

rm(temp, temp1, temp2, fdr2)


#### Tablet Press Data ####

file = "Tablet Press.csv"

loc = paste(getwd(),"/Data/Machine data/", file, sep = "")

tablet_press = read_csv(loc) %>% as_tibble()

rm(file, loc)

# Separating timestamp into date and time

temp = tablet_press %>% separate_wider_delim(time_stamp, delim = " ", names = c("date", "time"), too_few = "debug")

# Abnormal Records on row 73014 and 353384 : Dropping these two records

temp = temp %>% drop_na() %>% select(-time_stamp_ok, -time_stamp_pieces, -time_stamp_remainder)

# Separating date into three different variables and converting into numeric type

temp = temp %>% separate_wider_delim(date, delim = "/", names = paste0(c("var"), 1:3)) %>%
  mutate(across(.cols = c(var1, var2, var3), .fns = ~as.numeric(.x)))

### Creating cleaned date column

# Correcting day column
temp %>% count(var1)
temp = temp %>% rename(day = var1) %>% mutate(day = if_else(day == 1, 12, day))
temp %>% count(day)

# Correcting month column
temp %>% count(var2)
temp = temp %>% rename(month = var2) %>% mutate(month = if_else(month == 12, 1, month))
temp %>% count(month)

# Renaming var3 to year
temp = temp %>% rename(year = var3)

# Creating cleaned date column
temp = temp %>% mutate(date = make_date(year = year, month = month, day = day)) 

### Creating cleaned hour, minute and second columns

# Separating date into three different variables and converting into numeric type

temp = temp %>% separate_wider_delim(time, delim = ":", names = c("hour", "min", "sec"), too_few = "debug")  %>%
  mutate(across(.cols = c(hour, min, sec), .fns = ~as.numeric(.x)))

temp = temp %>% select(-time_ok, -time_pieces, -time_remainder)

# Parting the dataset into the ones with and without second information and filling missing second information

temp1 = temp %>% filter(is.na(sec))
temp1 = temp1 %>% group_by(time_stamp) %>% mutate(sec = 0:(n()-1)) %>% ungroup() 
temp1 = temp1 %>% filter(sec <= 59)

temp2 = temp %>% drop_na(sec)

temp = temp1 %>% bind_rows(temp2) %>% mutate(timestamp = make_datetime(year = year, month = month, day = day, hour = hour, min = min, sec = sec))
temp = temp %>% select(timestamp, date, hour, min, sec, precomp_height_bottom:eje_force_tablet) %>% arrange(timestamp)

temp = temp %>% mutate(across(.cols = c(precomp_height_bottom:eje_force_tablet), .fns = ~round(.x, 5)))

tablet_press = temp

write_rds(tablet_press, paste(getwd(),"/Data/", "tablet_press.rds", sep = ""))

rm(temp, temp1, temp2, tablet_press)



#### Temperature Data ####

file = "Temperature.csv"

loc = paste(getwd(),"/Data/Machine data/", file, sep = "")

temperature = read_csv(loc) %>% as_tibble()

rm(file, loc)

# Separating timestamp into date and time

temp = temperature %>% separate_wider_delim(time_stamp, delim = " ", names = c("date", "time"), too_few = "debug")

# Removing irrelevant columns

temp = temp %>% select(-time_stamp_ok, -time_stamp_pieces, -time_stamp_remainder)

# Separating date into three different variables and converting into numeric type

temp = temp %>% separate_wider_delim(date, delim = "/", names = paste0(c("var"), 1:3)) %>%
  mutate(across(.cols = c(var1, var2, var3), .fns = ~as.numeric(.x)))

### Creating cleaned date column

temp = temp %>% rename(year = var3, month = var2, day = var1) %>% mutate(date = make_date(year = year, month = month, day = day)) 

### Creating cleaned hour, minute and second columns

# Separating date into three different variables and converting into numeric type

temp = temp %>% separate_wider_delim(time, delim = ":", names = c("hour", "min", "sec"), too_few = "debug")  %>%
  mutate(across(.cols = c(hour, min, sec), .fns = ~as.numeric(.x)))

temp = temp %>% select(-time_ok, -time_pieces, -time_remainder)

temp = temp %>% mutate(timestamp = make_datetime(year = year, month = month, day = day, hour = hour, min = min))

temp = temp %>% select(timestamp, date, hour, min, parameter, temp_degc) %>% arrange(timestamp)

temp = temp %>% mutate(across(.cols = c(temp_degc), .fns = ~round(.x, 5)))

temperature = temp

write_rds(temperature, paste(getwd(),"/Data/", "temperature.rds", sep = ""))

rm(temp, temperature)


#### Humidity Data ####

file = "Humidity.csv"

loc = paste(getwd(),"/Data/Machine data/", file, sep = "")

humidity = read_csv(loc) %>% as_tibble()

rm(file, loc)

# Separating timestamp into date and time

temp = humidity %>% separate_wider_delim(time_stamp, delim = " ", names = c("date", "time"), too_few = "debug")

# Removing irrelevant columns

temp = temp %>% select(-time_stamp_ok, -time_stamp_pieces, -time_stamp_remainder)

# Separating date into three different variables and converting into numeric type

temp = temp %>% separate_wider_delim(date, delim = "/", names = paste0(c("var"), 1:3)) %>%
  mutate(across(.cols = c(var1, var2, var3), .fns = ~as.numeric(.x)))

### Creating cleaned date column

temp = temp %>% rename(year = var3, month = var2, day = var1) %>% mutate(date = make_date(year = year, month = month, day = day)) 

### Creating cleaned hour, minute and second columns

# Separating date into three different variables and converting into numeric type

temp = temp %>% separate_wider_delim(time, delim = ":", names = c("hour", "min", "sec"), too_few = "debug")  %>%
  mutate(across(.cols = c(hour, min, sec), .fns = ~as.numeric(.x)))

temp = temp %>% select(-time_ok, -time_pieces, -time_remainder)

temp = temp %>% mutate(timestamp = make_datetime(year = year, month = month, day = day, hour = hour, min = min))

temp = temp %>% select(timestamp, date, hour, min, parameter, humidity_perrh) %>% arrange(timestamp)

temp = temp %>% mutate(across(.cols = c(humidity_perrh), .fns = ~round(.x, 5)))

humidity = temp

write_rds(humidity, paste(getwd(),"/Data/", "humidity.rds", sep = ""))

rm(temp, humidity)
