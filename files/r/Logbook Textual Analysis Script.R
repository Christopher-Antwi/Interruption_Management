library(tidyverse)
library(readxl)
library(writexl)
library(tidytext)
library(SentimentAnalysis) 

# Creating The Workbooks
Day_1_CPP = read_excel("Logbook Long Run Day 1.xlsx", sheet = "CPP")
Day_1_CPP = Day_1_CPP %>%
  select(`Observations:`, `Time Index`, Feeding, Blending, Compression, `RTD/PAT`, Quality)
Day_1_CPP = Day_1_CPP %>%
  mutate( `Observations:` = format(`Observations:`, "%H:%M:%S"),
          Date = 12)
Day_1_CQA = read_excel("Logbook Long Run Day 1.xlsx", sheet = "CQA", skip = 1, col_names = TRUE)
Day_1_CQA = Day_1_CQA %>%
  select(`Observations:`, `Time Index`, Cleaning, Adjustment, `Electronic Communication`, `PAT Error/adjustment`, Feeders, Feeder, RTD, NIR, tablets)
Day_1_CQA = Day_1_CQA %>%
  mutate( `Observations:` = format(`Observations:`, "%H:%M:%S"),
          Date = 12)

Day_2_CPP = read_excel("Logbook Long Run Day 2.xlsx", sheet = "CPP")
Day_2_CPP = Day_2_CPP %>%
  select(`Observations:`, `Time Index`, Feeding, Blending, Compression, `RTD/PAT`, Quality)
Day_2_CPP = Day_2_CPP %>%
  mutate( `Observations:` = format(`Observations:`, "%H:%M:%S"),
          Date = 13)
Day_2_CQA = read_excel("Logbook Long Run Day 2.xlsx", sheet = "CQA", skip = 1, col_names = TRUE)
Day_2_CQA = Day_2_CQA %>%
  select(`Observations:`, `Time Index`, Cleaning, Adjustment, `Electronic Communication`, `PAT Error/adjustment`, Feeders, Feeder, RTD, NIR, tablets)
Day_2_CQA = Day_2_CQA %>%
  mutate( `Observations:` = format(`Observations:`, "%H:%M:%S"),
          Date = 13)

Day_3_CPP = read_excel("Logbook Long Run Day 3.xlsx", sheet = "CPP")
Day_3_CPP = Day_3_CPP %>%
  select(`Observations:`, `Time Index`, Feeding, Blending, Compression, `RTD/PAT`, Quality)
Day_3_CPP = Day_3_CPP %>%
  mutate( `Observations:` = format(`Observations:`, "%H:%M:%S"),
          Date = 14)
Day_3_CQA = read_excel("Logbook Long Run Day 3.xlsx", sheet = "CQA",  skip = 1, col_names = TRUE)
Day_3_CQA = Day_3_CQA %>%
  select(`Observations:`, `Time Index`, Cleaning, Adjustment, `Electronic Communication`, `PAT Error/adjustment`, Feeders, Feeder, RTD, NIR, tablets)
Day_3_CQA = Day_3_CQA %>%
  mutate( `Observations:` = format(`Observations:`, "%H:%M:%S"),
          Date = 14)

Day_4_CPP = read_excel("Logbook Long Run Day 4.xlsx", sheet = "CPP")
Day_4_CPP = Day_4_CPP %>%
  select(`Observations:`, `Time Index`, Feeding, Blending, Compression, `RTD/PAT`, Quality)
Day_4_CPP = Day_4_CPP %>%
  mutate( `Observations:` = format(`Observations:`, "%H:%M:%S"),
          Date = 15)
Day_4_CQA = read_excel("Logbook Long Run Day 4.xlsx", sheet = "CQA",  skip = 1, col_names = TRUE)
Day_4_CQA = Day_4_CQA %>%
  select(`Observations:`, `Time Index`, Cleaning, Adjustment, `Electronic Communication`, `PAT Error/adjustment`, Feeders, Feeder, RTD, NIR, tablets)
Day_4_CQA = Day_4_CQA %>%
  mutate( `Observations:` = format(`Observations:`, "%H:%M:%S"),
          Date = 15)

Day_5_CPP = read_excel("Logbook Long Run Day 5.xlsx", sheet = "CPP")
Day_5_CPP = Day_5_CPP %>%
  select(`Observations:`, `Time Index`, Feeding, Blending, Compression, `RTD/PAT`, Quality)
Day_5_CPP = Day_5_CPP %>%
  mutate( `Observations:` = format(`Observations:`, "%H:%M:%S"),
          Date = 16)
Day_5_CQA = read_excel("Logbook Long Run Day 5.xlsx", sheet = "CQA",  skip = 1, col_names = TRUE)
Day_5_CQA = Day_5_CQA %>%
  select(`Observations:`, `Time Index`, Cleaning, Adjustment, `Electronic Communication`, `PAT Error/adjustment`, Feeders, Feeder, RTD, NIR, tablets)
Day_5_CQA = Day_5_CQA %>%
  mutate( `Observations:` = format(`Observations:`, "%H:%M:%S"),
          Date = 16)

Day_6_CPP = read_excel("Logbook Long Run Day 6.xlsx", sheet = "CPP")
Day_6_CPP = Day_6_CPP %>%
  select(`Observations:`, `Time Index`, Feeding, Blending, Compression, `RTD/PAT`, Quality)
Day_6_CPP = Day_6_CPP %>%
  mutate( `Observations:` = format(`Observations:`, "%H:%M:%S"),
          Date = 17)
Day_6_CQA = read_excel("Logbook Long Run Day 6.xlsx", sheet = "CQA",  skip = 1, col_names = TRUE)
Day_6_CQA = Day_6_CQA %>%
  select(`Observations:`, `Time Index`, Cleaning, Adjustment, `Electronic Communication`, `PAT Error/adjustment`, Feeders, Feeder, RTD, NIR, tablets)
Day_6_CQA = Day_6_CQA %>%
  mutate( `Observations:` = format(`Observations:`, "%H:%M:%S"),
          Date = 17)

Logbook_Data = bind_rows(Day_1_CPP,Day_1_CQA,Day_2_CPP, Day_2_CQA, Day_3_CPP, Day_3_CQA, Day_4_CPP, Day_4_CQA, Day_5_CPP, Day_5_CQA, Day_6_CPP, Day_6_CQA)
long_Logbook_Data = Logbook_Data %>%
  pivot_longer(
    cols = -c(`Observations:`, Date, `Time Index`),
    names_to = "Operation",
    values_to = "note"
  ) %>%
  filter(!is.na(note)) %>%
  filter(note !="")

write_xlsx(long_Logbook_Data, "long_logbook_data.xlsx")
# Creating Dictionary

tokens = long_Logbook_Data %>%
  unnest_tokens(word, note) %>%
  count(word, sort = TRUE)
library(readxl)
library(DT)
long_Logbook_Data = read_excel("long_logbook_data.xlsx")
DT::datatable(
  long_Logbook_Data,
  rownames = FALSE,
  filter = "top",
  extensions = c("Scroller", "Buttons"),
  options = list(
    dom = "Bfrtip",
    buttons = c("copy", "csv", "excel"),
    deferRender = TRUE,
    scrollX = TRUE,
    scrollY = "600px",
    pagelength = 15,
    lengthMenu = c(10, 15, 25, 50, 100)
  )
)
