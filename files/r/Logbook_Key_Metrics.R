library(tidytext)
library(stringr)
library(dplyr)
library(writexl)


# Failure Log Key Metrics Run
key_metrics = read_excel("data/processed/LogBook_Data_Analysis.xlsx", sheet = "Failure Log")
text_metrics = key_metrics %>%
  mutate(
    # Basic counts
    char_count = nchar(note),
    char_no_space = nchar(gsub(" ", "", note)),
    word_count = str_count(note, "\\w+"),
    sentence_count = str_count(note, "[.!?]+"),
    
    # Averages
    avg_word_length = char_no_space / word_count,
    avg_sentence_length = word_count / pmax(sentence_count, 1),
    
    # Vocabulary richness
    unique_words = sapply(strsplit(tolower(note), "\\W+"), function(x) length(unique(x[x != ""]))),
    type_token_ratio = unique_words / pmax(word_count, 1)
  )

# Summary by cluster
summary_by_group <- text_metrics %>%
  group_by(Group) %>%
  summarise(
    n = n(),
    mean_char_count = mean(char_count, na.rm = TRUE),
    mean_word_count = mean(word_count, na.rm = TRUE),
    mean_unique_words = mean(unique_words, na.rm = TRUE),
    mean_ttr = mean(type_token_ratio, na.rm = TRUE),
    mean_avg_word_length = mean(avg_word_length, na.rm = TRUE),
    sd_word_count = sd(word_count, na.rm = TRUE)
  )

print(summary_by_group)

# Failure Log Clustered Key Metrics Run


text_metrics_clustered <- df %>%
  mutate(
    # Basic counts
    char_count = nchar(note),
    char_no_space = nchar(gsub(" ", "", note)),
    word_count = str_count(note, "\\w+"),
    sentence_count = str_count(note, "[.!?]+"),
    
    # Averages
    avg_word_length = char_no_space / word_count,
    avg_sentence_length = word_count / pmax(sentence_count, 1),
    
    # Vocabulary richness
    unique_words = sapply(strsplit(tolower(note), "\\W+"), function(x) length(unique(x[x != ""]))),
    type_token_ratio = unique_words / pmax(word_count, 1)
  )

# Summary by cluster
summary_by_cluster <- text_metrics_clustered %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    mean_char_count = mean(char_count, na.rm = TRUE),
    mean_word_count = mean(word_count, na.rm = TRUE),
    mean_unique_words = mean(unique_words, na.rm = TRUE),
    mean_ttr = mean(type_token_ratio, na.rm = TRUE),
    mean_avg_word_length = mean(avg_word_length, na.rm = TRUE),
    sd_word_count = sd(word_count, na.rm = TRUE)
  )

print(summary_by_cluster)

write_xlsx(summary_by_group, "Key_Metrics_Summary_by_Group.xlsx")
write_xlsx(summary_by_cluster, "Key_Metrics_Summary_by_Cluster.xlsx")
