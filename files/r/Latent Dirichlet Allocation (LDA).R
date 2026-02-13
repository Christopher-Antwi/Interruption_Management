library(topicmodels)
library(tidytext)
library(dplyr)
library(knitr)
library(kableExtra)
library(flextable)
library(openxlsx)


lda_model = LDA(dtm_clean, k=2, control=list(seed=1234))

# Step 1: Get top terms per topic with probabilities
top_terms_with_probs_4 = tidy(lda_model_4, matrix = "beta") %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  arrange(topic, desc(beta)) %>%
  mutate(beta = round(beta, 2))

# Step 2: Topics Table Creation

#Equipment/Communication Events
topic1 = top_terms_with_probs %>%
  filter(topic == 1) %>%
  select(term, beta) %>%
  rename(Word = term, Prob = beta)
#Process Stoppage Events
topic2 = top_terms_with_probs %>%
  filter(topic == 2) %>%
  select(term, beta) %>%
  rename(Word = term, Prob = beta)
combined_table = cbind(topic1, topic2)
colnames(combined_table) = c("Equipment/Communication Events", "T1 Top Words", "Prob T1", "Process Stoppage Events Word","T2 Top Words", "Prob T2")

# Step 2: Topics Table Creation 4 Clusters

topic1_4 = top_terms_with_probs_4 %>%
  filter(topic == 1) %>%
  select(term, beta) %>%
  rename(Word = term, Prob = beta)
#Process Stoppage Events
topic2_4 = top_terms_with_probs_4 %>%
  filter(topic == 2) %>%
  select(term, beta) %>%
  rename(Word = term, Prob = beta)
# Topic 3
topic3 = top_terms_with_probs_4 %>%
  filter(topic == 3) %>%
  select(term, beta) %>%
  rename(Word = term, Prob = beta)
# Topic 4
topic4 = top_terms_with_probs_4 %>%
  filter(topic == 4) %>%
  select(term, beta) %>%
  rename(Word = term, Prob = beta)

combined_table_4 = cbind(topic1_4, topic2_4, topic3, topic4)
colnames(combined_table_4) = c("Equipment/Communication Events", "T1 Top Words", "Prob T1", "Process Stoppage Events Word","T2 Top Words", "Prob T2", "Topic 3", "T3 Top Words", "Prob T3", "Topic 4", "T4 Top Words", "Prob T4")


# Step 3: Create document-topic assignments
doc_topics = tidy(lda_model, matrix = "gamma") %>%
  mutate(gamma = round(gamma, 2)) %>%
  arrange(document, topic)

doc_topic_wide = doc_topics %>%
  tidyr::pivot_wider(
    names_from = topic,
    values_from = gamma,
    names_prefix = "Topic_"
  )


#Step 3: Create document-topic assingments 4 Topics
doc_topics_4 = tidy(lda_model_4, matrix = "gamma") %>%
  mutate(gamma = round(gamma, 2)) %>%
  arrange(document, topic)

doc_topic_wide_4 = doc_topics_4 %>%
  tidyr::pivot_wider(
    names_from = topic,
    values_from = gamma,
    names_prefix = "Topic_"
  )


# Step 4: Create Table
topics_ft = combined_table %>%
  flextable() %>%
  theme_vanilla() %>%
  align(align = "center", part = "all") %>%
  bold(part = "header") %>%
  autofit()

# Step 4: Create Table for 4

topics_ft_4 = combined_table_4 %>%
  flextable() %>%
  theme_vanilla() %>%
  align(align = "center", part = "all") %>%
  bold(part = "header") %>%
  autofit()


# WIP HTML Table
combined_table %>%
  kable("html", caption = "LDA Topic Modeling Results") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  add_header_above(c("Topic 1" = 2, "Topic 2" = 2))

# Step 5 Excel Creation
wb = createWorkbook()
addWorksheet(wb, "Topics")
writeData(wb, "Topics", combined_table)
addWorksheet(wb, "Document Assignments")
writeData(wb, "Document Assignments", doc_topic_wide)
saveWorkbook(wb, "lda_results.xlsx", overwrite = TRUE)

wb_4 = createWorkbook()
addWorksheet(wb_4, "Topics")
writeData(wb_4, "Topics", combined_table_4)
addWorksheet(wb_4, "Document Assignments")
writeData(wb_4, "Document Assignments", doc_topic_wide_4)
saveWorkbook(wb_4, "lda_results_4.xlsx", overwrite = TRUE)


#LDA DGP 
# HyperParameters
alpha <- 50 / 2   # 25 for K=2 topics
beta <- 0.01

# Run LDA with these parameters
lda_model_DGP = LDA(
  dtm_clean, 
  k = 2,                        
  method = "Gibbs",               
  control = list(
    seed = 1234,
    alpha = alpha,                
    delta = beta,                 
    iter = 2000,                  
    burnin = 500,              
    thin = 1
  )
)






# New Work


library(dplyr)

# Step 1: Create text features (make sure these are calculated)
text_features <- text_metrics %>%
  select(word_count, char_count, unique_words, type_token_ratio) %>%
  na.omit() %>%
  filter(is.finite(word_count) & 
           is.finite(char_count) & 
           is.finite(unique_words) & 
           is.finite(type_token_ratio))

# Step 2: Check the data
print(summary(text_features))
print(dim(text_features))

# Step 3: Scale the features
text_features_scaled <- scale(text_features)

# Step 4: Run k-means clustering
set.seed(25)
clusters_text <- kmeans(text_features_scaled, centers = 2, nstart = 25)

# Step 5: Add cluster labels to your data
# Match by row index (make sure alignment is correct)
df_with_clusters <- text_metrics %>%
  na.omit() %>%
  filter(is.finite(word_count) & 
           is.finite(char_count) & 
           is.finite(unique_words) & 
           is.finite(type_token_ratio)) %>%
  mutate(doc_detail_cluster = clusters_text$cluster)

# Step 6: Examine cluster characteristics
summary_by_cluster <- df_with_clusters %>%
  group_by(doc_detail_cluster) %>%
  summarise(
    n = n(),
    mean_word_count = mean(word_count),
    mean_char_count = mean(char_count),
    mean_unique_words = mean(unique_words),
    mean_ttr = mean(type_token_ratio)
  )

print(summary_by_cluster)

# Step 7: Assign names to clusters based on characteristics
# Look at the summary above to decide which is "High-Detail" vs "Low-Detail"
df_with_clusters <- df_with_clusters %>%
  mutate(
    documentation_level = case_when(
      doc_detail_cluster == 1 ~ "Low-Detail",  # Adjust based on your results
      doc_detail_cluster == 2 ~ "High-Detail"    # Adjust based on your results
    )
  )


library(writexl)

write_xlsx(df_with_clusters, "fail_log_with_clusters.xlsx")


library(tidytext)

dtm_tidy <- tidy(dtm_clean)

# View the tidy format
head(dtm_tidy, 20)

# Output:
#   document term      count
# 1        1 feeder        3
# 2        1 system        2
# 3        1 communication 1
# 4        2 process       2
# 5        2 stopped       1
# ...

# View in RStudio
View(dtm_tidy)

write_xlsx(dtm_tidy, "DTM.xlsx")
