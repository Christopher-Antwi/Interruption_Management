# Install required packages
install.packages(c("tm", "textclean", "tidytext", "dplyr", "ggplot2", "quanteda", "topicmodels", "text2vec", "readxl"))

# Load libraries
library(tm)
library(textclean)
library(dplyr)
library(ggplot2)
library(readxl)
library(topicmodels)
library(tidytext)

# Step 1: Load and Prep Your Data
df <- read_excel("data/processed/LogBook_Data_Analysis.xlsx", sheet = "Failure Log")

# Create a corpus from the 'note' column
corpus <- Corpus(VectorSource(df$note))

# Clean the text
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)

# Step 2: Create Document-Term Matrix
dtm <- DocumentTermMatrix(corpus)

# Remove sparse terms (appear in <5% of documents)
dtm <- removeSparseTerms(dtm, 0.95)
rowTotals = apply(dtm, 1, sum)
dtm_clean = dtm[rowTotals > 0, ]

df_clean = df[rowTotals > 0, ]

# Step 3A: K-means Clustering (Simple)
# Convert to matrix
m <- as.matrix(dtm_clean)

# Determine optimal number of clusters (elbow method)
wss <- sapply(2:10, function(k) {
  kmeans(m, centers=k, nstart=25)$tot.withinss
})
plot(2:10, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares")

# Run k-means (adjust k based on plot)
k <- 2
clusters = kmeans(m, centers=k, nstart=25)
k <- 4
clusters_4 = kmeans(m, centers=k, nstart=25)

# Add cluster labels to your data
df_clean$cluster <- clusters_4$cluster

# Step 3B: Topic Modeling with LDA (Better for themes)
# Run LDA (adjust k = number of topics)
lda_model_4 <- LDA(dtm_clean, k=4, control=list(seed=1234))

# Get top terms per topic
terms(lda_model_4, 10)

# Assign dominant topic to each document
df_clean$topic <- topics(lda_model_4)

# Step 4: Examine Results
# View clusters/topics with sample notes
df_clean %>% 
  group_by(topic) %>%  # or 'topic' if using LDA
  slice_head(n=3) %>%
  select(note, cluster)

# Get top words per cluster
tidy_dtm <- tidy(dtm_clean)
top_terms <- tidy_dtm %>%
  group_by(document) %>%
  inner_join(df_clean %>% mutate(document = as.character(row_number())), 
             by="document") %>%
  group_by(cluster, term) %>%
  summarise(freq = sum(count), .groups="drop") %>%
  group_by(cluster) %>%
  top_n(10, freq) %>%
  arrange(cluster, desc(freq))

cluster2_terms = top_terms %>%
  filter(cluster == 2)

top_terms %>%
  arrange(cluster, desc(freq)) %>%
  print(n=20)
print(top_terms)

# Export results
write.csv(df_clean, "failure_log_clustered.csv", row.names=FALSE)
