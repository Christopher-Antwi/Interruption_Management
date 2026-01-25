library(tidyverse)
library(tidytext)
library(stopwords)
library(wordcloud2)
library(stringr)
library(webshot)
library(htmlwidgets)

word_cloud = long_Logbook_Data %>%
  mutate(note = tolower(note)) %>%
  mutate(note = str_replace_all(note, "[^a-z\\s]", " ")) %>%  # keep letters/spaces
  unnest_tokens(word, note) %>%
  filter(!word %in% stopwords("en")) %>%
  filter(nchar(word) >= 3) %>%                               # optional
  count(word, sort = TRUE)

 wc = wordcloud2(word_cloud, size = 0.8)

saveWidget(wc, "logbook_wordcloud.html", selfcontained = TRUE)

webshot("logbook_wordcloud.html", "logbook_wordcloud.png", 
        vwidth = 1200, vheight = 800)
