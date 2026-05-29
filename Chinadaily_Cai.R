

# Keyword frequency

library(tidyverse)
library(tidytext)
library(stopwords)

# 1. Tokenize
words <- df %>%
  unnest_tokens(word, text)

# 2. Basic cleaning
words_clean <- words %>%
  filter(!word %in% stopwords("en")) %>%
  filter(str_detect(word, "^[a-z]+$")) %>%
  filter(nchar(word) > 2)

# 3. Custom stopwords
custom_stopwords <- c(
  "ai", "artificial", "intelligence",
  "china", "chinese",
  "said", "also", "will", "can",
  "new", "data", "technology", "technologies",
  "percent", "one", "two", "year", "years"
)

words_clean2 <- words_clean %>%
  filter(!word %in% custom_stopwords)

# 4. Count words
word_freq2 <- words_clean2 %>%
  count(word, sort = TRUE)

head(word_freq2, 30)

# 5. Plot top keywords
word_freq2 %>%
  slice_max(n, n = 15) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top Keywords in China Daily AI Coverage",
    subtitle = "Keyword Frequency Analysis",
    x = "",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14)

#查看
word_freq2 %>%
  slice_max(n, n = 20)


