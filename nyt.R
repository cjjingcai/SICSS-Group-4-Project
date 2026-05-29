library(tidyverse)
library(readxl)
library(rvest)

df_nyt <- read_excel("~/Desktop/nyt_ai.xlsx")
head(df_nyt)
glimpse(df_nyt)

df_nyt <- df_nyt %>%
  mutate(
    country = "US",
    outlet = "New York Times"
  )

library(tidytext)

words_nyt <- df_nyt %>%
  unnest_tokens(word, text)

library(stopwords)

words_nyt_clean <- words_nyt %>%
  filter(!word %in% stopwords("en")) %>%
  filter(str_detect(word, "^[a-z]+$")) %>%
  filter(nchar(word) > 2)

custom_stopwords_nyt2 <- c(
  "ai", "artificial", "intelligence",
  "said", "new", "also", "will", "can",
  "one", "two", "year", "years",
  "use", "used", "using",
  "york", "times", "people", "many", "just", "now",
  "technology", "company", "companies", "builder",
  "like", "data", "may"
)

words_nyt_clean2 <- words_nyt_clean %>%
  filter(!word %in% custom_stopwords_nyt2)

word_freq_nyt2 <- words_nyt_clean2 %>%
  count(word, sort = TRUE)

print(
  word_freq_nyt2 %>% slice_max(n, n = 20),
  n = 20
)

#new round
custom_stopwords_nyt3 <- c(
  "ai", "artificial", "intelligence",
  "said", "new", "also", "will", "can",
  "one", "two", "year", "years",
  "use", "used", "using",
  "york", "times","like", "data",
  "people", "many", "just", "now",
  "technology", "company", "companies",
  "work", "make", "made", "time",
  "start", "last", "chief", "executive","may", "even", "article",
  "book", "get"
)

words_nyt_clean3 <- words_nyt_clean %>%
  filter(!word %in% custom_stopwords_nyt3)

word_freq_nyt3 <- words_nyt_clean3 %>%
  count(word, sort = TRUE)

print(
  word_freq_nyt3 %>%
    slice_max(n, n = 20),
  n = 20
)

word_freq_nyt3 %>%
  slice_max(n, n = 20) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 20 Keywords in New York Times AI Coverage",
    subtitle = "Keyword Frequency Analysis",
    x = "",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14)