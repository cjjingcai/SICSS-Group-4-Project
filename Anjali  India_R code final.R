# STEP 1: LOAD REQUIRED LIBRARIES
library(tidyverse)
library(tidytext)
library(topicmodels)
library(ggplot2)
library(tm)

# STEP 2: IMPORT DATASETS: Read CSV files
d1 <- read_csv("Generative AI Hindu India.csv")
d2 <- read_csv("Responsible AI hindu India.csv")

# STEP 3: MERGE DATASETS: combine both datasets into one dataframe
india <- bind_rows(d1, d2)

# STEP 4: REMOVE DUPLICATE ARTICLES: Remove duplicate URLs
india <- india %>%
  distinct(url, .keep_all = TRUE)

# STEP 5: REMOVE BRANDHUB / PRESS RELEASE ARTICLES: Remove sponsored PR content
india <- india %>%
  filter(!str_detect(url, "brandhub"))

# STEP 6: CREATE TEXT COLUMN:  Use headlines/titles for analysis
india$text <- india$title

# STEP 7: TOKENIZE TEXT: break headlines into individual words
tokens <- india %>%
  unnest_tokens(word, text)

# STEP 8: REMOVE STOP WORDS: Load stop words dataset
data(stop_words)

# Remove common English stop words

tokens_clean <- tokens %>%
  anti_join(stop_words)

# STEP 9: REMOVE GENERIC AI TERMS: remove overly common AI words
tokens_clean <- tokens_clean %>%
  filter(!word %in% c(
    "ai",
    "artificial",
    "intelligence",
    "chatgpt",
    "india",
    "indian",
    "2026"
  ))

# STEP 10: REMOVE COMPANY NAMES: remove company names that dominate topics
tokens_clean <- tokens_clean %>%
  filter(!word %in% c(
    "openai",
    "google",
    "meta",
    "nvidia",
    "xai"
  ))

# STEP 11: REMOVE SHORT WORDS & NUMBERS: remove very short words
tokens_clean <- tokens_clean %>%
  filter(nchar(word) > 2)

# Remove numbers
tokens_clean <- tokens_clean %>%
  filter(!str_detect(word, "^[0-9]+$"))

Last step -Keyword frequency analysis 
slice_max(n, n = 20)
ggplot(top20,
       aes(x = reorder(word, n),
           y = n,
           fill = word)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top Keywords in The Hindu's AI Coverage",
    x = "Keywords",
    y = "Frequency"
  )
tokens_clean <- tokens_clean %>%
  filter(!word %in% c(
    "elon",
    "musk's",
    "gemini",
    "ai’s",
    "chatbot",
    "chips"
  ))
top_words <- tokens_clean %>%
  count(word, sort = TRUE)

top20 <- top_words %>%
  slice_max(n, n = 20)
fill = word
ggplot(top20,
       aes(x = reorder(word, n),
           y = n)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top Keywords in The Hindu's AI Coverage",
    x = "Keywords",
    y = "Frequency"
  )
top20 <- top_words %>%
  slice_max(n, n = 20)
ggplot(top20,
       aes(x = reorder(word, n),
           y = n)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top Keywords in The Hindu's AI Coverage",
    x = "Keywords",
    y = "Frequency"
  )
top20 <- top_words %>%
  arrange(desc(n)) %>%
  slice(1:20)
nrow(top20)
ggplot(top20,
       aes(x = reorder(word, n),
           y = n)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 20 Keywords in The Hindu's AI Coverage",
    subtitle = "Keyword Frequency Analysis",
    x = "Keywords",
    y = "Frequency"
  )
write_csv(india, "India_Merged_AI_Data.csv")
