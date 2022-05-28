library(tidyverse)
df <- read_csv("http://github.com/phon-dicts-project/comparative_andic_dictionary_database/blob/master/andic_dicts.csv?raw=true")

df %>%
  glimpse()

df %>%
  count(na = is.na(meaning_ru), glottocode) %>%
  pivot_wider(values_from = n, names_from = na)
