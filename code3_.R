library(tidyverse)
install.packages("tidytext")
library(tidytext)
read_csv("/Users/tatanagnedina/table.csv") %>% 
  filter(language != "Tokita") ->
  df
df %>% 
  mutate(languages_source = str_c(language, " (", reference, ")")) %>% 
  filter(!is.na(meaning_ru)) %>% 
  select(languages_source, ipa, meaning_ru, borrowing_source_language, definition) %>% 
  mutate(borrowing_source_language = ifelse(is.na(borrowing_source_language),
                                            "native",
                                            borrowing_source_language)) %>% 
  select(languages_source, ipa, meaning_ru, borrowing_source_language) %>% 
  group_by(meaning_ru, languages_source) %>%
  mutate(id = 1:n()) %>% 
  sample_n(1) %>% 
  pivot_longer(names_to = "names", values_to = "value",  c(ipa, borrowing_source_language)) %>% 
  mutate(languages_source = ifelse(names != "ipa", str_c(languages_source, " bor"), languages_source)) %>%
  select(-names) %>% 
  pivot_wider(names_from = languages_source, values_from = value) %>% 
  select(-id) ->
  result
result$n_languages <- 9-rowSums(is.na(result))/2
df %>% 
  mutate(language_source = str_c(language, " (", reference, ")")) %>% 
  distinct(language_source) %>% 
  pull(language_source) %>% 
  combn(2) %>% 
  t() %>% 
  as.data.frame() %>% 
  rename(lemma_1 = V1,
         lemma_2 = V2) %>% 
  mutate(bor_1 = str_c(lemma_1, " bor"),
         bor_2 = str_c(lemma_2, " bor")) ->
  all_combinations
map_dfr(1:nrow(all_combinations), function(i){
  result %>% 
    filter(n_languages > 1) %>% 
    select(unlist(all_combinations[i,])) %>%  
    na.omit() %>% 
    filter(bor_1 == "native",
           bor_2 == "native") %>% 
    mutate(language_1 = all_combinations[i,]$lemma_1,
           language_2 = all_combinations[i,]$lemma_2) %>% 
    select(-bor_1, -bor_2)
}) ->
  lemmata_by_pair
lemmata_by_pair %>%
  mutate(language_merge = str_c(language_1, " | ", language_2)) %>% 
  select(-language_1, -language_2) %>% 
  pivot_longer(names_to = "source", values_to = "lemma", lemma_1:lemma_2) %>% 
  filter(!str_detect(lemma, " ")) %>% 
  distinct() %>% 
  group_by(meaning_ru, language_merge, source) %>% 
  unnest_tokens(output = segments, input = lemma,drop = FALSE, token = stringr::str_split, pattern = "-") %>% 
  mutate(cons_type = ifelse(str_detect(segments, "[aiueo]"), "V", "C"))  %>% 
  group_by(meaning_ru, language_merge, source, lemma) %>% 
  summarize(syllable_structure = str_c(cons_type, collapse = "-")) %>% 
  mutate(syllable_structure = str_replace_all(syllable_structure, "V-C-V", "V C-V"),
         syllable_structure = str_replace_all(syllable_structure, "V-C-V", "V C-V"),
         syllable_structure = str_replace_all(syllable_structure, "V-V", "V V"),
         syllable_structure = str_replace_all(syllable_structure, "V-V", "V V"),
         syllable_structure = str_replace_all(syllable_structure, "V-C-C-V", "V-C C-V"),
         syllable_structure = str_replace_all(syllable_structure, "V-C-C-V", "V-C C-V"),
         syllable_structure = str_replace_all(syllable_structure, "V-C-C-C-V", "V-C-C C-V"),
         syllable_structure = str_replace_all(syllable_structure, "V-C-C-C-V", "V-C-C C-V")) %>%
  filter(str_detect(syllable_structure, "V")) ->
  # check all possible syllables
  # mutate(syllable = str_split(syllable_structure, " ")) %>% unnest_longer(syllable) %>% ungroup() %>% count(syllable)
  for_merge
for_merge %>% 
  select(-lemma) %>% 
  pivot_wider(names_from = source, values_from = syllable_structure) %>% 
  rename(syl_str_1 = lemma_1,
         syl_str_2 = lemma_2) ->
  for_merge_syllable_structure
for_merge %>% 
  select(-syllable_structure) %>% 
  pivot_wider(names_from = source, values_from = lemma) %>% 
  left_join(for_merge_syllable_structure) %>% 
  separate(language_merge, into = c("language_1", "language_2"), sep = " \\| ") %>% 
  na.omit() %>% 
  write_csv("table.csv")