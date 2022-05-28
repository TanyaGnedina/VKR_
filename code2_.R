library(tidyverse)
library(readxl)
df <- read_excel("/Users/tatanagnedina/table.xlsx")



df %>% 
  
  glimpse()




df %>% 
  
  count(na = is.na(meaning_ru), glottocode) %>% 
  
  pivot_wider(values_from = n, names_from = na)







df %>% 
  
  filter(glottocode != "botl1242",
         
         glottocode != "botl1242") %>% 
  
  select(glottocode, ipa, meaning_ru) %>% 
  
  group_by(glottocode) %>% 
  
  count(meaning_ru) %>% 
  
  filter(n == 1) %>% 
  
  mutate(SELECT = TRUE) %>% 
  
  left_join(df) %>% 
  
  filter(SELECT) %>% 
  
  select(glottocode, ipa, meaning_ru) %>% 
  
  pivot_wider(names_from = glottocode, values_from = ipa) %>% 
  
  View()