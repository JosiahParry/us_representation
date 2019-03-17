library(tidyverse)
library(rvest)

# taken from https://en.wikipedia.org/wiki/New_Hampshire_House_of_Representatives#Members,_2018-2020 wikipedia
session <- html_session("https://en.wikipedia.org/wiki/New_Hampshire_House_of_Representatives#Members,_2018-2020")

current_nh_reps <- session %>% 
  html_nodes(".wikitable") %>% 
  .[3:12] %>% 
  map_dfr(~html_table(.x, fill = T), .id = "county_id") %>% 
  as_tibble() %>% 
  setNames(c("county_id", "district_num", "rep_name", "party", "municipality", "first_elected"))


counties <- tibble(
  county_name = session %>% 
  html_nodes(".mw-headline") %>% 
  html_text() %>% 
  .[5:14] %>% tolower(),
  county_id = as.character(1:10))

nh_reps <- current_nh_reps %>% 
  mutate_if(is.character, tolower) %>% 
  mutate(municipality = str_remove_all(municipality, ("\\s*\\([^\\)]+\\)"))) %>% 
  mutate(municipality = str_split(municipality, ", ")) %>% 
  unnest() %>% 
  mutate(municipality = str_remove_all(municipality, "[[:punct:]]")) %>% 
  select(-first_elected) %>% 
  mutate(district_num = str_pad(district_num, 2, "left", "0")) %>% 
  left_join(counties) %>% 
  mutate(district = glue("{county_name}-{district_num}"))

#write_csv(nh_reps, "data/nh/nh_reps_18.csv")
  