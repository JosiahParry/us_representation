library(rvest)
library(glue)
library(tidyverse)


# Create a tibble with senate district numbers 
# create a url from statisticsatlas.com
sds <- tibble(sd_id = 1:24L) %>% 
  mutate(sd_url = glue("https://statisticalatlas.com/state-upper-legislative-district/New-Hampshire/State-Senate-District-{sd_id}/Overview"))




sa_scrape <- function(url) {
  session <- html_session(url)
  
  categories <- session %>%
    html_nodes(".info-table-title-td") %>% html_text() %>%
    str_replace_all("\n", "") %>% as_tibble() %>% 
    transmute(category = str_remove(str_trim(value),":") %>% tolower())
  
  values <- session %>%
    html_nodes(".info-table-contents-td") %>% html_text() %>%
    str_replace_all("\n", "") %>% as_tibble() 
  
  return(bind_cols(categories, values))
}

sd_results <- sds %>% 
  mutate(results = map(sd_url, sa_scrape)) %>% 
  unnest()

town_categories <-  c("grants", "locations", "purchases", "towns", "townships", "unincorporated places", "location", "cities", "town")

sd_towns <- sd_results %>%
  filter(category %in% town_categories) %>%
  mutate(value = str_split(value, ", ")) %>% 
  unnest() %>% 
  mutate(town = tolower(value)) %>% 
  select(-value) %>% 
  mutate(town = case_when(
    category == "locations" ~ paste(town, "location"),
    category == "purchases" ~ paste(town, "purchase"),
    category == "grants" ~ paste(town, "grant"),
    TRUE ~ town
  )) %>% 
  select(-sd_url)

#write_csv(sd_towns, "data/nh/nh_senate_towns.csv")



