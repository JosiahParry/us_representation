library(rvest)
library(tidyverse)
library(USAboundaries)

#-----------------------------------------------------------------------------#
#------------------------------- Population Data -----------------------------#
#-----------------------------------------------------------------------------#

# create function to parse html table from wikipedia
wiki_census <- function(x) {
  x <- html_table(x, header = F)
  names(x) <- x[1, ]
  x[2:nrow(x),] %>% 
    gather(year, population, -1) %>% 
    filter(year != "Admitted") %>% 
    janitor::clean_names() %>% 
    mutate(population = as.integer(str_remove_all(population, ",")),
           name = str_remove_all(name, "\\[[0-9]+\\]"),
           year = as.integer(year)) %>% 
    as_tibble() %>% 
    return()
}


# read in data from wikipedia
census_tables <- read_html("https://en.wikipedia.org/wiki/List_of_U.S._states_by_historical_population") %>% 
  html_nodes("table.wikitable") %>% 
  # these are the tables that I will actually use with the same structure
  .[c(1,3,4)] %>% 
  # apply the wiki_census() function to each wikipedia table
  map_df(~ wiki_census(.x))

# Get enslaved population from the same source
# enslaved population has a different structure
enslaved <- read_html("https://en.wikipedia.org/wiki/List_of_U.S._states_by_historical_population") %>% 
  html_nodes("table.wikitable") %>% 
  # these are the tables that I will actually use
  .[2] %>% 
  map_df(~ wiki_census(.x)) %>% 
  rename(enslaved_pop = population)


#-----------------------------------------------------------------------------#
#------------------------------- Congress Data -------------------------------#
#-----------------------------------------------------------------------------#
# read the representatives apportionment table from wikipedia
reps_wiki <- read_html("https://en.wikipedia.org/wiki/United_States_congressional_apportionment") %>% 
  html_nodes("table.wikitable") %>% 
  .[[2]] %>%  html_table(header = F) %>% 
  as_tibble() 

# restructuring data frame to be cleaned
col_names <- reps_wiki[2, ]
reps <- reps_wiki[6:nrow(reps_wiki),]
names(reps) <- col_names


# cleaning data frame, making it tidy
reps_clean <- reps %>% 
  select(-Statehoodorder) %>% 
  gather(year, n_reps, -Year) %>% 
  rename(state = Year) %>% 
  mutate(n_reps = as.integer(n_reps),
         # rounding to the nearest census year to match census data
         year = round(as.integer(year), -1)) %>% 
  # get the states name and abbreviations from the us state shapefile 
  # from usaboundaries and join onto the reps data set
  left_join(us_states() %>% 
              as_tibble() %>% 
              select(state_name, state_abbr), 
            by = c("state" = "state_abbr")) %>% 
  group_by(year) %>% 
  # find the total number of representatives for a given year
  mutate(total_reps = sum(n_reps, na.rm = TRUE)) %>% 
  ungroup() %>% 
  # scale the number of reps by the total number of reps
  mutate(relative_rep_power = n_reps/total_reps)



#-------------------------------- Join Data ----------------------------------#

# Join the tables and create a secondary population column
# pop represents apportioned population based on 3/5th rule :( 
census <- left_join(census_tables, enslaved,
                    by = c("name", "year")) %>% 
  mutate(free_pop = population - enslaved_pop,
         apportioned_pop = coalesce(as.integer(round(population + enslaved_pop * 0.6)), population))


#------------------------------- Write Data ----------------------------------#
write_csv(census_tables, "data/census_state_pop.csv")
write_csv(enslaved, "data/us_enslaved_pop.csv")
write_csv(reps_clean, "data/us_house.csv")
write_csv(census, "data/census_cleaned_data.csv")

