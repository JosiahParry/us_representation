library(tidyverse)
library(ggrepel)
library(glue)

# set ggplot theme
theme_set(theme_void()  + 
            theme(legend.position = "bottom",
                  plot.title = element_text(hjust = 0)))

# read in house of rep data
us_reps <- read_csv("https://raw.githubusercontent.com/JosiahParry/us_representation/master/data/us/us_house.csv") 

# read in historical us population data
us_pop <- read_csv("https://raw.githubusercontent.com/JosiahParry/us_representation/master/data/us/census_state_pop.csv")

# join reps to pop data, clean, calculate rep index
us_house_pop <- left_join(us_reps, us_pop,
                          by = c("state_name" = "name", "year")) %>% 
  filter(year %in% c(1800, 1870, 1940, 2010)) %>% 
  group_by(year) %>% 
  mutate(id = row_number(),
          total_pop = sum(population, na.rm = TRUE),
         share_pop = population / total_pop,
         rep_index = log((n_reps/total_reps) / share_pop),
         senate_rep_index = log((1/50) / share_pop),
         id = reorder(id, rep_index),
         h_ri_cat = case_when(
           rep_index < log(1/.95) & rep_index > log(1/1.05) ~ "One person, one vote",
           rep_index > log(1/.95) ~ "Overrepresented",
           rep_index < log(1/1.05) ~ "Underrepresented"),
         s_ri_cat = case_when(
           senate_rep_index < log(1/.95) & senate_rep_index > log(1/1.05) ~ "One person, one vote",
           senate_rep_index > log(1/.95) ~ "Overrepresented",
           senate_rep_index < log(1/1.05) ~ "Underrepresented"),
         State = glue("{state}: {round(share_pop*100, 2)}%, RI: {round(senate_rep_index, 2)}"))


# get states geospatial data
states <- map_data("state") %>% 
  mutate(region = str_to_title(region)) %>% 
  rename(state_name = region)

# join data to spatial data
states_df <- inner_join(us_house_pop, states, by = "state_name") %>% 
  as_tibble()


# Create map of Senate Rep Index since 1800 in 4 different periods
# This can be thought of as 1800: post constitution
# 1870: post Civil war
# 1940: post depression
# 2010: today-ish
map_sri <- states_df %>% 
  ggplot(aes(long, lat, group = group, 
             # make it a percent
             fill = senate_rep_index)) + 
  geom_polygon(size = 0) + 
  # none of this flat top ugly continental US stuff
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(x = "", y = "", title = "Senate Representation Index") + 
  scale_fill_gradient2(low = "#637dff", high = "#ce6d6d", mid = "white",
                       na.value = "#D1D1D1",
                      guide = guide_colorbar("")) +
  facet_wrap(~year, ncol = 2) 


us_house_pop %>% 
  filter(year == 2010) %>% 
  ungroup() %>% 
  arrange(-senate_rep_index) %>% 
  mutate(rank = row_number(),
         State = ifelse(rank %in% 6:45, NA, State)) %>% 
  mutate(id = fct_reorder(id, senate_rep_index)) %>% 
ggplot(aes(as.numeric(id), senate_rep_index, color = s_ri_cat, label = State)) +
  # geom_ribbon(aes(ymin = -.05, ymax = .05, x = id),
  #             #inherit.aes = FALSE,
  #             fill = "#637dff", alpha = 0.2) +
  geom_polygon(data = tibble(x = c(1, 1, 50, 50),
                             y = c(.05, -.05, -.05, .05)),
                             aes(x = x, y = y), 
               inherit.aes = F, 
               fill = "#637dff", 
               alpha = .25) + 
  geom_hline(yintercept = -.05, color = "#637dff", lty = 2, alpha = .5) +
  geom_hline(yintercept = .05, color = "#637dff", lty = 2, alpha = .5) +
  geom_point() +
  theme_minimal() +
  labs(title = "US House of Representatives" , x = "", y = "Representation Index") +
  scale_color_manual(values = c("#637dff", "#ce6d6d", "#fcb06a")) +
  geom_text_repel(force = 1.2, show.legend = FALSE) + 
  facet_wrap(~i_ri_cat)



x <- tibble(x = c(1, 1, 50, 50),
     y = c(.05, -.05, -.05, .05))

# get senator level information to determine party
state_lookup <- state.name
names(state_lookup) <- state.abb

senators <- read_csv('https://theunitedstates.io/congress-legislators/legislators-current.csv') %>% 
  filter(type == "sen") %>% 
  select(full_name, state, party) %>% 
  mutate(state_name = str_replace_all(state, state_lookup))


state_party <- senators %>%
  mutate(x = 1) %>% 
  spread(party, x, fill = 0) %>% 
  arrange(-Independent) %>% 
  group_by(state) %>% 
  summarise(n_dem = as.factor(sum(Democrat))) %>% 
  left_join(us_house_pop) %>% 
  mutate(senate_rep_index = log((1/50)/(share_pop)),
         id = fct_reorder(id, senate_rep_index),
         party = case_when(
           n_dem == 2 ~ "Democrat",
           n_dem == 0 ~ "Republican", 
           n_dem == 1 ~ "Split / Other"
         ))

# create plot of rank by party.
# only show information for the 10 most extreme values.
state_party %>% 
  filter(year == 2010) %>% 
  ungroup() %>%
  arrange(-senate_rep_index) %>% 
  mutate(rank = row_number(),
         State = ifelse(rank %in% 6:45, NA, State),
         id = fct_reorder(id, senate_rep_index)) %>% 
ggplot(aes(as.numeric(id), senate_rep_index, color = party, label = State)) +
  geom_hline(yintercept = -.05, color = "#637dff", lty = 2, alpha = .5) +
  geom_hline(yintercept = .05, color = "#637dff", lty = 2, alpha = .5) +
  geom_point(show.legend = FALSE) + 
  theme_minimal() +
  labs(title = "", x = "", y = "Representation Index") +
  scale_color_manual(values = c("#637dff", "#ce6d6d", "#fcb06a")) +
  facet_wrap(~party) +
  geom_text_repel(show.legend = FALSE, nudge_y = .05) +
  theme(axis.text.x = element_blank())

state_party %>% 
  mutate(party = fct_rev(party)) %>% 
  ggplot(aes(x = party, y = rep_index,
             fill = party, color = party)) +
  geom_violin(alpha = 0.5, lwd = 0) +
  scale_color_manual(values = c("Democrat" = "#637dff", 
                                "Republican" = "#ce6d6d",
                                "Split / Other" = "#fcb06a")) +
  scale_fill_manual(values = c("Democrat" = "#637dff",
                               "Republican" = "#ce6d6d",
                               "Split / Other" = "#fcb06a")) +
  geom_point(alpha = .5) + 
  #geom_jitter() + 
  theme_minimal() +
  coord_flip() +
  theme(legend.position = "none") + 
  labs(title = "Senate Representation Index",
       subtitle = "Distribution by party", x = "", y = "")
