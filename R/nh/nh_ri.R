library(tidyverse)
library(ggrepel)
library(plotly)
library(glue)

# pop estimates come from https://www.nh.gov/osi/data-center/population-estimates.htm
nh_pop <- read_csv("data/nh/nh_population-estimates-2017.csv") %>% 
  na.omit() %>% 
  gather(year, pop, -county, -municipality) %>% 
  mutate_if(is.character, tolower) %>% 
  filter(year == 2017)


nh_reps <- read_csv("data/nh/nh_reps_18.csv")

total_reps <- length(unique(nh_reps$rep_name))

nh_house_ri <- nh_reps %>% 
  group_by(municipality) %>% 
  summarise(n_reps = n()) %>% 
  mutate(share_reps = n_reps / total_reps) %>% 
  inner_join(nh_pop) %>% 
  mutate(total_pop = sum(pop, na.rm = TRUE),
         share_pop = pop / total_pop,
         house_rep_index = share_reps/ share_pop - 1)


gg_hri <- nh_house_ri %>% 
  mutate(id = row_number(),
         rep_group = case_when(
    house_rep_index > -0.05 & house_rep_index < 0.05  ~ "Directly Represented",
    house_rep_index < -0.05 ~ "Underrepresented",
    house_rep_index > 0.05 ~ "Overrepresented"
  ), 
  label = ifelse(house_rep_index > 20, 
                 glue("{str_to_title(municipality)}: {round(house_rep_index)}"), NA)) %>% 
  ggplot(aes(id, house_rep_index, color = rep_group, label = label)) + 
  geom_point(alpha = 0.5) + 
  geom_text_repel(color = "#7a7a73") + 
  #coord_flip() +
  #josi_theme() +
  theme_minimal() + 
  scale_color_manual(values = c("#fff370", "#609dff", "orange"))

ggplotly(gg_hri)

sd_towns <- read_csv("data/nh/nh_senate_towns.csv")


nh_senate_ri <- nh_pop %>% 
  inner_join(sd_towns, by = c("municipality" = "town")) %>% 
  select(-category, town = municipality) %>% 
  group_by(sd_id) %>% 
  summarise(pop = sum(pop)) %>% 
  #group_by(year) %>% 
  mutate(total_pop = sum(pop)) %>% 
  ungroup() %>% 
  mutate(share_pop = pop / total_pop,
         senate_rep_index = (1/24)/ share_pop - 1)


nh_s_ri <- nh_senate_ri %>% 
  mutate(rep_group = case_when(
            (senate_rep_index > -0.05 & senate_rep_index < 0.05)  ~ "Directly Represented",
            senate_rep_index < -0.05 ~ "Underrepresented",
            senate_rep_index > 0.05 ~ "Overrepresented"
         ), 
         label = ifelse(senate_rep_index > 20, 
                        glue("{str_to_title(municipality)}: {round(senate_rep_index)}"), NA)) %>% 
  ggplot(aes(sd_id, senate_rep_index, color = rep_group, label = label)) + 
  geom_point(alpha = 0.5) + 
  geom_text_repel(color = "#7a7a73") + 
  #coord_flip() +
  #josi_theme() +
  theme_minimal() + 
  scale_color_manual(values = c("#fff370", "#609dff", "green"))
  
ggplotly(nh_s_ri)

