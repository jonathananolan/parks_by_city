
library(tidyverse)
library(qs)
library(osmdata)
library(sf)
library(leaflet)

source("../population-density/R/functions/ggplot_theme.R")
library(ggrepel)



city_areas_grouped <- qread("city_summaries.qs") %>% 
  filter(!(name %in% c("Nairobi","Puebla"))) %>% 
  filter(population_cum>1000000)

city_areas_grouped %>% filter(name %in% c("Melbourne",
                        "Sydney",
                        "Brisbane",
                        "Los Angeles",
                        "Brisbane",
                        "Perth"
                        )) %>%
  ungroup() %>% 
  mutate(name = fct_reorder(name,park_share_of_land),
         type = if_else(type == "golf","Including golf courses","Excluding golf courses"),
         type = fct_reorder(type,-park_share_of_land)) %>%
  ggplot(aes(x      = name, 
             y      = park_share_of_land, 
             group  = name, 
             colour = type))+
  geom_point(size = 3) +
  theme_jn_caption(plot_type = "point")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0,NA))+
  labs(y = "Share of total land",
       title = "Even not counting gold courses,\nAustalian cities have huge amounts of open space",
       subtitle = "Parks, Nature Reserves, Gardens and Golf Courses within 20km of city centre",
       caption = "Data from Open Street maps.",
       group = element_blank(),
       colour = element_blank(),
       x = element_blank())
