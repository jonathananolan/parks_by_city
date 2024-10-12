
library(tidyverse)
library(qs)
library(osmdata)
library(sf)
library(leaflet)

source("../population-density/R/functions/ggplot_theme.R")
library(ggrepel)



city_areas_grouped <- qread("city_summaries.qs") %>% 
  filter(!(name %in% c("Nairobi","Puebla"))) %>% 
  filter(population_cum>2000000)


city_areas_grouped %>% 
  filter(type == "golf") %>% 
  mutate(mel = if_else(name == "Melbourne","Melbourne","Other cities"),
         city_type = if_else(park_share_of_land>.15,name,"Other cities"),
         city_type_2 = if_else(name %in% c("London",
                                          "New York City",
                                          #"Berlin",
                                          "Melbourne",
                                          "Brisbane",
                                          "Los Angeles",
                                          "Madrid",
                                          "Sydney"),name,"Other cities"),
         city_label = if_else(city_type_2 == "Other cities","",city_type_2)) %>% 
  ggplot(aes(x = density_cum_without_water, y = park_share_of_land,colour = city_type_2))+
  geom_point(size = 2)+
  theme_jn_caption(plot_type = "point")+
  labs(x = "Population Density (people per square km, excluding ocean)",
       y = "Share of city that is parkland",
       title = "Western cities tend to have ample parkland",
       subtitle = "Parks, Gardens, Nature Reserves and Golf Courses within 20km of city centre",
       caption = "Data from Open Street maps.\nOrange dots are other cities with a population >2m within 20km of center. Nairobi and Puebla excluded due to poor data quality",
       colour = element_blank())+
  geom_text_repel(aes(label = city_label),vjust = 2,position = position_dodge2())+
  theme(legend.position = "none")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(labels = scales::number_format(big.mark = ","))

city_areas_grouped %>% 
  filter(type == "park") %>% 
  mutate(mel = if_else(name == "Melbourne","Melbourne","Other cities"),
         city_type = if_else(park_share_of_land>.15,name,"Other cities"),
         city_type_2 = if_else(name %in% c("London",
                                           "New York City",
                                           #"Berlin",
                                           "Melbourne",
                                           "Brisbane",
                                           "Los Angeles",
                                           "Madrid",
                                           "Sydney"),name,"Other cities"),
         city_label = if_else(city_type_2 == "Other cities","",city_type_2)) %>% 
  ggplot(aes(x = density_cum_without_water, y = park_share_of_land,colour = city_type_2))+
  geom_point(size = 2)+
  theme_jn_caption(plot_type = "point")+
  labs(x = "Population Density (people per square km, excluding ocean)",
       y = "Share of city that is parkland",
       title = "Western cities tend to have ample parkland",
       subtitle = "Parks, Gardens, and Nature Reserves within 20km of city centre",
       caption = "Data from Open Street maps.\nOrange dots are other cities with a population >2m within 20km of center. Nairobi and Puebla excluded due to poor data quality",
       colour = element_blank())+
  geom_text_repel(aes(label = city_label),vjust = 2,position = position_dodge2())+
  theme(legend.position = "none")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(labels = scales::number_format(big.mark = ","))
