
library(tidyverse)
library(qs)
library(osmdata)
library(sf)
library(leaflet)
library(ggrepel)
source("../population-density/R/functions/ggplot_theme.R")

if(!file.exists("data/city_locations.csv")){
download.file("https://raw.githubusercontent.com/jonathananolan/city-density/main/input_data/city_locations_cleaned.csv","data/city_locations.csv")
}
city_locations <- read_csv("data/city_locations.csv") %>% 
  filter(population>2500000)






park_area_collector <- function(input_city) {

  #input_city = "Melbourne"
city<- city_locations %>% filter(name == input_city)


rings <- qread(paste0("../population-density/data/city_summaries/1km_without_water/",city$geoname_id,".qs")) %>% 
  st_transform(6933) %>% 
  filter(dist_km_round<21) %>%
  st_union() 



ring_data <- qread(paste0("../population-density/data/city_summaries/1km_without_water/",city$geoname_id,".qs")) %>% 
  filter(dist_km_round ==20) %>% 
  st_drop_geometry() %>% 
  select(area_cum_without_water,population_cum,area_cum_with_water)
  
# Define the point
point <- st_sfc(st_point(c(city$lon, city$lat)),crs = 4326) %>%
  st_set_crs(4326)

radius = 35000
# Transform to a CRS that uses meters, buffer by the radius, then transform back
buffer <- point %>% 
  st_buffer(radius) 



leaflet() %>% 
  addTiles() %>% 
  addMarkers(data = point) %>% 
  addPolygons(data = buffer)



# Calculate the bounding box from the buffer
bbox <- st_bbox(buffer)

# Create the Overpass query
park <- opq(bbox = bbox) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf()

nature <- opq(bbox = bbox) %>%
  add_osm_feature(key = 'leisure', value = 'nature_reserve') %>% 
  osmdata_sf()

golf <- opq(bbox = bbox) %>%
   add_osm_feature(key = 'leisure', value = 'golf_course') %>%
   osmdata_sf()

gardens <- opq(bbox = bbox) %>%
  add_osm_feature(key = 'leisure', value = 'garden') %>%
  osmdata_sf() 


parklands <- bind_rows(park$osm_polygons,
                       nature$osm_polygons,
                       gardens$osm_polygons) %>%
  mutate(type = "park") %>%
  bind_rows(golf$osm_polygons %>% mutate(type = "golf") )


total_area <- parklands %>% 
  st_transform(st_crs(rings)) %>% 
  st_intersection(rings) 

leaflet(rings %>% st_transform("wgs84")) %>% 
  addTiles() %>% 
 #addPolygons() %>% 
 addPolygons(data = total_area %>% st_transform("wgs84")) 


city_area <-  st_area(rings)

total_area <- parklands %>% 
  st_transform(st_crs(rings)) %>% 
  st_intersection(rings) %>% 
  mutate(area = st_area(.)) %>% 
  st_drop_geometry() %>% 
  group_by(type) %>% 
  summarise(area = sum(area)) %>% 
  mutate(total_area =city_area) %>% 
  mutate(city = input_city) %>% 
  bind_cols(ring_data) %>% 
  mutate(area = as.numeric(area),
         total_area = as.numeric(total_area)) %>% 
  arrange(desc(type)) %>%
  mutate(park_share_of_land = cumsum(area)/total_area,
         park_per_person = cumsum(area)/population_cum)
  

return(total_area)

}


park_area_collector("Sydney")

rm(city_areas)
city_areas <- map_df(cities_list <- c("Melbourne",
                                      "Sydney",
                                      "Brisbane",
                                      "Los Angeles",
                                      "London"
                                      ),park_area_collector, .progress = T)
city_areas
city_areas %>%
  ggplot(aes(x      = city, 
             y      = park_share_of_land, 
             group  = city, 
             colour = type))+
  geom_point(size = 3) 



big_cities <- city_locations %>% filter(population>2500000) %>% 
  mutate(status = replace_na(status,"ok")) %>% 
  filter(country != "China",
         country != "India",
         !(str_detect(status,"Removed from list"))) %>% 
  pull(name)

city_areas <- map_df(big_cities,park_area_collector, .progress = T)

city_areas %>% 
  mutate(total_area = as.numeric(total_area),
         diff = area_cum_without_water/total_area) %>% view()

city_areas %>% 
  filter(type == "golf") %>%
  mutate(mel = if_else(city == "Melbourne","Melbourne","Other cities"),
         city_type = if_else(park_share_of_land > .15,city,"Other cities"),
         city_type_2 = if_else(city %in% c("London",
                                           "New York City",
                                           "Berlin",
                                           "Melbourne",
                                           "Brisbane",
                                           #"Los Angeles",
                                           "Sydney"),city,"Other cities"),
         city_label = if_else(city_type_2 == "Other cities","",city_type_2),
         density_cum_without_water = population_cum/area_cum_without_water) %>% 
  ggplot(aes(x = density_cum_without_water, y = park_share_of_land,colour = city_type_2))+
  geom_point(size = 2)+
  theme_jn_caption(plot_type = "point")+
  labs(x = "Population Density (people per square km, excluding ocean)",
       y = "Share of city that is parkland",
       title = "Melbourne has amazing amounts of parkland",
       subtitle = "Open space and gardens within 20km of city centre",
       colour = element_blank())+
  geom_text_repel(aes(label = city_label),vjust = 2,position = position_dodge2())+
  #theme(legend.position = "none")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(labels = scales::number_format(big.mark = ","))
