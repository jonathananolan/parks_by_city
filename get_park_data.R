
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
  filter(population>1500000) %>%
  mutate(status = replace_na(status,"ok")) %>% 
  filter(!(str_detect(status,"Removed from list")))


park_area_collector <- function(input_city) {
  
 # input_city = 209228

city<- city_locations %>% filter(geoname_id == input_city)
print(city$name)
print(city$geoname_id)

if(!file.exists(paste0("data/",input_city,".qs"))) {

#City area without water can be created from the city-density package here: https://github.com/jonathananolan/city-density
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


city_area <-  as.numeric(st_area(rings))

rings_short <- qread(paste0("../population-density/data/city_summaries/1km_without_water/",city$geoname_id,".qs")) %>% 
  filter(dist_km_round==20) %>% 
  st_drop_geometry()

total_area <- parklands %>% 
  st_transform(st_crs(rings)) %>% 
  st_intersection(rings) %>% 
  select(osm_id,type) %>%
  mutate(city_area_manual = city_area,
         geoname_id = input_city) %>% 
  left_join(city %>% select(geoname_id,name)) %>%
  left_join(rings_short)

qs::qsave(total_area,paste0("data/",input_city,".qs"))
return(total_area)

}

}

city_areas <- map_df(city_locations$geoname_id,park_area_collector, .progress = T)
