
library(tidyverse)
library(qs)
library(osmdata)
library(sf)
library(leaflet)

source("../population-density/R/functions/ggplot_theme.R")

if(!file.exists("data/city_locations.csv")){
  download.file("https://raw.githubusercontent.com/jonathananolan/city-density/main/input_data/city_locations_cleaned.csv","data/city_locations.csv")
}

city_locations <- read_csv("data/city_locations.csv")


qs_files <- list.files("data",pattern = ".qs",full.names = T)

parks_data <- map_df(qs_files,qread) 

city_summaries <- parks_data %>% 
  mutate(area = st_area(.)) %>% 
  st_drop_geometry() %>% 
  group_by(name,type,geoname_id) %>% 
  summarise(area = sum(area),
            area_cum_without_water = first(area_cum_without_water),
            population_cum = first(population_cum),
            city_area_manual = first(city_area_manual)
            ,.groups = "drop") %>% 
  group_by(name,geoname_id) %>% 
  mutate(area = as.numeric(area),
         error_rate = area_cum_without_water/city_area_manual) %>% 
  arrange(desc(type)) %>%
  mutate(park_share_of_land = cumsum(area)/city_area_manual,
         park_per_person = cumsum(area)/population_cum)


city_summaries %>% qsave("city_summaries.qs")

geonames <- unique(parks_data$geoname_id)


city <- city_locations %>% 
  filter(geoname_id == geonames[50]) 
# Define the point
point <- st_sfc(st_point(c(city$lon, city$lat)), crs = st_crs(4326))

radius = 21000
# Transform to a CRS that uses meters, buffer by the radius, then transform back
buffer <- st_transform(point, 3857) %>% 
  st_buffer(radius) %>% 
  st_transform(4326)

est_area <- parks_data %>% 
  filter(geoname_id == geonames[50]) %>% 
  pull(area_cum_with_water)

proj_area = st_area(buffer)



library(tidyverse)
library(qs)
library(osmdata)
library(sf)
library(leaflet)

source("../population-density/R/functions/ggplot_theme.R")

if(!file.exists("data/city_locations.csv")){
  download.file("https://raw.githubusercontent.com/jonathananolan/city-density/main/input_data/city_locations_cleaned.csv","data/city_locations.csv")
}

city_locations <- read_csv("data/city_locations.csv")


qs_files <- list.files("data",pattern = ".qs",full.names = T)

parks_data <- map_df(qs_files,qread) 

city_summaries <- parks_data %>% 
  mutate(area = st_area(.)) %>% 
  st_drop_geometry() %>% 
  group_by(name,type,geoname_id) %>% 
  summarise(area = sum(area),
            area_cum_without_water = first(area_cum_without_water),
            population_cum = first(population_cum),
            city_area_manual = first(city_area_manual),
            density_cum_without_water = first(density_cum_without_water),
            .groups = "drop") %>% 
  group_by(name,geoname_id) %>% 
  mutate(area = as.numeric(area),
         error_rate = area_cum_without_water/city_area_manual) %>% 
  arrange(desc(type)) %>%
  mutate(park_share_of_land = cumsum(area)/city_area_manual,
         park_per_person = cumsum(area)/population_cum)


city_summaries %>% qsave("city_summaries.qs")

geonames <- unique(parks_data$geoname_id)

leaflet_maker <- function(geoname_city){
  
 # geoname_city = geonames[1]
city <- city_locations %>% 
    filter(geoname_id ==geoname_city) 
# Define the point
point <- st_sfc(st_point(c(city$lon, city$lat)), crs = st_crs(4326))

radius = 21000
# Transform to a CRS that uses meters, buffer by the radius, then transform back
buffer <- st_transform(point, "wgs84") %>% 
  st_buffer(radius) %>% 
  st_transform(4326)

est_area <- parks_data %>% 
  filter(geoname_id == geoname_city) %>% 
  pull(area_cum_with_water)

proj_area = st_area(buffer)


print(est_area[1]/proj_area)

leaflet_map <- parks_data %>% 
  filter(geoname_id == geoname_city) %>% 
  st_transform("wgs84") %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons() %>% 
  addPolygons(data = buffer,opacity = .1)

htmlwidgets::saveWidget(leaflet_map,paste0("maps/",city$geoname_id,"_",city$name,".html"))

}

walk(geonames,leaflet_maker)
