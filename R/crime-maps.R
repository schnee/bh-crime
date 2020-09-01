library(tidyverse)
library(janitor)
library(lubridate)
library(h3)
library(sf)
library(rnaturalearth)
library(ggmap)

# Define a function to fix the bbox to be in EPSG:3857
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector,
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")),
                       c("ymin", "xmin", "ymax", "xmax"))

  # Coonvert the bbox to an sf polygon, transform it to 3857,
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))

  # Overwrite the bbox of the ggmap object with the transformed coordinates
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}


crime <- read_csv(here::here("./data/BH_Reported_Crime.csv")) %>%
  clean_names()

crime <- crime %>%
  mutate(occured_date = mdy_hm(occurred_d)) %>%
  mutate(year = as.factor(year(occured_date)))

# use the extents in the data, but expand out a bit. Maybe someone can provide
# the no-kidding bounding box that defines Barton Hills
bh_bbox <- c(left = min(crime$longitude) * 1.00002,
             right = max(crime$longitude) * 0.99998,
             top = max(crime$latitude) * 1.0002,
             bottom = min(crime$latitude) * 0.9998)

# get my favorite map-type
map <- get_stamenmap(bbox = bh_bbox, zoom = 15,
                     maptype="toner-2011")

ggmap(map) +
  geom_jitter(data = crime, aes(x=longitude, y=latitude), size=.2, color="blue") +
  facet_wrap(~year, ncol = 6) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())

ggsave("bh-crime.png", dpi=300, width=4, height=4)

# now bin per hex
h3_res <- 9 # seems about right for Barton Hills

crime <- crime %>%
  mutate(h3_index = geo_to_h3(c(.$latitude, .$longitude), h3_res))

hexes <- crime %>%
  group_by(year,h3_index) %>%
  tally() %>%
  summarise(h3_index = h3_index,
            ct=n,
            sf_geom = h3_to_geo_boundary_sf(h3_index) %>% st_geometry() %>% st_sfc(crs = 4326)) %>%
  mutate(q = as.factor(ntile(ct, 5)))

hexes_3857 <-hexes %>%
  mutate(sf_geom = st_transform(sf_geom, crs = 3857))

# Use the function to convert from crs 4326 to crs 3857:
map_3857 <- ggmap_bbox(map)

ggmap(map_3857) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = hexes_3857, aes(fill = q, geometry = sf_geom),
          alpha = 0.5, color = "blue",
          inherit.aes = FALSE) +
  facet_wrap(~year, ncol = 6) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_brewer("Rank")

ggsave("bh-crime-binned.png", dpi=300, width=4, height=4)
