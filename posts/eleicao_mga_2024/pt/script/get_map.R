library(osmdata)
library(geobr)
library(sf)
library(dplyr)

streets <- getbb("Maringá, Paraná, Brasil")%>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary",
                            "secondary", "tertiary",
                            "residential")) %>%
  osmdata_sf()

mga <- read_municipality(code_muni = 4115200) %>% 
  sf::st_transform(crs = "WGS84")

new_streets <- streets$osm_lines %>%
  sf::st_transform(crs = "WGS84") %>%
  sf::st_intersection(mga) |> 
  rmapshaper::ms_simplify()

# sf::st_write(new_streets, "./posts/eleicao_mga_2024/pt/data/new_streets.geojson")
