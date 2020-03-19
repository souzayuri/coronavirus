#' ---
#' title: coronavirus municipios de sao paulo
#' author: mauricio vancine
#' date: 2020-03-19
#' ---

# packages
library(geobr)
library(sf)
library(tmap)
library(tidyverse)

# import data
da <- readr::read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities.csv")
da

# preparate data
da_mun <- da %>% 
  tidyr::separate(city, c("name_muni", "abbrev_state"), sep = "/") %>% 
  dplyr::filter(abbrev_state == "SP", is.na(totalCases) == FALSE) %>% 
  dplyr::mutate(name_muni = stringr::str_to_title(name_muni))
da_mun

# summary
tibble::glimpse(da_mun)

# munipality data
mun <- geobr::read_municipality(code_muni = "all", year = 2018)  %>% 
  dplyr::filter(abbrev_state == "SP")
mun

# join data
da_mun_spa <- mun %>% 
  dplyr::mutate(name_muni = stringr::str_to_title(name_muni)) %>% 
  dplyr::left_join(da_mun[, c(3, 5)], by = "name_muni")
da_mun_spa

# summary and confer data
da_mun_spa %>% 
  dplyr::filter(is.na(totalCases) == FALSE) %>% 
  sf::st_drop_geometry()

# map
map <- tm_shape(da_mun_spa) +
  tm_polygons(border.col = "gray40", col = "totalCases", palette = "Reds", textNA = "Sem registros", 
              title = "Casos", n = 5, style = "jenks") +
  tm_graticules(lines = FALSE) +
  tm_compass(size = 2.5) +
  tm_scale_bar(text.size = .8) +
  tm_layout(title = "Casos confirmados de \n Corona Vírus \n no Estado de São Paulo \n (18-03-2020)",
            title.position = c(.67, .85)) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.2, 0))
map

# export
tmap::save_tmap(map, "corona_virus_municipios_sp.png", dpi = 300)

# end ---------------------------------------------------------------------
