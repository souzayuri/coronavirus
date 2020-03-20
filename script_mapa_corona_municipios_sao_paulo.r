#' ---
#' title: coronavirus municipios de sao paulo
#' author: mauricio vancine
#' date: 2020-03-20
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
  dplyr::mutate(name_muni = stringr::str_to_title(name_muni))
da_mun

# summary
tibble::glimpse(da_mun)

# munipality data
mun <- geobr::read_municipality(code_muni = "all", year = 2018)
mun

# state data
sta <- geobr::read_state(code_state = "all", year = 2018)
sta

# join data
da_mun_spa <- mun %>% 
  dplyr::mutate(name_muni = stringr::str_to_title(name_muni)) %>% 
  dplyr::left_join(da_mun[, c(3, 5)], by = "name_muni")
da_mun_spa

# summary and confer data
da_mun_spa %>% 
  dplyr::filter(is.na(totalCases) == FALSE) %>% 
  sf::st_drop_geometry()

# map for sao paulo
map_sp <- da_mun_spa %>% 
  dplyr::filter(abbrev_state == "SP") %>% 
  tm_shape() +
  tm_polygons(border.col = "gray40", col = "totalCases", palette = "Reds", textNA = "Sem registros", 
              title = "Casos", n = 5, style = "jenks") +
  tm_graticules(lines = FALSE) +
  tm_compass(size = 2.5) +
  tm_scale_bar(text.size = .8) +
  tm_layout(title = "Casos confirmados de \n Corona Vírus \n no Estado de São Paulo \n (19-03-2020)",
            title.position = c(.67, .85)) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.2, 0))
map_sp

# export
tmap::save_tmap(map_sp, "corona_virus_municipios_sp.png", dpi = 300)


# map for brazil
map_br_mun <- da_mun_spa %>% 
  tm_shape() +
  tm_polygons(border.col = "gray50", col = "totalCases", palette = "Reds", textNA = "Sem registros", 
              title = "Casos", n = 5, style = "jenks") +
  tm_shape(sta) +
  tm_borders(lwd = 1, col = "gray20") +
  tm_graticules(lines = FALSE) +
  tm_compass(position = c(0, 0)) +
  tm_scale_bar(text.size = .8, position = c(0, 0)) +
  tm_layout(title = "Casos confirmados de \n Corona Vírus no Brasil \n (19-03-2020)",
            title.position = c(.6, .9),
            legend.position = c(.15, .1)) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.5, 0))
map_br_mun

# export
tmap::save_tmap(map_br_mun, "corona_virus_municipios_br.png", dpi = 300)


# state map
# summarise data
da_sta <- da_mun %>% 
  dplyr::select(abbrev_state, totalCases) %>% 
  dplyr::group_by(abbrev_state) %>% 
  dplyr::summarise(casos = sum(totalCases))
da_sta

# join data
da_sta_spa <- sta %>%
  dplyr::mutate(abbrev_state = as.character(abbrev_state)) %>% 
  dplyr::left_join(da_sta, by = "abbrev_state")
da_sta_spa

# confer
sf::st_drop_geometry(da_sta_spa)

# map for brazil
map_br_sta <- da_sta_spa %>% 
  tm_shape() +
  tm_polygons(border.col = "gray50", col = "casos", palette = "Reds", textNA = "Sem registros", 
              title = "Casos", n = 5, style = "jenks") +
  tm_graticules(lines = FALSE) +
  tm_compass(position = c(0, 0)) +
  tm_scale_bar(text.size = .8, position = c(0, 0)) +
  tm_layout(title = "Casos confirmados de \n Corona Vírus no Brasil \n (19-03-2020)",
            title.position = c(.6, .9),
            legend.position = c(.15, .1)) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.5, 0))
map_br_sta

# export
tmap::save_tmap(map_br_sta, "corona_virus_estados_br.png", dpi = 300)

# end ---------------------------------------------------------------------
