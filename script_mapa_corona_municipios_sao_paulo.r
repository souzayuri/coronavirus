#' ---
#' title: coronavirus municipios de sao paulo
#' author: mauricio vancine
#' date: 2020-03-19
#' ---

# packages
# remotes::install_github("liibre/coronabr")
library(geobr)
library(sf)
library(tmap)
library(tidyverse)

# import data
da <- readr::read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities.csv")
da

# preparate data
da_mun <- da %>% 
  tidyr::separate(city, c("name_muni", "abbrev_state"), sep = "/")
da_mun

# summary
tibble::glimpse(da_mun)

# munipality data
mun <- geobr::read_municipality(code_muni = "all", year = 2018)
mun

# join data
da_mun_spa <- mun %>% 
  dplyr::left_join(da_mun, by = c("name_muni", "abbrev_state"))
da_mun_spa

# summary data
da_mun_spa %>% 
  sf::st_drop_geometry() %>% 
  tibble::glimpse()

# filter sp
da_mun_spa_sp <- da_mun_spa %>% 
  dplyr::filter(abbrev_state == "SP") %>% 
  dplyr::mutate(casos = totalCases)
da_mun_spa_sp
  
# map
map <- tm_shape(da_mun_spa_sp) +
  tm_polygons(border.col = "gray90", col = "casos", palette = "Reds", textNA = "Sem registros", 
              title = "Casos", n = 10, style = "jenks") +
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
