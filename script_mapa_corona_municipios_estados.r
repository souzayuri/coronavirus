#' ---
#' title: covid19 municipios do brasil e sao paulo
#' author: mauricio vancine
#' date: 2020-03-21
#' ---

# packages
library(geobr)
library(magick)
library(sf)
library(tmap)
library(tidyverse)

# state -------------------------------------------------------------------
# import data
state_cases <- readr::read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-total.csv") %>% 
  dplyr::rename(abbrev_state = state)
state_cases

# state geodata
state_geo <- geobr::read_state(code_state = "all", year = 2018)
state_geo

# join data
state_geo_cases <- state_geo %>%
  dplyr::mutate(abbrev_state = as.character(abbrev_state)) %>% 
  dplyr::left_join(state_cases, by = "abbrev_state")
state_geo_cases

# confer
sf::st_drop_geometry(state_geo_cases)

# state total
map_state_total <- state_geo_cases %>% 
  tm_shape() +
  tm_polygons(border.col = "gray50", col = "totalCases", palette = "Reds", textNA = "Sem registros", 
              title = "Casos", n = 5, style = "pretty") +
  tm_graticules(lines = FALSE) +
  tm_compass(position = c(.8, .1)) +
  tm_scale_bar(text.size = .8, position = c(.55, .02)) +
  tm_layout(title = "Casos confirmados de \n COVID19 no Brasil \n (20-03-2020) \n (Total)",
            title.position = c(.65, .9)) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.5, 0))
map_state_total

# export
tmap::save_tmap(map_state_total, "covid19_estados_brasil_total.png", dpi = 300)

# state ms
map_state_ms <- state_geo_cases %>% 
  tm_shape() +
  tm_polygons(border.col = "gray50", col = "totalCasesMS", palette = "Reds", textNA = "Sem registros", 
              title = "Casos", n = 5, style = "pretty") +
  tm_graticules(lines = FALSE) +
  tm_compass(position = c(.8, .1)) +
  tm_scale_bar(text.size = .8, position = c(.55, .02)) +
  tm_layout(title = "Casos confirmados de \n COVID19 no Brasil \n (20-03-2020) \n (Ministério da Saúde)",
            title.position = c(.65, .9)) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.5, 0))
map_state_ms

# export
tmap::save_tmap(map_state_ms, "covid19_estados_brasil_ministerio_saude.png", dpi = 300)

# state not ms
map_state_not_ms <- state_geo_cases %>% 
  tm_shape() +
  tm_polygons(border.col = "gray50", col = "notConfirmedByMS", palette = "Reds", textNA = "Sem registros", 
              title = "Casos", n = 5, style = "pretty") +
  tm_graticules(lines = FALSE) +
  tm_compass(position = c(.8, .1)) +
  tm_scale_bar(text.size = .8, position = c(.55, .02)) +
  tm_layout(title = "Casos confirmados de \n COVID19 no Brasil \n (20-03-2020) \n (não confirmados pelo \n Ministério da Saúde)",
            title.position = c(.65, .9)) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.5, 0))
map_state_not_ms

# export
tmap::save_tmap(map_state_not_ms, "covid19_estados_brasil_nao_confirmado_ministerio_saude.png", dpi = 300)

# state deaths
map_state_dea <- state_geo_cases %>% 
  tm_shape() +
  tm_polygons(border.col = "gray50", col = "deaths", palette = "Reds", textNA = "Sem registros", 
              title = "Casos", n = 5, style = "pretty") +
  tm_graticules(lines = FALSE) +
  tm_compass(position = c(.8, .1)) +
  tm_scale_bar(text.size = .8, position = c(.55, .02)) +
  tm_layout(title = "Casos de mortes de \n COVID19 no Brasil \n (20-03-2020)",
            title.position = c(.65, .9)) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.5, 0))
map_state_dea

# export
tmap::save_tmap(map_state_dea, "covid19_estados_brasil_mortes.png", dpi = 300)

# municipality ------------------------------------------------------------
# import data
mun_cases <- readr::read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities.csv") %>% 
  tidyr::separate(city, c("name_muni", "abbrev_state"), sep = "/") %>% 
  dplyr::mutate(name_muni = stringr::str_to_title(name_muni))
mun_cases

# summary
tibble::glimpse(mun_cases)

# munipality geodata
mun_geo <- geobr::read_municipality(code_muni = "all", year = 2018)
mun_geo

# join data
mun_geo_cases <- mun_geo %>% 
  dplyr::mutate(name_muni = stringr::str_to_title(name_muni),
                abbrev_state = as.character(abbrev_state)) %>% 
  dplyr::left_join(mun_cases[, 3:5], by = c("name_muni", "abbrev_state"))
mun_geo_cases

# municipality for brazil
map_br_mun <- mun_geo_cases %>% 
  tm_shape() +
  tm_polygons(border.col = "gray50", col = "totalCases", palette = "Reds", textNA = "Sem registros", 
              title = "Casos", n = 5, style = "pretty") +
  tm_shape(state_geo) +
  tm_borders(lwd = 1, col = "gray20") +
  tm_graticules(lines = FALSE) +
  tm_compass(position = c(.8, .1)) +
  tm_scale_bar(text.size = .8, position = c(.55, .02)) +
  tm_layout(title = "Casos confirmados de \n COVID19 no Brasil \n (20-03-2020)",
            title.position = c(.65, .9),
            legend.position = c(.02, .02)) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.5, 0))
map_br_mun

# export
tmap::save_tmap(map_br_mun, "covid19_municipios_brasil.png", dpi = 300)

# summary and confer data
mun_geo_cases %>% 
  dplyr::filter(is.na(totalCases) == FALSE) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::filter(abbrev_state == "SP")

# map for sao paulo
map_sp <- mun_geo_cases %>% 
  dplyr::filter(abbrev_state == "SP") %>% 
  tm_shape() +
  tm_polygons(border.col = "gray40", col = "totalCases", palette = "Reds", textNA = "Sem registros", 
              title = "Casos", n = 5, style = "pretty") +
  tm_graticules(lines = FALSE) +
  tm_compass(size = 2.5) +
  tm_scale_bar(text.size = .8) +
  tm_layout(title = "Casos confirmados de \n COVID19 no Estado de \n São Paulo \n (20-03-2020)",
            title.position = c(.68, .85)) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.2, 0))
map_sp

# export
tmap::save_tmap(map_sp, "covid19_municipios_sao_paulo.png", dpi = 300)

# state in time -----------------------------------------------------------
# import data
state_cases_time <- readr::read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv") %>% 
  dplyr::rename(abbrev_state = state) %>% 
  dplyr::filter(abbrev_state != "TOTAL")
state_cases_time

# centroids
state_geo_cen <- sf::st_centroid(state_geo)
state_geo_cen

# join data
state_geo_cen_cases_time <- state_geo_cen %>%
  dplyr::mutate(abbrev_state = as.character(abbrev_state)) %>% 
  dplyr::left_join(state_cases_time, by = "abbrev_state") %>% 
  dplyr::mutate(date = as.factor(date)) %>% 
  tidyr::drop_na(date)
state_geo_cen_cases_time

# confer
sf::st_drop_geometry(state_geo_cen_cases_time)

m_save = world %>% filter(continent != "Antarctica") %>% 
  tm_shape() + 
  tm_polygons() +
  tm_shape(urban_agglomerations) +
  tm_dots(size = "population_millions", title.size = "Population (m)", alpha = 0.5, col = "red") +
  tm_facets(along = "year", free.coords = FALSE)

# state total - demora
map_state_total_time <- tm_shape(state_geo, bbox = sf::st_bbox(state_geo)) +
  tm_polygons() +
  tm_shape(state_geo_cen_cases_time) +
  tm_symbols(size = "totalCases", scale = 2, title.size = "Casos", alpha = 0.5, col = "red", border.col = "darkred") +
  tm_facets(along = "date") +
  tm_graticules(lines = FALSE) +
  tm_compass(position = c(.8, .1)) +
  tm_scale_bar(text.size = .8, position = c(.55, .02)) +
  tm_layout(title = "Casos confirmados de \n COVID19 no Brasil \n ao longo dos dias",
            title.position = c(.6, .9),
            legend.position = c("left", "bottom")) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.5, 0))
map_state_total_time

# export
tmap::tmap_animation(tm = map_state_total_time, filename = "brasil_states_time.gif", wi = 2000, he = 2000, delay = 100)
# magick::image_read("brasil_states.gif")

# end ---------------------------------------------------------------------