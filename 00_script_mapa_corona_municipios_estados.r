#' ---
#' title: covid19 municipios e estados do brasil
#' author: mauricio vancine
#' date: 2021-03-23
#' ---

# packages
library(geobr)
library(lubridate)
library(magick) 
library(sf)
library(tmap)
library(tidyverse)

# packages in linux (ubuntu e linux mint)
# sudo apt-get install imagemagick


# graphics ----------------------------------------------------------------
state_cases_time <- readr::read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv")
state_cases_time

# figura casos
fig_casos <- state_cases_time %>% 
  dplyr::filter(state == "TOTAL") %>% 
  ggplot() +
  aes(x = date, y = totalCases) +
  geom_line(size = 2, color = "steelblue") +
  geom_label(aes(x = date, y = totalCases, label = totalCases)) +
  labs(x = "Data", 
       y = "Número de casos confirmados", 
       title = "Número de casos confirmados de coronavírus no Brasil") +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90), 
        legend.position = "none")
fig_casos
ggsave(filename = "covid19_total_casos.png", plot = fig_casos, width = 25, height = 20, units = "cm", dpi = 300)

# figura states
uf10 <- state_cases_time %>% 
  filter(date == max(date), totalCases > 10, state != "TOTAL") %>%
  select(state)
uf10

fig_uf <- state_cases_time %>% 
  dplyr::filter(state %in% uf10$state) %>% 
  dplyr::mutate(nome = reorder(state, -totalCases)) %>% 
  ggplot(aes(x = date, y = totalCases, col = state)) + 
  geom_line() +
  geom_point(size = 2) +
  labs(x = "Data", 
       y = "Número de casos confirmados", 
       title = "Estados com mais de 10 casos", 
       fill = "UF") +
  guides(color = guide_legend("UF")) +
  scale_color_viridis_d() +
  scale_x_date(date_breaks = "1 day", 
               date_labels = "%d/%m") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90), 
        legend.title = element_text(size = 7), 
        legend.text = element_text(size = 7))
fig_uf

ggsave(filename = "covid19_total_casos_estados.png", plot = fig_uf, width = 25, height = 20, units = "cm", dpi = 300)

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
              title = "Casos", n = 15, style = "pretty") +
  tm_graticules(lines = FALSE) +
  tm_compass(position = c(.8, .1)) +
  tm_scale_bar(text.size = .8, position = c(.55, .02)) +
  tm_layout(title = paste0("Casos confirmados de \n COVID19 no Brasil \n (Total) \n (", lubridate::today(), ")"),
            title.position = c(.65, .9)) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.5, 0))
map_state_total

# export
tmap::save_tmap(map_state_total, "covid19_brasil_estados_total.png", dpi = 300)

# state ms
map_state_ms <- state_geo_cases %>% 
  tm_shape() +
  tm_polygons(border.col = "gray50", col = "totalCasesMS", palette = "Reds", textNA = "Sem registros", 
              title = "Casos", n = 15, style = "pretty") +
  tm_graticules(lines = FALSE) +
  tm_compass(position = c(.8, .1)) +
  tm_scale_bar(text.size = .8, position = c(.55, .02)) +
  tm_layout(title = paste0("Casos confirmados de \n COVID19 no Brasil \n (Ministério da Saúde) \n (", lubridate::today(), ")"),
            title.position = c(.65, .9)) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.5, 0))
map_state_ms

# export
tmap::save_tmap(map_state_ms, "covid19_brasil_estados_ministerio_saude.png", dpi = 300)

# state not ms
map_state_not_ms <- state_geo_cases %>% 
  tm_shape() +
  tm_polygons(border.col = "gray50", col = "notConfirmedByMS", palette = "Reds", textNA = "Sem registros", 
              title = "Casos", n = 10, style = "pretty") +
  tm_graticules(lines = FALSE) +
  tm_compass(position = c(.8, .1)) +
  tm_scale_bar(text.size = .8, position = c(.55, .02)) +
  tm_layout(title = paste0("Casos confirmados de \n COVID19 no Brasil \n (não confirmados pelo \n Ministério da Saúde) \n (", lubridate::today(), ")"),
            title.position = c(.68, .9),
            title.size = 1) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.5, 0))
map_state_not_ms

# export
tmap::save_tmap(map_state_not_ms, "covid19_brasil_estados_nao_confirmado_ministerio_saude.png", dpi = 300)

# state deaths
map_state_dea <- state_geo_cases %>% 
  tm_shape() +
  tm_polygons(border.col = "gray50", col = "deaths", palette = "Reds", textNA = "Sem registros", 
              title = "Casos", n = 10, style = "pretty") +
  tm_graticules(lines = FALSE) +
  tm_compass(position = c(.8, .1)) +
  tm_scale_bar(text.size = .8, position = c(.55, .02)) +
  tm_layout(title = paste0("Casos de mortes de \n COVID19 no Brasil \n (", lubridate::today(), ")"),
            title.position = c(.65, .9)) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.5, 0))
map_state_dea

# export
tmap::save_tmap(map_state_dea, "covid19_brasil_estados_mortes.png", dpi = 300)

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

# state map time
map_state_total_time <- tm_shape(state_geo, bbox = sf::st_bbox(state_geo)) +
  tm_polygons() +
  tm_shape(state_geo_cen_cases_time) +
  tm_symbols(size = "totalCases", scale = 2, title.size = "Casos", alpha = .5, col = "red", border.col = "darkred") +
  tm_facets(along = "date") +
  tm_graticules(lines = FALSE) +
  tm_compass(position = c(.8, .1)) +
  tm_scale_bar(text.size = .8, position = c(.55, .02)) +
  tm_layout(title = "Casos confirmados de \n COVID19 no Brasil \n ao longo dos dias \n (Estado)",
            title.position = c(.6, .9),
            legend.position = c("left", "bottom")) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.5, 0))

# export
tmap::tmap_animation(tm = map_state_total_time, filename = "covid19_brasil_estados_evolucao.gif", wi = 2000, he = 2000, delay = 100)
# magick::image_read("brasil_states.gif")

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
  tm_fill(col = "totalCases", palette = "Reds", 
              textNA = "Sem registros", colorNA = "gray70",
              title = "Casos", n = 10, style = "pretty") +
  tm_borders(col = "gray30", lwd = .1) +
  tm_shape(state_geo) +
  tm_borders(lwd = .5, col = "gray20") +
  tm_graticules(lines = FALSE) +
  tm_compass(position = c(.8, .1)) +
  tm_scale_bar(text.size = .8, position = c(.55, .02)) +
  tm_layout(title = paste0("Casos confirmados de \n COVID19 no Brasil \n (", lubridate::today(), ")"),
            title.position = c(.65, .9),
            legend.position = c(.02, .02)) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.5, 0))
map_br_mun

# export
tmap::save_tmap(map_br_mun, "covid19_brasil_municipios.png", dpi = 300)

# summary and confer data
mun_geo_cases %>% 
  dplyr::filter(is.na(totalCases) == FALSE) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::filter(abbrev_state == "SP")

# map for sao paulo
mun_geo_cases_sp <- mun_geo_cases %>% 
  dplyr::filter(abbrev_state == "SP")

map_sp <- tm_shape(mun_geo_cases_sp) +
  tm_polygons(border.col = "gray40", col = "totalCases", palette = "Reds", textNA = "Sem registros", 
              title = "Casos", n = 10, style = "pretty") +
  tm_graticules(lines = FALSE) +
  tm_compass(size = 2.5) +
  tm_scale_bar(text.size = .8) +
  tm_layout(title = paste0("Casos confirmados de \n COVID19 no Estado de \n São Paulo \n (", lubridate::today(), ")"),
            title.position = c(.68, .85)) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.2, 0))
map_sp

# export
tmap::save_tmap(map_sp, "covid19_sao_paulo_municipios.png", dpi = 300)

# municipality in time ----------------------------------------------------
# import data
mun_cases_time <- readr::read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv") %>% 
  dplyr::filter(city != "TOTAL") %>% 
  tidyr::separate(city, c("name_muni", "abbrev_state"), sep = "/") %>% 
  dplyr::mutate(name_muni = stringr::str_to_title(name_muni))

# centroids
mun_geo_cen <- geobr::read_municipal_seat(year = 2010)
mun_geo_cen

# join data
mun_geo_cen_cases_time <- mun_geo_cen %>%
  dplyr::mutate(name_muni = as.character(name_muni)) %>% 
  dplyr::left_join(mun_cases_time, by = "name_muni") %>% 
  dplyr::mutate(date = as.factor(date)) %>% 
  tidyr::drop_na(date)
mun_geo_cen_cases_time

# confer
sf::st_drop_geometry(mun_geo_cen_cases_time)

# municipality map time
map_mun_total_time <- tm_shape(state_geo, bbox = sf::st_bbox(state_geo)) +
  tm_polygons() +
  tm_shape(mun_geo_cen_cases_time) +
  tm_symbols(size = "totalCases", scale = 2, title.size = "Casos", alpha = .5, col = "red", border.col = "darkred") +
  tm_facets(along = "date") +
  tm_graticules(lines = FALSE) +
  tm_compass(position = c(.8, .1)) +
  tm_scale_bar(text.size = .8, position = c(.55, .02)) +
  tm_layout(title = "Casos confirmados de \n COVID19 no Brasil \n ao longo dos dias \n (Municípios)",
            title.position = c(.6, .9),
            legend.position = c("left", "bottom")) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.5, 0))

# export
tmap::tmap_animation(tm = map_mun_total_time, filename = "covid19_brasil_municipios_evolucao.gif", wi = 2000, he = 2000, delay = 100)
# magick::image_read("brasil_states.gif")

# sao paulo municipality map time
mun_geo_sp <- mun_geo %>% 
  dplyr::filter(abbrev_state == "SP")

map_mun_sp_time <- tm_shape(mun_geo_cases_sp, bbox = sf::st_bbox(mun_geo_cases_sp)) +
  tm_polygons(border.col = "gray40", col = "totalCases", palette = "Reds", textNA = "Sem registros", 
              title = "Casos", n = 5, style = "pretty") +
  tm_shape(mun_geo_cen_cases_time %>% dplyr::filter(abbrev_state.x == "SP")) +
  tm_symbols(size = "totalCases", scale = 2, title.size = "Casos", alpha = .5, col = "red", border.col = "darkred") +
  tm_facets(along = "date") +
  tm_graticules(lines = FALSE) +
  tm_compass(position = c(.8, .1)) +
  tm_scale_bar(text.size = .8, position = c(.55, .02)) +
  tm_layout(title = "Casos confirmados de \n COVID19 em São Paulo \n ao longo dos dias \n (Municípios)",
            title.position = c(.7, .8),
            title.size = .9,
            legend.position = c("left", "bottom")) +
  tm_credits("Fonte: https://labs.wesleycota.com/sarscov2/br", position = c(.5, 0))

# export
tmap::tmap_animation(tm = map_mun_sp_time, filename = "covid19_sao_paulo_municipios_evolucao.gif", wi = 2000, he = 1700, delay = 100)
# magick::image_read("brasil_states.gif")

# end ---------------------------------------------------------------------