library(HHSKwkl)
library(tidyverse)
library(glue)
library(waterplanten) 
library(twn)

theme_set(hhskthema())

# library(readxl)
# library(openxlsx)
library(sf)
library(leaflet)

meetpunten <- data_online("meetpunten.rds")
biologie <- data_online("biologie.rds")

# biologie <- 
#   biologie %>% 
#   select(-contains("stadium")) %>% 
#   distinct()

# get_ws_grens()
# get_logo()



param_sel <- "chloride"
compart_sel <- "OW"

biologie %>% 
  filter(!str_detect(mp, "^V"),
         methode == "VEG%",
          year(datum) %in% 2008:2010
         ) %>% 
  add_jaar() %>% 
  # filter(jaar > 2010) %>% 
  select(-contains("stadium")) %>% 
  left_join(waterplanten::preferentie_waarden, by = join_by(naam), relationship = "many-to-many") %>% 
  filter(indicatiewaarde > 0) %>%
  mutate(gewogen_gem = ifelse(is.na(omrekenfactor_naar_mg_l), gewogen_gem, omrekenfactor_naar_mg_l * gewogen_gem)) %>% 
  group_by(mp, parameter, compartiment, jaar) %>% 
  summarise(gewogen_gem = mean(gewogen_gem)) %>% 
  # filter(parameter ==  "totaal fosfor", compartiment == "PW") %>% 
  filter(parameter ==  param_sel, compartiment == compart_sel) %>%
  left_join(meetpunten) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>% 
  ggplot() + 
  geom_sf(data = ws_grens_rd, colour = grijs_m, fill = NA) +
  geom_sf(aes(color = gewogen_gem), size = 2) +
  scale_color_viridis_c() + 
  facet_wrap(~jaar) +
  hhskthema_kaart() + 
  labs(title = paste(param_sel, "-", compart_sel))


biologie %>% 
  filter(str_detect(mp, "^VKP"),
         # methode == "VEG%",
         year(datum) %in% c(2011:2025)
  ) %>% 
  add_jaar() %>% 
  # filter(jaar > 2010) %>% 
  select(-contains("stadium")) %>% 
  left_join(waterplanten::preferentie_waarden, by = join_by(naam), relationship = "many-to-many") %>% 
  filter(indicatiewaarde > 0) %>%
  mutate(gewogen_gem = ifelse(is.na(omrekenfactor_naar_mg_l), gewogen_gem, omrekenfactor_naar_mg_l * gewogen_gem)) %>% 
  group_by(mp, parameter, compartiment, jaar) %>% 
  summarise(gewogen_gem = mean(gewogen_gem)) %>% 
  # filter(parameter ==  "totaal fosfor", compartiment == "PW") %>% 
  filter(parameter ==  param_sel, compartiment == compart_sel) %>%
  left_join(meetpunten) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>% 
  ggplot() + 
  # geom_sf(data = ws_grens_rd, colour = grijs_m, fill = NA) +
  geom_sf(aes(color = gewogen_gem), size = 2) +
  scale_color_viridis_c() + 
  facet_wrap(~jaar) +
  hhskthema_kaart() + 
  labs(title = paste(param_sel, "-", compart_sel))
  

biologie %>% 
  filter(str_detect(mp, "VKP"),
         # year(datum) == 2016
  ) %>% 
  add_jaar() %>% 
  # filter(jaar > 2010) %>% 
  select(-contains("stadium")) %>% 
  left_join(waterplanten::preferentie_waarden, by = join_by(naam), relationship = "many-to-many") %>% 
  # filter(indicatiewaarde > 0) %>%
  mutate(gewogen_gem = ifelse(is.na(omrekenfactor_naar_mg_l), gewogen_gem, omrekenfactor_naar_mg_l * gewogen_gem)) %>% 
  group_by(mp, parameter, compartiment, jaar) %>% 
  summarise(gewogen_gem = mean(gewogen_gem)) %>% 
  # filter(parameter ==  "totaal fosfor", compartiment == "PW") %>% 
  filter(parameter ==  param_sel, compartiment == compart_sel) %>%
  group_by(jaar) %>% 
  summarise(gewogen_gem = mean(gewogen_gem)) %>% 
  ggplot(aes(jaar, gewogen_gem)) +
  geom_line() + geom_point() +
  hhskthema() + 
  labs(title = paste(param_sel, "-", compart_sel)) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(c(0, 0.1)))



biologie %>% 
  filter(str_detect(mp, "S_0674"), methode == "VEG%", year(datum) == 2019) %>% 
  left_join(waterplanten::preferentie_klassen, by = join_by(naam), relationship = "many-to-many") %>% 
  mutate(klasse = fct_reorder(klasse, ondergrens)) %>% 
  filter(indicatiewaarde > 1) %>% 
  group_by(parameter, compartiment, klasse) %>% 
  summarise(relatief_voorkomen = mean(relatief_voorkomen)) %>% 
  ggplot(aes(relatief_voorkomen, klasse)) + 
  geom_col() +
  facet_wrap(~paste(parameter, compartiment, sep = " - "), scales = "free_y")
  
         