library(tidyverse)
library(readxl)
# library(janitor)


add_grenzen <- function(df) {
  df %>% 
    mutate(
      ondergrens = case_when(
        str_detect(klasse, "-") ~ str_remove(klasse, "-.*$"),
        str_detect(klasse, ">") ~ str_remove(klasse, ">")),
      bovengrens = case_when(
        str_detect(klasse, "-") ~ str_remove(klasse, "^.*-"),
        str_detect(klasse, "<") ~ str_remove(klasse, "<"),
        str_detect(klasse, ">") ~ NA),
      .after = klasse
    )
  
} 

verwerk_klassegrenzen <- function(df) {
  
  df %>% 
    .[-(1:3),] %>% 
    janitor::row_to_names(1) %>% 
    select(-ind, -`gewogen gemid.`, -optim., -`gemod. chi waarde`, -any_of("90-quantiel")) %>% 
    rename(naam = 1, nednaam = 2, n_wateren_met_soort = 3) %>% 
    filter(!is.na(naam)) %>% 
    pivot_longer(cols = !1:3, names_to = "klasse", values_to = "relatief_voorkomen") %>% 
    mutate(klasse = str_remove_all(klasse, "[[:space:]]")) %>% 
    add_grenzen() %>% 
    type_convert()
    
}


# Geautomatiseerd ---------------------------------------------------------

referentie_bestanden <- read_excel("data-raw/overzicht_referentiebestanden.xlsx") 

referentie_raw <- referentie_bestanden %>% mutate(raw = map(bestandsnaam, read_excel))

alle_klassen <-
  referentie_raw %>% 
  # group_by(parameter, eenheid, compartiment) %>% 
  mutate(klassen = map(raw, verwerk_klassegrenzen)) %>% 
  unnest(klassen)

# Testcode ----------------------------------------------------------------

library(HHSKwkl)
theme_set(hhskthema())

soorten <- c("Chara globularis", "Elodea nuttallii", "Ceratophyllum demersum")
soorten <- c("Chara globularis", "Utricularia vulgaris", "Lemna minor")



alle_klassen %>%
  mutate(klasse = fct_reorder(klasse, ondergrens)) %>%
  filter(naam %in% soorten) %>%
  group_by(parameter, compartiment, klasse) %>%
  summarise(rel_voorkomen = mean(relatief_voorkomen)) %>%
  ggplot(aes(rel_voorkomen, klasse)) + geom_col() +
  scale_x_continuous(limits = c(0, NA), expand = expansion(c(0, 0.1))) +
  HHSKwkl::hhskthema() +
  facet_wrap(~paste(parameter, compartiment), scales = "free_y")

# klassen_combi <-
#   bind_rows(
#     alkaliniteit_klassen,
#     kooldioxide_ow_klassen,
#     waterstofcarbonaat_ow_klassen,
#     ph_ow_klassen,
#     totaal_p_ow_klassen,
#     ortho_p_ow_klassen
#   )

# alkaliniteit_ow_klassen %>%
#   mutate(klasse = fct_reorder(klasse, ondergrens)) %>%
#   filter(naam %in% soorten) %>%
#   group_by(parameter, klasse) %>%
#   summarise(rel_voorkomen = mean(relatief_voorkomen)) %>%
#   ggplot(aes(rel_voorkomen, klasse)) + geom_col() +
#   scale_x_continuous(limits = c(0, NA), expand = expansion(c(0, 0.1))) +
#   HHSKwkl::hhskthema()
# 
# kooldioxide_ow_klassen %>%
#   mutate(klasse = fct_reorder(klasse, ondergrens)) %>%
#   filter(naam %in% soorten) %>%
#   group_by(parameter, klasse) %>%
#   summarise(rel_voorkomen = mean(relatief_voorkomen)) %>%
#   ggplot(aes(rel_voorkomen, klasse)) + geom_col() +
#   scale_x_continuous(limits = c(0, NA), expand = expansion(c(0, 0.1))) +
#   HHSKwkl::hhskthema() +
#   facet_wrap(~parameter)



# Oud ---------------------------------------------------------------------

# 
# 
# # 1 Alkaliniteit in OW ------------------------------------------------------------
# 
# alkaliniteit_ow_raw <- read_excel("data-raw/preferenties/Preferentietabel 1 alkaliniteit in oppervlaktewater.xlsx") 
# 
# 
# alkaliniteit_ow_klassen <- 
#   alkaliniteit_ow_raw %>% 
#   verwerk_klassegrenzen() %>% 
#   mutate(parameter = "alkaliniteit" , eenheid = "umol/l", compartiment = "OW", .before = klasse) 
# 
# 
# # 2 CO2 in OW -------------------------------------------------
# 
# kooldioxide_ow_raw <- read_excel("data-raw/preferenties/Preferentietabel 2 kooldioxide (CO2) in oppervlaktewater.xlsx") 
# 
# kooldioxide_ow_klassen <-
#   kooldioxide_ow_raw %>% 
#   verwerk_klassegrenzen() %>% 
#   mutate(parameter = "koolstofdioxide" , eenheid = "umol/l", compartiment = "OW", .before = klasse) 
# 
# # 3 waterstofcarbonaat in OW ------------------------------------------------
# 
# waterstofcarbonaat_ow_raw <- read_excel("data-raw/preferenties/Preferentietabel 3 bicarbonaat oppervlaktewater.xlsx") 
# 
# waterstofcarbonaat_ow_klassen <-
#   waterstofcarbonaat_ow_raw %>% 
#   verwerk_klassegrenzen() %>% 
#   mutate(parameter = "waterstofcarbonaat" , eenheid = "umol/l", compartiment = "OW", .before = klasse) 
# 
# 
# # 4 pH in OW ----------------------------------------------------------------
# 
# ph_ow_raw <- read_excel("data-raw/preferenties/Preferentietabel 4 veld-pH  oppervlaktewater.xlsx") 
# 
# ph_ow_klassen <-
#   ph_ow_raw %>% 
#   verwerk_klassegrenzen() %>% 
#   mutate(parameter = "pH" , eenheid = "DIMSLS", compartiment = "OW", .before = klasse) 
# 
# 
# 
# # 5 Totaal fosfor in OW ---------------------------------------------------
# 
# totaal_p_ow_raw <- read_excel("data-raw/preferenties/Preferentietabel 5 totaal-fosfor in oppervlaktewater.xlsx") 
# 
# totaal_p_ow_klassen <-
#   totaal_p_ow_raw %>% 
#   verwerk_klassegrenzen() %>% 
#   mutate(parameter = "fosfor totaal" , eenheid = "umol/l", compartiment = "OW", .before = klasse) 
# 
# 
# # 6 Ortho fosfaat in OW ---------------------------------------------------
# 
# ortho_p_ow_raw <- read_excel("data-raw/preferenties/Preferentietabel 6 ortho-fosfaat oppervlaktewater.xlsx") 
# 
# ortho_p_ow_klassen <-
#   ortho_p_ow_raw %>% 
#   verwerk_klassegrenzen() %>% 
#   mutate(parameter = "ortho-fosfaat" , eenheid = "umol/l", compartiment = "OW", .before = klasse) 
# 
# # basis klassen -----------------------------------------------------------
# 
# _ow_raw <- read_excel("data-raw/preferenties/") 
# 
# _ow_klassen <-
#   _ow_raw %>% 
#   verwerk_klassegrenzen() %>% 
#   mutate(parameter = "" , eenheid = "umol/l", compartiment = "OW", .before = klasse) 
# 
