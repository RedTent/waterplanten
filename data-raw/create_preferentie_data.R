library(tidyverse)
library(readxl)

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
    rename(naam = 1, nednaam = 2, n_wateren_met_soort = 3, 
           indicatiewaarde = ind, gewogen_gem = `gewogen gemid.`, optimum = optim., gemod_chi = `gemod. chi waarde`, p90 = any_of("90-quantiel")) %>% 
    filter(!is.na(naam)) %>% 
    pivot_longer(cols = !c(1:3, indicatiewaarde, gewogen_gem, optimum, gemod_chi, any_of("p90")), names_to = "klasse", values_to = "relatief_voorkomen") %>% 
    mutate(klasse = str_remove_all(klasse, "[[:space:]]")) %>% 
    add_grenzen() %>% 
    type_convert()
  
}

# Geautomatiseerd ---------------------------------------------------------

referentie_bestanden <- read_excel("data-raw/overzicht_referentiebestanden.xlsx") 

referentie_raw <- referentie_bestanden %>% mutate(raw = map(bestandsnaam, read_excel))

referentie_opgeschoond <-
  referentie_raw %>% 
  mutate(klassen = map(raw, verwerk_klassegrenzen)) %>% 
  unnest(klassen) %>% 
  select(-raw, -bestandsnaam)

referentie_opgeschoond %>% 
  select(parameter, eenheid, compartiment)

# Testcode ----------------------------------------------------------------

# library(HHSKwkl)
# theme_set(hhskthema())
# 
# soorten <- c("Chara globularis", "Elodea nuttallii", "Ceratophyllum demersum")
# soorten <- c("Chara globularis", "Utricularia vulgaris", "Lemna minor")
# 
# 
# 
# referentie_opgeschoond %>%
#   mutate(klasse = fct_reorder(klasse, ondergrens)) %>%
#   filter(naam %in% soorten) %>%
#   group_by(parameter, compartiment, klasse) %>%
#   summarise(rel_voorkomen = mean(relatief_voorkomen)) %>%
#   ggplot(aes(rel_voorkomen, klasse)) + geom_col() +
#   scale_x_continuous(limits = c(0, NA), expand = expansion(c(0, 0.1))) +
#   HHSKwkl::hhskthema() +
#   facet_wrap(~paste(parameter, compartiment), scales = "free_y")

