library(tidyverse)
library(readxl)
library(janitor)


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

verwerk_klassegrenzen <- funntion(df) {
  
  df %>% 
    .[-(1:3),] %>% 
    janitor::row_to_names(1) %>% 
    select(-ind, -`gewogen gemid.`, -optim., -`gemod. chi waarde`) %>% 
    rename(naam = 1, nednaam = 2, n_wateren_met_soort = 3) %>% 
    pivot_longer(cols = !1:3, names_to = "klasse", values_to = "relatief_voorkomen") %>% 
    mutate(klasse = str_remove_all(klasse, "[[:space:]]")) %>% 
    add_grenzen() %>% 
    type_convert()
    
}

# 1 Alkaliniteit in OW ------------------------------------------------------------


alkaliniteit_ow_raw <- read_excel("data-raw/preferenties/Preferentietabel 1 alkaliniteit in oppervlaktewater.xlsx") 


alkaliniteit_klassen <- 
  alkaliniteit_ow_raw %>% 
  .[-(1:3),] %>% 
  janitor::row_to_names(1) %>% 
  select(-ind, -`gewogen gemid.`, -optim., -`gemod. chi waarde`) %>% 
  rename(naam = 1, nednaam = 2, n_wateren_met_soort = 3) %>% 
  pivot_longer(cols = 4:9, names_to = "klasse", values_to = "relatief_voorkomen") %>% 
  mutate(klasse = str_remove_all(klasse, "[[:space:]]")) %>% 
  add_grenzen() %>% 
  type_convert() %>% 
  mutate(parameter = "alkaliniteit" , eenheid = "umol/l", compartiment = "OW", .before = klasse) 


# 2 CO2 in OW -------------------------------------------------

kooldioxide_ow_raw <- read_excel("data-raw/preferenties/Preferentietabel 2 kooldioxide (CO2) in oppervlaktewater.xlsx") 


kooldioxide_ow_klassen <-
  kooldioxide_ow_raw %>% 
  .[-(1:3),] %>% 
  janitor::row_to_names(1) %>% 
  select(-ind, -`gewogen gemid.`, -optim., -`gemod. chi waarde`) %>% 
  rename(naam = 1, nednaam = 2, n_wateren_met_soort = 3) %>% 
  pivot_longer(cols = !1:3, names_to = "klasse", values_to = "relatief_voorkomen") %>% 
  mutate(klasse = str_remove_all(klasse, "[[:space:]]")) %>% 
  add_grenzen() %>% 
  type_convert() %>% 
  mutate(parameter = "koolstofdioxide" , eenheid = "umol/l", compartiment = "OW", .before = klasse) 
  


# 3 waterstofcarbonaat in OW ------------------------------------------------

waterstofcarbonaat_ow_raw <- read_excel("data-raw/preferenties/Preferentietabel 3 bicarbonaat oppervlaktewater.xlsx") 


kooldioxide_ow_klassen <-
  kooldioxide_ow_raw %>% 
  .[-(1:3),] %>% 
  janitor::row_to_names(1) %>% 
  select(-ind, -`gewogen gemid.`, -optim., -`gemod. chi waarde`) %>% 
  rename(naam = 1, nednaam = 2, n_wateren_met_soort = 3) %>% 
  pivot_longer(cols = !1:3, names_to = "klasse", values_to = "relatief_voorkomen") %>% 
  mutate(klasse = str_remove_all(klasse, "[[:space:]]")) %>% 
  add_grenzen() %>% 
  type_convert() %>% 
  mutate(parameter = "alkaliniteit" , eenheid = "umol/l", compartiment = "OW", .before = klasse) 



# Testcode ----------------------------------------------------------------

# library(HHSKwkl)
# theme_set(hhskthema())
# 
# soorten <- c("Chara globularis", "Elodea nuttallii", "Ceratophyllum demersum")
# soorten <- c("Chara globularis", "Utricularia vulgaris", "Lemna minor")
# 
# alkaliniteit_klassen %>%
#   mutate(klasse = fct_reorder(klasse, ondergrens)) %>%
#   filter(naam %in% soorten) %>%
#   group_by(parameter, klasse) %>%
#   summarise(rel_voorkomen = mean(relatief_voorkomen)) %>%
#   ggplot(aes(rel_voorkomen, klasse)) + geom_col() +
#   scale_x_continuous(limits = c(0, NA), expand = expansion(c(0, 0.1))) +
#   HHSKwkl::hhskthema()

