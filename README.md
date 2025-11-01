# waterplanten

Mogelijke opzet voor een package op basis van de dataset Waterplanten en Waterkwaliteit.

Data en achtergrondrapporten zijn beschikbaar op <https://www.stowa.nl/publicaties/rapporten-en-dataset-behorende-bij-het-boek-waterplanten-en-waterkwaliteit>.

``` r
# install.packages("devtools")
devtools::install_github("RedTent/waterplanten")
```

``` r
# Laad alle data
laad_data_waterplanten()

```

## Geïmplementeerde functionaliteit

Dataset en bijbehorende databestanden.

De databestanden zijn in parquet format. In het package worden ze geïnstalleerd onder extdata. Ze zijn op Github ook beschikbaar onder data-raw.


## Ideeën voor functies

- Vertalen van umol/l naar mg/l en vice versa
- Leggen van soortrelaties
- Vinden locaties (+ kenmerken) met gelijkende vegetaties
- Kaart-weergave van locaties.
- Nadenken over niet taxonomische groepen en soorten die niet in TWN staan



