#' Tabellen
#' 
#' Dit zijn alle tabellen van de dataset waterplanten en waterkwaliteit.
#' Zie ook <https://www.stowa.nl/publicaties/rapporten-en-dataset-behorende-bij-het-boek-waterplanten-en-waterkwaliteit>
#' 
#' De databestanden zijn opgenomen in het package als parquet-bestanden onder extdata.
#' 
#' @name data_waterplanten


#' @rdname data_waterplanten
"compartments"

#' @rdname data_waterplanten
"datasets"

#' @rdname data_waterplanten
"landscape_data"

#' @rdname data_waterplanten
"landscape_parameters"

#' @rdname data_waterplanten
"locations"

#' @rdname data_waterplanten
"methods"

#' @rdname data_waterplanten
"physicochemical_data"

#' @rdname data_waterplanten
"physicochemical_parameters"

#' @rdname data_waterplanten
"samples"

#' @rdname data_waterplanten
"units"

#' @rdname data_waterplanten
"vegetation_data"

#' @rdname data_waterplanten
"zones"


if (getRversion() >= "2.15.1")  utils::globalVariables(c("compartments",
                                                         "datasets",
                                                         "landscape_data",
                                                         "landscape_parameters",
                                                         "locations",
                                                         "methods",
                                                         "physicochemical_data",
                                                         "physicochemical_parameters",
                                                         "samples",
                                                         "units",
                                                         "vegetation_data",
                                                         "zones"
))