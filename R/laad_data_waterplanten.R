#' Laad alle data uit Waterplanten en Waterkwaliteit
#' 
#' De functie maakt de tabellen beschikbaar in het Global Environment. De tabellen zijn ook 
#' beschikbaar zonder deze functie uit te voeren.
#'
#' @returns Datatabellen zichtbaar in het Global Environment
#' 
#' @export
#'
#' @examples
#' 
#' \dontrun{
#' laad_data_waterplanten()
#' }
laad_data_waterplanten <- function(){
  utils::data(
    compartments,
    datasets,
    landscape_data,
    landscape_parameters,
    locations,
    methods,
    physicochemical_data,
    physicochemical_parameters,
    samples,
    units,
    vegetation_data,
    zones)
  
}
