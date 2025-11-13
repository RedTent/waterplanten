tabellen <- list.files("data-raw/parquet", full.names = TRUE)

tabellen <- tabellen[stringr::str_detect(basename(tabellen), "^\\d", negate = TRUE)]

for (tabelnaam in tabellen){
  
  tabel_objectnaam <-  
    tabelnaam |> 
    basename() |> 
    stringr::str_remove("\\.parquet") |> 
    stringr::str_replace_all(" ", "_") |> 
    stringr::str_to_lower() 
  
  
  print(tabel_objectnaam)
  tabel <- arrow::read_parquet(tabelnaam) |> dplyr::collect() |> tibble::as_tibble()
  
  assign(tabel_objectnaam, tabel, envir = .GlobalEnv)
  
  
  
}

usethis::use_data(compartments,
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
                  zones,
                  overwrite = TRUE)
