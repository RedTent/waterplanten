load_dataset_orig <- function(){
  
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
    tabel <- arrow::open_dataset(tabelnaam) |> dplyr::collect()
    
    assign(tabel_objectnaam, tabel, envir = .GlobalEnv)



  }
  
}
# load_dataset_orig()
