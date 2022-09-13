crear_base_trabajo <- function(){

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ Cargo base primaria  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ### Traigo archivo .zip del servidor
  archivo_zip <- list.files("/srv/DataDNMYE/evyth/", pattern = ".zip")

  ### Listo archivos dentro del .zip
  archivo <- utils::unzip(glue::glue("/srv/DataDNMYE/evyth/{archivo_zip}"), list = TRUE)[1,1]

  ### Importar según el formato del archivo .csv / .sav
  if(stringr::str_ends(archivo, ".csv")){

    b_evyth <- data.table::fread(file = utils::unzip(zipfile = paste0("/srv/DataDNMYE/evyth/", archivo_zip),
                                             files = archivo))
  }

  if(stringr::str_ends(archivo, ".sav")){

    b_evyth <- foreign::read.spss(file = utils::unzip(zipfile = paste0("/srv/DataDNMYE/evyth/", archivo, ".zip"),
                                                      files = paste0(archivo, ".sav")),
                                  use.value.labels = FALSE,
                                  to.data.frame = TRUE,
                                  trim.factor.names = TRUE,
                                  reencode = "utf8")
  }


  ### Limpieza
  b_evyth <- b_evyth %>%
    dplyr::mutate(
      dplyr::across(c(px09, px10_1, px13), ~ ifelse(. == 9, 99, .)),
      dplyr::across(tidyselect::starts_with("pxb16_1_"), ~ dplyr::case_when(. == 0 ~ 2,
                                                                            . == 1 ~ 1)),
      p006_agrup = dplyr::case_when(p006_agrup == 0 ~ 1,
                                    p006_agrup == 1 ~ 2,
                                    p006_agrup == 2 ~ 3,
                                    p006_agrup == 3 ~ 4,
                                    p006_agrup == 4 ~ 5,
                                    p006_agrup == 99 ~ 99),
      p007 = ifelse(p007 == 0, NA_real_, p007),
      cond_act = ifelse(cond_act == 0, 4, cond_act))


  ### Escribo la base de trabajo
  arrow::write_parquet(x = b_evyth,
                       sink = glue::glue("/srv/DataDNMYE/evyth/base_trabajo/evyth_base_de_trabajo.parquet"),
                       compression = "uncompressed")


}
