#' Title
#'
#' @param anio
#' @param trimestre
#' @param backup
#'
#' @return
#' @export
#'
#' @examples
crear_base_microdato <- function(anio, trimestre, backup = FALSE){

  if(backup == FALSE){
    print("No se está creando un backup de la base de microdatos anterior. Para ello setear backup = TRUE en la función")
  }

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ Cargo base primaria  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  b_evyth <- arrow::read_parquet("/srv/DataDNMYE/evyth/base_trabajo/evyth_base_de_trabajo.parquet",
                                 as_data_frame = TRUE)

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ Chequeo que la base cuente con información para el período especificado en la función  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  anioo <- anio
  trimm <- trimestre

  ultimo_trim_evyth <- max(unique(b_evyth$trimestre[b_evyth$anio == max(b_evyth$anio)]))

  ### Chequeo que haya información para el año definido
  assertthat::assert_that(anioo %in% unique(b_evyth$anio),
                          msg = "La base no cuenta con información para el año especificado")

  ### Chequeo que haya información para el trimestre definido
  assertthat::assert_that(anioo %in% unique(b_evyth$anio) & trimm %in% unique(b_evyth$trimestre[b_evyth$anio == anioo]),
                          msg = "La base no cuenta con información para el trimestre especificado")

  ### Chequeo que no haya valores NA en pondera para armar el trimestre
  assertthat::assert_that(b_evyth %>%
                            dplyr::select(anio, trimestre, Mes, pondera) %>%
                            dplyr::filter(anio == anioo & trimestre == trimm) %>%
                            dplyr::pull(pondera) %>%
                            is.na() %>%
                            any() == FALSE,
                          msg = "No se puede armar la base, hay valores NA en la variable pondera. Chequear si el trimestre está completo")



  ### Defino variables para la base de microdatos abierta
  variables <- c("id_hogar", "id_viajes", "miembro", "anio", "trimestre", "region_origen",
                 "aglomerado_origen", "region_destino_actual", "provincia_destino", "localidad_destino",
                 "codigode_localidad", "pondera", "tipo_visitante", "cantidad_destinos", "multidestino",
                 "px06", "px06_agrup", "px07", "px07_agrup", "px08", "px08_agrup", "px09", "px10_1", "px11",
                 "px12_1", "px12_2", "px12_3", "px12_4", "px12_5", "px12_6", "px12_7", "px12_8", "px13",
                 "px14", "px15_1", "px15_2", "px15_3", "px15_4", "pxb16_1_1", "pxb16_1_2", "pxb16_1_3", "pxb16_1_4",
                 "pxb16_1_5", "pxb16_1_6", "pxb16_1_7", "pxb16_1_9", "pxb16_2", "px17_1", "px17_2_1",
                 "px17_2_2", "px17_2_3", "px17_2_4", "px17_2_5", "px17_2_6", "px17_2_7", "px17_2_8",
                 "px17_2_9", "px17_2_10", "px17_2_11", "px17_2_12", "px17_2_13", "px18_1", "px18_2",
                 "px18_3", "px18_4", "px18_5", "px18_6", "px18_7", "gasto_pc", "quintil_pcf_visitante",
                 "p002", "p004", "p005", "p006", "p006_agrup", "p007", "nivel_ed", "cond_act", "p013",
                 "j_sexo", "j_edad", "j_nivel_ed", "j_cond_act")


  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ Limpieza de la base  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  b_evyth <- b_evyth %>%
    dplyr::filter(arg_o_ext == 1) %>%
    dplyr::filter(anio %in% c(2012:anioo) | anio == anioo & trimestre %in% c(1:trimm)) %>%
    dplyr::select(tidyselect::all_of(variables)) %>%
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


  ### chequeo
  #table(b_evyth$anio, b_evyth$trimestre)

  ############################ Asignación de código INDEC a base EVyTH ----

  ### Cargo tabla con codigos indec
  #b_cod_loc_evyth <- read_excel("salidas/localidades_evyth_con_codigo_2010.xlsx")
  b_cod_loc_evyth <- readxl::read_excel("/srv/DataDNMYE/evyth/nomenclatura_geo/localidades_evyth_con_codigo_2010.xlsx", progress = FALSE)

  ### Join de codigo de localidades indec a base evyth
  b_evyth <- b_evyth %>%
    dplyr::left_join(dplyr::select(b_cod_loc_evyth,
                     codprov, codloc, codigo_2001, codigo_2010, cod_prov_2010 = cod_prov,
                     cod_depto_2010, cod_loc_2010),
              by = c(c("codigode_localidad" = "codloc"),
                     c("provincia_destino" = "codprov")), suffix = c("evyth_", "geo")) %>%
    dplyr::rename(cod_loc_2001 = codigode_localidad) %>%
    dplyr::relocate(tidyselect::contains("cod"), .after = localidad_destino)
  #mutate(across(everything(.), ~ replace_na(., "")))

  rm(b_cod_loc_evyth)



  #################################### Armo backup de última versión en server ----

  if(backup == TRUE){

    dir('/srv/DataDNMYE/evyth/microdatos/')

    # Read the 2 CSV file names from working directory
    Zip_Files <- list.files(path = "/srv/DataDNMYE/evyth/microdatos/",
                            pattern = "evyth_microdatos",
                            full.names=TRUE)

    # Zip the files and place the zipped file in working directory
    zip::zipr(zipfile = glue::glue("/srv/DataDNMYE/evyth/microdatos/backup/backup_microdatos_{lubridate::today()}.Zip"),
              files = Zip_Files)

    print("Se ha creado el backup correctamente")

  }


  #################################### Generación de base usuario ----

  formatos <- c("csv", "txt", "sav", "dta", "xlsx")
  n_iteracion <- length(formatos)

  barra_progreso <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                                   max = n_iteracion, # Maximum value of the progress bar
                                   style = 3,    # Progress bar style (also available style = 1 and style = 2)
                                   width = 50,   # Progress bar width. Defaults to getOption("width")
                                   char = "=")   # Character used to create the bar

  for (i in seq_along(formatos)) {

    escribo_base(archivo = b_evyth,
                 formato = formatos[i])

    cat(glue::glue(" --> base en .{formatos[i]} creada correctamente"))

    setTxtProgressBar(barra_progreso, i)

  }

  close(barra_progreso)

  cat("Proceso finalizado con éxito")

}
