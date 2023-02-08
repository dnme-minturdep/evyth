#' Crear Base de Trabajo
#' @description
#' Pre proceamiento a partir de base cruda de microdatos para generacion de una base de trabajo
#' @return Base de datos en formato de archivo parquet
#' @importFrom rlang :=
#' @param escritura por defecto TRUE
#' @param ambiente por defecto FALSE
#' @param fecha_ipc Introducir la fecha de referencia para el calculo del indice y coeficiente del ipc
#'
#' @export
#'
crear_base_trabajo <- function(escritura = TRUE, ambiente = FALSE,
                               fecha_ipc = NULL){

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ Cargo base primaria  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ### Traigo archivo .zip del servidor
  archivo_zip <- list.files("/srv/DataDNMYE/evyth/", pattern = ".zip")

  ### Listo archivos dentro del .zip
  archivos <- utils::unzip(glue::glue("/srv/DataDNMYE/evyth/{archivo_zip}"), list = TRUE)

  ### Importar segun el formato del archivo .csv / .sav
  if(any(stringr::str_ends(archivos$Name, ".csv"))){

    b_evyth <- data.table::fread(file = utils::unzip(zipfile = paste0("/srv/DataDNMYE/evyth/", archivo_zip),
                                                     files = archivos$Name[stringr::str_detect(archivos$Name, "csv")]),
                                 dec = ",")
  } else {

    if(any(stringr::str_ends(archivos$Name, ".sav"))){

      archivo <- stringr::str_remove(archivos$Name[stringr::str_detect(archivos$Name, "sav")], ".sav")

      ### Pruebo importar con {haven}, si el problema del encoding persiste se prueba con {foreign}
      tryCatch(

        expr = {
          b_evyth <- haven::read_sav(file = paste0("/srv/DataDNMYE/evyth/", archivo_zip),
                                     #user_na = FALSE,
                                     encoding = "utf8") %>%
            haven::zap_formats() %>%
            haven::zap_widths() %>%
            haven::zap_label() %>%
            haven::zap_labels()

          message("Base cargada con {haven}")
        },

        error = function(e){          # Specifying error message
          message("No se pudo cargar con {haven} por problemas en el encoding del .sav, se procede a cargar con {foreign}")
        }
      )

      b_evyth <- foreign::read.spss(file = utils::unzip(zipfile = paste0("/srv/DataDNMYE/evyth/",
                                                                         archivo, ".zip"), files = paste0(archivo, ".sav")),
                                    to.data.frame = TRUE, use.value.labels = FALSE, trim.factor.names = TRUE,
                                    reencode = "utf8")
    }
  }
  file.remove(archivos$Name)


  anioo <- as.character(lubridate::year(fecha_ipc))
  mess <- stringr::str_pad(string = as.character(lubridate::month(fecha_ipc)),
                           width = 2,
                           side = "left",
                           pad = "0")

  ### Limpieza
  serie_ipc <- evyth::obtener_ipc(mes = mess, anio = anioo)
  
  # Chequeo si la variable de gasto es character
  if(is.character(b_evyth$gasto_pc)) {
    b_evyth$gasto_pc <- stringr::str_replace_all(b_evyth$gasto_pc, ',', '.')
    b_evyth$gasto_pc <- as.numeric(b_evyth$gasto_pc)
  }

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
      p007 = ifelse(test = p007 == 0,
                    yes = NA_real_,
                    no = p007),
      cond_act = ifelse(test = cond_act == 0,
                        yes = 4,
                        no = cond_act),
      fecha_viaje = lubridate::ymd(paste(anio, mes_viaje, "01", sep = "-"))) %>%
    dplyr::left_join(dplyr::select(serie_ipc,
                                   Mes, ipc_indice, coef_gastoreal),
                     by = "Mes") %>%
    dplyr::mutate("gasto_viajetot_pc_pesos_{anioo}_{dplyr::if_else(as.numeric(mess) %in% c(2:12), as.numeric(mess)-1, 12)}" := gasto_pc * coef_gastoreal)

  ### Escribo la base de trabajo
  arrow::write_parquet(x = b_evyth,
                       sink = glue::glue("/srv/DataDNMYE/evyth/base_trabajo/evyth_base_de_trabajo.parquet"),
                       compression = "uncompressed")


  ### Borro archivo que se genera con read_sav
  aux <- length(archivos$Name[stringr::str_detect(archivos$Name, "sav")])

  if(aux > 0){

    if(file.exists(archivos$Name[stringr::str_detect(archivos$Name, "sav")])){

      file.remove(archivos$Name[stringr::str_detect(archivos$Name, "sav")])
    }
  }

  aux <- length(archivos$Name[stringr::str_detect(archivos$Name, "csv")])

  if(aux > 0){

    if(file.exists(archivos$Name[stringr::str_detect(archivos$Name, "csv")])){

      file.remove(archivos$Name[stringr::str_detect(archivos$Name, "csv")])

    }
  }

  print("La base de trabajo se creo correctamente")
}


