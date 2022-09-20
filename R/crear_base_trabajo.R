#' Title
#'
#' @return
#' @export
#'
#' @examples
crear_base_trabajo <- function(escritura = TRUE, ambiente = FALSE){

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ Cargo base primaria  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ### Traigo archivo .zip del servidor
  archivo_zip <- list.files("/srv/DataDNMYE/evyth/", pattern = ".zip")

  ### Listo archivos dentro del .zip
  archivos <- utils::unzip(glue::glue("/srv/DataDNMYE/evyth/{archivo_zip}"), list = TRUE)

  ### Importar según el formato del archivo .csv / .sav
  if(any(stringr::str_ends(archivos$Name, ".csv"))){

    b_evyth <- data.table::fread(file = utils::unzip(zipfile = paste0("/srv/DataDNMYE/evyth/", archivo_zip),
                                                     files = archivos$Name[str_detect(archivos$Name, "csv")]))
  } else {

    if(any(stringr::str_ends(archivos$Name, ".sav"))){

      archivo <- stringr::str_remove(archivos$Name[str_detect(archivos$Name, "sav")], ".sav")

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

          message("Se cargó con {haven}")
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


  ### Borro archivo que se genera con read_sav
  if(file.exists(archivos$Name[str_detect(archivos$Name, "sav")])){

    file.remove(archivos$Name[str_detect(archivos$Name, "sav")])
  }

  if(file.exists(archivos$Name[str_detect(archivos$Name, "csv")])){

    file.remove(archivos$Name[str_detect(archivos$Name, "csv")])
  }

  print("La base de trabajo se creo correctamente")
}


