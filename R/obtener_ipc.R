
#' Descarga de tabla con serie de Indices IPC Cobertura Nacional
#'
#' @param anio Variable numerica que indica el anio de publicacion del ipc que se busca
#' @param mes Variable numerica que indica el mes de publicacion del ipc que se busca
#'
#' @return dataframe con serie del indice de IPC Cobertura Nacional
#' @export
#'
#' @examples
#' \dontrun{
#' obtener_ipc(mes = "10", anio = "2022")
#' }
obtener_ipc <- function(mes, anio){

  assertthat::assert_that(is.character(anio), msg = "La variable tiene que ser de texto")
  assertthat::assert_that(nchar(anio) == 4, msg = "La variable tiene que de 4 digitos. Por ejemplo: 2022, y no '22")
  assertthat::assert_that(is.character(mes), msg = "La variable tiene que ser de texto")


  fecha <- glue::glue("{anio}-{as.numeric(mes)-1}-01") %>%
    as.Date()

  pagina <- glue::glue("https://www.indec.gob.ar/ftp/cuadros/economia/sh_ipc_{mes}_{substring(anio, 3,4)}.xls")

  temp <- tempfile()
  utils::download.file(url = pagina, destfile = temp)

  encabezado <- readxl::read_xls(path = temp, sheet = 3, skip = 5, n_max = 1)
  encabezado_tot <- encabezado[ , 1]
  encabezado_resto <- encabezado[ ,2:ncol(encabezado)]
  colnames(encabezado_resto) <- janitor::excel_numeric_to_date(as.numeric(names(encabezado_resto)))
  encabezado <- cbind(encabezado_tot, encabezado_resto)

  base <- readxl::read_xls(path = temp,
                           sheet = 3,
                           skip = 5,
                           n_max = 10)

  unlink(temp)

  base <- base %>%
    janitor::clean_names() %>%
    dplyr::filter(total_nacional == "Nivel general")

  names(base) <- names(encabezado)

  base <- base %>%
    tidyr::pivot_longer(cols = 2:ncol(.),
                        names_to = "Fecha",
                        values_to = "ipc_indice")


  ultimo_dato <- base$ipc_indice[base$Fecha == fecha]
  ultima_fecha <- max(base$Fecha)

  assertthat::assert_that(fecha > "2016-12-01",
                          msg = "La fecha debe ser mayor al 2016-12-01")

  assertthat::assert_that(as.character(fecha) %in% unique(base$Fecha),
                          msg = "La fecha indicada no corresponde con el ultimo dato publicado por el INDEC")

  base <- base %>%
    dplyr::mutate(coef_gastoreal = base$ipc_indice[base$Fecha == ultima_fecha] / ipc_indice,
                  anio = as.character(lubridate::year(Fecha)),
                  mes = stringr::str_pad(as.character(lubridate::month(Fecha)), width = 2, side = "left", pad = "0"),
                  Mes = as.numeric(paste0(anio,mes)))


  print(glue::glue("El calculo de variacion fue realizado en funcion del mes {fecha}"))

  return(base)

}

