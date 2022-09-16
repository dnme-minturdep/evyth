#'Descarga de Bases de la Encuesta de Viajes y Turismo de los Hogares (EVyTH)
#'@description
#'Funcion que descarga bases de la Encuesta de Viajes y Turismo de lo Hogares
#'@param anio un integer o vector de integers a partir de 2012
#'@param trimestre un integer o vector de integers con el numero de trim: 1,2,3,4
#'@param variables un vector de characters. variables a seleccionar. Default='all' trae todas las variables
#'@details
#'
#'@return
#'
#'@examples
#'
#'base_evyth <- descargar_microdato(anio = 2018:2019,
#'                                   trimestre = 1,
#'                                   variables = c('anio','tipo_visitante','pondera'))
#'
#'
#'@export

descargar_microdato <- function(anio = 2018,
                                 trimestre = NA,
                                 variables = 'all'){

  anioo <- anio
  trim <- trimestre

  #controles de los parametros
  assertthat::assert_that(is.numeric(anioo))
  assertthat::assert_that((is.numeric(trim)))


  if (any(!is.na(trim))) {
    assertthat::assert_that(any(trim %in% 1:4), msg = "Por favor ingresa un numero de trimeste valido: 1,2,3,4")
  }

  if (any(!is.na(anioo))) {
    assertthat::assert_that(any(anioo >= 2012), msg='La EVyTH cuenta con información a partir del año 2012')

  }

  if (any(anioo>=2012)){

    link = glue::glue('http://datos.yvera.gob.ar/dataset/b5819e9b-5edf-4aad-bd39-a81158a2b3f3/resource/645e5505-68ee-4cfa-90f9-fcc9a4a34a85/download/evyth_microdatos.csv')


    temp <- tempfile(pattern = glue::glue('microdatos'))

    check <- NA
    try(check <- utils::download.file(link,temp),silent = TRUE)
    assertthat::assert_that(assertthat::noNA(check),msg = glue::glue("problema con la descarga"))

    base <- data.table::fread(temp) %>%
      tidyr::as_tibble() %>%
      dplyr::filter(anio %in% anioo) %>%
      dplyr::filter(trimestre %in% trim)

  }

  unlink(temp)

  if (all(variables == 'all')) {
    variables <- names(base)
  }
  if (nrow(base)>0) {

    chequeo <- variables %in% names(base)

    assertthat::assert_that(all(chequeo), msg=glue::glue('Las variables: {glue::glue_collapse(variables[!chequeo],sep = ", ", last = ", y ")} no se encuentran disponibles para esta base.'))

    dplyr::select(base, variables)

  } else {base}

}
