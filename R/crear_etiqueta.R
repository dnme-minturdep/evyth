#' Title
#'
#' @param base dataset que contiene la variable a etiquetar
#' @param variables variable o conjunto de variables a etiquetar
#' @param drop_vars ¿?
#'
#' @return
#' @export
#'
#' @examples
#' crear_etiqueta(base = comunicacion::toy_evyth, variables = "tipo_visitante")
#'

crear_etiqueta <- function(base, variables = NULL, drop_vars = T) {

  diccionario <- utils::read.csv("http://datos.yvera.gob.ar/dataset/b5819e9b-5edf-4aad-bd39-a81158a2b3f3/resource/d8107236-f5ba-428d-8a31-f9b2034d8c8f/download/evyth_diccionario_registro.csv")


  if (is.null(variables)) {
    variables <- names(base)
  }

  if (is.numeric(variables)) {
    variables <- names(base[variables])
  }

  coincidencias <- c()

  for (i in variables) {

    labels <- diccionario[diccionario$variable == i & !is.na(diccionario$opcion), c("opcion","descripcion")]

    if (nrow(labels) == 0) {
      next
    } else {
      coincidencias <- c(coincidencias, i)
      names(labels) <- c(i, paste0(i,"_label"))

      base <- dplyr::left_join(base, labels)
    }

    if (drop_vars == T) {
      base <- dplyr::select(base, -dplyr::any_of(i))
      base <- dplyr::rename_with(base, .cols= dplyr::matches(match = paste0(i,"_label")), .fn = ~i)
    }

  }
  message(paste(length(coincidencias), 
                ifelse(length(coincidencias) == 1, "variable etiquetada:",
                       "variables etiquetadas:")))
  message(paste(coincidencias, collapse = ", "))
  base
}
