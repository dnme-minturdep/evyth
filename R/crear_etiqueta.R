etiquetar <- function(base, variables = NULL, drop_vars = T) {

  diccionario <- read.csv("http://datos.yvera.gob.ar/dataset/b5819e9b-5edf-4aad-bd39-a81158a2b3f3/resource/d8107236-f5ba-428d-8a31-f9b2034d8c8f/download/evyth_diccionario_registro.csv")


  if (is.null(variables)) {
    variables <- colnames(base)
  }

  if (is.numeric(variables)) {
    variables <- colnames(base[variables])
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
      base <- select(base, -any_of(i))
      base <- rename_with(base, .cols= matches(match = paste0(i,"_label")), .fn = ~i)
    }

  }
  message(paste(length(coincidencias), "variables etiquetadas"))
  message(paste(coincidencias))
  base
}
