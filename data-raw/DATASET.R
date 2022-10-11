## Armado de base diccionario
diccionario <- data.table::fread("http://datos.yvera.gob.ar/dataset/b5819e9b-5edf-4aad-bd39-a81158a2b3f3/resource/d8107236-f5ba-428d-8a31-f9b2034d8c8f/download/evyth_diccionario_registro.csv")

### Transformo a utf8
diccionario$variable <- enc2utf8(diccionario$variable)
diccionario$descripcion <- enc2utf8(diccionario$descripcion)

### Escribo data
usethis::use_data(diccionario, overwrite = TRUE)
