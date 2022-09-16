test_that("Descarga de EVyTH", {
  skip_if_offline()
  expect_error(base <- descargar_microdato(anio = 2011, trimestre = 2, variables =  c('anio','tipo_visitante','pondera')))
  base <- descargar_microdato(anio = 2019, trimestre = 2, variables =  c('anio', 'trimestre', 'tipo_visitante','pondera'))
  expect_equal(unique(base$anio), 2019)
  expect_equal(unique(base$trimestre), 2)
  variables <- names(base)
  expect_equal(variables, c("anio", "trimestre", "tipo_visitante", "pondera"))
  dimensiones <- dim(base)
  expect_equal(dimensiones, c(8135,4))
}
)
