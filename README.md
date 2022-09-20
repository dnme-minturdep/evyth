
<!-- README.md is generated from README.Rmd. Please edit that file -->

# {evyth} <a href="https://dnme-minturdep.github.io/evyth/"><img src="man/figures/logo.png" align="right" height="360" /></a>

<!-- badges: start -->
<!-- badges: end -->

El objetivo del paquete es proveer a la [**Dirección Nacional de
Mercados y Estadísticas**](https://www.yvera.tur.ar/sinta/) de un set de
funciones para el procesamiento de la Encuesta de Viaje y Turismo de los
Hogares (EVyTH)

## Instrucciones de instalación

Se puede instalar la versión en desarrollo de `{evyth}` desde
[GitHub](https://github.com/) con la siguiente sentencia:

``` r
# install.packages("devtools")
devtools::install_github("d4t4tur/herramientas")
```

Alternativamente se puede isntalar desde el r-universe de la DNMyE, con
los siguientes pasos:

### Habilitar repositorio de paquetes de la dnme-minturdep

    options(repos = c(
      dnmeminturdep = 'https://dnme-minturdep.r-universe.dev',
      CRAN = 'https://cloud.r-project.org'))

### Descargar e instalar `{evyth}` en R del modo usual

    install.packages('evyth')

## Ejemplo

Un ejemplo básico de como usar la librería:

``` r
library(evyth)
#> Warning: package 'evyth' was built under R version 4.1.3

# Descargo datos de primeros trimestres
evyth::descargar_microdato(anio = 2019, trimestre = 1)
#> # A tibble: 12,845 x 88
#>    id_ho~1 id_vi~2 miembro  anio trime~3 regio~4 aglom~5 regio~6 provi~7 local~8
#>      <dbl>   <int>   <int> <int>   <int>   <int>   <int>   <int>   <int> <chr>  
#>  1  230886   61201       1  2019       1       7      10       3       6 Sierra~
#>  2  230886   61201       2  2019       1       7      10       3       6 Sierra~
#>  3  230886   61201       3  2019       1       7      10       3       6 Sierra~
#>  4  230886   61201       4  2019       1       7      10       3       6 Sierra~
#>  5  230886   61201       5  2019       1       7      10       3       6 Sierra~
#>  6  230887   92201       1  2019       1       7      10       7      50 Potrer~
#>  7  230889   91201       1  2019       1       7      10       7      50 Potrer~
#>  8  230889   91201       2  2019       1       7      10       7      50 Potrer~
#>  9  230889   91201       3  2019       1       7      10       7      50 Potrer~
#> 10  230892   61201       1  2019       1       7      10       3       6 Villa ~
#> # ... with 12,835 more rows, 78 more variables: cod_loc_2001 <int>,
#> #   codigo_2001 <int>, codigo_2010 <int>, cod_prov_2010 <int>,
#> #   cod_depto_2010 <int>, cod_loc_2010 <int>, pondera <int>,
#> #   tipo_visitante <int>, cantidad_destinos <int>, multidestino <int>,
#> #   px06 <int>, px06_agrup <int>, px07 <int>, px07_agrup <int>, px08 <int>,
#> #   px08_agrup <int>, px09 <int>, px10_1 <int>, px11 <int>, px12_1 <int>,
#> #   px12_2 <int>, px12_3 <int>, px12_4 <int>, px12_5 <int>, px12_6 <int>, ...
#> # i Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names
```
