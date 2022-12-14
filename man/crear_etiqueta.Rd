% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crear_etiqueta.R
\name{crear_etiqueta}
\alias{crear_etiqueta}
\title{Crear Etiqueta}
\usage{
crear_etiqueta(base, variables = NULL, drop_vars = T)
}
\arguments{
\item{base}{dataset que contiene la variable a etiquetar}

\item{variables}{variable o conjunto de variables a etiquetar}

\item{drop_vars}{TRUE pisa variable. FALSE si queres conservar la columna.}
}
\description{
Genera etiquetas asociadas a valores de repuestas de la EVyTH
}
\examples{
crear_etiqueta(base = comunicacion::toy_evyth, variables = "tipo_visitante")

}
