% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/obtener_ipc.R
\name{obtener_ipc}
\alias{obtener_ipc}
\title{Descarga de tabla con serie de Indices IPC Cobertura Nacional}
\usage{
obtener_ipc(fecha)
}
\arguments{
\item{fecha}{variable de texto que indica la fecha del ultimo dato a partir del cual se construye el indice. El formato debe ser anio/mes/dia ("2022-08-01")}
}
\value{
dataframe con serie del indice de IPC Cobertura Nacional
}
\description{
Descarga de tabla con serie de Indices IPC Cobertura Nacional
}
\examples{
obtener_ipc("2022-07-01")

}