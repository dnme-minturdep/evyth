#' Title
#'
#' @param formato
#'
#' @return
#' @export
#'
#' @examples
#'
#'


escribir_base_microdato <- function(archivo = NULL, formato = "csv"){

  if(formato == "cvs"){
    ### Armo base usuaria en formato .csv
    readr::write_csv(x = archivo,
                     file = "/srv/DataDNMYE/evyth/microdatos/evyth_microdatos.csv", na = "",
                     progress = FALSE)
  }

  if(formato == "txt"){


    ### Armo base usuaria en formato .txt
    readr::write_delim(x = archivo,
                       file = "/srv/DataDNMYE/evyth/microdatos/evyth_microdatos.txt",
                       delim = ",", na = "",
                       progress = FALSE)
  }

  if(formato == "sav"){

    ### Armo base usuaria en formato .sav (spss)
    haven::write_sav(data = archivo,
                     path = "/srv/DataDNMYE/evyth/microdatos/evyth_microdatos.sav")

  }

  if(formato == "dta"){

    ### Armo base usuaria en formato .dta (stata)
    haven::write_dta(data = archivo,
                     path = "/srv/DataDNMYE/evyth/microdatos/evyth_microdatos.dta")
  }

  if(formato == "xlsx"){

    ### Armo base usuaria en formato .xlsx (Excel)
    writexl::write_xlsx(x = archivo,
                        path = "/srv/DataDNMYE/evyth/microdatos/evyth_microdatos.xlsx")
  }
}