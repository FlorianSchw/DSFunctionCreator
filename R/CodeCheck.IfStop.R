#'
#' @title XXXX
#' @description XXXX
#' @details XXXX
#' @param path_to_dsPackages XXXX
#' @param package_type XXXXX
#' @return XXXX
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @export
#'

CodeCheck.IfStop <- function(path_to_dsPackages = NULL, package_type = NULL){

  # Input Check
  if(is.null(path_to_dsPackages)){
    stop("Please provide a path, where the DataSHIELD packages are located!", call.=FALSE)
  }

  # Null Check
  if(is.null(package_type)){
    stop("Please provide a DataSHIELD package_type: 'Client' or 'Server'!", call.=FALSE)
  }

  # Class Check
  if(!('Client' %in% package_type) && !('Server' %in% package_type)){
    stop("Only 'Client' and 'Server' are allowed inputs for the package_type argument.", call.=FALSE)
  }


  Client_R_Paths_ds <- FilePathFinder(path = path_to_dsPackages, type = "Client")

  Client_Arguments <- internal_arguments(df = Client_R_Paths_ds, type = "codelines")


  codeline_nr <- c()
  stop_message_collector <- list()
  index <- 1L

  for (k in 1:length(Client_Arguments)){
    for (i in 1:length(Client_Arguments[[k]][[1]])){

      if_stop_message <- c()

      if(substr(stringr::str_trim(Client_Arguments[[k]][[1]][i]), 1, 3) == "if("){
        if(substr(stringr::str_trim(Client_Arguments[[k]][[1]][i+1]), 1, 5) == "stop("){
          if(substr(stringr::str_trim(Client_Arguments[[k]][[1]][i+2]), 1, 1) == "}"){

            if_stop_message[1] <- stringr::str_trim(Client_Arguments[[k]][[1]][i])
            if_stop_message[2] <- stringr::str_trim(Client_Arguments[[k]][[1]][i+1])
            if_stop_message[3] <- stringr::str_trim(Client_Arguments[[k]][[1]][i+2])

            stop_message_collector[[index]] <- if_stop_message
            index <- index + 1L

          }
        }
      }

    }
  }



  return(stop_message_collector)

}











