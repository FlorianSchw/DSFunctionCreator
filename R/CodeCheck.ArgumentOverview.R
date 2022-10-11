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

CodeCheck.ArgumentOverview <- function(path_to_dsPackages = NULL, package_type = NULL){

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

  Client_Arguments <- internal_arguments(df = Client_R_Paths_ds, type = "arguments")

  Client_Analysis <- internal_argumentCleaner(Client_Arguments = Client_Arguments)



  return(Client_Analysis)

}

