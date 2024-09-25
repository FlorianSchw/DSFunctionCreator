#'
#' @title Retrieving an overview of arguments per function for DS packages
#' @description The function checks all DS functions in the R folder and retrieves argument names
#' @details This function checks all pulled DS packages and their DS functions for argument names they are using and lists them
#' in a data.frame. Currently, there is no selection / filtering possible yet.
#' @param path_to_dsPackages this is the path in which the DataSHIELD packages are located. Typically by pulling
#' them in order to have the newest version available.
#' @param package_type Client or Server. However, development has been concentrated on the Client-Side for now.
#' So Server might not be working at the moment.
#' @return data.frame in which the function and their different arguments are shown.
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

