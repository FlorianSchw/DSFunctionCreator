#'
#' @title File Path Finder
#' @description This is an internal function.
#' @details This is an internal function to retrieve the file path and function file name of either client or server side DataSHIELD functions.
#' @param path is the directory path to where the DataSHIELD packages are stored.
#' @param type refers to the type of DataSHIELD packages: 'Client' or 'Server'
#' @return a data.frame with the function file name and respective file path of either client- or server-side DataSHIELD functions. Internal functions are omitted for now.
#' @author Florian Schwarz for the German Institute of Human Nutrition
#


FilePathFinder <- function(path = path_to_dsPackages, type = package_type){

  # Separating client-side and server-side packages
  dsPackages <- data.frame(list.files(path))
  colnames(dsPackages) <- "Packages"

  for (i in 1:length(dsPackages$Packages)){
    if(grepl("Client", dsPackages$Packages[i], fixed = TRUE) == TRUE){
      dsPackages$Type[i] <- "Client"
    } else {
      dsPackages$Type[i] <- "Server"
    }
  }

  dsPackagesClient <- dsPackages[which(dsPackages$Type == "Client"),]
  dsPackagesServer <- dsPackages[which(dsPackages$Type == "Server"),]


  # Client-side R Files Path Detection
  Client_R_Files <- list()
  Client_total_path <- list()

  for (i in 1:length(dsPackagesClient$Packages)){
    Client_R_Files[[i]] <- list.files(paste0(path, "/", dsPackagesClient$Packages[[i]],"/R"))
    Client_total_path[[i]] <- paste0(path, "/", dsPackagesClient$Packages[[i]],"/R/", Client_R_Files[[i]])
  }

  Client_R_Paths <- data.frame(unlist(Client_R_Files),unlist(Client_total_path))
  colnames(Client_R_Paths) <- c("Function_FileName", "Function_Path")

  # Only selecting pure "ds." functions. Internal functions are not checked!
  Client_R_Paths_ds <- Client_R_Paths[which(substr(Client_R_Paths$Function_FileName, 1,3) == "ds."),]



  # Server-side R Files Path Detection
  Server_R_Files <- list()
  Server_total_path <- list()

  for (i in 1:length(dsPackagesServer$Packages)){
    Server_R_Files[[i]] <- list.files(paste0(path, "/", dsPackagesServer$Packages[[i]],"/R"))
    Server_total_path[[i]] <- paste0(path, "/", dsPackagesServer$Packages[[i]],"/R/", Server_R_Files[[i]])
  }

  ####################################################################################
  #######ERROR MESSAGE################################################################
  ####################################################################################
  #######Fehler in data.frame(unlist(Server_R_Files), unlist(Server_total_path))######
  #######arguments imply differing number of rows ####################################
  ####################################################################################
  ####################################################################################



  #Server_R_Paths <- data.frame(unlist(Server_R_Files),unlist(Server_total_path))
  #colnames(Server_R_Paths) <- c("Function_FileName", "Function_Path")

  # Only selecting pure "DS" functions. Internal functions are not checked! Also some "...DS1.R" won't be selected this way.
  #Server_R_Paths_ds <- Server_R_Paths[which(substr(Server_R_Paths$Function_FileName, nchar(Server_R_Paths$Function_FileName)-4+1, nchar(Server_R_Paths$Function_FileName)) == "DS.R"),]

  if(type == "Client"){
      return(Client_R_Paths_ds)
 # } else if(type == "Server"){
  #    return(Server_R_Paths_ds)
  }

}
