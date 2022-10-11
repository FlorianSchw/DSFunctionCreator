#'
#' @title XXXX
#' @description XXXX
#' @details XXXX
#' @param path_to_dsPackages XXXX
#' @param package_type XXXXX
#' @param stopMessage XXXXX
#' @return XXXX
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @import DT
#' @export
#'

CodeCheck.ArgumentNull <- function(path_to_dsPackages = NULL, package_type = NULL, stopMessage = NULL){

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

  Client_Codelines <- internal_arguments(df = Client_R_Paths_ds, type = "codelines")



  if(stopMessage == "strict"){

    Client_Results <- internal_codelineAnalysis(codelines = Client_Codelines, Client_Analysis= Client_Analysis)

  }

  if(stopMessage == "flexible"){

    Client_Results <- internal_codelineAnalysis_flexible(codelines = Client_Codelines, Client_Analysis= Client_Analysis)

  }


    Client_Summary <- Client_Results[, c(1,2,17,3,18,4,19,5,20,6,21,7,22,8,23,9,24,10,25,11,26,12,27,13,28,14,29,15,30,16,31)]


  Client_Summary$Arg1_NullCheck[which(is.na(Client_Summary$Arg1))] <- "No Argument"
  Client_Summary$Arg2_NullCheck[which(is.na(Client_Summary$Arg2))] <- "No Argument"
  Client_Summary$Arg3_NullCheck[which(is.na(Client_Summary$Arg3))] <- "No Argument"
  Client_Summary$Arg4_NullCheck[which(is.na(Client_Summary$Arg4))] <- "No Argument"
  Client_Summary$Arg5_NullCheck[which(is.na(Client_Summary$Arg5))] <- "No Argument"
  Client_Summary$Arg6_NullCheck[which(is.na(Client_Summary$Arg6))] <- "No Argument"
  Client_Summary$Arg7_NullCheck[which(is.na(Client_Summary$Arg7))] <- "No Argument"
  Client_Summary$Arg8_NullCheck[which(is.na(Client_Summary$Arg8))] <- "No Argument"
  Client_Summary$Arg9_NullCheck[which(is.na(Client_Summary$Arg9))] <- "No Argument"
  Client_Summary$Arg10_NullCheck[which(is.na(Client_Summary$Arg10))] <- "No Argument"
  Client_Summary$Arg11_NullCheck[which(is.na(Client_Summary$Arg11))] <- "No Argument"
  Client_Summary$Arg12_NullCheck[which(is.na(Client_Summary$Arg12))] <- "No Argument"
  Client_Summary$Arg13_NullCheck[which(is.na(Client_Summary$Arg13))] <- "No Argument"
  Client_Summary$Arg14_NullCheck[which(is.na(Client_Summary$Arg14))] <- "No Argument"
  Client_Summary$Arg15_NullCheck[which(is.na(Client_Summary$Arg15))] <- "No Argument"


  options(DT.options = list(pageLength = 200))
  Client_Return <- datatable(Client_Summary) %>%
    formatStyle(c("Arg1_NullCheck","Arg2_NullCheck","Arg3_NullCheck","Arg4_NullCheck","Arg5_NullCheck","Arg6_NullCheck","Arg7_NullCheck",
                  "Arg8_NullCheck","Arg9_NullCheck","Arg10_NullCheck","Arg11_NullCheck","Arg12_NullCheck","Arg13_NullCheck",
                  "Arg14_NullCheck","Arg15_NullCheck"),
                backgroundColor = styleEqual(c("No Argument","Complete", "Incomplete", "Absent"), c("blue","green", "yellow", "red")))


  return(Client_Return)

}

