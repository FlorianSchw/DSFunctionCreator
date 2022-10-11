#'
#' @title Argument Cleaner
#' @description This is an internal function.
#' @details This is an internal function to clean up the arguments from the internal_arguments function.
#' @param Client_Arguments is the data.frame input object containing the function file name, path and listed arguments.
#' @return a data.frame containing the function name and arguments of the DataSHIELD client functions.
#' @author Florian Schwarz for the German Institute of Human Nutrition
#

internal_argumentCleaner <- function(Client_Arguments = NULL){

  argument_split1 <- list()
  argument_split2 <- list()


  # Splitting the argument call into argument objects
  for (i in 1:length(Client_Arguments$Argument_Call)){

    if(grepl("function(",Client_Arguments$Argument_Call[i], fixed = TRUE) == TRUE){

      argument_split1[[i]] <- strsplit(Client_Arguments$Argument_Call[[i]], "function(", fixed = TRUE)
      argument_split2[[i]] <- strsplit(argument_split1[[i]][[1]], ",", fixed = TRUE)[2]


    } else if(grepl("function (",Client_Arguments$Argument_Call[i], fixed = TRUE) == TRUE){

      argument_split1[[i]] <- strsplit(Client_Arguments$Argument_Call[[i]], "function (", fixed = TRUE)
      argument_split2[[i]] <- strsplit(argument_split1[[i]][[1]], ",", fixed = TRUE)[2]

    } else {

      stop("In the function(arguments) codeline something breaks the code. Please contact the maintainer of this package.", call. = FALSE)

    }

  }



  for (i in 1:length(Client_Arguments$Argument_Call)){
    Client_Arguments$Arguments[[i]] <- argument_split2[[i]][1]
    Client_Arguments$Arguments[[i]] <- unlist(Client_Arguments$Arguments[[i]])
  }


  dd <- data.frame(matrix(ncol = 15, nrow = length(Client_Arguments$Argument_Call)))
  Client_Arguments <- cbind(Client_Arguments, dd)
  colnames(Client_Arguments) <- c("Function_FileName", "Function_Path", "Argument_Call", "Arguments", "Arg1", "Arg2", "Arg3", "Arg4", "Arg5", "Arg6", "Arg7", "Arg8", "Arg9", "Arg10", "Arg11", "Arg12", "Arg13", "Arg14", "Arg15")

  # Assigning arguments to separate columns
  for (i in 1:length(Client_Arguments$Argument_Call)){
    Client_Arguments$Arg1[[i]] <- Client_Arguments$Arguments[[i]][1]
    Client_Arguments$Arg2[[i]] <- Client_Arguments$Arguments[[i]][2]
    Client_Arguments$Arg3[[i]] <- Client_Arguments$Arguments[[i]][3]
    Client_Arguments$Arg4[[i]] <- Client_Arguments$Arguments[[i]][4]
    Client_Arguments$Arg5[[i]] <- Client_Arguments$Arguments[[i]][5]
    Client_Arguments$Arg6[[i]] <- Client_Arguments$Arguments[[i]][6]
    Client_Arguments$Arg7[[i]] <- Client_Arguments$Arguments[[i]][7]
    Client_Arguments$Arg8[[i]] <- Client_Arguments$Arguments[[i]][8]
    Client_Arguments$Arg9[[i]] <- Client_Arguments$Arguments[[i]][9]
    Client_Arguments$Arg10[[i]] <- Client_Arguments$Arguments[[i]][10]
    Client_Arguments$Arg11[[i]] <- Client_Arguments$Arguments[[i]][11]
    Client_Arguments$Arg12[[i]] <- Client_Arguments$Arguments[[i]][12]
    Client_Arguments$Arg13[[i]] <- Client_Arguments$Arguments[[i]][13]
    Client_Arguments$Arg14[[i]] <- Client_Arguments$Arguments[[i]][14]
    Client_Arguments$Arg15[[i]] <- Client_Arguments$Arguments[[i]][15]
  }


  # Cleaning of Arguments
  Client_Analysis <- subset(Client_Arguments, select = -c(2,3,4))


  for (i in 1:length(Client_Analysis$Function_FileName)){

    if(grepl("=", Client_Analysis$Arg1[[i]], fixed = TRUE) == TRUE){
      Client_Analysis$Arg1[[i]] <- strsplit(Client_Analysis$Arg1[[i]], "=", fixed = TRUE)[[1]][1]
    }

    if(grepl("=", Client_Analysis$Arg2[[i]], fixed = TRUE) == TRUE){
      Client_Analysis$Arg2[[i]] <- strsplit(Client_Analysis$Arg2[[i]], "=", fixed = TRUE)[[1]][1]
    }

    if(grepl("=", Client_Analysis$Arg3[[i]], fixed = TRUE) == TRUE){
      Client_Analysis$Arg3[[i]] <- strsplit(Client_Analysis$Arg3[[i]], "=", fixed = TRUE)[[1]][1]
    }

    if(grepl("=", Client_Analysis$Arg4[[i]], fixed = TRUE) == TRUE){
      Client_Analysis$Arg4[[i]] <- strsplit(Client_Analysis$Arg4[[i]], "=", fixed = TRUE)[[1]][1]
    }

    if(grepl("=", Client_Analysis$Arg5[[i]], fixed = TRUE) == TRUE){
      Client_Analysis$Arg5[[i]] <- strsplit(Client_Analysis$Arg5[[i]], "=", fixed = TRUE)[[1]][1]
    }

    if(grepl("=", Client_Analysis$Arg6[[i]], fixed = TRUE) == TRUE){
      Client_Analysis$Arg6[[i]] <- strsplit(Client_Analysis$Arg6[[i]], "=", fixed = TRUE)[[1]][1]
    }

    if(grepl("=", Client_Analysis$Arg7[[i]], fixed = TRUE) == TRUE){
      Client_Analysis$Arg7[[i]] <- strsplit(Client_Analysis$Arg7[[i]], "=", fixed = TRUE)[[1]][1]
    }

    if(grepl("=", Client_Analysis$Arg8[[i]], fixed = TRUE) == TRUE){
      Client_Analysis$Arg8[[i]] <- strsplit(Client_Analysis$Arg8[[i]], "=", fixed = TRUE)[[1]][1]
    }

    if(grepl("=", Client_Analysis$Arg9[[i]], fixed = TRUE) == TRUE){
      Client_Analysis$Arg9[[i]] <- strsplit(Client_Analysis$Arg9[[i]], "=", fixed = TRUE)[[1]][1]
    }

    if(grepl("=", Client_Analysis$Arg10[[i]], fixed = TRUE) == TRUE){
      Client_Analysis$Arg10[[i]] <- strsplit(Client_Analysis$Arg10[[i]], "=", fixed = TRUE)[[1]][1]
    }

    if(grepl("=", Client_Analysis$Arg11[[i]], fixed = TRUE) == TRUE){
      Client_Analysis$Arg11[[i]] <- strsplit(Client_Analysis$Arg11[[i]], "=", fixed = TRUE)[[1]][1]
    }

    if(grepl("=", Client_Analysis$Arg12[[i]], fixed = TRUE) == TRUE){
      Client_Analysis$Arg12[[i]] <- strsplit(Client_Analysis$Arg12[[i]], "=", fixed = TRUE)[[1]][1]
    }

    if(grepl("=", Client_Analysis$Arg13[[i]], fixed = TRUE) == TRUE){
      Client_Analysis$Arg13[[i]] <- strsplit(Client_Analysis$Arg13[[i]], "=", fixed = TRUE)[[1]][1]
    }

    if(grepl("=", Client_Analysis$Arg14[[i]], fixed = TRUE) == TRUE){
      Client_Analysis$Arg14[[i]] <- strsplit(Client_Analysis$Arg14[[i]], "=", fixed = TRUE)[[1]][1]
    }

    if(grepl("=", Client_Analysis$Arg15[[i]], fixed = TRUE) == TRUE){
      Client_Analysis$Arg15[[i]] <- strsplit(Client_Analysis$Arg15[[i]], "=", fixed = TRUE)[[1]][1]
    }


    # Cleaning up spaces
    if(grepl(" ", Client_Analysis$Arg1[[i]], fixed = TRUE) == TRUE){
      Client_Analysis$Arg1[[i]] <- gsub(pattern = " ", "", Client_Analysis$Arg1[[i]], fixed = TRUE)
    }

    if(grepl(" ", Client_Analysis$Arg2[[i]], fixed = TRUE) == TRUE){
      Client_Analysis$Arg2[[i]] <- gsub(pattern = " ", "", Client_Analysis$Arg2[[i]], fixed = TRUE)
    }

    if(grepl(" ", Client_Analysis$Arg3[[i]], fixed = TRUE) == TRUE){
      Client_Analysis$Arg3[[i]] <- gsub(pattern = " ", "", Client_Analysis$Arg3[[i]], fixed = TRUE)
    }

    if(grepl(" ", Client_Analysis$Arg4[[i]], fixed = TRUE) == TRUE){
      Client_Analysis$Arg4[[i]] <- gsub(pattern = " ", "", Client_Analysis$Arg4[[i]], fixed = TRUE)
    }

    if(grepl(" ", Client_Analysis$Arg5[[i]], fixed = TRUE) == TRUE){
      Client_Analysis$Arg5[[i]] <- gsub(pattern = " ", "", Client_Analysis$Arg5[[i]], fixed = TRUE)
    }

    if(grepl(" ", Client_Analysis$Arg6[[i]], fixed = TRUE) == TRUE){
      Client_Analysis$Arg6[[i]] <- gsub(pattern = " ", "", Client_Analysis$Arg6[[i]], fixed = TRUE)
    }

    if(grepl(" ", Client_Analysis$Arg7[[i]], fixed = TRUE) == TRUE){
      Client_Analysis$Arg7[[i]] <- gsub(pattern = " ", "", Client_Analysis$Arg7[[i]], fixed = TRUE)
    }

    if(grepl(" ", Client_Analysis$Arg8[[i]], fixed = TRUE) == TRUE){
      Client_Analysis$Arg8[[i]] <- gsub(pattern = " ", "", Client_Analysis$Arg8[[i]], fixed = TRUE)
    }

    if(grepl(" ", Client_Analysis$Arg9[[i]], fixed = TRUE) == TRUE){
      Client_Analysis$Arg9[[i]] <- gsub(pattern = " ", "", Client_Analysis$Arg9[[i]], fixed = TRUE)
    }

    if(grepl(" ", Client_Analysis$Arg10[[i]], fixed = TRUE) == TRUE){
      Client_Analysis$Arg10[[i]] <- gsub(pattern = " ", "", Client_Analysis$Arg10[[i]], fixed = TRUE)
    }

    if(grepl(" ", Client_Analysis$Arg11[[i]], fixed = TRUE) == TRUE){
      Client_Analysis$Arg11[[i]] <- gsub(pattern = " ", "", Client_Analysis$Arg11[[i]], fixed = TRUE)
    }

    if(grepl(" ", Client_Analysis$Arg12[[i]], fixed = TRUE) == TRUE){
      Client_Analysis$Arg12[[i]] <- gsub(pattern = " ", "", Client_Analysis$Arg12[[i]], fixed = TRUE)
    }

    if(grepl(" ", Client_Analysis$Arg13[[i]], fixed = TRUE) == TRUE){
      Client_Analysis$Arg13[[i]] <- gsub(pattern = " ", "", Client_Analysis$Arg13[[i]], fixed = TRUE)
    }

    if(grepl(" ", Client_Analysis$Arg14[[i]], fixed = TRUE) == TRUE){
      Client_Analysis$Arg14[[i]] <- gsub(pattern = " ", "", Client_Analysis$Arg14[[i]], fixed = TRUE)
    }

    if(grepl(" ", Client_Analysis$Arg15[[i]], fixed = TRUE) == TRUE){
      Client_Analysis$Arg15[[i]] <- gsub(pattern = " ", "", Client_Analysis$Arg15[[i]], fixed = TRUE)
    }


  }

  return(Client_Analysis)

}

