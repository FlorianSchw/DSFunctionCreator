#'
#' @title Codeline Analysis function
#' @description This is an internal function.
#' @details This is an internal function to compare the codelines of actual functions with pre-defined standards.
#' @param codelines refers to a list of codelines from DataSHIELD functions
#' @param Client_Analysis refers to a data.frame with information on Arguments and DataSHIELD functions
#' @return a data.frame with the function file name and whether the codecheck is present in the function.
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @import fs
#



internal_codelineAnalysis <- function(codelines = NULL, Client_Analysis = NULL){



  #### idea using another loop fromthe b value from loop to the right
  #### get("x") could be an option

  codecheckDirectory <- fs::path_package(package = "DSFunctionCreator", "templates/CodeCheck/Final/ArgumentNull")


  codecheck_arg_null_2 <- readLines(paste0(codecheckDirectory, "/codecheck_final_arg_null_2.R"))
  codecheck_arg_null_3 <- readLines(paste0(codecheckDirectory, "/codecheck_final_arg_null_3.R"))
  codecheck_arg_null_4 <- readLines(paste0(codecheckDirectory, "/codecheck_final_arg_null_4.R"))


  Client_Results <- data.frame(Client_Analysis$Function_FileName)
  colnames(Client_Results) <- "Function_FileName"
  result <- list()
  check_arg_null <- list()
  correctArgument <- list()


  if(!(length(codelines) = length(Client_Analysis$Function_FileName))){
    stop("Codelines and Client_Analysis have a different length! You might want to contact support.", call.=FALSE)
  }

 for (k in names(Client_Analysis[2:16])){

  for (n in 1:length(Client_Analysis$Function_FileName)){

    # Check whether argument-specific function already exists in directory or write it alternatively from template using use_templateDS
    if(file.exists(paste0(codecheckDirectory, "/codecheck_final_arg_null_1_", Client_Analysis[[k]][n], ".R"))){

      codecheck_arg_null_1 <- readLines(paste0(codecheckDirectory, "/codecheck_final_arg_null_1_", Client_Analysis[[k]][n], ".R"))

    } else {

      use_templateDS("/CodeCheck/Blank/codecheck_blank_arg_null_1.R",
                     save_as = paste0("/codecheck_final_arg_null_1_", Client_Analysis[[k]][n], ".R"),
                     data = list(argument_name = Client_Analysis[[k]][n]),
                     package = "DSFunctionCreator")

      codecheck_arg_null_1 <- readLines(paste0(codecheckDirectory, "/codecheck_final_arg_null_1_", Client_Analysis[[k]][n], ".R"))

    }


    for (i in 1:length(codelines[[n]][[1]])){
       result_init <- "Absent"

     if((grepl(codecheck_arg_null_1, codelines[[n]][[1]][i], fixed = TRUE)) &&
        (grepl(codecheck_arg_null_2, codelines[[n]][[1]][i+1], fixed = TRUE)) &&
        (grepl(codecheck_arg_null_3, codelines[[n]][[1]][i+2], fixed = TRUE))){
       result_init <- "Incomplete"

     }
     if((grepl(codecheck_arg_null_1, codelines[[n]][[1]][i], fixed = TRUE)) &&
        (grepl(codecheck_arg_null_2, codelines[[n]][[1]][i+1], fixed = TRUE)) &&
        (grepl(codecheck_arg_null_3, codelines[[n]][[1]][i+2], fixed = TRUE)) &&
        (grepl(codecheck_arg_null_4, codelines[[n]][[1]][i-1], fixed = TRUE))){
       result_init <- "Complete"
     }

       result[[i]] <- result_init



     if(any(result == "Complete")){
       xxx <- "Complete"

     } else if((any(result == "Incomplete")) && (!(any(result == "Complete")))){
       xxx <- "Incomplete"

     } else if(!(any(result == "Incomplete") && any(result == "Complete"))) {
       xxx <- "Absent"
     }


    }
    correctArgument[[n]] <- codecheck_arg_null_1

    Client_Results[[k]][n] <- xxx
  }

    Client_Analysis[[paste(k, "NullCheck", sep = "_")]] <- Client_Results[[k]]


 }
 return(Client_Analysis)


}


