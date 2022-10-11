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



internal_datasources <- function(codelines = NULL, Client_Analysis = NULL){




  for (i in 1:length(Client_Analysis$Function_FileName)){
    if("datasources" %in% Client_Analysis[i, 2:16]){
      Client_Analysis$DatasourcesPresent[i] <- "Yes"
    } else {
      Client_Analysis$DatasourcesPresent[i] <- "No"
    }
  }

  codecheckDirectory <- fs::path_package(package = "DSFunctionCreator", "templates/CodeCheck/Final/DatasourcesNull")

  codecheck_datasources_null_1 <- readLines(paste0(codecheckDirectory, "/codecheck_final_datasources_null_1.R"))
  codecheck_datasources_null_2 <- readLines(paste0(codecheckDirectory, "/codecheck_final_datasources_null_2.R"))
  codecheck_datasources_null_3 <- readLines(paste0(codecheckDirectory, "/codecheck_final_datasources_null_3.R"))
  codecheck_datasources_null_4 <- readLines(paste0(codecheckDirectory, "/codecheck_final_datasources_null_4.R"))

  codecheckDirectory2 <- fs::path_package(package = "DSFunctionCreator", "templates/CodeCheck/Final/DatasourcesClass")

  codecheck_datasources_class_1 <- readLines(paste0(codecheckDirectory2, "/codecheck_final_datasources_class_1.R"))
  codecheck_datasources_class_2 <- readLines(paste0(codecheckDirectory2, "/codecheck_final_datasources_class_2.R"))
  codecheck_datasources_class_3 <- readLines(paste0(codecheckDirectory2, "/codecheck_final_datasources_class_3.R"))
  codecheck_datasources_class_4 <- readLines(paste0(codecheckDirectory2, "/codecheck_final_datasources_class_4.R"))




  Datasources_Results <- data.frame(Client_Analysis$Function_FileName, Client_Analysis$DatasourcesPresent)
  colnames(Datasources_Results) <- c("Function_FileName", "DatasourcesPresent")
  result <- list()


  if(!(length(codelines) = length(Datasources_Results$Function_FileName))){
    stop("Codelines and Client_Analysis have a different length! You might want to contact support.", call.=FALSE)
  }

    for (n in 1:length(Datasources_Results$Function_FileName)){

      for (i in 1:length(codelines[[n]][[1]])){
        result_init <- "Absent"

        if((grepl(codecheck_datasources_null_1, codelines[[n]][[1]][i], fixed = TRUE)) &&
           (grepl(codecheck_datasources_null_2, codelines[[n]][[1]][i+1], fixed = TRUE)) &&
           (grepl(codecheck_datasources_null_3, codelines[[n]][[1]][i+2], fixed = TRUE))){
          result_init <- "Incomplete"

        }
        if((grepl(codecheck_datasources_null_1, codelines[[n]][[1]][i], fixed = TRUE)) &&
           (grepl(codecheck_datasources_null_2, codelines[[n]][[1]][i+1], fixed = TRUE)) &&
           (grepl(codecheck_datasources_null_3, codelines[[n]][[1]][i+2], fixed = TRUE)) &&
           (grepl(codecheck_datasources_null_4, codelines[[n]][[1]][i-1], fixed = TRUE))){
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

        Datasources_Results$NullCheck[n] <- xxx

      }

    }


  for (n in 1:length(Datasources_Results$Function_FileName)){

    for (i in 1:length(codelines[[n]][[1]])){
      result_init <- "Absent"

      if((grepl(codecheck_datasources_class_1, codelines[[n]][[1]][i], fixed = TRUE)) &&
         (grepl(codecheck_datasources_class_2, codelines[[n]][[1]][i+1], fixed = TRUE)) &&
         (grepl(codecheck_datasources_class_3, codelines[[n]][[1]][i+2], fixed = TRUE))){
        result_init <- "Incomplete"

      }
      if((grepl(codecheck_datasources_class_1, codelines[[n]][[1]][i], fixed = TRUE)) &&
         (grepl(codecheck_datasources_class_2, codelines[[n]][[1]][i+1], fixed = TRUE)) &&
         (grepl(codecheck_datasources_class_3, codelines[[n]][[1]][i+2], fixed = TRUE)) &&
         (grepl(codecheck_datasources_class_4, codelines[[n]][[1]][i-1], fixed = TRUE))){
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

      Datasources_Results$ClassCheck[n] <- xxx

    }

  }




  return(Datasources_Results)


}


