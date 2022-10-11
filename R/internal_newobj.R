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



internal_newobj <- function(codelines = NULL, Client_Analysis = NULL){




  for (i in 1:length(Client_Analysis$Function_FileName)){
    if("newobj" %in% Client_Analysis[i, 2:16]){
      Client_Analysis$NewobjPresent[i] <- "Yes"
    } else if("newobj.name" %in% Client_Analysis[i, 2:16]){
      Client_Analysis$NewobjPresent[i] <- "newobj.name"
    } else {
      Client_Analysis$NewobjPresent[i] <- "No"
    }
  }

  codecheckDirectory <- fs::path_package(package = "DSFunctionCreator", "templates/CodeCheck/Final/NewobjNull")

  codecheck_newobj_null_1 <- readLines(paste0(codecheckDirectory, "/codecheck_final_newobj_null_1.R"))
  codecheck_newobj_null_2 <- readLines(paste0(codecheckDirectory, "/codecheck_final_newobj_null_2.R"))
  codecheck_newobj_null_3 <- readLines(paste0(codecheckDirectory, "/codecheck_final_newobj_null_3.R"))
  codecheck_newobj_null_4 <- readLines(paste0(codecheckDirectory, "/codecheck_final_newobj_null_4.R"))
  codecheck_newobjname_null_1 <- readLines(paste0(codecheckDirectory, "/codecheck_final_newobjname_null_1.R"))
  codecheck_newobjname_null_2 <- readLines(paste0(codecheckDirectory, "/codecheck_final_newobjname_null_2.R"))
  codecheck_newobjname_null_3 <- readLines(paste0(codecheckDirectory, "/codecheck_final_newobj_null_3.R"))
  codecheck_newobjname_null_4 <- readLines(paste0(codecheckDirectory, "/codecheck_final_newobj_null_4.R"))


  Newobj_Results <- data.frame(Client_Analysis$Function_FileName, Client_Analysis$NewobjPresent)
  colnames(Newobj_Results) <- c("Function_FileName", "NewobjPresent")
  result <- list()
  result2 <- list()



  if(!(length(codelines) = length(Newobj_Results$Function_FileName))){
    stop("Codelines and Client_Analysis have a different length! You might want to contact support.", call.=FALSE)
  }








  for (n in 1:length(Newobj_Results$Function_FileName)){



      for (i in 1:length(codelines[[n]][[1]])){
        result_init <- "Absent"

        if((grepl(codecheck_newobjname_null_1, codelines[[n]][[1]][i], fixed = TRUE)) &&
           (grepl(codecheck_newobjname_null_2, codelines[[n]][[1]][i+1], fixed = TRUE)) &&
           (grepl(codecheck_newobjname_null_3, codelines[[n]][[1]][i+2], fixed = TRUE))){
          result_init <- "Incomplete"

        }
        if((grepl(codecheck_newobjname_null_1, codelines[[n]][[1]][i], fixed = TRUE)) &&
           (grepl(codecheck_newobjname_null_2, codelines[[n]][[1]][i+1], fixed = TRUE)) &&
           (grepl(codecheck_newobjname_null_3, codelines[[n]][[1]][i+2], fixed = TRUE)) &&
           (grepl(codecheck_newobjname_null_4, codelines[[n]][[1]][i-1], fixed = TRUE))){
          result_init <- "Complete"
        }

        result[[i]] <- result_init



        if(any(result == "Complete")){
          xxx <- "Complete"

        } else if((any(result == "Incomplete")) && (!(any(result == "Complete")))){
          xxx <- "Incomplete"

        } else if(!(any(result == "Incomplete") && any(result == "Complete"))){
          xxx <- "Absent"
        }


        Newobj_Results$check_newobjname[n] <- xxx

     }



  }


    for (n in 1:length(Newobj_Results$Function_FileName)){
      for (i in 1:length(codelines[[n]][[1]])){
        result_init2 <- "Absent"

        if((grepl(codecheck_newobj_null_1, codelines[[n]][[1]][i], fixed = TRUE)) &&
           (grepl(codecheck_newobj_null_2, codelines[[n]][[1]][i+1], fixed = TRUE)) &&
           (grepl(codecheck_newobj_null_3, codelines[[n]][[1]][i+2], fixed = TRUE))){
          result_init2 <- "Incomplete"

        }
        if((grepl(codecheck_newobj_null_1, codelines[[n]][[1]][i], fixed = TRUE)) &&
           (grepl(codecheck_newobj_null_2, codelines[[n]][[1]][i+1], fixed = TRUE)) &&
           (grepl(codecheck_newobj_null_3, codelines[[n]][[1]][i+2], fixed = TRUE)) &&
           (grepl(codecheck_newobj_null_4, codelines[[n]][[1]][i-1], fixed = TRUE))){
          result_init2 <- "Complete"
        }

        result2[[i]] <- result_init2



        if(any(result2 == "Complete")){
          yyy <- "Complete"

        } else if((any(result2 == "Incomplete")) && (!(any(result2 == "Complete")))){
          yyy <- "Incomplete"

        } else if(!(any(result2 == "Incomplete") && any(result2 == "Complete"))){
          yyy <- "Absent"
        }


        Newobj_Results$check_newobj[n] <- yyy

    }



  }

  for (n in 1:length(Newobj_Results$Function_FileName)){
    if(Newobj_Results$NewobjPresent[n] == "newobj.name"){
      Newobj_Results$NullCheck[n] <- Newobj_Results$check_newobjname[n]
    } else {
      Newobj_Results$NullCheck[n] <- Newobj_Results$check_newobj[n]
    }

  }

 return(Newobj_Results)

}


