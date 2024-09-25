#'
#' @title Codeline and Argument Finder
#' @description This is an internal function.
#' @details This is an internal function to retrieve the codelines and function arguments of DataSHIELD client functions.
#' @param df is the data.frame input object containing the function file name and path.
#' @param type refers to the type of output: either codelines or arguments of a function
#' @return either codelines (a list) of the DataSHIELD functions or a data.frame containing arguments of the DataSHIELD functions.
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @import dplyr
#

internal_arguments <- function(df = NULL, type = NULL){

  if(is.null(df)){
    stop("Please provide a data.frame, where the function file name and paths are stored!", call.=FALSE)
  }

  if(is.null(type)){
    stop("Please provide a valid type, either 'codelines' or 'arguments'!", call.=FALSE)
  }

  if(!('codelines' %in% type) && !('arguments' %in% type)){
    stop("Only 'codelines' and 'arguments' are allowed inputs for the type argument.", call.=FALSE)
  }

  #### Grab the lines via readlines and then find the line which starts like the .R file
  names_with_space <- list()
  name_without_R <- list()
  codelines <- list()
  length_function_name <- list()

  #### Currently function filtered because of non-standard sign causing issues with function
  df <- df |>
    dplyr::filter(!(Function_FileName == "ds.GenotypeData.R"))

  for (i in 1:length(df$Function_FileName)){
    names_with_space[[i]] <- paste0(df$Function_FileName[[i]], " ")
    name_without_R[[i]] <- gsub(pattern = ".R ", "", names_with_space[[i]], fixed = TRUE)
    codelines[[i]] <- data.frame(readLines(df$Function_Path[[i]]))
    length_function_name[[i]] <- nchar(name_without_R[[i]])


    arg_def_vect <- c()
    codeline_active <- which(substr(codelines[[i]][[1]], 1, length_function_name[[i]]) == name_without_R[[i]])
    arg_def_complete <- TRUE
    line_break_index <- 0L

    while(arg_def_complete == TRUE){

      arg_def_temp <- codelines[[i]][[1]][(codeline_active + line_break_index)]
      arg_def_vect <- paste0(arg_def_vect, arg_def_temp)

      if(grepl(pattern = "\\{", x = arg_def_vect)){

        arg_def_complete <- FALSE

      }

      line_break_index <- line_break_index + 1

    }

    df$Argument_Call[[i]] <- arg_def_vect

  }


  if(type == "codelines"){
    return(codelines)
  } else if(type == "arguments"){
    return(df)
  }

}
