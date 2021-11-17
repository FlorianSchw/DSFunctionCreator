#'
#' @title Object input name
#' @description This is an internal function.
#' @details This is an internal function to get the correct object name based on the type of the input object
#' @param input_object is the name of the input object.
#' @return the name of the primary input object according to good practise agreement from DataSHIELD Developer Team
#' @author Florian Schwarz for the German Institute of Human Nutrition
#'

input_object_template <- function(input_object){

  # Check for allowed input options
  allowedinput <- c("data.frame", "vector", "matrix", "distance matrix", "cluster object")

  if(!(input_object %in% allowedinput)){
    stop("input_object needs to be one of the following: 'data.frame', 'vector', 'matrix', 'distance matrix' or 'cluster object'.", call.=FALSE)
  }


  # Assignment of names
  if(input_object == "data.frame"){
    input_object_assign <- "df.name"
  }

  if(input_object == "vector"){
    input_object_assign <- "x"
  }

  if(input_object == "matrix"){
    input_object_assign <- "M"
  }

  if(input_object == "distance matrix"){
    input_object_assign <- "diss"
  }

  if(input_object == "cluster object"){
    input_object_assign <- "clust"
  }

  return(input_object_assign)

}

