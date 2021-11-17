#'
#' @title Template Builder for client-side DataSHIELD functions
#' @description The function creates a new basic R file for client-side DataSHIELD functions.
#' @details This function operates in two steps. Initially, a template is being built based on the selection of the building blocks. In the second step,
#' this template is filled with information, which were provided by the user and this new R file is being saved in the R folder of the active package.
#' @param include_function_information is a logical parameter to include the function information in the new R file.
#' @param include_function_arguments is a logical parameter to include the function arguments in the new R file.
#' @param include_DS_Connections is a logical parameter to include the DSConnection find part in the new R file.
#' @param include_DS_Connections_Class is a logical parameter to include the DSConnection class check in the new R file.
#' @param include_isDefined is a logical parameter to include the 'defined in all studies' part in the new R file.
#' @param include_checkClass is a logical parameter to include the class check of the input variable in the new R file.
#' @param include_type_Check is a logical parameter to include the second part of the class check in the new R file.
#' @param include_methods_Check is a logical parameter to include the methods check part in the new R file.
#' @param include_newobj is a logical parameter to include the newobj generating part in the new R file.
#' @param include_DS_servercall is a logical parameter to include the building of the 'cally' object and the call of the server-side function in the new R file.
#' @param function_name is the name of the new DataSHIELD function.
#' @param input_object is the name of the input object.
#' @param object_type is the class of the input object.
#' @param datashield_type represent the typ of DataSHIELD server-side function; either 'assign' or 'aggregate'.
#' @return creates a new R file for programming a DataSHIELD function
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @import usethis
#' @import fs
#' @export
#'

createDSfunction <- function(include_function_information = TRUE,
                             include_function_arguments = TRUE,
                             include_DS_Connections = TRUE,
                             include_DS_Connections_Class = TRUE,
                             include_isDefined = TRUE,
                             include_checkClass = TRUE,
                             include_type_Check = TRUE,
                             include_methods_Check = TRUE,
                             include_newobj = TRUE,
                             include_DS_servercall = TRUE,
                             function_name = NULL,
                             input_object = NULL,
                             object_type = NULL,
                             datashield_type = NULL){


  # Input checks
  if(is.null(function_name)){
    stop("Please provide a name for the new DataSHIELD function!", call.=FALSE)
  }

  if(is.null(input_object)){
    stop("Please provide a name that characterises the input object!", call.=FALSE)
  }

  if(is.null(object_type)){
    stop("Please provide the class of the input object of the new DataSHIELD function!", call.=FALSE)
  }

  if(include_DS_servercall == TRUE && is.null(datashield_type)){
    stop("You want to include the call to the server-side but have not provided the typ of server-side function ('assign', 'aggregate')!", call.=FALSE)
  }


  # Check for allowed DataSHIELD types
  allowedDStype <- c("assign", "aggregate", "assign.table", "assign.resource", "assign.expr")

  if(!(datashield_type %in% allowedDStype)){
    stop("The datashield_type needs to be one of the following: 'aggregate', 'assign', 'assign.table', 'assign.resource' or 'assign.expr'.", call.=FALSE)
  }






  block1 <- c()
  block2 <- c()
  block3 <- c()
  block4 <- c()
  block5 <- c()
  block6 <- c()
  block7 <- c()
  block8 <- c()
  block9 <- c()
  block10 <- c()



  if(include_function_information == TRUE){
    block1 <- readLines(find_buildingblock("function_information.R"))
  }

  if(include_function_arguments == TRUE){
    block2 <- readLines(find_buildingblock("function_arguments.R"))
  }

  if(include_DS_Connections == TRUE){
    block3 <- readLines(find_buildingblock("DS_Connections.R"))
  }

  if(include_DS_Connections_Class == TRUE){
    block4 <- readLines(find_buildingblock("DS_Connections_Class.R"))
  }

  if(include_isDefined == TRUE){
    block5 <- readLines(find_buildingblock("isDefined.R"))
  }

  if(include_checkClass == TRUE){
    block6 <- readLines(find_buildingblock("checkClass.R"))
  }

  if(include_type_Check == TRUE){
    block7 <- readLines(find_buildingblock("type_Check.R"))
  }

  if(include_methods_Check == TRUE){
    block8 <- readLines(find_buildingblock("methods_Check.R"))
  }

  if(include_newobj == TRUE){
    block9 <- readLines(find_buildingblock("newobj.R"))
  }

  if(include_DS_servercall == TRUE){
    block10 <- readLines(find_buildingblock("DS_servercall.R"))
  }


  directory <- fs::path_package(package = "DSFunctionCreator", "templates")



  # This completes the first part of the function by creating a template file upon choosing the building blocks (TRUE/FALSE)
  writeLines(text = c(block1,
                      block2,
                      block3,
                      block4,
                      block5,
                      block6,
                      block7,
                      block8,
                      block9,
                      block10),
             con = paste0(directory,"/user_template.R"))




  # This part fills the newly created template with user input
  usethis::use_template("user_template.R",
                        save_as = paste0("R/ds.", function_name, ".R"),
                        data = list(function_name = function_name,
                                    input_object_assign = input_object_template(input_object),
                                    object_type = object_type,
                                    datashield_type = datashield_type),
                        package = "DSFunctionCreator")

}







