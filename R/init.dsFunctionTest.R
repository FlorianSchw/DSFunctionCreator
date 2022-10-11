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

init.dsFunctionTest <- function(ds.function_name = NULL){

  # Input checks
  if(is.null(ds.function_name)){
    stop("Please provide the name of the DataSHIELD function for which a test shall be created!", call.=FALSE)
  }



  #### for a single function first


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



  block1 <- readLines(find_testblock("test_connection_dataset_cnsim_type1.R"))
  block2 <- readLines(find_testblock("test_testthat_function_opening.R"))
  block3 <- readLines(find_testblock("test_testthat_data.frame_preparations.R"))





  # Here I need to retrieve information from the other R Script




  if(include_DS_Connections == TRUE){
    block3 <- readLines(find_testblock("DS_Connections.R"))
  }

  if(include_DS_Connections_Class == TRUE){
    block4 <- readLines(find_testblock("DS_Connections_Class.R"))
  }

  if(include_isDefined == TRUE){
    block5 <- readLines(find_testblock("isDefined.R"))
  }

  if(include_checkClass == TRUE){
    block6 <- readLines(find_testblock("checkClass.R"))
  }

  if(include_type_Check == TRUE){
    block7 <- readLines(find_testblock("type_Check.R"))
  }

  if(include_methods_Check == TRUE){
    block8 <- readLines(find_testblock("methods_Check.R"))
  }

  if(include_newobj == TRUE){
    block9 <- readLines(find_testblock("newobj.R"))
  }

  if(include_DS_servercall == TRUE){
    block10 <- readLines(find_testblock("DS_servercall.R"))
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





  return()

}




