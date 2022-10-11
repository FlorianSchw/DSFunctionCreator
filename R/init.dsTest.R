#'
#' @title XXXX
#' @description XXXX
#' @details XXXX
#' @return XXXX
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @import usethis
#' @import fs
#' @import here
#' @export
#'

init.dsTest <- function(){

  # Initiate the tests and testthat folder and the testthat.R file
  usethis::use_testthat()



  # Create a connection_to_datasets sub-directory in the testthat folder
  usethis::use_directory(path = "tests/testthat/connection_to_datasets")


  directory <- fs::path_package(package = "DSFunctionCreator", "templates/Tests/Client")


  file.copy(from = paste0(fs::path_package(package = "DSFunctionCreator", "templates/Tests/Client"), "/setup.R"), to = paste0(here::here("tests/testthat"), "/setup.R"))
  file.copy(from = paste0(fs::path_package(package = "DSFunctionCreator", "templates/Tests/Client"), "/init_local_settings.R"), to = paste0(here::here("tests/testthat/connection_to_datasets"), "/init_local_settings.R"))
  file.copy(from = paste0(fs::path_package(package = "DSFunctionCreator", "templates/Tests/Client"), "/init_studies_datasets.R"), to = paste0(here::here("tests/testthat/connection_to_datasets"), "/init_studies_datasets.R"))
  file.copy(from = paste0(fs::path_package(package = "DSFunctionCreator", "templates/Tests/Client"), "/init_testing_datasets.R"), to = paste0(here::here("tests/testthat/connection_to_datasets"), "/init_testing_datasets.R"))
  file.copy(from = paste0(fs::path_package(package = "DSFunctionCreator", "templates/Tests/Client"), "/local_settings.csv"), to = paste0(here::here("tests/testthat/connection_to_datasets"), "/local_settings.csv"))

  return()

}



