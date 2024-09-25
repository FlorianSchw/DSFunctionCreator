#'
#' @title Initialising testthat Framework for DataSHIELD
#' @description The function creates a testthat framework including folder structure,
#' testthat.R file, addition of testthat in the Description file. In the DSLite setup,
#' standard DataSHIELD test datasets will be copied into the test suite folder, and a setup.R file
#' will be constructed for the use of DSLite. This also includes overwriting the testthat.R file.
#' Dependencies will be added to the Description file.
#' @details see above
#' @return No message so far
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @import usethis
#' @import fs
#' @import here
#' @export
#'

init.dsTest <- function(testType = c("local", "DSLite"),
                        datasetType = c("CNSIM", "INTIMIC")){

  if(testType == "local"){


  # Initiate the tests and testthat folder and the testthat.R file
  usethis::use_testthat()

  # Create a connection_to_datasets sub-directory in the testthat folder
  usethis::use_directory(path = "tests/testthat/connection_to_datasets")


  directory <- fs::path_package(package = "DSFunctionCreator", "templates/Tests/Client/Local")


  file.copy(from = paste0(fs::path_package(package = "DSFunctionCreator", "templates/Tests/Client/Local"), "/setup.R"), to = paste0(here::here("tests/testthat"), "/setup.R"))
  file.copy(from = paste0(fs::path_package(package = "DSFunctionCreator", "templates/Tests/Client/Local"), "/init_local_settings.R"), to = paste0(here::here("tests/testthat/connection_to_datasets"), "/init_local_settings.R"))
  file.copy(from = paste0(fs::path_package(package = "DSFunctionCreator", "templates/Tests/Client/Local"), "/init_studies_datasets.R"), to = paste0(here::here("tests/testthat/connection_to_datasets"), "/init_studies_datasets.R"))
  file.copy(from = paste0(fs::path_package(package = "DSFunctionCreator", "templates/Tests/Client/Local"), "/init_testing_datasets.R"), to = paste0(here::here("tests/testthat/connection_to_datasets"), "/init_testing_datasets.R"))
  file.copy(from = paste0(fs::path_package(package = "DSFunctionCreator", "templates/Tests/Client/Local"), "/local_settings.csv"), to = paste0(here::here("tests/testthat/connection_to_datasets"), "/local_settings.csv"))

  } else if(testType == "DSLite"){

    # Initiate the tests and testthat folder and the testthat.R file
    usethis::use_testthat()

    # Create a sub-directory in the testthat folder for the test datasets
    usethis::use_directory(path = "tests/testthat/testdata")


    #directory <- fs::path_package(package = "DSFunctionCreator", "templates/Tests/Client/DSLite")

    if(datasetType == "CNSIM"){

    file.copy(from = paste0(fs::path_package(package = "DSFunctionCreator", "templates/Tests/Datasets/CNSIM"), "/CNSIM1.csv"), to = paste0(here::here("tests/testthat/testdata", "/CNSIM1.csv")))
    file.copy(from = paste0(fs::path_package(package = "DSFunctionCreator", "templates/Tests/Datasets/CNSIM"), "/CNSIM2.csv"), to = paste0(here::here("tests/testthat/testdata", "/CNSIM2.csv")))
    file.copy(from = paste0(fs::path_package(package = "DSFunctionCreator", "templates/Tests/Datasets/CNSIM"), "/CNSIM3.csv"), to = paste0(here::here("tests/testthat/testdata", "/CNSIM3.csv")))

    factor_variables <- c("DIS_CVA",
                          "MEDI_LPD",
                          "DIS_DIAB",
                          "DIS_AMI",
                          "GENDER",
                          "PM_BMI_CATEGORICAL")

    input_dataset_name <- "CNSIM"


    }

    if(datasetType == "INTIMIC"){

    file.copy(from = paste0(fs::path_package(package = "DSFunctionCreator", "templates/Tests/Datasets/INTIMIC"), "/TestData_Microb_1.csv"), to = paste0(here::here("tests/testthat/testdata", "/TestData_Microb_1.csv")))
    file.copy(from = paste0(fs::path_package(package = "DSFunctionCreator", "templates/Tests/Datasets/INTIMIC"), "/TestData_Microb_2.csv"), to = paste0(here::here("tests/testthat/testdata", "/TestData_Microb_2.csv")))
    file.copy(from = paste0(fs::path_package(package = "DSFunctionCreator", "templates/Tests/Datasets/INTIMIC"), "/TestData_Microb_3.csv"), to = paste0(here::here("tests/testthat/testdata", "/TestData_Microb_3.csv")))

    factor_variables <- c("Sex",
                          "Education",
                          "Hypertension",
                          "Eyes",
                          "Children")

    input_dataset_name <- "TestData_Microb_"

    }


    dsPackageClient <- usethis:::project_name()
    dsPackage <- gsub("Client", "", dsPackageClient)


    #### This part fills the DSLite Setup template
    usethis::use_template("/Tests/Client/DSLite/dslite_setup_template.R",
                          save_as = paste0("tests/testthat/setup.R"),
                          data = list(input_dataset = input_dataset_name,
                                      factor_variables_transform = factor_variables,
                                      dsPackage = dsPackage),
                          package = "DSFunctionCreator")

    #### This part overwrites the standard testthat.R file to accomodate DataSHIELD

    usethis::use_template("/Tests/Client/DSLite/dslite_testthat_template.R",
                          save_as = paste0("tests/testthat.R"),
                          data = list(dsPackage = dsPackage),
                          package = "DSFunctionCreator")

    #### Check whether Description file already exists (should always before doing these steps...)
    if(!(file.exists(here::here("Description")))){
      usethis::use_description()
    }

    #### This part creates necessary dependencies in the Description File
    #### Note that DS Installations in this version are installed via remotes from GitHub
    usethis:::use_dependency("DSLite","suggests")
    usethis:::use_dependency("DSI","imports")
    usethis:::use_dependency("DSOpal","imports")
    usethis:::use_dependency("remotes","imports")

    #### Special case if dsBase is the DS package where FunctionCreator is being used
    if(!(dsPackage == "dsBase")){

      usethis:::use_dependency("dsBase","suggests")
      usethis:::use_dependency("dsBaseClient","imports")

      usethis:::proj_desc_field_update("Remotes", "datashield/dsBaseClient@6.3.0, datashield/dsBase@6.3.0", overwrite = TRUE)
    }


  }


  return()

}



