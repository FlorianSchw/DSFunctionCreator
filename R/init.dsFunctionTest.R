#'
#' @title Initialising test file for a specific function
#' @description This function initialises a test-ds.function.R file in the respective testthat structure
#' @details The initial file for now contains the typical open and closing as well as the simple null-argument
#' test of the first argument in the function call.
#' @param function_name Name of the ds.function
#' @return Nothing for now. Files will be created
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @import dplyr
#' @import here
#' @import stringr
#' @import fs
#' @export
#'

init.dsFunctionTest <- function(function_name = NULL){

  # Input checks
  if(is.null(function_name)){
    stop("Please provide the name of the DataSHIELD function for which a test shall be created!", call.=FALSE)
  }

  #### for a single function first

  block1 <- c()
  block2 <- c()
  block3 <- c()


  #### Checking Argument Names of the Function


  function_filename <- paste0(function_name, ".R")

  R_functions <- CodeCheck.ArgumentOverview(path_to_dsPackages = here::here() %>% dirname(),
                                            package_type = "Client")


  Arguments_Function <- R_functions |>
    dplyr::filter(Function_FileName == paste0(function_name, ".R")) |>
    dplyr::select(!(Function_FileName)) |>
    unlist()

  Arguments_Function <- na.omit(Arguments_Function)

  Client_R_Paths_ds <- FilePathFinder(path = here::here() %>% dirname(),
                                                          type = "Client") |>
    dplyr::filter(Function_FileName == paste0(function_name, ".R"))


  Client_Arguments <- internal_arguments(df = Client_R_Paths_ds, type = "codelines")


  stop_message <- c()
  stop_argument <- c()
  codeline_nr <- c()
  index <- 1L

  for (k in 1:length(Arguments_Function)){
    for (i in 1:length(Client_Arguments[[1]][[1]])){

      length_argument <- nchar(Arguments_Function[k])
      null_test <- paste0("if(is.null(", Arguments_Function[k], ")){")


      if(substr(stringr::str_trim(Client_Arguments[[1]][[1]][i]), 1, 14+length_argument) == null_test){
        if(substr(stringr::str_trim(Client_Arguments[[1]][[1]][i+1]), 1, 5) == "stop("){
          if(substr(stringr::str_trim(Client_Arguments[[1]][[1]][i+2]), 1, 1) == "}"){

            if_stop_message <- stringr::str_trim(Client_Arguments[[1]][[1]][i+1])
            if_stop_message <- gsub("stop\\(", "", if_stop_message)
            stop_message[index] <- gsub(", call.=FALSE\\)", "", if_stop_message)
            stop_argument[index] <- Arguments_Function[k]

            index <- index + 1L

          }
        }
      }

    }
  }

  stop_message_collector <- data.frame(stop_argument,
                                       stop_message)




    testopeningDirectory <- fs::path_package(package = "DSFunctionCreator", "templates/Tests/Client/TestStructure")

    use_templateDS("/Tests/Client/TestStructure/opening_template.R",
                             save_as = paste0("/client_test_opening_", function_name, ".R"),
                             data = list(function_input = function_name),
                             directory = "Tests/Client/TestStructure",
                             package = "DSFunctionCreator")

    block1 <- readLines(paste0(testopeningDirectory, "/client_test_opening_", function_name, ".R"))



  if(!(is.null(stop_message_collector))){

    argNullDirectory <- fs::path_package(package = "DSFunctionCreator", "templates/Tests/Client/Null")

    use_templateDS("/Tests/Client/Null/expect_error_null1.R",
                   save_as = paste0("/client_test_arg_null_1_", function_name, ".R"),
                   data = list(function_input = function_name,
                               stop_message = stop_message_collector$stop_message[1]),
                   directory = "Tests/Client/Null",
                   package = "DSFunctionCreator")

    block2 <- readLines(paste0(argNullDirectory, "/client_test_arg_null_1_", function_name, ".R"))
  }


    block3 <- readLines(fs::path_package(package = "DSFunctionCreator",
                                                "templates/Tests/Client/TestStructure/closingLine.R"))


  # This completes the first part of the function by creating a template file upon choosing the building blocks (TRUE/FALSE)
  writeLines(text = c(block1,
                      block2,
                      block3),
             con = here::here(paste0("tests/testthat/test-", function_name, ".R")))


  return()

}



