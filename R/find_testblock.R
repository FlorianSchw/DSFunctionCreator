#'
#' @title Internal Function for pathing of the building blocks for the test template of a function
#' @description This is an internal function which will find the correct path for the test buildingblocks.
#' @details This function adapts the code from the internal function find_template from the usethis::use_template function.
#' @param testblock_name specifies which buildingblock is called.
#' @param package is the name of the package where the buildingblocks are included ("DSFunctionCreator").
#' @return the path of the buildingblock.
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @import fs
#' @import usethis
#'


find_testblock <- function(testblock_name, package = "DSFunctionCreator"){
  usethis:::check_installed(package)
  path <- tryCatch(
    fs::path_package(package = package, "templates/Tests/Client/TestBlocks", testblock_name),
    error = function(e) ""
  )
  if (identical(path, "")){
    usethis::ui_stop(
      "Could not find testblock {ui_value(testblock_name)} \\
      in package {ui_value(package)}."
    )
  }
  path
}
