#'
#' @title Creating initial GitHub Actions folder and file
#' @description The function initialises the GitHub Actions folder and a simple rmd-check file
#' @details The files are stored in the DSFunctionCreator package and will be copied as they are
#' for now. Note, that currently also a connection to codecov is part of the github-actions.yml.
#' @return some message to the user
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @import usethis
#' @import fs
#' @import here
#' @export
#'

init.GitHubActions <- function(){


  #### Initiate GitHub-Actions
  usethis::use_directory(path = ".github/workflows")
  file.copy(from = paste0(fs::path_package(package = "DSFunctionCreator", "templates/GitHubActions/Client"), "/githubactions_template.yml"), to = paste0(here::here(".github/workflows", "/github-actions.yml")))

  message("GitHub Actions with Codecov has been initiated. Be aware that you need to link them both.")

}
