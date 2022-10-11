#'
#' @title Template filler for CodeCheck
#' @description This function is very similar to the usethis original but changes the pathing options of where template will be save.
#' @details The function pulls a blank codeline file and fills it with arguments to create a final codeline file which can be used for CodeCheck.
#' @param template Path to template file relative to `templates/` directory
#'   within `package`; see details.
#' @param data A list of data passed to the template.
#' @param save_as A name for the file which is going to be saved.
#' @param package Name of the package where the template is found.
#' @return A logical vector indicating if file was modified.
#' @import usethis
#' @import fs
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @export

use_templateDS <- function(template,
                           data = list(),
                           save_as = NULL,
                           package = "DSFunctionCreator") {
  template_contents <- usethis:::render_template(template, data, package = package)
  directoryCodeCheckFinal <- fs::path_package(package = "DSFunctionCreator", "templates/CodeCheck/Final/ArgumentNull")
  new <- usethis::write_over(paste0(directoryCodeCheckFinal, save_as), template_contents)

  invisible(new)
}

