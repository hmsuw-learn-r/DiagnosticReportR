#' Get Available Report Templates
#'
#' Get the names of the valid templates for use when generating reports
#'
#' @return `character()` Vector of template names
#' @export
#'
#' @examples
#' list_templates()
list_templates <- function() {

  template_dir <- system.file(
    "report_templates",
    package = "DiagnosticReportR",
    mustWork = TRUE
  )

  template_files <- list.files(template_dir, pattern = "*.Rmd")

  gsub(basename(template_files), pattern = ".Rmd$", replacement = "")

}
