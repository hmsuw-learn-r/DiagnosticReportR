#' Generate Diagnostic Report
#'
#' Using one of the included templates, generate a diagnostic report based on a
#' set of input parameters.
#'
#' @param template `character()` Name of template used to generate report.
#' @param params `list()` Parameters passed along to the chosen report template.
#' @param output_dir `character()` Directory to write generated report to.
#'   Defaults to the current working directory.
#' @param ... Additional arguments passed on to [rmarkdown::render()].
#'
#' @return `character()` Invisible path to the generated report.
#' @export
#'
#' @examples
#' \dontrun{
#' # Proof of concept report
#' generate_report("test_report")
#'
#' # Basic tend/level summary report
#' report_params <- list(
#'   old_data = old_data,
#'   new_data = new_data,
#'   level_threshold = 20,
#'   trend_bins = 1,
#'   comparison_var = "outcome",
#'   id_vars = c("year", "group"),
#'   group_vars = "group",
#'   trend_var = "year",
#'   scatter_color_var = "group"
#' )
#'
#' generate_report(
#'   "level_trend_summary",
#'   params = report_params,
#'   output_dir = "~"
#' )
#'
#' }
generate_report <- function(template,
                            params = NULL,
                            output_dir = NULL,
                            ...) {


  # Set defaults ------------------------------------------------------------

  if (is.null(output_dir)) {
    output_dir <- getwd()
  }


  # Validate inputs ---------------------------------------------------------

  if (length(template) != 1L) {
    stop("Only a single template can be specified.")
  }

  if (!template %in% list_templates()) {
    stop(paste0(
      "'", template, "'", "cannot be found in the available report templates. ",
      "Try running `DiagnosticReportR::list_templates()` to see possible ",
      "templates."
    ))
  }

  if (!dir.exists(output_dir)) {
    stop("Given output directory doesn't exist.")
  }


  # Render report -----------------------------------------------------------

  template_file <- system.file(
    "report_templates", paste0(template, ".Rmd"),
    package = "DiagnosticReportR",
    mustWork = TRUE
  )

  report_file <- rmarkdown::render(
    input = template_file,
    params = params,
    output_dir = output_dir,
    envir = new.env(),
    ...
  )

  return(invisible(report_file))

}
