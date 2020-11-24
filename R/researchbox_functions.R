# FROM https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script -
# Specifically: https://stackoverflow.com/a/36777602/14647087
#
# works with Rscript, source() or in RStudio Run selection, RStudio Console
#
get.script.folder <- function() {
  # http://stackoverflow.com/a/32016824/2292993
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  needle <- "--file="
  match <- grep(needle, cmdArgs)

  if (length(match) > 0) {
    # Rscript via command line
    path <- sub(needle, "", cmdArgs[match])
    return(normalizePath())
  }

  if ("fileName" %in% ls(sys.frames()[[1]])) {
    # Source'd via RStudio
    return(normalizePath(sys.frames()[[1]]$fileName))
  }

  if (!is.null(sys.frames()[[1]]$ofile)) {
    # Source'd via R console
    return(normalizePath(sys.frames()[[1]]$ofile))
  }

  if (is_rstudio() && requireNamespace("rstudioapi", quietly = TRUE)) {
    # RStudio Run Selection
    # http://stackoverflow.com/a/35842176/2292993
    pth <- rstudioapi::getActiveDocumentContext()$path
    if (pth != "") {
      return(normalizePath(pth))
    }

    pth <- rstudioapi::getSourceEditorContext()$path
    if (pth != "") {
      return(normalizePath(pth))
    }
  }

  return("")
}

#' Get researchbox project paths
#'
#' Get paths relative to the different subfolders of a
#' [researchbox](https://researchbox.org/).
#'
#' @return
#' - For `researchbox_code()`, the path to the `Code` folder of the researchbox
#' - For `researchbox_data()`, the path to the `Data` folder of the researchbox
#' - For `researchbox_other()`, the path to the `Other` folder of the
#' researchbox
#'
#' @examples
#' x <- researchbox_other()
#' \dontrun{
#' setwd(researchbox_data())
#' }
#'
#' @export
#'
#' @rdname researchbox
researchbox_code <- function() {

  return(dirname(get.script.folder()))
}

researchbox_dir <- function() {

  code_dir <- researchbox_code()
  researchbox_dir <- dirname(code_dir)

  return(researchbox_dir)
}

#' @rdname researchbox
#' @export
researchbox_data <- function() {

  data_dir <- file.path(researchbox_dir(), "Data")

  if (!file.exists(data_dir)) {
    message(
      "groundhog says: The 'ResearchBox/Data' folder you are referencing ",
      "does not exist (", data_dir, ")."
    )
  }

  return(data_dir)
}

#' @rdname researchbox
#' @export
researchbox_other <- function() {

  other_dir <- file.path(researchbox_dir(), "Other")

  if (!file.exists(other_dir)) {
    message(
      "groundhog says: The 'ResearchBox/Other' folder you are referencing ",
      "does not exist (", other_dir, ")."
    )
  }

  return(other_dir)
}
