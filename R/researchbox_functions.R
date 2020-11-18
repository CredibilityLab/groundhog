

# FROM https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script -
# Specifically: https://stackoverflow.com/a/36777602/14647087
#' current script file (in full path)
#' @description current script file (in full path)
#' works with Rscript, source() or in RStudio Run selection, RStudio Console
#'
#' @examples
#' setwd(researchbox_data())
#' x <- researchbox_other()
#'
#' @export
get.script.folder <- function() {
  # http://stackoverflow.com/a/32016824/2292993
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  needle <- "--file="
  match <- grep(needle, cmdArgs)
  if (length(match) > 0) {
    # Rscript via command line
    return(normalizePath(sub(needle, "", cmdArgs[match])))
  } else {
    ls_vars <- ls(sys.frames()[[1]])
    if ("fileName" %in% ls_vars) {
      # Source'd via RStudio
      return(normalizePath(sys.frames()[[1]]$fileName))
    } else {
      if (!is.null(sys.frames()[[1]]$ofile)) {
        # Source'd via R console
        return(normalizePath(sys.frames()[[1]]$ofile))
      } else {
        # RStudio Run Selection
        # http://stackoverflow.com/a/35842176/2292993
        pth <- rstudioapi::getActiveDocumentContext()$path
        if (pth != "") {
          return(normalizePath(pth))
        } else {
          # RStudio Console
          tryCatch(
            {
              pth <- rstudioapi::getSourceEditorContext()$path
              pth <- normalizePath(pth)
            },
            error = function(e) {
              # normalizePath('') issues warning/error
              pth <- ""
            }
          )
          return(pth)
        }
      }
    }
  }
}

researchbox_code <- function() {

  return(dirname(get.script.folder()))
}

researchbox_dir <- function() {

  code_dir <- researchbox_code()
  researchbox_dir <- dirname(code_dir)

  return(researchbox_dir)
}

researchbox_data <- function() {

  data_dir <- file.path(researchbox_dir(), "Data")

  if (!dir.exists(data_dir)) {
    message(
      "groundhog says: The 'ResearchBox/Data' folder you are referencing ",
      "does not exist (", data_dir, ")."
    )
  }

  return(data_dir)
}

researchbox_other <- function() {

  other_dir <- file.path(researchbox_dir(), "Other")

  if (!dir.exists(other_dir)) {
    message(
      "groundhog says: The 'ResearchBox/Other' folder you are referencing ",
      "does not exist (", other_dir, ")."
    )
  }

  return(other_dir)
}
