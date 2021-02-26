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

#' Get path to data directory in researchbox structure
#'
#' [researchbox](https://researchbox.org/) organizes files into a main folder with 5 subfolders:
#' code, data, other, pre-registration, and materials. This function allows referencing the 
#' data folder if such structure is used, whereby the current r script is assumed to be in the code
#' folder, and the data in a neighboring folder. 
#' For example, if locally the script is saved into 
#' `c:/dropbox/researchbox_1821/code/script.r`, then researchbox_data() will return 
#' `c:/dropbox/researchbox_1821/data/` allowing making a dynamic reference to the data path. 
#' That way, when a user downloads the ResearchBox files to their folder, no matter where the that is locally, 
#' because subdirectories will maintain their relative position, code that uses researchbox_data() to set the 
#' data path will run in the new computer without needed adjustments.
#' If the user who downloaded the the files saves them into, say, "d:/temp/researchbox/code/script.r"
#' the command `researchbox_data()` will output "d:/temp/researchbox/data/" allowing the same code to load the 
#' same data despite the different parental folder structure.

#'
#' @return
#' Path to the `Data` folder within  [researchbox](https://researchbox.org/) structure 
#' @examples
#' #' \dontrun{
#' data_path <- researchbox_data()           #assign dynamically path as a sibling to current script's path
#' file_name <- 'example.csv'                #name of data file
#' file_path <- paste0(data_path, file_name) #full path, dynamically generated
#' data1 <- read.csv(file_path)
#' }
#'
#' @export
#'
researchbox_data <- function() {
  data_dir <- file.path(researchbox_dir(), "Data")
  if (!file.exists(data_dir)) {
    message(
      "warning: this folder does not exist"
    )
  }

  return(data_dir)
}


#' Get path to parent directory in  [ResearchBox](https://researchbox.org/) structure

#' See `researchbox_data()`
#' @export

researchbox_dir <- function() {
  code_dir <- get.script.folder()
  researchbox_dir <- dirname(code_dir)

  return(researchbox_dir)
}

#' 


#' Get path to 'other' directory in  [ResearchBox](https://researchbox.org/) structure

#' See `researchbox_data()`
#' @export

researchbox_other <- function() {

  other_dir <- file.path(researchbox_dir(), "Other")

  if (!file.exists(other_dir)) {
    message(
      "warning: this folder does not exist"
    )
  }

  return(other_dir)
}

