#' Get groundhogR folder location
#'
#' @return the path to the groundhogR folder where groundhogR files will be
#'   stored and where packages loaded with [groundhog.library()] will be
#'   installed.
#'
#' @note you can change the location of this folder by editing the environment
#'   variable GROUNDHOG_FOLDER. In R, you can do this with the command
#'   `Sys.setenv(GROUNDHOG_FOLDER = "path")`.
#'
#' @export
#'
get.groundhogr.folder <- function() {

  groundhogR.folder <- path.expand(Sys.getenv("GROUNDHOGR_FOLDER"))

  if (groundhogR.folder == "") {

    message(
      "groundhogR needs to save files on your computer to run.\n",
      "It looks you haven't specified a folder for groundhogR yet."
    )
    answer <- readline(paste0(
      "Would you like to use '~/groundhogR' to save files? If so, type 'yes' ",
      "or just press 'Enter': "
    ))

    if (tolower(answer) %in% c("", "yes")) {
      groundhogR.folder <- path.expand(file.path("~", "groundhogR"))
      Sys.setenv(GROUNDHOGR_FOLDER = groundhogR.folder)
    } else {
      stop('Interactive specification of custom paths is not yet implemented. ',
           'Please set the desired location for groundhogR.folder by running ',
           'Sys.setenv(GROUNDHOGR_FOLDER = "path").', call. = FALSE)
    }
  }

  return(groundhogR.folder)
}
