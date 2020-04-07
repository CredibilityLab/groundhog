#' @export
#'
get.groundhogr.folder <- function() {

  groundhogR.folder <- Sys.getenv("GROUNHOGR_FOLDER")

  if (groundhogR.folder == "") {

    answer <- readline(paste0(
      "groundhogR needs to save files on your computer to run.\n",
      "It looks you haven't specified a folder for groundhogR yet.\n",
      "Would you like to use '~/groundhogR' to save files? If so, type 'yes' ",
      "or just press 'Enter'"
    ))

    if (tolower(answer) %in% c("", "yes")) {
      groundhogR.folder <- path.expand(file.path("~", "grounhogR"))
    } else {
      stop('Interactive specification of custom paths is not yet implemented. ',
           'Please set the desired location for groundhogR.folder by running ',
           'Sys.setenv(GROUNDHOGR_FOLDER = "path").', call. = FALSE)
    }
  }
}
