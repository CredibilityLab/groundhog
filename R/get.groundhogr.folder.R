#' Get groundhogR folder location
#'
#' @return the path to the groundhogR folder where groundhogR files will be
#'   stored and where packages loaded with [groundhog.library()] will be
#'   installed.
#'
#' @note you can change the location of this folder by editing the environment
#'   variable GROUNDHOG_FOLDER. In R, you can do this with the command
#'   `set.groundhogr.folder("path")`.
#'
#' @examples
#' get.groundhogr.folder()
#'
#' @export
#'
#' @seealso [set.groundhogr.folder()]
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
      set.groundhogr.folder(groundhogR.folder)
    } else {
      stop('Interactive specification of custom paths is not yet implemented. ',
           'Please set the desired location for groundhogR.folder by running ',
           'set.groundhogr.folder("path").', call. = FALSE)
    }
  }

  return(groundhogR.folder)
}

#' Set groundhogR folder location
#'
#' @param path Character. The path to the groundhogR folder where groundhogR
#'   files will be stored and where packages loaded with [groundhog.library()]
#'   will be installed.
#'
#' @note This setting can also be achieved by manually editing the `.Renviron`
#'   file. You can set this globally by editing `~/.Renviron` or only for a
#'   specific project by editing the `.Renviron` file at the root of your
#'   project.
#'
#' @examples
#' \dontrun{
#' set.groundhogr.folder("~/.groundhogR")
#' }
#'
#' @return (invisibly) `TRUE` upon success.
#'
#' @export
#'
#' @seealso [get.groundhogr.folder()]
#'
set.groundhogr.folder <- function(path) {

  renviron_path <- Sys.getenv("R_ENVIRON", "~/.Renviron")

  if (!file.exists(renviron_path)) {
    file.create(renviron_path)
  }

  env_vars <- readLines(renviron_path)

  new_setting <-paste0("GROUNDHOGR_FOLDER=", path)
  setting_line <- grep("GROUNDHOGR_FOLDER", env_vars)

  if (length(setting_line) > 0) {

    env_vars[setting_line] <- new_setting
    writeLines(env_vars, renviron_path)

  } else {

    write(new_setting, renviron_path, sep = "\n", append = TRUE)

  }

  Sys.setenv(GROUNDHOGR_FOLDER = path)
}
