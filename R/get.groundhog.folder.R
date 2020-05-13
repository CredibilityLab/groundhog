#' Get groundhog folder location
#'
#' @return the path to the groundhog folder where groundhogR files will be
#'   stored and where packages loaded with [groundhog.library()] will be
#'   installed.
#'
#' @note you can change the location of this folder by editing the environment
#'   variable GROUNDHOG_FOLDER. In R, you can do this with the command
#'   `set.groundhog.folder("path")`.
#'
#' @examples
#' get.groundhog.folder()
#' @export
#'
#' @seealso [set.groundhog.folder()]
#'

# Function that gets the groundhogR folder, or prompts user to create it.
get.groundhog.folder <- function() {

  groundhog.folder <- path.expand(Sys.getenv("GROUNDHOG_FOLDER"))

  # If a folder for has not been set, prompt user
  if (groundhog.folder == "") {
    # 1. Put the default folder into a variable to show user
    default.folder <- paste0(path.expand("~"), "/groundhogR/")

    # a=function() {
    # 2. Show message asking for the desired folder
    message2(
      "*****************  Groundhog's directory *********************************************************\n",
      "<PRESS ENTER> to accept the default directory ('", default.folder, "') for saving packages"
    )
    message1(
      "groundhogR needs a directory to use as a library for saving downloaded and installed packages.\n",
      "<PRESS ENTER> to use the default for this computer ('", default.folder, "').\n",
      "Or, type in the folder you would like to use instead (e.g, 'c:/dropbox/groundhogR'). Do not include quotes.\n",
      "If the folder does not already exists, it will be created.\n",
      "Type 'quit' to not choose a directory at this time"
    )

    message2("************************************************************************************************")
    answer <- readline(prompt = "Please enter your a path for your groundhog folder")


    # 3 Quite process if asked to, or asnswer is too short or no slashes
    # Looks like 'quit'
    if (tolower(answer) %in% c("quit", "q", "qui", "qit")) {
      stop("OK folder setting process was stopped.")
    }
    # too short
    if (nchar(answer) < 4 & nchar(answer) > 0) {
      stop(paste0("The folder name '", answer, "' seems too short to be a folder."))
    }

    # no slashes
    slash.count <- regexpr("/", answer)[1] + regexpr("\\\\", answer)[1]
    if (slash.count < 0 & nchar(answer) > 0) {
      stop(paste0("The directory name '", answer, "' is missing slashes. It does not seem like a proper directory."))
    } # End if no slashes

    # 4 If different folder was entered, create it as  means to check for possible error
    if (!file.exists(answer) & nchar(answer) > 0) {
      folder.create <- dir.create(answer, recursive = TRUE, showWarnings = FALSE)
      if (!folder.create) {
        stop("The folder you entered is invalid, maybe it has symbols like {,'}]]?")
      }
    } # End if file dodsn't exist

    # 5. Assign groundghog folder to default or answer
    if (nchar(answer) == 0) {
      groundhog.folder <- path.expand(file.path("~", "groundhogR"))
    } # End if default answer
    if (nchar(answer) > 0) {
      groundhog.folder <- answer
    } # End if entered answer

    # 6   Set it
    set.groundhog.folder(groundhog.folder) # see function below

    # 7  Tell user, wait 7 seconds.
    message1(
      "The folder was succesfully set to: '", groundhog.folder, "'\n",
      "You can undo this selection by running: set.groundhog.folder('')\n\n\n\n"
    )
    Sys.sleep(7)
  } # End if no groundhogR folder
  return(groundhog.folder)
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
#' set.groundhog.folder("~/.groundhogR")
#' }
#'
#' @return (invisibly) `TRUE` upon success.
#'
#' @export
#'
#' @seealso [get.groundhog.folder()]
#'
set.groundhog.folder <- function(path) {
  renviron_path <- Sys.getenv("R_ENVIRON", "~/.Renviron")

  if (!file.exists(renviron_path)) {
    file.create(renviron_path)
  }

  env_vars <- readLines(renviron_path)

  new_setting <- paste0("GROUNDHOG_FOLDER=", path)
  setting_line <- grep("GROUNDHOG_FOLDER", env_vars)

  if (length(setting_line) > 0) {
    env_vars[setting_line] <- new_setting
    writeLines(env_vars, renviron_path)
  } else {
    write(new_setting, renviron_path, sep = "\n", append = TRUE)
  }

  Sys.setenv(GROUNDHOG_FOLDER = path)
}
