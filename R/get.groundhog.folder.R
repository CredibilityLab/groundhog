  #' Get groundhog folder location
#'
#' @return the path to the groundhog folder, the meta-library where 
#'   [groundhog.library()] downloads and stores binaries and source files 
#'
#' @note you can change the location of this folder by editing the environment
#'   variable GROUNDHOG_FOLDER. In R, you can do this with the command
#'   `set.groundhog.folder("path")`.
#' 
#' @examples
#' \dontrun{
#' get.groundhog.folder()
#' }
#'
#' @export
#'
#' @seealso [set.groundhog.folder()]
#'

# Function that gets the groundhog folder, or prompts user to create it.
get.groundhog.folder <- function() {
  
  #Home path for this user
    home_path <- Sys.getenv("home")
    cookie_path <- paste0(home_path, "/R_groundhog/")
    folder_cookie <- paste0(cookie_path ,"current_groundhog_folder.txt")
    if (file.exists(folder_cookie)) {
        groundhog.folder <- scan(folder_cookie,what='character', quiet=TRUE)
        } else {
        groundhog.folder <-""
        }
    

  # If a folder for has not been set, prompt user
  if (groundhog.folder == "") {
    # 1. Put the default folder into a variable to show user
    default.folder <- paste0(path.expand("~"), "/groundhog/")

    # a=function() {
    # 2. Show message asking for the desired folder
    message2(
      "*****************  Setting a directory for groundhog   ***********************\n\n",
      "         <PRESS ENTER> to accept the default directory:\n",
      "         '",default.folder, "'\n"
    )#End of message 2

    message1(
      "You need to set a directory save all packages installed by groundhog.\n",
      "You may <PRESS ENTER> to use the default for this computer ('", default.folder, "').\n",
      "Or, type in the folder you would like to use instead (e.g, 'c:/dropbox/groundhog').\n",
      "Do not include quotes.\n",
      "If the folder does not already exists, it will be created.\n\n",
      "Type 'quit' to not choose a directory at this time"
    )

    message2("************************************************************************************************")
    answer <- readline(prompt = "Please enter a path for your groundhog folder: >")

    if (tolower(answer) == "quit") {
      exit("OK folder setting process was stopped.")
    }

    # 5. Assign groundhog folder to default or answer
    if (nchar(answer) == 0) {
      groundhog.folder <- path.expand(file.path("~", "groundhog"))
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
  } # End if no groundhog folder
  return(groundhog.folder)
}

#' Set groundhog folder location
#'
#' @param path Character. The path to the groundhog folder where groundhog
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
#' set.groundhog.folder("~/.groundhog")
#' }
#'
#' @return (invisibly) `TRUE` upon success.
#'
#' @export
#'
#' @seealso [get.groundhog.folder()]
#'
set.groundhog.folder <- function(groundhog.folder) {
  
  #Home path for this user
    home_path <- Sys.getenv("home")
    cookie_path <- paste0(home_path, "/R_groundhog/")
    dir.create(cookie_path, showWarnings = FALSE, recursive = TRUE)
    
  #Path to file saving groundhog folder
    folder_cookie <- paste0(cookie_path ,"current_groundhog_folder.txt")
    
  #Save the cookie
    cat(groundhog.folder, file = folder_cookie)
  
  #Assign it to the live environment
    Sys.setenv(GROUNDHOG_FOLDER = groundhog.folder)
    }
