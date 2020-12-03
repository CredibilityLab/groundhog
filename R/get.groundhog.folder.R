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
  
  #Set main folder with 'cookie files' and default for library
    main_folder <-  paste0(path.expand("~"), "/R_groundhog/")
    
  #Create main folder 
    dir.create(main_folder, showWarnings = FALSE, recursive = TRUE)

  #Path to cookie file with location of folder
    path_file_storing_groundhog_library_location <- paste0(main_folder ,"current_groundhog_folder.txt")
    
  #If cookie file exists, use it, otherwise, use that same location for library
      if (file.exists(path_file_storing_groundhog_library_location)) {
        #Read the cookie file with the location of the library
          groundhog.folder <- scan(path_file_storing_groundhog_library_location,what='character', quiet=TRUE)
        
        } else {
          
        #This is the default
          groundhog.folder <-paste0(main_folder, "/groundhog_library/")
          
        #Set it using function below
          set.groundhog.folder(groundhog.folder) 
          
        } #End if cookie file does not exist
    
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
