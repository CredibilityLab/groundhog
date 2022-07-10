#' Get current local path to groundhog folder
#'
#' @return the path to the groundhog folder, the meta-library where 
#'   [groundhog.library()] downloads and stores packages that can be loaded 
#'
#' @note you can change the location of this folder with the command
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
    main_folder <-  paste0(path.expand("~"), "/R_groundhog")
    
    
  #See if consent has been given  \
        consent <- (file.exists(main_folder))
        
       #If no folder, ask for consent 
         if (consent == FALSE) {
            message2()
            message1("groundhog needs authorization to save files to '",main_folder, "'\n",
                    "Enter 'OK' to provide authorization")
                                  
            answer <- readline()
            answer <- gsub("'", "", answer)  #kill the ' if entered

            if (toupper(answer)=="OK")
             {
             consent <- TRUE
            }
         } #End if consent == FALSE
        
        if (consent == FALSE)
          {
          
          message("You typed '",answer,"'. Only if you type 'OK' can you use groundhog.library()")
          exit()
          }
    
  #Create main folder 
      dir.create(main_folder, showWarnings = FALSE, recursive = TRUE)

  #Path to cookie file with location of folder
    path_file_storing_groundhog_library_location <- paste0(main_folder ,"/current_groundhog_folder.txt")
    
  #If cookie file exists, use it, otherwise, use that same location for library
      if (file.exists(path_file_storing_groundhog_library_location)) {
        
        #Read the 'cookie' file with the location of the library
          f <- path_file_storing_groundhog_library_location
          groundhog.folder <- readChar(f, file.info(f)$size)  #Reads full file into a string, fixes bug in groundhog 1.1.0
        
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
#' @param path Character. The path to the groundhog folder containing the library
#'   where packages are downloaded and installed.
#'

#'
#' @examples
#' \dontrun{
#' set.groundhog.folder("~/.R_groundhog")
#' }
#'
#' @return (invisibly) `TRUE` upon success.
#'
#' @export
#'
#' @seealso [get.groundhog.folder()]
#'
set.groundhog.folder <- function(path) {
  
  #Set main folder with 'cookie files' and default for library
    main_folder <-  paste0(path.expand("~"), "/R_groundhog/")
    
  #Create main folder 
    dir.create(main_folder, showWarnings = FALSE, recursive = TRUE)
    
  #Path to file saving groundhog folder
    path_file_storing_groundhog_library_location <- paste0(main_folder ,"current_groundhog_folder.txt")
    
  #Save the cookie
    path <- trimws(path)
    cat(path, file = path_file_storing_groundhog_library_location)
  
  #Create groundhog
    dir.create(path, showWarnings = FALSE, recursive = TRUE)

    
  #Assign it to the live environment
    Sys.setenv(GROUNDHOG_FOLDER = path)
 
  #Load cran toc rds
    load.cran.toc() #this will copy the rds files
    
  #Show message
    message1("The groundhog folder path is now:\n",path)
    }
