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
  
  
  #Verify folder with 'cookie files' and default for library exist. giving consent to save files
    consent <- check.consent() 
    if (consent==FALSE) {
      message(' -- not authorized to save to folder necessary for groundhog to work --')
      exit()
      
      }
  
  #Set main folder with 'cookie files' and default for library
    main_folder <-  paste0(path.expand("~"), "/R_groundhog")
 
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
#'   where packages are downloaded and installed. For best performance and reliability, 
#'   it is recommended that the groundhog folder be in the same volume/drive as the
#'   folder used as the default R library, and
#'   that it not be in a Dropbox (or similar) folders. Groundhog will be slower 
#'   and occasionally produce errors when the groundhog folder is in Dropbox or 
#'   a different drive.
#'

#'
#' @examples
#' \dontrun{
#' set.groundhog.folder("c:/groundhog_folder")
#' }
#'
#' @return (invisibly) `TRUE` upon success.
#'
#' @export
#'
#' @seealso [get.groundhog.folder()]
#'
set.groundhog.folder <- function(path) {
  
#Ensure path was set
  if (missing(path)) gstop("You forgot to enter the <path> you wanted to set.")
  
#Early return if already set to path
  if (dir.exists(path) && normalizePath(path) == normalizePath(get.groundhog.folder())) {
      
    #Note: do dir.exists() first becuase we cannot normalizePath() if it does not exist
    #and if it does not exist, obviously it is not the current path
    
    message1("The groundhog folder already was '", path , "'")
    return(invisible(TRUE))
  }
  
  ###########################################
  #1 Warnings for path
  
    #1.1 DROPBOX
        #If path contains dropbox and we have not warned them within 10 minutes

        if  (regexpr('dropbox', tolower(path))>0) {

          #Draft message
            msg=paste0("The path '",path,"' seems to be a Dropbox folder. Groundhog will be slower and ",
              "more likely to produce occasional errors if the library is on Dropbox; ",
              "consider using a different path. If you want to set it to  '",path,"' anyway, ",
              "type 'anyway', else type 'cancel'")

          #Show it
            answer.dropbox<-infinite.prompt(format_msg(msg),valid_answers=c('anyway','cancel'),must.restart=FALSE)

            if (tolower(answer.dropbox)=='cancel') {
              message1("OK. Request cancelled.")
              exit()
            }
            } #End if dropbox

 

  

  #############################################################################

  #Set main folder with 'cookie files' and default for library
    main_folder <-  paste0(path.expand("~"), "/R_groundhog/")
    
  #Create main folder 
    dir.create(main_folder, showWarnings = FALSE, recursive = TRUE)
    
    #this si redundant with the test that we can rename from it done above
    
  #Path to file saving groundhog folder
    path_file_storing_groundhog_library_location <- paste0(main_folder ,"current_groundhog_folder.txt")
    
  
  #Save the cookie
    path <- trimws(path)
    
  
  #Create groundhog_folder
    dir.create(path, showWarnings = FALSE, recursive = TRUE)

  #Save cookie
    cat(path, file = path_file_storing_groundhog_library_location)
    
  #Assign it to the live environment
    Sys.setenv(GROUNDHOG_FOLDER = path)
 
   #Show confirmation message
    message2("\nGroundhog folder set to: '",path)
    
  #If path is the default, offer to change
    if (dirname(path)== fw(paste0(path.expand("~"), "/R_groundhog"))) {  #fw: function #50
      #Offer to change
       message2("\n--- You may change it with`set.groundhog.folder(<path>)`---\n")
    } else {
      #Otherwise just add a \n
        message("") #Otherwise just 
    }

  #Load cran toc rds
    load.cran.toc(TRUE)       
    
    
  } #End of set.groundhog.folder
