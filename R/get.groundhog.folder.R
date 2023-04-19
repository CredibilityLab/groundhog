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
  
  if (missing(path)) gstop("You forgot to enter the <path> you wanted to set.")
  
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
            answer.dropbox<-infinite.prompt(format.msg(msg),valid_answers=c('anyway','cancel'),must.restart=FALSE)

            if (tolower(answer.dropbox)=='cancel') {
              message1("OK. Request cancelled.")
              exit()
            }
            } #End if dropbox

    # #1.2 TWO DRIVES
        if  (get.drive(path) != get.drive(.libPaths()[1])) {

          #Draft message
              msg<- paste0("The path '",path,
                      " seems to be a on a different drive (or 'volume') than ",
                      "the default R library '",.libPaths(),"'. Groundhog will be slower and ",
                      "more likely to produce occasional errors if paths for libraries are ",
                      "on different drives. It is strongly recommended to use the ",
                      "same physical drive for both paths instead. ",
                      "If you want to set it to  '",path,"' anyway, type 'anyway', else type 'cancel'")


           #Show it
            answer.two_drives<-infinite.prompt(format.msg(msg),valid_answers=c('anyway','cancel'),must.restart=FALSE)

            if (tolower(answer.two_drives)=='cancel') {
              message1("OK. Request cancelled.")
              exit()
            }
            

        } #End if two drives


      
  #############################################################################

  #Set main folder with 'cookie files' and default for library
    main_folder <-  paste0(path.expand("~"), "/R_groundhog/")
    
  #Create main folder 
    dir.create(main_folder, showWarnings = FALSE, recursive = TRUE)
    
  #Path to file saving groundhog folder
    path_file_storing_groundhog_library_location <- paste0(main_folder ,"current_groundhog_folder.txt")
    
   #See if cookie existed
      cookie_existed <- file.exists(path_file_storing_groundhog_library_location)
    
  #Save the cookie
    path <- trimws(path)
    
  
  #Create groundhog_folder
    dir.create(path, showWarnings = FALSE, recursive = TRUE)

  #Save cookie
    cat(path, file = path_file_storing_groundhog_library_location)
    
  #Verify we can save there
    path1 <- file.path(path,"testing ability to save.txt")
    write('testing ability to save',path1)
    saved.succesfully <- file.exists(path1) 
    unlink(path1)
    
    if (!saved.succesfully) {
      gstop(paste0("groundhog says: Unable to save to '",path,"'. Make sure you are allowed to save files to that directory."))
    } 
    
  #Assign it to the live environment
    Sys.setenv(GROUNDHOG_FOLDER = path)
 
  #Load cran toc rds
    .pkgenv[['cran.toc']] <- NULL  #set it to null so that load.cran.toc() will not early return
    load.cran.toc() #this will copy the rds files
    
  #Show confirmation message
    message1("The folder where groundhog packages will be saved is:\n",path)
    
  #Reminder they can change it if it is the default path
    if (paste0(fw(dirname(path)),"/")==fw(main_folder)) message1("\nYou can change the location at any time running  `set.groundhog.folder(<path>)`")
    #fw() utils.R #50
    
           
  
           
        } #End of set.groundhog.folder
