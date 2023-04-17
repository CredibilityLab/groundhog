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
      return(FALSE)
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
  
  #Warning if path includes dropbox (users can over-rule it by rerunning it within 10 minutes)
    if (get.minutes.since.cookie('dropbox_path')>10)
      {
      if  (regexpr('dropbox', tolower(path))>0) {
        save.cookie('dropbox_path')
        msg=paste0("The path '",path,"' seems to be a Dropbox folder. Groundhog will be slower and ",
            "more likely to produce ocassional errors if the library is on Dropbox; ",
            "consider using a different path. If you want to set it to '",path,"' anyway, ",
            "please run `set.groundhog.folder('",path,"')` within 10 minutes.")
        message(format.msg(msg))
        exit()
        
      }
    }

  
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
    cat(path, file = path_file_storing_groundhog_library_location)
  
  #Create groundhog_folder
    dir.create(path, showWarnings = FALSE, recursive = TRUE)

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
    
  #Verify we can rename files between here and .libPaths()
        #Find path where groundhog is installed
          ip.local     <- get.ip('local')
          ip.groundhog <- ip.local[ip.local$Package=="groundhog",]
          
        #Make a text file there to test with
          path2        <- paste0(ip.groundhog$LibPath,"/groundhog/testing ability to rename.txt")
          write('testing ability to rename',path2)
    
        #Set path in groundhog folder to rename that file to
          path3        <- paste0(path, "/testing ability to rename.txt")
   

        #Rename it, delete file,  and evaluate success
           outcome      <- file.rename(path2,path3)
           
        if (outcome==FALSE) {
              msg=paste0("Groundhog is unable to quickly move files between the directory ",
                      "where packages are installed by R '",.libPaths()[1], "' and the selected ",
                      "groundhog folder '", path, "'. This usually occur when these ",
                      "two directories are on different volumes (e.g., USB drive vs hard ",
                      "drive or external drive). Groundhog will still work, but it will be ",
                      "slower than it would otherwise be, as it will have to move packages (copy & delete)",
                      "instead of renaming them.")
              
              message(format.msg(msg))
              save.cookie("copy_instead_of_renaming")
              
            #Delete file in old location
              unlink(path2)
    
              } #End if it failed
           
          #If success, only delete the file in new location
          if (outcome==TRUE) {
           # unlink(path3)
          }
           
        } #End of set.groundhog.folder
