#' Get current local path to groundhog folder
#'
#' @return the path to the groundhog folder, the meta-library where 
#'   [groundhog.library()] downloads and stores packages that can be loaded 
#'
#' @note you can change the location of this folder with the command
#'   `set.groundhog.folder("path")`. You may also set environment variable
#'   `GROUNDHOG_FOLDER`. If neither is set and `~/R_groundhog` is not writable,
#'   groundhog falls back to `./.groundhog`.
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

# Internal helpers for folder resolution.
.groundhog.main.folder <- function() {
  fw(paste0(path.expand("~"), "/R_groundhog"))
}

.groundhog.cookie.path <- function() {
  paste0(.groundhog.main.folder(), "/current_groundhog_folder.txt")
}

.groundhog.default.folder <- function() {
  paste0(.groundhog.main.folder(), "/groundhog_library/")
}

.groundhog.workspace.fallback.folder <- function() {
  fw(file.path(getwd(), ".groundhog"))
}

.groundhog.env.folder <- function() {
  path <- trimws(Sys.getenv("GROUNDHOG_FOLDER", unset = ""))
  if (path == "") return("")
  fw(path)
}

.groundhog.path.writable <- function(path) {
  path <- fw(path)
  ok <- tryCatch({
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    probe <- file.path(path, paste0(".groundhog_write_probe_", Sys.getpid(), ".tmp"))
    on.exit(if (file.exists(probe)) unlink(probe), add = TRUE)
    writeLines("ok", probe)
    file.exists(probe)
  }, error = function(e) FALSE, warning = function(w) FALSE)
  isTRUE(ok)
}

# Function that gets the groundhog folder, with a sandbox-safe fallback.
get.groundhog.folder <- function() {
  # 1) Explicit override (best for CI/sandboxed runs)
  env.folder <- .groundhog.env.folder()
  if (env.folder != "") {
    if (.groundhog.path.writable(env.folder)) {
      return(env.folder)
    }
    message1("GROUNDHOG_FOLDER was set to '", env.folder, "' but is not writable. Trying fallback locations.")
  }

  # 2) Normal behavior for standard R users (~ + cookie/default)
  consent <- check.consent()
  if (consent == FALSE) {
    message(" -- not authorized to save to folder necessary for groundhog to work --")
    exit()
  }

  main_folder <- .groundhog.main.folder()
  if (.groundhog.path.writable(main_folder)) {
    cookie_path <- .groundhog.cookie.path()

    if (file.exists(cookie_path)) {
      f <- cookie_path
      groundhog.folder <- trimws(readChar(f, file.info(f)$size))
      if (groundhog.folder != "" && .groundhog.path.writable(groundhog.folder)) {
        return(fw(groundhog.folder))
      }
    }

    groundhog.folder <- .groundhog.default.folder()
    dir.create(groundhog.folder, showWarnings = FALSE, recursive = TRUE)
    cat(groundhog.folder, file = cookie_path)
    Sys.setenv(GROUNDHOG_FOLDER = groundhog.folder)
    return(fw(groundhog.folder))
  }

  # 3) Automatic fallback for sandboxed/non-writable home folders
  fallback <- .groundhog.workspace.fallback.folder()
  if (.groundhog.path.writable(fallback)) {
    Sys.setenv(GROUNDHOG_FOLDER = fallback)
    if (is.null(.pkgenv[["fallback_folder_noted"]])) {
      message1("Home groundhog folder is not writable; using fallback folder: '", fallback, "'")
      .pkgenv[["fallback_folder_noted"]] <- TRUE
    }
    return(fallback)
  }

  gstop("Groundhog could not find any writable folder. Set GROUNDHOG_FOLDER to a writable path.")
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
      
    #Note: do dir.exists() first because we cannot normalizePath() if it does not exist
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
    main_folder <-  paste0(.groundhog.main.folder(), "/")
    
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
    if (dirname(path)== .groundhog.main.folder()) {  #fw: function #50
      #Offer to change
       message2("\n--- You may change it with`set.groundhog.folder(<path>)`---\n")
    } else {
      #Otherwise just add a \n
        message("") #Otherwise just 
    }

  #Load cran toc rds
    load.cran.toc(TRUE)       
    
    
  } #End of set.groundhog.folder
