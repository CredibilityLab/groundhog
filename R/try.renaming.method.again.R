#' Attempt faster method of copying packages across libraries in the future
#' 
#' groundhog often moves packages between the groundhog library and the default personal library.
#' This is done by renaming the package folder, which is a nearly instantaneous process even for 
#' 100+ packages. This renaming method, however, is sometimes unavailable for some configurations 
#' (e.g., when the groundhog and personal folders are on different drives/volumes, say external vs internal hard drives). When
#' groundhog determines this is the case, a warning is issued and groundhog switches to a slower process where packages are  
#' copied and deleted. If you do not believe you are relying on to different drives/volumes with your present configuration, 
#' you can try again the fast method going forward by running `try.renaming.method.again()`. Future groundhog.library() calls 
#' will again attempt it. If it fails again you will just get a new error message and groundhog will again switch methods. 
#' It is safe to err on the side of trying again, so
#' unless you know you are using multiple physical drives, you probably should try again.
#' 
#'@param quiet logical, defaults to `FALSE`. When set to `TRUE` it does not display confirmation message.
#'
#' @export

try.renaming.method.again<-function(quiet=FALSE)
  {
  cookies_dir <- paste0(get.groundhog.folder(),"/cookies")
  cookie_path <- file.path(cookies_dir, paste0("copy_instead_of_renaming.csv"))
  if (file.exists(cookie_path)) {
    unlink(cookie_path)
    if (quiet==FALSE) message1("OK. Will attempt the renaming method again")
  } else {
    if (quiet==FALSE) message1("Groundhog was already relying on the renaming method.")
    }
  
  
}
