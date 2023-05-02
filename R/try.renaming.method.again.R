#' Attempt faster method of copying packages across libraries in the future
#' 
#' Groundhog often moves packages between the groundhog library and the default personal library.
#' This is done by renaming the package folders ('renaming' the parent directory of a folder, effectively moves it). 
#' This process is nearly instantaneous  even for 100+ packages. The renaming method, however, is sometimes unavailable for some configurations 
#' (e.g., when the groundhog and personal folders are on different drives/volumes, say external vs internal hard drives).  When
#' groundhog fails to move a package by renaming it, it will produce an error, and will also make a note to permanently switch to 
#' the slower method of moving packages by first coping them, and then deleting the original, which takes up to a few second per package, and
#' is thus much slower than renaming. If you believe the error was circumstantial and want to give 
#' renaming files another chance, you may run `try.renaming.method.again()`. Future groundhog.library() calls 
#' will again attempt it. If it fails again you will just get a new error message and groundhog will again switch methods. 
#' It is safe to err on the side of trying again, so unless you know you are using multiple physical drives, you probably should try again, and
#' investigate a possible alternative source of the problem. To debug, you may choose `cores=1` to force sequential installation, 
#' `force.install=TRUE` to reinstall possibly poorly installed dependencies, and as always, inspecting the console log as packages get installed.
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
