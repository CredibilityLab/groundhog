#' Attempt faster method of copying packages across libraries in the future
#' 
#' groundhog often moves packages between the groundhog library and the default personal library.
#' This is done by renaming the package folder, which is a nearly instantaneous process even for 
#' 100+ packages. This renaming method, however, is sometimes unavailable for some configurations 
#' (e.g., when the groundhog and personal folders are on different drives/volumes, say external vs internal hard drives). When
#' groundhog determines this is the case, a cookie file tells groundhog to rely on the slower methods of 
#' moving the packages between libraries by copying-and-deleting. To over-rule this determination 
#' run `try.renaming.method.again()` and the next groundhog.library() call will again attempt it. If it fails again
#' the cookie file will be created anew, returning to copying-and-deleting.
#' 
#' @export

try.renaming.method.again<-function()
  {
  cookies_dir <- paste0(get.groundhog.folder(),"/cookies")
  cookie_path <- file.path(cookies_dir, paste0("copy_instead_of_renaming.csv"))
  if (file.exists(cookie_path)) {
    unlink(cookie_path)
    message1("OK. Will attempt the renaming method again")
  } else {
    message1("Groundhog was already relying on the renaming method.")
    }
  
  
}
