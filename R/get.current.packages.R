get.current.packages <- function(type) {
  
  #path to local copy of available packages
    home_path <- Sys.getenv("home")
    cookie_path <- paste0(home_path, "/R_groundhog/")
    ap_file_path <- paste0(cookie_path ,"available_packages_",get.rversion(),"_",Sys.Date(),".rds")

  
  #If file with today's date exist, use it
    if (file.exists(ap_file_path)) {
        current.packages <- readRDS(ap_file_path)
        } else {
  #Else get available packages to see if each attempted to install is new
      current.packages <- tryCatch({
          as.data.frame(
          available.packages(contriburl = contrib.url(repos = "https://cloud.r-project.org/", type = type))[, c(1, 2)],
          stringsAsFactors = FALSE
        )},
        error = function(e) NULL)
        if (is.null(current.packages)) {
        exit("Cannot install packages, connection to CRAN server failed. Perhaps you are offline?")
        }
        current.packages$pkg_vrs <- paste0(current.packages$Package, "_", current.packages$Version)
        saveRDS(current.packages, ap_file_path)
    } #End if file found
    
  return(current.packages)
   
  } #ENd function
