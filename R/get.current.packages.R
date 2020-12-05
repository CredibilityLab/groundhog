get.current.packages <- function(type) {
  
  #path to local copy of available packages
    main_folder <-  paste0(path.expand("~"), "/R_groundhog/")
    ap_file_path <- paste0(main_folder , "available_packages_" , type , "_" , get.rversion() , "_" , Sys.Date() , ".rds")

  #Delete  available.packages files older than 1 hour
    cookie_files <- list.files(main_folder)
    for (filek in cookie_files)
      {
      filek_path <- paste0(main_folder , filek)             #full path to file
      pos.ap_file <- regexpr('available_packages', filek)   #>0 if file contains the words available_packages
      age <- as.numeric(difftime(Sys.time() , file.mtime(filek_path),units='mins'))
      if (pos.ap_file>0 & age>60) unlink(filek_path)        #Delete available_package files older than 60 minutes
    }
    
	#If file with today's and current R version, use it, bypassing slow available.packages() command
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
