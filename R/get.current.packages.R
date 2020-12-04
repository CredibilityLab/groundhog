get.current.packages <- function(type) {
  
  #path to local copy of available packages
    main_folder <-  paste0(path.expand("~"), "/R_groundhog/")
    ap_file_path <- paste0(main_folder ,"available_packages_",get.rversion(),"_",Sys.Date(),".rds")

  #Delete outdated available.packages files
    cookie_files <- list.files(ap_file_path)
    for (filek in cookie_files)
      {
      filek_path <- paste0(cookie_path, filek)
      pos.ap_file <- regexpr('available_packages', filek)   #file is called available_packages
      pos.date    <- regexpr(Sys.Date(), filek)             #file has today's date
      if (pos.ap_file>0 & pos.date<0) unlink(filek_path) 
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
