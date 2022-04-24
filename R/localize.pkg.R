#Takes a snowball and it copies all packages from the groundhog library to the default one
#
#This function was created to allows `foreach` to run based on a version-controlled package
#It is necessary because each of the parallel 'workers' looks for the foreachc pkg in the default library
#it is executed in groundhog.library.single(), in #10.3, after verifying the snowball has been 
#successfully installed.

  localize.pkg <- function(pkg_vrs)
  {
  #1 split name
    pkg <- get.pkg(pkg_vrs)
    vrs <- get.vrs(pkg_vrs)

  #2 Exit if local folder already has this version

    ip <- data.frame(installed.packages(lib.loc =.pkgenv[["orig_lib_paths"]][1] ))
    ip.pkg_vrs <- paste0(ip$Package,"_",ip$Version)

  
    if (pkg_vrs %in% ip.pkg_vrs) return(invisible(TRUE))
    
  #3 Exit if groundog.folder does not have the target pkg
      #Path to pkg in groundhog
        groundhog_path <- get.pkg_search_paths(pkg,vrs)
        pkg_path.groundhog <- paste0(groundhog_path,"/",pkg)
      
      #Does it exist?
        if (nrow(data.frame(installed.packages(lib=groundhog_path)))==0) {
          return(invisible(FALSE))
        }
        
  #4 Delete other version of package from local folder
      local_folder <- .pkgenv[["orig_lib_paths"]][1]
      unlink(file.path(local_folder,pkg),recursive=TRUE)

  #5 Copy the folder from groundhog folder
        file.copy(pkg_path.groundhog,        #copy contents of the "pkg_vrs/pkg" folder
                  local_folder,              #to the local library
                  recursive = TRUE)          #include all files
        
     
      message1("Copied  '",pkg_vrs, "' to local (non-groundhog) folder.")
    
  }
  
