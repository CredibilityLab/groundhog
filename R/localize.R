#Takes a snowball and it copies all packages from the groundhog library to the default one
#This serves the following purposes
#  1) Avoid conflicts between packages loaded automatically without needing to disable folders
#  2) Have pkgs available if run on background (e.g., with parallel processing)


  localize.pkg <- function(pkg_vrs,localize.quietly=FALSE,ip=NULL)
  {
  #0 Early return if already localized 
    if (pkg_vrs %in% .pkgenv[['localized']]) return(invisible(TRUE))
    
  #1 split name
    pkg <- get.pkg(pkg_vrs)
    vrs <- get.vrs(pkg_vrs)

  #2 Early return if local folder already has this version

    if (is.null(ip)) ip <- data.frame(utils::installed.packages(lib.loc =.pkgenv[["orig_lib_paths"]][1] ), stringsAsFactors=FALSE)
    ip.pkg_vrs <- paste0(ip$Package,"_",ip$Version)
    if (pkg_vrs %in% ip.pkg_vrs) return(invisible(TRUE))
    
  #3 Early return if groundhog.folder does not have the target pkg
      #Path to pkg in groundhog
        groundhog_path <- get.pkg_search_paths(pkg,vrs)
        pkg_path.groundhog <- paste0(groundhog_path,"/",pkg)
      
      #Does it exist?
        if (nrow(data.frame(utils::installed.packages(lib=groundhog_path), stringsAsFactors=FALSE))==0) {
          return(invisible(FALSE))
        }
        
  #4 Remove other version of package from local folder
       # Set of packages installed in personal library
          #packages_df <- get.packages_df() #utils.R #Function 33
    
        
        #all paths except the last one
          local_folder <- .pkgenv[["orig_lib_paths"]][-length(.pkgenv[["orig_lib_paths"]])]
      
      #If this one is installed, uninstall it 
        if (pkg %in% ip$Package) {
            old<- file.path(ip$LibPath[ip$Package==pkg] , pkg) 
            new <- paste0(old , "_PURGE")
            purged   <- file.rename(old , new)
            
            }
            
        
  #5 Copy the folder from groundhog folder
        file.copy(pkg_path.groundhog,        #copy contents of the "pkg_vrs/pkg" folder
                  local_folder,              #to the local library
                  recursive = TRUE)          #include all files
        
     
      if (localize.quietly==FALSE) message1("  copying: ",pkg_vrs)
     
  #6 Add to localized vector
        .pkgenv[['localized']]<-c(.pkgenv[['localized']], pkg_vrs)
  }
  
#---------------------------
  
#Function that loops localizing all files in a snowball
  localize.snowball <- function(snowball, localize.quietly=TRUE)
    {
    ip <- data.frame(utils::installed.packages(lib.loc =.pkgenv[["orig_lib_paths"]][1] ), stringsAsFactors=FALSE)

    for (pkg_vrs.k in snowball$pkg_vrs)
    {
    localize.pkg(pkg_vrs.k, localize.quietly=localize.quietly,ip=ip)
    
    }
  }
    
    
    
