#Takes a snowball and it copies all packages from the groundhog library to the default one
#This serves the following purposes


  localize.snowball <- function(snowball , localize.quietly = TRUE)
  {
    
    
    #1 Load installed packages
        ip <- data.frame(utils::installed.packages(lib.loc =.pkgenv[["orig_lib_paths"]][1] ), stringsAsFactors=FALSE)
        ip$pkg_vrs<-paste0(ip$Package,"_",ip$Version)
      
    
    #3 If entire snowball is not remote, assign sha='' to variables
      if (!'sha' %in% names(snowball)) snowball$sha=''
    
        
    #4 Start the loop over the snowball
      for (k in 1:nrow(snowball))
      {
        
    #5 Short varnames
      pkg     <- snowball$pkg[k]
      pkg_vrs <- snowball$pkg_vrs[k]
      installation.path <- snowball$installation.path[k]
      sha <- snowball$sha[k]
        
    #6 If localized already, skip  
      if (pkg_vrs %in% .pkgenv[['localized']]) next

    #7 If local folder already has this version and the pkg in snowball is not remote
      if (pkg_vrs  %in% ip$pkg_vrs & sha %in% c('', NA)) next
      
        
    #8 iF package does not exist in groundhog folder, error    
        if (nrow(data.frame(utils::installed.packages(lib=installation.path), stringsAsFactors=FALSE))==0) {
          message("groundhog says: failed to install '",pkg_vrs,"', localization failed (Error: localize.R #8 - try http://groundhogr.com/troubleshoot)")
          exit()
        }

    #9 all paths except the last one
         local_folder <- .pkgenv[["orig_lib_paths"]][-length(.pkgenv[["orig_lib_paths"]])]
      
    #10 If this one is installed, uninstall it 
        if (pkg %in% ip$Package) {
            old<- file.path(ip$LibPath[ip$Package==pkg] , pkg) 
            new <- paste0(old , "_PURGE")
            purged   <- file.rename(old , new)
            
            }
    #11  Copy the folder from groundhog folder
         #path to copy pkg from and to
            from_path <-paste0(installation.path,'/',pkg)  #groundhog_folder
            to_path <-  paste0(local_folder[1])    #local_folder
                               
        
        #Make to path if it does not exist (libpath with pkg specific folder)
        if (!file.exists(to_path)) {
          dir.create(to_path,recursive=TRUE)
          }
        
        copy.outcome <- file.copy(from_path ,        #copy contents of the "pkg_vrs/pkg" folder
                        to_path,           #to the local library listed first
                        recursive = TRUE)          #include all files
        
        if (copy.outcome==FALSE) {
          message("groundhog says: failed to copy '", pkg_vrs,"' to default personal library")
        }
        
    #12 Show feedback 
      if (localize.quietly==FALSE & copy.outcome==TRUE) message1("  copying: ",pkg_vrs)
     
    #13 Add to localized vector
        .pkgenv[['localized']]<-c(.pkgenv[['localized']], pkg_vrs)
  }

    
  } #End localize function
  
  
  
  