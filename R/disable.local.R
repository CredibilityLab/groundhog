#' Avoid package version conflicts by temporarily disabling packages in the local (non-groundhog) library
#' 
#' To avoid version conflicts between the version of the package `groundhog` is trying
#' to load, and the version R Studio loads automatically from the local library, you can
#' disable all packages in the default local library. This action is fully and instantaneously 
#' reversible, but if you rely on groundhog to load packages you may not need to ever reverse it.
#' To re-enable all packages simply  run `enable.local()`
#' @usage 
#' disable.local()
#' @details
#' Packages are stored into folders inside the main folder consisting of the local folder.
#' You can find your default local library running `.libPaths()[1]`.
#' When you run `disable.local()` every folder inside the library is renamed, adding the string
#' `_DISABLED`. This prevents R Studio from loading those packages automatically. The packages are still
#' listed when running `installed.packages()` but they are not actually available to be used (e.g., 
#' using `library()` will fail to load them. When the command `enable.local()` is run, all folders are renamed back, eliminated the term `_DISABLED`
#' and they are again loadable by R Studio and by users with the `library()` command
#' @seealso [enable.local()]
#' @export

#Function 1 - Disable
    disable.local <- function() {
     
      # Find current default path
         local_library <- .libPaths()[1]
         
      #Get all package and file names
         all_packages <- list.files(local_library)
       
      #Drop those already  disabled 
         disabled <- regexpr('_DISABLED', all_packages) >0
         all_packages <- all_packages[!disabled]
         n <- length(all_packages) - 1
      
      #If there are pkgs to be disabled
        if (n>0)
          {
      
      #Drop groundhog
          all_but_groundhog <- all_packages [all_packages != "groundhog"]
                                                 
       #Disable them
          disabled <- file.rename(file.path(local_library, all_but_groundhog) , 
                            file.path(local_library, paste0(all_but_groundhog,"_DISABLED")))
          
           
      #7 Confirm it worked
            #Get all package and file names
              all_packages <- list.files(local_library)
          
          #flag those already  disabled 
            disabled <- regexpr('_DISABLED', all_packages) >0
            all_packages <- all_packages[!disabled]
            n.left <- length(all_packages) - 1
      
          if (n.left==0) message1("groundhog says: " , n, " packages in the local library, '",local_library,"', have been disabled")
          if (n.left>0)  message1("groundhog says: " , n-n.left, " (out of ",n," total) packages in the local library, '",local_library,"', have been disabled")
        
                
      #8 early return  
          return(invisible(local_library))
              
      } #End if more than 0 packages
              
              
      #9 If only 1 pkg, just message
             if (n ==0)
                {
                message1("groundhog says: all packages in the local library, '", local_library, "', are already disabled.")
                return(invisible(local_library))  
              } #End if only groundhog is left

      
              
    } #End of disable.local

    
    
#' Avoid package version conflicts by temporarily disabling packages in the local (non-groundhog) library
#' 
#' This function reverses the actions taken by `disable.local()`
#' @seealso [disable.local()]
#' 
#' @export


  enable.local <- function() {
    
    #1 Folder paths
      local_library <- .libPaths()[1]

    #2 Get all package 
       all_packages <- list.files(local_library)
       
    #3 Keep those disabled 
         disabled <- regexpr('_DISABLED', all_packages) >0
         disabled_packages <- all_packages[disabled]
         n <- length(disabled_packages)
      
    #4 If no disable packages exist, early return
        if (n==0) {
          message1("All packages in the local library '", local_library,"' are already enabled.")
          return(invisible(TRUE))  
        }
    
    #5 enable any disabled packages
       #5.1 Remake names without  _DISABLED in tjhem
            enabled_packages<- gsub("_DISABLED", "", disabled_packages)
        
       #5.2 Rename files
           enabled <- file.rename(file.path(local_library, disabled_packages) , 
                                  file.path(local_library, enabled_packages))
          n.enabled <- length(enabled)
     
              
    #3.5 success
         if (n.enabled>0)   message1(n," packages in the local library, '",local_library,"' were re-enabled.")
         if (n.enabled==0)  message ("Somethign went wrong. No package was enabled in the local library.",
                                     "\nVisit https://groundhogr.com/troubleshooting for help")

         return(invisible(TRUE))  

  }
  
  