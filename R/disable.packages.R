#' Disable packages previously installed without groundhog to avoid conflicts when loading via groundhog
#' 
#' To avoid version conflicts between the version of the package `groundhog` is trying
#' to load, and the version R Studio loads automatically from the local library, you can
#' disable all packages in the default local library. This action is fully and instantaneously 
#' reversible, but if you rely on groundhog to load packages you may not need to ever reverse it.
#' To re-enable all packages simply  run `enable.packages()`
#' 

#'@examples
#' \dontrun{
#' disable.packages()
#' }
#' @details
#' Packages installed with `install.packages()` are saved in directories with the name of the package.
#' When you run `disable.packages()` the directories of the specified packages (or all directories if none
#' is specified) are renamed, adding the string `_DISABLED`. 
#' This prevents R Studio from loading those packages #' automatically. The packages are still 
#' listed when running `installed.packages()` but they are not #' actually available to be 
#' used (e.g., #' using `library()` will fail to load them. When the #' command 
#' `enable.packages()` is run, all #' folders are renamed back, eliminated the term `_DISABLED`
#' and they are again loadable by R Studio and by users with the `library()` command.
#' @seealso [enable.packages()]
#' 
#' @export


#Function 1 - Disable
    disable.packages <- function(disable.quietly=FALSE) {
    
      #1 Get packages in all paths except last, which is the base R path
         local_library <-   .pkgenv[['default_libpath']][ -length(.pkgenv[['default_libpath']])]
         

      #2 Set of packages
          pkg_current   <- list.files(local_library)
          pkg <- gsub("_DISABLED", "", pkg_current)
          path  <- list.files(local_library,full.names=TRUE)
          disabled <- regexpr('_DISABLED', pkg_current) >0
          all_df <-data.frame(pkg, pkg_current, path,disabled)
          
      #3 Start with all packages except 'groundhog'
         packages_df <- all_df[all_df$pkg!="groundhog",]
        
      #3.5 if all packages are disabled end here
         if (sum(packages_df$disabled) == nrow(packages_df) && disable.quietly==FALSE) {
           message1("All ",nrow(packages_df)," user-installed packages are already disabled.")
           return(invisible(TRUE))
          }
           
     
     #4 Warnings
            
          #4.1 Groundhog is to be excluded
                  if ('groundhog' %in% packages && disable.quietly==FALSE) {
                    message("Warning: You requested 'groundhog' to be disabled. Request respectfully denied.")
                  }
                  
            } #End of #4
      
         
      #5 Avoid name collisions
         #5.1 See if a package to be disabled already has a disabled version
            disabled_has.enabled.version <- packages_df$disabled==TRUE & (packages_df$pkg %in% packages_df$pkg[packages_df$disabled==FALSE])
              
        #5.2 Delete the enabled version
              unlink(packages_df$path[disabled_has.enabled.version], recursive = TRUE)
        
        #5.3 Drop from database
             packages_df <- packages_df[!disabled_has.enabled.version,]
          
      #6 Keep only packages that still need to be disabled
          packages_df <- packages_df[packages_df$disabled==FALSE,]
         
      #6 If there are pkgs to be disabled
          disabled <- file.rename(packages_df$path , paste0(packages_df$path,"_DISABLED"))
          
      #7 Message with success
          n=sum(disabled)
          
          if (disable.quietly==FALSE)
          {
          if (n==1) message1("groundhog says: 1 package has been disabled")
          if (n!=1) message1("groundhog says: ",n," packages have been disabled")

          message("\n   -> You should restart your R session now (in R Studio: CTRL/CMD-SHIFT-F10).")
          message("                     Then run `library('groundhog')` again.")
          }
      #Early return  
          return(invisible(TRUE))
       
    } #End of disable.packages

  
    
#' Re-enable previously disabled packages in your local (non-groundhog) library
#' 
#' This function reverses the actions taken by `disable.packages()`
#' @seealso [disable.packages()]
#' 
#' @export

    
     enable.packages <- function(packages) {
    
      #1 Get packages in all paths except last, which is the base R path
         local_library <-   .pkgenv[['default_libpath']][1:(length(.pkgenv[['default_libpath']])-1)]
      
      #2 Set of packages
          pkg_current   <- list.files(local_library)
          pkg <- gsub("_DISABLED", "", pkg_current)
          path  <- list.files(local_library,full.names=TRUE)
          disabled <- regexpr('_DISABLED', pkg_current) >0
          all_df <-data.frame(pkg, pkg_current, path,disabled)
          
      #3 Start with all packages except 'groundhog'
         packages_df <- all_df[all_df$pkg!="groundhog",]
        
      #3.5 if all packages are disabled end here
         if (sum(packages_df$disabled) == 0)
          {
           message1("All ",nrow(packages_df)," user-installed packages are already enabled.")
           return(invisible(TRUE))
          }
           
         
    
  
     #5 if packages are both enabeld and disabled delete the enabled ones
         
         
              
     #5 Avoid name collisions
         #5.1 See if an already enabled  package  has a disabled version
            disabled_has.enabled.version <- packages_df$disabled==TRUE & (packages_df$pkg %in% packages_df$pkg[packages_df$disabled==FALSE])
              
        #5.2 Delete the enabled version
              unlink(packages_df$path[disabled_has.enabled.version], recursive = TRUE)
        
        #5.3 Drop from database
             packages_df <- packages_df[!disabled_has.enabled.version,]
          
      #6 Keep only packages that still need to be disabled
          packages_df <- packages_df[packages_df$disabled==FALSE,]
         
      #6 If there are pkgs to be disabled
          disabled <- file.rename(packages_df$path , paste0(packages_df$path,"_DISABLED"))
          
         
         
         
         
         
      #7 If there are pkgs to be enabled, go for it
          enabled <- file.rename(packages_df$path , 
                                 file.path(dirname(packages_df$path), packages_df$pkg))  #Drop _DISABLED from name
          
      #8 Message with success
          n=sum(enabled)
          if (n==1) message1("groundhog says: 1 package has been enabled")
          if (n!=1) message1("groundhog says: ",n," packages have been enabled")

      #Early return  
          return(invisible(TRUE))
       
    } #End of disable.packages


    