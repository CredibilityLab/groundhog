#' Disable packages installed without groundhog, to avoid conflicts when loading via groundhog
#' 
#' To avoid version conflicts between the version of the package `groundhog` is trying
#' to load, and the version R Studio loads automatically from your default personal library, you can
#' disable all packages in the default personal library. This action is fully and instantaneously 
#' reversible, but if you rely on groundhog to load packages you may not need to ever reverse it.
#' To re-enable all packages simply  run `enable.packages()`
#' 
#'@param disable.quietly defaults to `FALSE`. If set to `TRUE`, feedback on console is not shown
#'@param skip.prompt defaults to `FALSE`. If set to `TRUE`, prompts asking for confirmation are skipped
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
    disable.packages <- function(disable.quietly=FALSE,skip.prompt=FALSE) {
    
       
      #1 Set of packages installed in personal library
          packages_df <- get.packages_df() #utils.R #Function 33
         
      #2 Count disabled
          n.disabled <-sum(packages_df$disabled)
          n.total    <- nrow(packages_df)
              
      #4 if all packages are disabled end here
         if (n.disabled == n.total  && disable.quietly==FALSE) {
           message1("All " , n.total , " in the non-groundhog personal library are disabled.")
           return(invisible(TRUE))
          }
           
      #4.5 Warning if there already are disabled packages
          if (n.disabled > 0)
          {
            txt<-paste0(
                "|DECISION:\n",
                "|   Groundhog says: You already had disabled some packages. \n",
                "|   While those packages can be re-enabled with `enable.packages()`,\n",
                "|   any additional packages you disable will be uninstalled\n",
                "|   (you can always re-install them). Type 'OK' to continue and\n",
                "|   uninstall the conflicting package(s), or 'x' to stop.")
            answer<-infinite.prompt(txt,c('ok','x'))
            if (answer=='x') return(invisible(FALSE))
          }
    
      #5 rename files
          enabled_df <- packages_df[packages_df$disabled==FALSE,]
          old <- enabled_df$path
              
        #5.1 If there already R disabled packages _PURGE the new ones
          purged = disabled = FALSE  #TF array with outcome
          
          if (n.disabled >0) 
            {
              new <- paste0(enabled_df$path,"_PURGE")
              purged   <- file.rename(old , new)
            }
          
        #5.2 If there are no packages disabled, disable all the new ones 
          if (n.disabled == 0) 
            {
              new <- paste0(enabled_df$path,"_DISABLED")
              disabled   <- file.rename(old , new)
            }
    
    
      #6 Message with outcome
          if (disable.quietly==FALSE)
          {
          n <- sum(disabled) + sum(purged)
          if (n==1) message1("groundhog says: 1 package has been disabled")
          if (n!=1) message1("groundhog says: ",n," packages have been disabled")

        #Infinite prompt
          if (skip.prompt==FALSE)
          {
            txt<-paste0("|IMPORTANT:\n",
                "|   Groundhog says: to complete this process you must restart the R Session.\n",
                "|   In R Studio: CMD/CTRL-SHFT-F10")
          
          infinite.prompt(txt,"stop")
          }#End if skip prompt==FALSE
          
         
          } #End if disable quietly
      #Early return  
          return(invisible(TRUE))
       
    } #End of disable.packages

  
    
#' Re-enable previously disabled packages in your non-groundhog library
#' 
#' This function reverses the actions taken by `disable.packages()`
#' @seealso [disable.packages()]
#' 
#' @export

    
     enable.packages <- function() {
    
      #1 Get packages in all paths except last, which is the base R path
        packages_df <- get.packages_df() #utils.R #Function 33

        
      #2 if no package is disabled, end here
         n.total <- nrow(packages_df)
         n.disabled <- sum(packages_df$disabled) 
         n.purged   <- sum(packages_df$purged) 
         if (n.disabled + n.purged== 0)
          {
           message1("All ",nrow(packages_df)," user-installed packages are already enabled.")
           return(invisible(TRUE))
          }
           
      #3 purge new packages
          enabled_df <- packages_df[packages_df$disabled==FALSE & packages_df$purged==FALSE,]
          
          if (nrow(enabled_df)>0) {
            old <- enabled_df$path
            new <- paste0(old,"_PURGE")
            purged <- file.rename(old,new)
          }
       
      #4 enable the disable 

          #4.1 Subset of packges_df with disabled ones
             disabled_df <- subset(packages_df, packages_df$disabled==TRUE)
    
          #4.2 Rename them all
             old <- disabled_df$path 
             new <- file.path(dirname(disabled_df$path), disabled_df$pkg)
             enabled <- file.rename(old, new)   
             
      #5 Message with success
            n <- sum(enabled)
            if (n==1) message1("1 package has been enabled")
            if (n!=1) message1(n," packages have been enabled")
  
            
            txt<-paste0("|IMPORTANT:\n",
                        "|   Groundhog says: to complete this process you must restart the R Session.\n",
                        "|   In R Studio: CMD/CTRL-SHFT-F10\n")
            infinite.prompt(txt,"stop")
          
            
               
      #Early return  
          return(invisible(TRUE))
       
    } #End of disable.packages


    