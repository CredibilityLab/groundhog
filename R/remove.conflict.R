
#This function removes all packages found in the local library that match the pkg_vrs it is fed, asking for "OK" for confirmation
  
  remove.conflict <- function(conflict.pkg_vrs)
      {
    
        #Currently installed packages
            original_lib_path <- show.orig_lib_paths()
            installed.packages_current <- data.frame(utils::installed.packages(noCache = TRUE, lib.loc=original_lib_path),stringsAsFactors = FALSE)
            installed.pkg_vrs <- paste0(installed.packages_current$Package,"_",installed.packages_current$Version)
            
        #Subset that has conflict
            remove.pkg <- as.character(installed.packages_current$Package [installed.pkg_vrs %in% conflict.pkg_vrs] )
            
        #Add reverse dependencies
            remove.revdep <- tools::dependsOnPkgs (remove.pkg, recursive=TRUE, lib.loc=original_lib_path)
            remove.pkg <- c(remove.pkg, remove.revdep)
        
        #Find their lib paths
            remove.lib <- installed.packages_current$LibPath [installed.packages_current$Package %in% remove.pkg] 
            
        #Make a record of deleted packages, with the date when it was deleted
            new.uninstall.conflicts <- data.frame(Package=remove.pkg, lib.loc=remove.lib, date  = as.numeric(Sys.time()),stringsAsFactors = FALSE)
      
        #Read and append existing and save
            #Start empty data.frame
              uninstalled.conflicts  <-data.frame(Package=c(), lib.loc=c())
              
            #Try reading existing one locally
              file_path <- paste0(get.groundhog.folder(), "/" , "uninstalled.conflicts.R-" , get.r.majmin() , ".rds")
              if (file.exists(file_path)) {
                #Read existing one
                  uninstalled.conflicts <- readRDS(file_path)
              }
            #Add new one
              uninstalled.conflicts <- rbind(uninstalled.conflicts, new.uninstall.conflicts)
              
            #Confimration
              message2()
              message1(
                    "Type OK to uninstall the following packages from your local non-groundhog library:",
                    paste0(new.uninstall.conflicts$Package,sep=" ")
                      )
              text.answer <-readline(prompt = "Type OK to proceed, anything else to stop >")
              if (tolower(text.answer)!="ok") {
                  message("You did not type OK, uninstall aborted.")
                  exit()
                  }
              
            #Save
              saveRDS(uninstalled.conflicts,file = file_path, version=2)
            
         #Remove them, if any
            if (length(remove.pkg)>0) {
              message2()
              message1(
                    "Uninstalling the following packages from your non-groundhog library:\n",
                    paste0(remove.pkg, sep=' '),
                    "\n\n",
                    "Recall that you can reinstall all packages uninstalled this way with: 'groundhog::reinstalled.conflicts()'\n\n",
                    "You will need to restart the R session for the uninstallation to take effect\n",
                    "In R Studio: CMD/CTRL-SHIFT-F10"
                    )
              utils::remove.packages(remove.pkg , remove.lib)
              }
  
        }
  
  
  
#' Re-install packages that created a conflict
#' 
#' Re-install uninstalled packages (from the non-groundhog local library) previously uninstalled to prevent 
#' conflict with installation via groundhog.library()
#'@param since (optional) character string  (yyyy-mm-dd), or date value, to specify that only packages uninstalled
#' since that date will be reinstalled. If not set then all packages ever uninstalled this way will be re-installed.
#' @details When installing packages with groundhog.library(), if another version of a needed package or dependency
#' is already loaded the process ends. This is usually solved by restarting the R session thus unloading the conflicting package. 
#' But on occasion the problem persists, often because R Studio loads the undesired version of the package automatically from
#' the local (non-groundhog) library. When this occurs, groundhog.library() will prompt users with a set of options, one of which is
#' to  uninstall these packages from the non-groundhog library.
#' The set of packages uninstalled in this way are recorded and saved to a data.frame, allowing easily undoing this action, 
#' even months later. Specifically, a user running the function `reinstalled.conflicts()` will loop over that data.frame, 
#' reinstalling all packages listed if they are not currently available (into the original path they were previously installed).
#' Again, these are not packages in the groundhog library, but rather, in the default library R uses to install packages.
#' IF the optiona parameter `since` is used, only packages uninstalled since that date will be reinstalled.
#' @note Because these packages are not managed by groundhog, there is no version control. The package that is re-installed
#' may be a newer version than that previously available (or the installation may fail if the package becomes archived).
#' @examples
#' \dontrun{
#' #Re-install all packages ever uninstalled due to conflict
#' reinstall.conflicts()
#' #Re-install all packages uninstalled since 2021-02-04 due to conflict
#' reinstall.conflicts("2021-02-04")
#' }
#'
#' @export

  
  
  #Re-install all packages in local library previously uninstalled with groundhog.
    reinstall.conflicts <- function(since="1970-01-01")
    {
      
      #Validate the date
          validate.date(since) #function in utils.r, exits() if not valid
      
      #Path to local libraries (obtained before temporarily modifying while groundhog runs in groundhog library)
        original_lib_path <- show.orig_lib_paths()

      #Path to file with documented uninstalled packages
        file_path <- paste0(get.groundhog.folder(), "/" , "uninstalled.conflicts.R-" , get.r.majmin() , ".rds")
        if (file.exists(file_path)) {
               uninstalled.conflicts <- readRDS(file_path)
              } else {
              message("groundhog says: There is no record of uninstalled packages that can be re-installed")
              exit()
              }
      
      #Select based on the date
        uninstalled.conflicts <- subset(uninstalled.conflicts, date > as.numeric(as.POSIXct(since)))

        
      #End if nothing to re-install
        if (!file.exists(file_path) || nrow(uninstalled.conflicts)==0) 
          {
          message2()
          message1("There is no record of package conflicts uninstalled with groundhog. No package will be reinstalled.")
          exit()
        }
        
        
      #Message
        n <- nrow(uninstalled.conflicts)
        message2()
        message1("Will reinstall '",n, "' previously uninstalled packages to the non-groundhog library")
        
      
      
        for (k in 1:n)
            {
            #Package being evaluated
              pkgk <- uninstalled.conflicts$Package[k]
              libk <- uninstalled.conflicts$lib.loc[k]
              
            #Message
              message1("groundhog says: reinstalling '" , pkgk , "'")
            #Updated installed packages
              installed.packages_current <- data.frame(utils::installed.packages(noCache = TRUE, lib.loc=original_lib_path),stringsAsFactors = FALSE)
            
            #If not there, install it
              if (!pkgk %in% installed.packages_current) 
                {
                utils::install.packages(pkgk,lib =libk)
                }
              
          }#End for
      
       
    #Update the file with uninstalled.conflicts dropping anything that was succesfully re-installed
            uninstalled.conflicts <- uninstalled.conflicts <- readRDS(file_path) #get full list of uninstalled packages (not by date)
            installed.packages_current <- data.frame(utils::installed.packages(noCache = TRUE, lib.loc=original_lib_path),stringsAsFactors = FALSE)           
            remain.uninstalled       <- uninstalled.conflicts[!uninstalled.conflicts$Package %in% installed.packages_current$Package,]
            saveRDS(remain.uninstalled, file_path, version=2)
      }
      
      
    
  
#' View uninstalled conflicting packages by groundhog 
#' 
#' List all packages that have been uninstalled with groundhog, by user, after creating a conflict with
#' package attempted to be loaded with groundhog.library(). These packages can be reinstalled into the local 
#' non-groundhog library with `reinstall.conflicts()`
#' 
#' @export

  see.unistalled.conflicts <- function()
    {
     #Path to file with documented uninstalled packages
        file_path <- paste0(get.groundhog.folder(), "/" , "uninstalled.conflicts.R-" , get.r.majmin() , ".rds")
        if (file.exists(file_path))  uninstalled.conflicts <- readRDS(file_path)
      
        
      #End if nothing to re-install
        if (!file.exists(file_path) || nrow(uninstalled.conflicts)==0) 
          {
          message2()
          message1("There is no record of package conflicts uninstalled with groundhog.")
          exit()
        }
        
      #Format date
        uninstalled.conflicts$date <- as.Date(as.POSIXct( uninstalled.conflicts$date, origin="1970-01-01"))
      #Return the packages
        return (uninstalled.conflicts)
   
    }