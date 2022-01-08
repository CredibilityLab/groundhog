
    validate.pkg_vrs <- function(pkg, vrs, date, ignore.deps)
    { 
    

    #0. Don't try to load 'groundhog'
        if ("groundhog" == pkg) {
          message("Error. May not use groundhog.library() to load groundhog.\n",
                  "To load the version of groundhog available on '", date, "', please use:\n",
                  "meta.groundhog('" ,  date , "')"
                   )
          exit()
        } #End if groundhog is a package being called

  
    #1.0 initial check,  stop if same version
      active=get.active()
      
  
    #1.1 Get version of requested package
        #vrs <- get.version(pkg, date)
        pkg_vrs <- paste0(pkg, "_", vrs)
      

    #1.2 Stop if  pkg_vrs already attached
        attached.list= utils::sessionInfo()$otherPkgs
        attached.pkg <- names(attached.list)
        attached.vrs <- lapply(attached.list, function(x) x$Version)
        
      #Add base packages  
        attached.base.pkg <- utils::sessionInfo()$basePkgs
        attached.base.vrs <- as.character(sapply(attached.base.pkg, get.version, date)) 
        attached.pkg <- c(attached.pkg, attached.base.pkg)
        attached.vrs <- c(attached.vrs, attached.base.vrs)
        attached.pkg_vrs <- paste0(attached.pkg , "_" , attached.vrs)
        
        if (pkg_vrs %in% attached.pkg_vrs) {
            message1("groundhog says: the package you requested ('", pkg_vrs, "') is already attached.")
            exit()
            
        }

  
        
    #1.2.5 Attach if package is loaded but not attached
        if (pkg_vrs %in% active$pkg_vrs)
        {
          attachNamespace(pkg)
          message1("groundhog says: the package you requested ('", pkg, "_", vrs, "') was loaded, now it is also attached")
          exit()

        }
   
   #1.3 Mismatched package already attached  
         if ((pkg %in% attached.pkg) &  (!pkg_vrs %in% attached.pkg_vrs)) {
            message1(
                    "groundhog says: another version of '", pkg,"' is already attached ('", active$pkg_vrs[active$pkg==pkg],"').\n",
                    "To solve this: restart the R session. Note: you will need to do 'library(groundhog)' again.\n\n",
                    "In R Studio press: CTRL/CMD-SHIFT-F10"
                    )
          message("\nThe package '", pkg_vrs,"' was *NOT* attached")
          exit()
         }
        
    
    #1.5 Attach mismatched version if ignore.deps is loaded but not attached (common scenario, trying to attach knitr in .rmd file)
       if ((pkg %in% active$pkg) & (!pkg_vrs %in%  active$pkg_vrs) & (pkg %in% ignore.deps))
        {
         #Recommended
             ip <- data.frame(utils::installed.packages(),stringsAsFactors = FALSE)
             recommended.pkgs <- unique(subset(ip, ip$Priority=="recommended")$Package) #unique because there may be two versions of the same package in different libraries
         
          attachNamespace(pkg)
          message1("groundhog says: succesfully attached '" , pkg , "'")
          message("\ngroundhog warning:\n", 
                   "'", pkg, "' was already loaded, and it is now attached,\n",
                  "BUT the loaded version ('" , active$pkg_vrs[active$pkg==pkg] , "') does not match the version for ",
                   "'" , date, "' ('", pkg_vrs , "').\n",
                  "To attach the desired version you can try restarting the R session.\n\n",
                  "In R Studio press: CTRL/CMD-SHIFT-F10\n\n",
                  "Note that it is possible that this package is being loaded automatically from your local\n",
                  "library. In this case, after restarting the session the problem will persist.\n",
                  "You can ignore this problem and tolerate lack of version control for the involved packages.\n", 
				  "You can also prevent this problem by uninstalling the package from your non-groundhog library\n",
				  "running: remove.packages('", pkg ,"'), but if R Studio is using the package (e.g., the\n",
				  "'knit' button requires 'knitr' outside of groundhog), you may need to rely on R rather than\n",
				  "R Studio for that disabled functionality."
				 )
		
          
        #Explain the recommended issue if appropriate
          if (pkg %in% recommended.pkgs) {
              message(
                    "The package in question '", pkg , "', is a 'recommended' package which makes removing it from the\n",
                    "local (non-groundhog) library potentially more problematic; this is why this conflict\n",
                    "is tolerated by groundhog, producing a warning rather than an error."
                    )
            }
          exit()

        }   
        
        
    } #End function