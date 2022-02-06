
    
  
  validate.pkg_vrs <- function(pkg, vrs, date, ignore.deps)
  { 
  
  #0 Merge pkg_vrs
    pkg_vrs <- paste0(pkg, "_", vrs)
  
    
  #1 No 'groundhog'
        if ("groundhog" == pkg) {
        message("groundhog says: Error. You may not use groundhog.library() to load groundhog.\n",
                "To load the version of groundhog available on '", date, "', please use:\n",
                "meta.groundhog('" ,  date , "')"
                 )
        exit()
        } #End if groundhogd

    
     
  #2 Read Active  
    #2.1 Read active
       active <- get.active()
       
       
    #2.2 Read attached
        attached.list= utils::sessionInfo()$otherPkgs
        attached.pkg <- names(attached.list)
        attached.vrs <- lapply(attached.list, function(x) x$Version)
        
      #Add base packages  
        attached.base.pkg <- utils::sessionInfo()$basePkgs
        attached.base.vrs <- as.character(sapply(attached.base.pkg, get.version, date)) 
        attached.pkg <- c(attached.pkg, attached.base.pkg)
        attached.vrs <- c(attached.vrs, attached.base.vrs)
        attached.pkg_vrs <- paste0(attached.pkg , "_" , attached.vrs)
        
  #3 Early return if already attached
            if (pkg_vrs %in% attached.pkg_vrs) {
                  message1("groundhog says: the package you requested ('", pkg_vrs, "') is already attached.")
                  return('already_attached')  
            }
                
              #Early return so that groundhog.library.single() knows to stop processing this pkg, 
              #but if more where submitted in pkg=c(pkg1,pkg2) it will just move on to the next package
              #instead of ending the entire groundhog.library() call
      
  #4 CONFLICT
             
   #4.1 Mismatched package already attached  
         if ((pkg %in% attached.pkg) &  (!pkg_vrs %in% attached.pkg_vrs)) {
            message1(
                    "Another version of '", pkg,"' is already attached ('", active$pkg_vrs[active$pkg==pkg],"').\n",
                    "To solve this: restart the R session. Note: you will need to do 'library(groundhog)' again.\n\n",
                    "In R Studio press: CTRL/CMD-SHIFT-F10"
                    )
           
           
        #Add message if dates mismatch across groundhog library calls
         if (length(.pkgenv[['hogdays']])>1) {
            message("groundhog says: Warning! You have used different groundhog days (dates) across groundhog.library() calls, \n",
                    "this may be causing the conflict of versions.") 
            message("\nDates you have used: ",paste0(.pkgenv[['hogdays']],collapse=' , '))
            message("Check your script and use a single groundhog day across all calls.")
          }
           
           
          message("\nThe package '", pkg_vrs,"' was *NOT* attached")
          exit()
         }
        
    
    #4.2 Attach mismatched version if ignore.deps is loaded but not attached (common scenario, trying to attach knitr in .rmd file)
       if ((pkg %in% active$pkg) &                         #pkg matches
           (!pkg_vrs %in%  active$pkg_vrs) & 
           (pkg %in% ignore.deps))                          #in ignoreable conflict list.
        {
           
        #Attach it 
		     base.library(pkg) #utils.R function 27

             
        #Make local variable with name of pkg_vrs already loaded oralready in libpath creating conflict
             #available.pkg_vrs <- ifelse(pkg  %in% active$pkg, active$pkg_vrs[active$pkg==pkg] , paths.pkg_vrs[paths.pkg==pkg])
             
        #Message  
          message1("groundhog says: succesfully attached '" , pkg , "'")
          message("\ngroundhog says: Warning!\n", 
                   "'", pkg, "' was already loaded and it is now attached,\n",
                  "BUT the version previously available and just attached ('" ,active$pkg_vrs , "')\n",
                  "does not match the version for '" , date, "' ('", pkg_vrs , "').\n",
                  "To attach the desired version you can try restarting the R session.\n\n",
                  "In R Studio press: CTRL/CMD-SHIFT-F10\n\n",
                  "Note that it is possible that this package is being loaded automatically from your local\n",
                  "library (e.g., by R Studio). In this case, after restarting the session the problem will persist.\n",
                  "You can ignore this problem and tolerate lack of version control for the involved packages.\n", 
        				  "You can also prevent this problem by uninstalling the package from your non-groundhog library\n",
        				  "running: remove.packages('", pkg ,"'), but if R Studio is using the package (e.g., the\n",
        				  "'knit' button requires 'knitr' outside of groundhog), you may need to rely on R rather than\n",
        				  "R Studio for that disabled functionality."
        				 )
		
          
        #Explain the recommended pkgs to highlight this may be trickier 
             ip <- data.frame(utils::installed.packages(),stringsAsFactors = FALSE)
             recommended.pkgs <- unique(subset(ip, ip$Priority=="recommended")$Package) 
             
               #Note:unique because there may be two versions of the same package in different libraries
    
            if (pkg %in% recommended.pkgs) {
              message(
                    "The package in question '", pkg , "', is a 'recommended' package which makes removing it from the\n",
                    "local (non-groundhog) library potentially more problematic; this is why this conflict\n",
                    "is tolerated by groundhog, producing a warning rather than an error."
                    )
            }
          exit()

        }   
        return('')
        
    } #End function