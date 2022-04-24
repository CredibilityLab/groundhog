# Check snowball conflict
# param ignore.deps optional character vector containing dependencies which 
#   may mismatch those implied by the entered date and be tolerated. This will
#   prevent the installation to stop and request restarting the R session for 
#   specified dependencies.



check.snowball.conflict <- function(snowball, force.install, ignore.deps, date) {
  
  
  #0 Message used a few times below
      msg_F10 <- "You may remove all loaded packages restarting the R session.\nIn R Studio press: CTRL/CMD-SHIFT-F10"
  
  
  #1 short name for package being installed/loaded
        requested_pkg_vrs <- snowball$pkg_vrs[length(snowball$pkg_vrs)]
        requested_pkg     <- snowball$pkg[length(snowball$pkg_vrs)]
  
  #2 Get sets of packages that are treated differently 
      #2.0 Verify ignore deps 
        
        #2.1 Ignore conflicts (cannot bypass default set, hardcoded in utils.R, 'ignore.deps_default()', but can add to it)
        ignore.deps <- c(ignore.deps_default(), ignore.deps)    #add any packages explicitly set by user
      
       
      #2.2 Active packages
        active <- get.active()
  

  #3 Force Install: is any package that needs to be *installed* is loaded?
    #separate check from because even SAME version created conflict
      if (force.install) {
        conflict.pkg <- (snowball$pkg %in% active$pkg) 
        if (any(conflict.pkg)) {
          message2()
          message(
            "You selected 'force.install=TRUE' but the following packages that would be installed\n",
            "are currently loaded:", paste0(snowball$pkg[conflict.pkg], collapse = ",  "),
            "\n\nYou need to restart your R session to carry out the installation.\n",
            "(in R Studio press: CTRL/CMD-SHIFT-F10 to do so)"
          
          )
          exit()
        } # End conflict found for forced install
      } # End check force install

  
  #4 Create conflict set 
    #These are packages that are needed and have a conflict with an active one
      conflict.needed     <- snowball$pkg_vrs[!(snowball$pkg_vrs %in% active$pkg_vrs) & (snowball$pkg %in% active$pkg) & (!snowball$pkg %in% ignore.deps)]
      conflict.needed.pkg <- snowball$pkg    [!(snowball$pkg_vrs %in% active$pkg_vrs) & (snowball$pkg %in% active$pkg) & (!snowball$pkg %in% ignore.deps)]
      conflict.needed <- sort(conflict.needed)
      
      
    #These are packages that are active and have a conflict with a needed one (do not include packages in ignore.deps)
      conflict.active     <- active$pkg_vrs[!(active$pkg_vrs %in% snowball$pkg_vrs) & (active$pkg %in% snowball$pkg) & (!active$pkg %in% ignore.deps)]
      conflict.active.pkg <- active$pkg    [!(active$pkg_vrs %in% snowball$pkg_vrs) & (active$pkg %in% snowball$pkg) & (!active$pkg %in% ignore.deps)]
      conflict.active     <- sort(conflict.active)

  #5 Generate variables with counts and list of packages in conflict
      n.conflict <- length(conflict.needed)
      n.needed   <- nrow(snowball)

  #6 Paste the package(s), which are vectors, into a string
      conflict.needed.string <- paste(conflict.needed, collapse = ",  ") # put a , between packages
      conflict.active.string <- paste(conflict.active, collapse = ",  ")


  #If conflict found
      if (n.conflict > 0 ) {
      
    #6.5 Read from and save to conflict cookie to detect repeated failures      
      #Read last conflict cookie
          cookie_path <- paste0(get.groundhog.folder(),"/package_conflict.txt")
          last_conflict <- 5  #assume 5 minutes
          last_conflict.pkg <- "" #empty last conflict pkg
           if (file.exists(cookie_path)) {
              last_conflict <- difftime(Sys.time(),file.info(cookie_path)$mtime,units='mins')
              last_conflict.pkg <- scan(cookie_path, what='character',quiet=TRUE)
              }
      #Save file with name of package being attempted 
          write(requested_pkg , cookie_path)
          
   #7 Show general message
         message2()
         message1(n.conflict, " of the ", n.needed, " packages needed for '", requested_pkg_vrs, "' have a version conflict with\n",
                  "a package already in your current R session.\n\n",
              "   Current: ",conflict.active.string,"\n",
              "   Needed:  ",conflict.needed.string,"\n\n")
			  
	#7.1 Add notice about remotes already loaded
        #Find remotes in the snowball
           snowball.remotes <- subset(snowball, snowball$from %in% c('github','gitlab'))
      
        #See if any of the conflicts involve them    
            flag.conflict_needed_remote <- any(conflict.active.pkg %in% snowball.remotes$pkg)
            
        #See if any of the conflicts involve already loaded remotes
            flag.conflict_active_remote <- any(conflict.active.pkg %in% .pkgenv[['remote_packages']])
    
           
      #msg for conflict with needed remote
       if (flag.conflict_active_remote==TRUE)
        {
            message1("Because the conflict involves packages not on CRAN, you may want to just allow the version conflict.\n",
				             "To do that: rerun groundhog.library() with the option: ",
                    "'ignore.deps=c(" ,  
                    paste0(conflict.active.pkg[conflict.active.pkg %in% .pkgenv[['remote_packages']]],collapse=', '),
                ")' \n\n")
           }
			  #msg for conflict with active remote
      
          if (flag.conflict_needed_remote==TRUE)
          {
            message1("Because the conflict involves packages not on CRAN, you may want to just allow the version conflict.\n",
				             "To do that: rerun groundhog.library() with the option: ",
                      "ignore.deps=c(" ,  
                    paste0(dQuote(conflict.needed.pkg[conflict.needed.pkg %in% snowball.remotes$pkg]),collapse=', '),
                ") \n\n")
          }

            
      #Add 'alternatively' before F10 if a msg about ignore.deps was shown before
        if (flag.conflict_needed_remote | flag.conflict_active_remote) message1("Alternatively, if you think you can avoid the conflict:")
     
         
	#7.3  Refresh F10
	      message1(msg_F10)
         
  #7.4 Add message if dates mismatch across groundhog library calls
         if (length(.pkgenv[['hogdays']])>1) {
            message("\n\nWarning: you have used different groundhog days (dates) across groundhog.library() calls, \n",
                    "this may be causing the conflict of versions. If you again load packages using different dates\n",
					"the problem may persist.") 
            message("\nDates you have used: ",paste0(.pkgenv[['hogdays']],collapse=' , '))
          }
       
	 #7.5 red failure message  
         message("\nThe package '", requested_pkg_vrs,"' was *NOT* attached")
       
    
  #8 Show fix, workaround message if same failure less than 5 minutes ago and not loading the conflicting package adn only 1 date was used and no package conflict is with remote
        if (last_conflict<5 & 
					requested_pkg == last_conflict.pkg & 
          flag.conflict_needed_remote==FALSE &  #Don't suggest disabling if remote conflict
          flag.conflict_active_remote==FALSE &  #Same concept, in one case the loaded is remote in the other the needed one
					(length(.pkgenv[['hogdays']])<2)) 
            
        {
          #Example of dependency with conflict to avoid
             dep.example <- ifelse(conflict.active.pkg[1] == requested_pkg, conflict.active.pkg[2], conflict.active.pkg[1] )

          #Assess if any packages in conflict are always locally available suggesting came from local library
                original_lib_path <- .pkgenv[["orig_lib_paths"]]
                installed.packages_current <- data.frame(utils::installed.packages(noCache = FALSE, lib.loc = original_lib_path), stringsAsFactors = FALSE)
                installed.pkg_vrs <- paste0(installed.packages_current$Package,"_",installed.packages_current$Version)
                remove.set <- (installed.pkg_vrs %in% conflict.active)
                
        # If at least one of them can be removed
                if (sum(remove.set)>0 )  {
                   message1("It's likely the packages creating the conflicts are being loaded automatically.\n",
                          "\n-->  A simple solution: run  `disable.local()`\n",
                				  "\n  This will disable all packages in your local (non-groundhog) library.\n"  ,
                				  "  If at any time you want to go back to using those packages (e.g., using `library()`),\n",
                				  "  you just run `enable.local()` and all packages will instantaneously be available again.\n\n",
                				  "  For other solutions see https://groundohgr.com/conflicts")
                  message("\nAgain, the simple solution is `disable.local()`")
                }
                      
           

             

     } #End if last_conflict<5
  #9 Return
     invisible(list(packages.needed=conflict.needed, packages.active=conflict.active))
     exit()
  }# End if some conflict found
} # End function
