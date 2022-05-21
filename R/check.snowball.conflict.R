# Check snowball conflict
# param ignore.deps optional character vector containing dependencies which 
#   may mismatch those implied by the entered date and be tolerated. This will
#   prevent the installation to stop and request restarting the R session for 
#   specified dependencies.


check.snowball.conflict <- function(snowball, force.install, ignore.deps, date) {
  
  
  #0 Message used a few times below
      msg_F10 <- "Unload conflicting packages restarting the R session.\nIn R Studio press: CTRL/CMD-SHIFT-F10"
  
  #1 short name for package being installed/loaded
        requested_pkg_vrs <- snowball$pkg_vrs[length(snowball$pkg_vrs)]
        requested_pkg     <- snowball$pkg[length(snowball$pkg_vrs)]
  
  #2 Get sets of packages that are treated differently 
      
      #2.1 Ignore conflicts (cannot bypass default set, hardcoded in utils.R, 'ignore.deps_default()', but can add to it)
        ignore.deps <- c(ignore.deps_default(), ignore.deps)    #add any packages explicitly set by user
      
            #Default became c() with v2.0.0
        
      #2.2 Active packages
        active <- get.active()
  

  #3 Force Install: is any package that needs to be *installed*  loaded?
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
    #4.1 These are packages that are needed and have a conflict with an active one
      conflict.needed     <- snowball$pkg_vrs[!(snowball$pkg_vrs %in% active$pkg_vrs) & (snowball$pkg %in% active$pkg) & (!snowball$pkg %in% ignore.deps)]
      conflict.needed.pkg <- snowball$pkg    [!(snowball$pkg_vrs %in% active$pkg_vrs) & (snowball$pkg %in% active$pkg) & (!snowball$pkg %in% ignore.deps)]
      conflict.needed <- sort(conflict.needed)
      
      
    #4.2 These are packages that are active and have a conflict with a needed one (do not include packages in ignore.deps)
      conflict.active     <- active$pkg_vrs[!(active$pkg_vrs %in% snowball$pkg_vrs) & (active$pkg %in% snowball$pkg) & (!active$pkg %in% ignore.deps)]
      conflict.active.pkg <- active$pkg    [!(active$pkg_vrs %in% snowball$pkg_vrs) & (active$pkg %in% snowball$pkg) & (!active$pkg %in% ignore.deps)]
      conflict.active     <- sort(conflict.active)

    #4.3 Indicator: TRUE/FALSE conflicting package was loaded with groundhog
      not.groundhog.conflicts <- !conflict.active.pkg %in% .pkgenv[['groundhog_loaded_pkgs']] 
                  
                #note:  .pkgenv[['groundhog_loaded_pkgs']] is updated in install.snowball()
  
      
    #4.4 If there is a conflict, but caused only by  groundhog,  ask for a restart
        if (length(conflict.needed)>0 & length(not.groundhog.conflicts)==0)
         {
          
          # Add message if dates mismatch across groundhog library calls and the conflicts involved 
            if (length(.pkgenv[['hogdays']])>1) {
              message("You have used different groundhog days (dates) across groundhog.library() calls, \n",
                      "this may be causing the conflict of versions.") 
              message("\nDates you have used: ",paste0(.pkgenv[['hogdays']],collapse=' , '),"\n\n")
            }
          
          msg <- paste0( "A conflicting version of a needed package was already loaded with groundhog\n",
                         "To proceed, restart the R Session  (in R Studio CMD/CTRL-SHFT-F10).\n\n",
                         "Type OK to confirm you read this message.")
      
         answer <- infinite.prompt(msg,'ok')   #utils.R function 33
         message("        ---   Restart the R Session, then rerun `library('groundhog')`  ---  ")  
         exit()
         }
          
    #4.5 Only keep conflicts not caused by groundhog-loaded packages
      conflict.needed     <- conflict.needed    [not.groundhog.conflicts]
      conflict.needed.pkg <- conflict.needed.pkg[not.groundhog.conflicts]
      conflict.active     <- conflict.active    [not.groundhog.conflicts]
      conflict.active.pkg <- conflict.active.pkg[not.groundhog.conflicts]

      
  #5 Generate variables with counts and list of packages in conflict
      n.conflict <- length(conflict.needed)
      n.needed   <- nrow(snowball)

  #6 Paste the package(s), which are vectors, into a string
      conflict.needed.string <- pasteQC(conflict.needed) #utils.R function 31
      conflict.active.string <- pasteQC(conflict.active)
      conflict.needed.string_first3 <- pasteQC(conflict.needed[1:3]) #utils.R function 31
      conflict.active.string_first3 <- pasteQC(conflict.active[1:3])


  #7 If conflict found
      if (n.conflict > 0 ) {
             
  #8 Show general message
         msg <- paste0("|PROBLEM.\n",
                "|    Groundhog says:\n",
                "|    ", n.conflict, " of the ", n.needed, " packages needed for '",requested_pkg_vrs, "' have a version conflict\n",
                "|    with packages already in your current R session.")
			  
	 #9 Add notice about remotes already loaded
      #9.1 Find remotes in the snowball
           snowball.remotes <- subset(snowball, snowball$from %in% c('github','gitlab'))
      
      #9.2 See if any of the conflicts involve them    
            flag.conflict_needed_remote <- any(conflict.active.pkg %in% snowball.remotes$pkg)
            
      #9.3 See if any of the conflicts involve already loaded remotes
            flag.conflict_active_remote <- any(conflict.active.pkg %in% .pkgenv[['remote_packages']])
    
               
      #9.4 msg for conflict with NEEDED remote
           if (flag.conflict_active_remote==TRUE)
            {
                msg <- paste0(msg, "\n",
                         "|   Because the conflict involves packages not on CRAN, you may want to just allow the version conflict.\n",
    				             "|   To do that: rerun groundhog.library() with the option: ",
                        "'ignore.deps=c(" ,  
                        paste0(conflict.active.pkg[conflict.active.pkg %in% .pkgenv[['remote_packages']]],collapse=', '),
                    ")' \n\n")
               }
    	 #9.5 msg for conflict with ACTIVE remote
          
              if (flag.conflict_needed_remote==TRUE)
              {
                msg <- paste0(msg, "\n",
                         "|   Because the conflict involves packages not on CRAN, you may want to just allow the version conflict.\n",
    				             "|   To do that: rerun groundhog.library() with the option: ",
                          "ignore.deps=c(" ,  
                        paste0(dQuote(conflict.needed.pkg[conflict.needed.pkg %in% snowball.remotes$pkg]),collapse=', '),
                    ") \n\n")
              }

            
      #11.5 Add 'alternatively' before F10 if a msg about ignore.deps was shown before
        if (flag.conflict_needed_remote | flag.conflict_active_remote) {
          msg <- paste0(msg, "\n",
                 "|   Alternatively, if you think you can avoid the conflict:")
             }
     
         
            
    #12  Prompt to disable all, or conflict ones only, 
            msg <- paste0(msg, "\n|\n",
                                  "|    The simplest solution to prevent this & future conflicts, is to disable\n",
                                  "|    all existing packages. You may quickly re-enable them at any time by\n",
                                  "|    running `enable.packages()`.\n\n",
                                  "|    Your options:\n",
                                  "|    1) Type 'disable.packages()' to disable existing packages.\n",
                                  "|    2) type 'x' to stop")
            
            answer <- infinite.prompt(msg, c('disable.packages()','x'))
            
            
     #13 Disable & Localize
          #13.1 Disable
            if (answer=='disable.packages()')  {
                
                #13.1 disable                
                  disable.packages(disable.quietly = FALSE, skip.prompt=TRUE)

                #13.2 MSG : will install snowball
                  message1("Verifying all needed packages are installed.")
                  
                #13.3 Install snowball
                 # groundhog.install(snowball)  #install full snowball, populated from the target pkg 
                  install.snowball(snowball,date=date,install.only = TRUE, skip.remotes=TRUE)
                  
                #13.3 Drop any remotes from the snowball
                    #snowball <- snowball[!snowball$from %in% c('github','gitlab') ,]
                  
                #13.3 Localize the snowball
                  message1("Copying needed packages from groundhog to the local library.")
                  localize.snowball(snowball, localize.quietly=TRUE)
            
                         
          #13.4 Ask them to restart the R Session
                  txt<-paste0("|IMPORTANT:\n",
                              "|   Groundhog says: to complete this process you must restart the R Session.\n",
                              "|   In R Studio: CMD/CTRL-SHFT-F10")
                  
                  infinite.prompt(txt,"stop")
                  exit()
              } #End If agreed to disable something
            
      #14 Chose not to disable
            if (answer=='none')  {
                message("You answered 'x', so no packages were disabled.\n",
                       "For alterantive solutions consider the `ignore.deps` argument.\n",
                       "For more information visit https://groundhogR.com/conflicts")
                   exit()
                  }

	 
     invisible(list(packages.needed=conflict.needed, packages.active=conflict.active))
     exit()
  }# End if some conflict found
} # End function

