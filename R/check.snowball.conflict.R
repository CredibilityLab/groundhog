# Check snowball conflict
# param ignore.deps optional character vector containing dependencies which 
#   may mismatch those implied by the entered date and be tolerated. This will
#   prevent the installation to stop and request restarting the R session for 
#   specified dependencies.


check.snowball.conflict <- function(snowball, force.install, ignore.deps, date) {
  #1 Get sets of packages that are treated differently 
      
  #1.1. Ignore conflicts (cannot bypass default set, hardcoded in utils.R, 'ignore.deps_default', but can add to it)
          #ignore.deps=c(ignore.deps_default(), ignore.deps)  
      
      #1.2 Recommended 
            #Deal with potential conflicts with non explicitly acknowledged dependencies with recommended packages
          ip <- data.frame(utils::installed.packages())
          recommended.pkg <- ip[ip$Priority=="recommended",]$Package
    
      #1.3 Active packages
        active <- get.active()
    
  #2 short name for package being installed/loaded
        requested_pkg_vrs <- snowball$pkg_vrs[length(snowball$pkg_vrs)]
        requested_pkg     <- snowball$pkg[length(snowball$pkg_vrs)]

  #3 Force Install:  any package that needs to be *installed* is loaded?
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
    n.needed <- nrow(snowball)

  #6 Paste the package(s), which are vectors, into a string
    conflict.needed <- paste(conflict.needed, collapse = ",  ") # put a , between packages
    conflict.active <- paste(conflict.active, collapse = ",  ")


  #If conflict found
      if (conflict.needed != "") {
  
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
         message1(n.conflict, " of the ", n.needed, " packages needed by '", requested_pkg_vrs, "' are currently loaded,",
               " but not with the version that is needed.\n",
              "Loaded: ",conflict.active,"\n",
              "Needed: ",conflict.needed,"\n\n",
              "To solve this: restart the R session. Note: you will need to do 'library(groundhog)' again.\n\n",
              "In R Studio press: CTRL/CMD-SHIFT-F10")
         message("The package '", requested_pkg_vrs,"' was *NOT* attached")
       
    
  #8 Show fix, workaround message if same failure less than 5 minutes ago
        if (last_conflict<5 & requested_pkg == last_conflict.pkg)
        {
          #Example of dependency with conflict to avoid
             dep.example <- ifelse(conflict.active.pkg[1] == requested_pkg, conflict.active.pkg[2], conflict.active.pkg[1] )

          #Assess if any packages in conflict are alway locally available suggesting came from local library
                original_lib_path <- show.orig_lib_paths()
                install.packages_current <- data.frame(installed.packages(noCache = FALSE, lib.loc = original_lib_path))
                installed.pkg_vrs <- paste0(install.packages_current$Package,"_",install.packages_current$Version)
                remove.set <- (installed.pkg_vrs %in% conflict.active)    
    
               
                
          #General introduction to the problem
            msg<-message(
                  "\n\n",
                  "YOU RECENTLY GOT THIS MESSAGE, SO A BIT MORE INFO:\n",
                  "If you get this message even after restarting the R session, the package generating the conflict is\n",
                  "being reloaded automatically. The two most common scenarios are that R Studio loads the package \n",
                  "(e.g., it loads 'knitr' when opening an .rmd file) or that another package loads it without\n",
                  " documenting that it does.\n\n",
                  "That's a problem, here are your possible solutions:\n",
            
                  "Option 1. Solution for the latter case.\n",
                  "          Use groundhog to explicitly load the conflicting packages first,\n",
                  "           e.g., run groundhog.library('",dep.example, "','" , date , "')  before groundhog.library('", requested_pkg , "',' " , date , "').\n",
                  "\nOption 2. Workaround for both cases (compromises version control):\n",
                  "           Tell groundhog to ignore this particular conflict using the 'ignore.deps' argument\n",
                  "           e.g., groundhog.library('" ,requested_pkg , "','" , date , "', ignore.deps='",dep.example,"')"
                  )
            
            
          #If conflict is generated by a package in the local library, offer to uninstall it
                if (sum(remove.set)>0)  
                  {
                  message(
                        "\nOption 3. Solution for both cases:\n",
                        "          Uninstall the conflicting package(s) from your non-groundhog library, ensuring they can't be loaded\n",
                        "          unintentionally. Groundhog can do it automatically, just type 'uninstall' and all those packages\n",
                        "          will be uninstalled from your non-groundhog library. Note that you can undo this action at any \n",
                        "          time, even months from today, by running 'reinstall.conflict()'. But if you use groundhog to manage \n",
                        "          your packages, you will not need to do so.\n\n"
                        )
                       
                       
                      text.answer <-readline(prompt = "To choose Option 3, type 'uninstall', anything else to turn it down >")
                      if (tolower(text.answer)=="uninstall") remove.conflict(conflict.active)
                }
                exit()
            }
      
         
  #9 Warning about ignored conflicts
       #9.1 Find Packages with a conflict that is being ignored
            conflict.ignored    <- snowball$pkg_vrs[!(snowball$pkg_vrs %in% active$pkg_vrs) & (snowball$pkg %in% active$pkg) & (snowball$pkg  %in% ignore.deps_default())]
            conflict.ignored.pkg <- snowball$pkg    [!(snowball$pkg_vrs %in% active$pkg_vrs) & (snowball$pkg %in% active$pkg) & (snowball$pkg %in% ignore.deps_default())]
          
       #9.2 If any, describe them
            
            if (length(conflict.ignored)>0)
              {
              message("warning")
              message2("groundhog.says: version mismatch")
              message1(
                      "A different version of at least one of the needed packages is already loaded, \n",
                      "this usually would terminate the groundhog.library() call and you would be asked\n",
                      "to restart the R Session, but the package(s) involved:\n",
                      "(" , conflict.ignored.pkg , ")", " are often loaded automatically (e.g., by R Studio) \n",
                      "from your local R library so even with a session restart they would be loaded again.\n",
                      "You may want to try a session restart (in R Studio: SHIFT-CTRL-F10) \n",
                      "but the problem may persist and you will just need to use your current version of those packages.\n",
                      "This issue is most common when using R Studio for .rmd files, but arises in a few other scenarios."
                      )
              }
             
  #9 Return
     invisible(list(packages.needed=conflict.needed, packages.active=conflict.active))
     exit()
  } # End if some conflict found
} # End function
