# Check snowball conflict
# param ignore.deps optional character vector containing dependencies which 
#   may mismatch those implied by the entered date and be tolerated. This will
#   prevent the installation to stop and request restarting the R session for 
#   specified dependencies.




check.snowball.conflict <- function(snowball, force.install, ignore.deps, date) {
  #1 Get sets of packages that are treated differently 
      #1.1 Ignore conflicts (cannot bypass default set, hardcoded in utils.R, 'ignore.deps_default()', but can add to it)
        ignore.deps <- c(ignore.deps_default(), ignore.deps)    #add any packages explicitly set by user
      
      #1.2 Active packages
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
         message1(n.conflict, " of the ", n.needed, " packages needed for attaching '", requested_pkg_vrs, "' are currently loaded,",
               " but not with the version that is needed.\n",
              "Loaded: ",conflict.active.string,"\n",
              "Needed: ",conflict.needed.string,"\n\n",
              "To solve this: restart the R session. Note: you will need to do 'library(groundhog)' again.\n\n",
              "In R Studio press: CTRL/CMD-SHIFT-F10")
         message("The package '", requested_pkg_vrs,"' was *NOT* attached")
       
    
  #8 Show fix, workaround message if same failure less than 5 minutes ago and not loading the conflicting package
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
            msg.repeat.conflict.header <-paste0(
                  "\n\n",
                  "YOU RECENTLY GOT THIS MESSAGE, SO A BIT MORE INFO:\n",
                  "If you get this message even after restarting the R session, the packages(s) generating\n",
                  "the conflict are being reloaded automatically. The most common scenario is R Studio \n",
                  "loading the packages behind the scenes before you run the groundhog.library() command \n",
                  "(e.g., it loads 'knitr' & packages referenced via :: when opening an .rmd file). \n",
                  "Less common scenarios involve packages you loaded earlier loading a dependency \n",
                  "without properly documenting it, having in your environment objects that require \n",
                  "those packages, and modifications to the .rprofile file, which load some packages \n",
                  "by deafult when you start R.\n\n",
                  "OK. That's the problem. Here are your possible *solution(s)*:"
                  )
            
          #1. New order
            msg.repeat.conflict.explicit.first <- paste0(
                  #Option 1 
                  "- Solution for the less common cases. Use groundhog to explicitly load the conflicting dependency first,\n",
                  "  e.g., run groundhog.library('",dep.example, "','" , date , "')  before groundhog.library('", requested_pkg , "',' " , date , "').\n"
                  )
                    
          #2. Ignore
                  msg.repeat.conflict.ignore <- paste0(
                  "- Workaround for all cases (this compromises version control): Tell groundhog to ignore this\n",
                  "  particular conflict and continue loading the '",requested_pkg, "', by using the \n",
                  "  'ignore.deps' argument. For example:\n",
                  "  groundhog.library('" ,requested_pkg , "','" , date , "', ignore.deps='",dep.example,"')\n"
                  )
            
          #3. Uninstall
                  msg.repeat.conflict.uninstall <- paste0(
                  "- Solution for all cases: Uninstall the conflicting package(s) from your non-groundhog library,\n",
                  "  ensuring they can't be loaded by R or R Studio. Groundhog can do this easily for you, just type \n",
                  "  'uninstall' below. All conflicting package(s), and all packages that depend on them, if any, \n",
                  "  will be uninstalled from your non-groundhog library.\n",
                  "  You can undo this action and reinstall them at any time, even months from today, by running\n",
                  "  'groundhog::reinstall.conflict()'.\n",
                  "  But if you use groundhog to manage your packages, you shouldn't need to.\n\n"
                  )
            
                  
          #Print options
          #0) If at least one of them applies, show header
                if ((!requested_pkg %in% conflict.active.pkg)  | (sum(remove.set)>0) )  {
                  message(msg.repeat.conflict.header)
                }
                  
                  
          #1) Unless the wanted package is the conflict, suggest putting it first or ignoring deps
                  if (!requested_pkg %in% conflict.active.pkg) {
                      message(msg.repeat.conflict.explicit.first)
                      message(msg.repeat.conflict.ignore)
                      }
                  
          #2) If the package is removable, suggest removing it 
                  if (sum(remove.set)>0)  
                    {
                      message(msg.repeat.conflict.uninstall)
                      text.answer <-readline(prompt = "To unistall conflicting packages type 'uninstall'. Type anything else to turn this solution down >")
                      if (tolower(text.answer)=="uninstall") remove.conflict(conflict.active)
                  }
                  exit()
              }
      
         
  #9 Warning about ignored conflicts, if all conflictrs were ignored
       #9.1 Find Packages with a conflict that is being ignored
            conflict.ignored    <- snowball$pkg_vrs[!(snowball$pkg_vrs %in% active$pkg_vrs) & (snowball$pkg %in% active$pkg) & (snowball$pkg  %in% ignore.deps_default())]
            conflict.ignored.pkg <- snowball$pkg    [!(snowball$pkg_vrs %in% active$pkg_vrs) & (snowball$pkg %in% active$pkg) & (snowball$pkg %in% ignore.deps_default())]
          
       #9.2 If any, describe them
            
            if (length(conflict.ignored) == length(conflict.active))
              {
              message("warning")
              message2("groundhog.says: version mismatch")
              message1(
                      "A different version of at least one of the needed packages is already loaded, \n",
                      "this usually would terminate the groundhog.library() call and you would be asked\n",
                      "to restart the R Session, but the package(s) involved:\n",
                      "(" , conflict.ignored.pkg , ")", " are in the set for which conflicts are tolerated either becuase\n",
                      "they are often loaded automatically (e.g., by R Studio) from your local R library, and/or are \n",
                      "'recommended' packages by R and thus version control can be attained by using the version of R matching\n",
                      "the date you entered.\n\n",
                      "You may want to try a session restart (in R Studio: SHIFT-CTRL-F10) to ensure version control\n",
                      "but if the problem may persist and you will just need to use your current version of those packages.\n",
                      "This issue is most common when using R Studio for .rmd files, but arises in a few other scenarios."
                      )
              }
             
  #9 Return
     invisible(list(packages.needed=conflict.needed, packages.active=conflict.active))
     exit()
  } # End if some conflict found
} # End function
