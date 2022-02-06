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

            
      #Add 'alterantively' before F10 if a msg about ignore.deps was shown before
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
          flag.conflict_needed_remote==FALSE &  #Don't sugggest uninstalling if remote conflict
          flag.conflict_active_remote==FALSE &  #Same concept, in one case the loaded is remote in the other
					(length(.pkgenv[['hogdays']])==1)) 
            
        {
          #Example of dependency with conflict to avoid
             dep.example <- ifelse(conflict.active.pkg[1] == requested_pkg, conflict.active.pkg[2], conflict.active.pkg[1] )

          #Assess if any packages in conflict are alway locally available suggesting came from local library
                original_lib_path <- .pkgenv[["orig_lib_paths"]]
                install.packages_current <- data.frame(utils::installed.packages(noCache = FALSE, lib.loc = original_lib_path), stringsAsFactors = FALSE)
                installed.pkg_vrs <- paste0(install.packages_current$Package,"_",install.packages_current$Version)
                remove.set <- (installed.pkg_vrs %in% conflict.active)
                
               
               
                
          #General introduction to the problem
            msg.repeat.conflict.header <-paste0(
                  "\n\n",
                  "YOU RECENTLY GOT THIS MESSAGE, SO A BIT MORE INFO:\n",
                  "If you get this message even after restarting the R session, the packages(s) generating\n",
                  "the conflict are being called on automatically. The most common scenario is R Studio\n",
                  "loading the packages behind the scenes before you run the groundhog.library() command.\n",
                  "For instance, R Studio often loads packages referred elsewhere in a script with the\n",
                  "<pkg>:: operator (e.g., load 'dplyr' if it finds dplyr::filter() ).\n",
                  "R Studio also loads packages when it works on .rmd files (e.g., 'knitr' & 'xfun).\n\n",
                  "Less common scenarios involve packages you loaded earlier loading a dependency \n",
                  "without properly documenting it, having in your environment objects that require \n",
                  "those packages, and modifications to the .rprofile file, which load some packages \n",
                  "by default when you start R.\n\n",
                  "OK. That's the problem. Here are your possible *solution(s)* :"
                  )
            
            
              
          #1. New order
            msg.repeat.conflict.explicit.first <- paste0(
                  #Option 1 
                  "- Solution for the less common case of improperly documented packages: Use groundhog to \n",
                  "  explicitly load the conflicting dependency first e.g., run:\n", 
                  "  groundhog.library('",dep.example, "','" , date , "')  before groundhog.library('", requested_pkg , "','" , date , "').\n"
                  )
                    
          #2. Ignore
                  msg.repeat.conflict.ignore <- paste0(
                  "- Workaround for all cases (this compromises version control): Tell groundhog to ignore this\n",
                  "  particular conflict and continue loading '",requested_pkg, "', by using the 'ignore.deps' argument.\n",
                  "  For example: groundhog.library('" ,requested_pkg , "','" , date , "', ignore.deps='",dep.example,"')\n"
                  )
            
          #3. Uninstall
                  msg.repeat.conflict.uninstall <- paste0(
                  "- Solution for all cases: Uninstall the conflicting package(s) from your non-groundhog library,\n",
                  "  ensuring they can't be loaded by R or R Studio. Groundhog can do this easily for you, just type \n",
                  "  'uninstall' below. All conflicting package(s), and all packages that depend on them, if any, \n",
                  "  will be uninstalled from your non-groundhog library.\n",
                  "  You can undo this action and reinstall them at any time, even months from today, by running\n",
                  "  'groundhog::reinstall.conflicts()'.\n",
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
                      
                  #While loop for readline to avoid submitted code to be interpreted as the answer
        						text <-''
        						j <- 1 #counter of times message is shown
        						while (text!="uninstall" & text !="keep")  
        						{
        						text <- readline(prompt = "To unistall conflicting packages type 'uninstall', to keep them type 'keep' >")
        						text <- strip.prompt(text)
        						if (text !="uninstall" & text != "keep") {
        						message(j , ") You answered: '", text , "'") 
        						message("   To ensure you are actively answering, only 'uninstall' and 'keep' are accepted as responses\n")
        						j<-j+1 #Add to counter of msgs rejected
        						} #end if
                    } #End while
									
					
					#If they typed uninstall, do it
                  if (text=="uninstall") {
                    remove.conflict(conflict.active)
                  } 
                      
           #if they typed keep
                  if (text == "keep") {
					          message("You typed 'keep', will NOT uninstall conflicts.")
					        }
					  
						
						exit()
               }
        }
      
           
  #9 Warning about ignored conflicts, if all conflicts were ignored
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
                      "to restart the R session, but the package(s) involved:\n",
                      "(" , conflict.ignored.pkg , ")", " are in the set for which conflicts are tolerated either because\n",
                      "they are often loaded automatically (e.g., by R Studio) from your local R library, and/or are \n",
                      "'recommended' packages by R and thus version control can be attained by using the version of R matching\n",
                      "the date you entered, or you previously loaded those a version of those packages from a remote\n",
                      "repository (e.g. github).\n\n",
                      "You may want to try a session restart (in R Studio: SHIFT-CTRL-F10) to remove the conflict and \n",
                      "enable loading the desired version but the problem may persist.\n",
                      "At that point you may either tolerate imperfect version control or resolve the issue by changing how \n",
                      "you are running the script. For example, if 'knitting', you could  bypass R Studio automatically loading\n",
                      "packages by writing the script as an .R file and using rmarkdown::render() instead of the point-and-click\n",
                      "'knit' button solution offered by R Studio. While this issue is most common when using R Studio for .rmd files, \n",
                      "it may arise in other scenarios."
                      )
              }
             
  #9 Return
     invisible(list(packages.needed=conflict.needed, packages.active=conflict.active))
     exit()
  } # End if some conflict found
} # End function
