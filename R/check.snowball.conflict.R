# Check snowball conflict

#1  short name for package being installed/loaded
#2  Ignore conflicts 
#3  Active packages
#4  Conflict 1: force Install (is any package that needs to be *installed*  loaded)
#5  Conflict 2: Same remote, different date
#6  Obtain vector with all conflicting packages
#7  Conflict 3 - A conflict caused by two groundhog loaded pkgs
#8  Conflict 4 - pkg 1 on CRAN, pkg 2 is remotes, so even same date creates conflict
#9  Conflict 5 - pkg 1 on Remote, pkg 2 is CRAN, so even same date creates conflict
#10 Conflict 6 - Typical conflict: both packages are in CRAN, one was loaded before groundhog tried 

#-------------------------------


check.snowball.conflict <- function(snowball, force.install, ignore.deps, date) {

  #0 F10 message
    msg.f10 <- paste0("You may unload all packages by restarting the R session \n ",
                      "(in R Studio CMD/CTRL-SHFT-F10).")
    
   #1 short name for package being installed/loaded
        requested_pkg_vrs <- snowball$pkg_vrs[length(snowball$pkg_vrs)]
        requested_pkg     <- snowball$pkg[length(snowball$pkg_vrs)]
  
        ignore.deps <- c(ignore.deps_default(), ignore.deps)    #add any packages explicitly set by user
      
            #Default became c() with v2.0.0
        
   #3 Active packages
        active <- get.active()
  
  
   #4 Conflict 1: force Install (is any package that needs to be *installed*  loaded)
    
            if (force.install) {
              pkg.loaded_need_installing <- snowball$pkg[snowball$pkg %in% active$pkg]
                                                         
                                                         
              
              if (length(pkg.loaded_need_installing)>0) {
                msg <- paste0(
                          "groundhog say: you selected 'force.install=TRUE' but the following packages that would be installed ",
                          "are currently loaded: ", pasteQC(pkg.loaded_need_installing), 
                           msg.f10
                          )
            
            
                
                infinite.prompt(format.msg(msg),'ok')
                exit()
                
              } # End conflict found for forced install
            } # End check force install

        
  #5 Conflict 2: Same remote, different date
        
        pkg.conflict_remote_date <- snowball$pkg[ (snowball$pkg %in% .pkgenv[['remotes_df']]$pkg) & (!date %in%  .pkgenv[['remotes_df']]$date) ]
        
        
        if (length(pkg.conflict_remote_date)>0)
        {
          msg <- paste0(
                "groundhog says: ",
                "The following packages were previously loaded from a non-CRAN repository, ",
                "using a date other than '",date,"': ",pasteQC(pkg.conflict_remote_date),". ",
                "This creates a potential version conflict. To avoid it you may need to modify ",
                "your script ensuring the same date is used for every groundhog.library() call. ",
                msg.f10)
  
          infinite.prompt(format.msg(msg),"ok")
          exit()
        }
<<<<<<< Updated upstream
=======

  #4 Create conflict set 
    #4.1 These are packages that are needed and have a conflict with an active one
      conflict.needed     <- snowball$pkg_vrs[!(snowball$pkg_vrs %in% active$pkg_vrs) & (snowball$pkg %in% active$pkg) & (!snowball$pkg %in% ignore.deps)]
      conflict.needed.pkg <- snowball$pkg    [!(snowball$pkg_vrs %in% active$pkg_vrs) & (snowball$pkg %in% active$pkg) & (!snowball$pkg %in% ignore.deps)]
      conflict.needed <- sort(conflict.needed)
      
      
    #4.2 These are packages that are active and have a conflict with a needed one (do not include packages in ignore.deps)
      conflict.active     <- active$pkg_vrs[!(active$pkg_vrs %in% snowball$pkg_vrs) & (active$pkg %in% snowball$pkg) & (!active$pkg %in% ignore.deps)]
      conflict.active.pkg <- active$pkg    [!(active$pkg_vrs %in% snowball$pkg_vrs) & (active$pkg %in% snowball$pkg) & (!active$pkg %in% ignore.deps)]
      conflict.active     <- sort(conflict.active)
>>>>>>> Stashed changes

        
        
        
  #6 Obtain vector with all conflicting packages
      pkg.conflict  <- snowball$pkg    [!(snowball$pkg_vrs %in% active$pkg_vrs) & (snowball$pkg %in% active$pkg) & (!snowball$pkg %in% ignore.deps)]

      
  #7 Conflict 3 - A conflict caused by two groundhog loaded pkgs
      
<<<<<<< Updated upstream
        #Find any pkg conflict loaded with groundhog
          if (sum(pkg.conflict  %in%  .pkgenv[['groundhog_loaded_pkgs']] ) >0)
          {
        #Start saying there is a conflict
            msg <- paste0("groundhog says: another version of a needed package was previously loaded with groundhog.")
          
        #if has used different dates, indicate that.
            if (length(.pkgenv[['hogdays']])>1) {
              msg<-paste0(msg,
                          " Across groundhog calls you have used different dates (",
                          paste0(.pkgenv[['hogdays']],collapse=' , '),
                          " ) that is proably the root cause of this conflict. ")
                }
          
        #Request restart   
           msg<-paste0(msg, msg.f10)
      
         answer <- infinite.prompt(format.msg(msg),'ok')
         exit()
         }
          
 #8 Conflict 4 - pkg 1 on CRAN, pkg 2 is remotes, so even same date creates conflict
       
      #8.1 Find remotes in the snowball
           snowball.remotes <- subset(snowball, snowball$from %in% c('github','gitlab'))
      
      #8.2 The to-be-loaded pkg is remote
           
           pkg.conflit_new.remote <- pkg.conflict [pkg.conflict %in% snowball.remotes$pkg]
           
           if (length(pkg.conflit_new.remote >0))
             {
              msg <- paste0("groundhog says: the following package(s) that need to be loaded: ",pasteQC(pkg.conflit_new.remote),
                    ", were previously loaded from CRAN. This creates a version conflict that may be unavoidable. ",
                    "Whichever version you load first is the one you will use. To allow this conflict include the following ",
                   "option in your groundhg.library() call: 'ignore.deps=c(" , pasteQC(pkg.conflit_new.remote) , ")'")
           
              msg <-paste0(msg, msg.f10)  
              msg<-paste0(msg,"\nType 'OK' to confirm you have read this message.")
              infinite.prompt(format.msg(msg),'ok')
              exit()

             }
      
           
           
 #9 Conflict 5 - pkg 1 on Remote, pkg 2 is CRAN, so even same date creates conflict
       
      #9.1 The to-be-loaded conflicting pkg was loaded as remote
           
           pkg.conflit_old.remote <- pkg.conflict [pkg.conflict %in% .pkgenv[['remotes_df']]$pkg]
           
           if (length(pkg.conflit_old.remote >0))
             {
              msg <- paste0("groundhog says: the following needed CRAN package(s) , ",pasteQC(pkg.conflit_old.remote),
                    ", were previously loaded from a non-CRAN repository (e.g., github or gitlab).",
                    " This creates a version conflict that may be unavoidable, and you will use Whichever version of the package you load first. ",
                    "But you need to allow this conflict, which you can do adding this option in subsequent ",
                    "groundhog.library() calls: 'ignore.deps=c(" , pasteQC(pkg.conflit_old.remote) , ")' \n ",
                   "Press 'ok' to confirm you have read this message")

              answer<-infinite.prompt(format.msg(msg),'ok')
              exit()
             
           }       
           
           
    
   
 #10 Conflict 6 - Typical conflict: both packages are in CRAN, one was loaded before groundhog tried 
    
	
    #10.0 If no conflict, early return
      if (length(pkg.conflict)==0) return(invisible(TRUE))
           
    #10.1 Remove all conflicting packages from local library
      for (pkgk in pkg.conflict)
      {
        remove.packages(pkgk)
      }

    #10.2 Install the snowball
       install.snowball(snowball,date=date,install.only = TRUE, skip.remotes=TRUE)
       
    #10.3 Localize the snowball
       localize.snowball(snowball)   
         
    #10.4 Message that a restart is needed 
        txt<-paste0("Some of the packages needed to load '",requested_pkg_vrs,"' conflicted with packages ",
                    "already in your R session. The conflict has been resolved, but you will need to ",
                    "restart the R session and re-run groundhog.library('",requested_pkg,"','",date,"') ",
                    "to complete the process.\n ",
                   "In R Studio you can restart R with CMD/CTRL-SHFT-F10")
          answer<-infinite.prompt(format.msg(txt),"stop")     
          exit()
  
=======
    #4.4 If there is a conflict, but caused only by  groundhog,  ask for a restart
        # message("#80 this is the list of groundhog loaded pkgs: ",.pkgenv[['groundhog_loaded_pkgs']] )
        # message("#81 there are ",length(conflict.needed), ' conflicts')
        # message("#82 there are ",sum(not.groundhog.conflicts), ' not from groundhog')
        
        if (length(conflict.needed)>0 & sum(not.groundhog.conflicts)==0)
         {
          
          
        #Start saying there is a conflict
            msg <- paste0( "Another version of a needed package was previously loaded with groundhog.")
          
        #if has used different dates, indicate that.
          if (length(.pkgenv[['hogdays']])>1) {
              msg<-paste0(msg,
                          "You have used different groundhog days (dates) across groundhog.library() calls, ",
                          "that is proably  causing the problem.\n ", 
                          "Dates you have used: ",paste0(.pkgenv[['hogdays']],collapse=' , '),"\n\n ")
                }
          
        #Request restart   
              msg<-paste0(msg,  "You may unload all packages by restarting the R Session\n (in R Studio CMD/CTRL-SHFT-F10).\n\n ",
                         "Type OK to confirm you read this message.")
      
         answer <- infinite.prompt(format.msg(msg),'ok')   #utils.R function 33
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
      # conflict.needed.string <- pasteQC(conflict.needed) #utils.R function 31
      # conflict.active.string <- pasteQC(conflict.active)
      # conflict.needed.string_first3 <- pasteQC(conflict.needed[1:3]) #utils.R function 31
      # conflict.active.string_first3 <- pasteQC(conflict.active[1:3])


  #7 If conflict found
      if (n.conflict > 0 ) {
             
  #8 Show general message
#         cn<-conflict.needed[1:min(5,length(conflict.needed))]
#         ca<-conflict.active[1:min(5,length(conflict.active))]
#  
#          msg <- paste0("|PROBLEM.\n",
#                 "|    Groundhog says:\n",
#                 "|    ", n.conflict, " of the ", n.needed, " packages needed for '",requested_pkg_vrs, "' have a version conflict\n",
#                 "|    with packages already in your current R session.\n|\n",
#                 "|    NEEDED: ",pasteQC(cn),"\n",
#                 "|    LOADED: ",pasteQC(ca),"\n"
#                 )
# 
#          
#         if (n.conflict>5) {
#           msg<-paste0(msg, "|    Showing first 5 of the ",n.conflict, " conflicting packages.\n",
#                            "|    (`.view.conflict` to see all ",n.conflict, ")") 
# 			     .view.conflict <<- data.frame(loaded=conflict.active, needed=conflict.needed)
#           }
			    
	 #9 Add notice about remotes already loaded
      #9.1 Find remotes in the snowball
           snowball.remotes <- subset(snowball, snowball$from %in% c('github','gitlab'))
      
      #9.2 See if any of the conflicts involve them    
            flag.conflict_needed_remote <- any(conflict.active.pkg %in% snowball.remotes$pkg)
            
      #9.3 See if any of the conflicts involve already loaded remotes
            flag.conflict_active_remote <- any(conflict.active.pkg %in% .pkgenv[['remote_packages']])
    
               
      #9.4 msg for conflict with NEEDED remote
            
            
           if (flag.conflict_active_remote==TRUE || flag.conflict_needed_remote==TRUE)
            {
                if (flag.conflict_active_remote) pkg.conf <-paste0(conflict.active.pkg[conflict.active.pkg %in% .pkgenv[['remote_packages']]],collapse=', ')
                if (flag.conflict_needed_remote) pkg.conf <-paste0(conflict.needed.pkg[conflict.needed.pkg %in% .pkgenv[['remote_packages']]],collapse=', ')

                msg <- paste0(msg, "\n",
                         "|    Because the conflict involves non-CRAN package(s), you may want to allow the version conflict\n",
    				             "|    and use the package version already loaded rather than the one you are trying to load.\n",
    				             "|    To do that: rerun groundhog.library() with the option: 'ignore.deps=c(" , pkg.conf , ")' \n\n",
    				             "|    Press 'OK' to confirm you have read this message.")
                       answer <- infinite.prompt(msg, c('ok'))
					   exit()
               }
    

            
   
            
    # #12  If the conflict does not involve remote packages, prompt to disable them
    #        if (flag.conflict_needed_remote==FALSE) {
    #          msg <- paste0(msg, "|\n",
    #                               "|    The simplest solution to prevent this & future conflicts, is to disable\n",
    #                               "|    all existing packages. You may quickly re-enable them at any time by\n",
    #                               "|    running `enable.packages()`.\n\n",
    #                               "|    Your options:\n",
    #                               "|    1) Type 'disable.packages()'\n",
    #                               "|    2) type 'x' to stop")
    #        
    #            answer <- infinite.prompt(msg, c('disable.packages()','x'))
    #         
    #          }
    #         
    #         
    #  #13 Disable & Localize
    #     
    #   if (answer=='disable.packages()')  {
    #   
    #     #13.0 installed.packages
    #       ip <- data.frame(installed.packages(lib=.pkgenv[['default_libpath']]))
    #   
    #     #13.1 disable                
    #       disable.packages(disable.quietly = FALSE, skip.prompt=TRUE)
    # 
    #     #13.2 MSG 
    #       message1("\nCopying packages needed by '",requested_pkg_vrs,"' to default R library.")
    #       
    #     #13.3 Install snowball
    #      # groundhog.install(snowball)  #install full snowball, populated from the target pkg 
    #       install.snowball(snowball,date=date,install.only = TRUE, skip.remotes=TRUE)
    #       
    #     #13.3 Drop any remotes from the snowball
    #         #snowball <- snowball[!snowball$from %in% c('github','gitlab') ,]
    #       
    #     #13.4 Localize the snowball
    #       localize.snowball(snowball, localize.quietly=FALSE)
    # 
    #       
    #     #13.5 If  r/markdown or knitr is active, groundhog install and localize all markdown packages
    #       if (sum(c('markdown','rmarkdown','knitr') %in% active$pkg)>0) 
    #         {
    #         for (pkgk in .pkgenv[['markdown_packages']])
    #         {
    #           snowball.k <- get.snowball(pkgk,date)
    #           install.snowball(snowball.k,date=date,install.only = TRUE, skip.remotes=TRUE)
    #           localize.snowball(snowball.k, localize.quietly=TRUE)
    #         } #End for
    #       }  #End markdown
    #           
       
                
                         
      #     #13.6 Ask them to restart the R Session
      #             txt<-paste0("Because packages were just disabled, to proceed, restart the R session ",
      #                         "and then run groundhog.library('",requested_pkg,"','",date,"') again\n ",
      #                         "In R Studio you can restart R with CMD/CTRL-SHFT-F10")
      #             
      #             answer<-infinite.prompt(format.msg(txt),"stop")
      #             exit()
      #         } #End If agreed to disable something
      #       
      # #14 Chose not to disable
      #       if (answer=='none')  {
      #           message("You answered 'x', so no packages were disabled.\n",
      #                  "For alternative solutions consider the `ignore.deps` argument.\n",
      #                  "For more information visit https://groundhogR.com/conflicts")
      #              exit()
      #             }

	
    #Remove all conflicting packages from local library
      for (pkgk in conflict.needed.pkg)
      {
        remove.packages(pkgk)
      }

    #Install the snowball
       install.snowball(snowball,date=date,install.only = TRUE, skip.remotes=TRUE)
       
    #Localize here so that if they were to restart the R session but not rerun groundhog library
    #the package would remain available
        #localize.snowball(snowball)   
         
     #Message that a rstart is needed 
        txt<-paste0("Some of the packages needed to load '",requested_pkg_vrs,"' conflicted with packages ",
                    "already in your R session. The conflict has been resolved, but you will need to ",
                    "restart the R session and then run groundhog.library('",requested_pkg,"','",date,"') ",
                    "to complete the process.\n ",
                   "In R Studio you can restart R with CMD/CTRL-SHFT-F10")
                   
                   answer<-infinite.prompt(format.msg(txt),"stop")     
        exit()
  }# End if some conflict found
>>>>>>> Stashed changes
} # End function


