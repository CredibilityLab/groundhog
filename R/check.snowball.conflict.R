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

        
        
        
  #6 Obtain vector with all conflicting packages
      pkg.conflict  <- snowball$pkg    [!(snowball$pkg_vrs %in% active$pkg_vrs) & (snowball$pkg %in% active$pkg) & (!snowball$pkg %in% ignore.deps)]

      
  #7 Conflict 3 - A conflict caused by two groundhog loaded pkgs
      
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
  
} # End function


