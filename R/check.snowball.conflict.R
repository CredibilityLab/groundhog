# Check snowball conflict

#1  short name for package being installed/loaded
#2  Ignore conflicts 
#3  Active packages
#4  Conflict 1: force Install (is any package that needs to be *installed*  loaded)
#5  Conflict 2: Same remote, different date
#6  Obtain vector with all conflicting packages
#7  Conflict 3 - different version
#8  Conflict 4 - pkg 1 on CRAN, pkg 2 is remotes, so even same date creates conflict
#9  Conflict 5 - pkg 1 on Remote, pkg 2 is CRAN, so even same date creates conflict
#10 Conflict 6 - Typical conflict: both packages are in CRAN, one was loaded before groundhog tried 

#-------------------------------


check.snowball.conflict <- function(snowball, force.install, ignore.deps, date) {

    
   #1 manipulate snowball and groundhog.session data.frames
  
      #1.1 Add repos
          snowball$repos <- get.repos_from.snowball(snowball)  #function utils.R #40
    
      #1.2 Get requested pkg
          requested_pkg_vrs <- snowball$pkg_vrs[length(snowball$pkg_vrs)]
          requested_pkg     <- snowball$pkg[length(snowball$pkg_vrs)]
          requested_repos   <- snowball$repos[length(snowball$repos)]
          
      #1.3 Create identifier
          requested_pkg_vrs_repos      <- paste0(requested_pkg_vrs,requested_repos)
          requested_pkg_vrs_date_repos <- paste0(requested_pkg_vrs,date,requested_repos)
    
          
          
      #1.4 Short name for session 
        gs <- .pkgenv[['groundhog.session_df']]
              
              #this is a dataframe with variables: pkg, vrs, pkg_vrs, repos, time, requested
              #it is loaded with zzz and updated in groundhog.library.single() and ''.remote()
              #time is when the pkg was loaded, and requested is TRUE for actively asked for package.
        
        
      #1.5 Create unique identifier  #will compare these to #1.3
          gs$pkg_vrs_repos      <- paste0(gs$pkg_vrs,gs$repos)
          gs$pkg_vrs_date_repos <- paste0(gs$pkg_vrs,gs$date, gs$repos)
          
        
          
   #2 Active packages
          active <- get.active()
  
  
   #3 DROPPED
    
   #4 CONFLICTS
      #Conflict 1: force Install (is any package that needs to be *installed*  loaded)
      #Conflict 2: Same remote, different date
      #Conflict 3: Requested package was previously loaded with groundhog but different version or repository
      #Conflict 4: Dependency conflicts with previously loaded groundhog package
      #Conflict 5: Any pkg in snowball was already loaded, different version, not with groundhog

          
          
      text.not.loaded <- paste0("The package ",requested_pkg_vrs, " was NOT loaded  \n \n ")
      type.ok <- "Type 'OK' to confirm you have read this message."
    #--------------------------                  
    #Conflict 1: force Install (is any package that needs to be *installed*  loaded)
    
            if (force.install) {
              
              #Snowball pkg already loaded
                  pkg.loaded_need_installing <- snowball$pkg[snowball$pkg %in% active$pkg]
                                                         
              
              if (length(pkg.loaded_need_installing)>0) {
                msg <- paste0(
                          "The package '",requested_pkg_vrs, "' was NOT installed \n \n ",
                          "You selected  'force.install=TRUE' but the following packages ",
                          "that would be installed are currently loaded: ", pasteQC(pkg.loaded_need_installing), ". ",
                          "You may unload all packages by restarting the R session. \n ",
                          restart.text(),
                          type.ok)

                answer<-infinite.prompt(format.msg(msg),c('ok','quit()'))
                if (answer=="quit()") quit()
                exit()
                
              } # End conflict found for forced install
            } # End check force install

    #--------------------------             
    #Conflict 2: Same remote, different date
        
        pkg.conflict_remote_date <- snowball$pkg[ (snowball$pkg %in% .pkgenv[['remotes_df']]$pkg) & (!date %in%  .pkgenv[['remotes_df']]$date) ]
        
        
        if (length(pkg.conflict_remote_date)>0)
        {
          msg <- paste0(
                text.not.loaded,
                "The following packages were previously loaded from a non-CRAN repository, ",
                "using a date other than '",date,"': ",pasteQC(pkg.conflict_remote_date),". ",
                "This creates a potential package version conflict that is not detectable ",
                "by version number. To avoid this conflict it you may need to modify ",
                "your script ensuring the same date is used for every groundhog.library() call. ",
                "You may unload all packages by restarting the R session \n ",
                restart.text(),
                type.ok)

  
          answer <- infinite.prompt(format.msg(msg),c("ok","quit()"))
          if (answer=="quit()") quit()

          exit()
        }

        
  #--------------------------               
  #Conflict 3 - requested.pkg was previously loaded with groundhog but different version or repository
        
        if   (requested_pkg           %in% gs$pkg           &     #requested pkg was already loaded with groundhog
             !requested_pkg_vrs_repos %in% gs$pkg_vrs_repos )     #but not same vrs or repository 
            
           {
          
              #Note: conflict 2 already took care of same pkg_vrs_repos, but different date for remotes, which could b
              #      be a conflict too.
              
            
        #Start message flagging problem
          msg<-paste0(text.not.loaded,
                      "Another version of '", requested_pkg ,
                      "', was previously loaded with groundhog in this R session. ")
          
        #Add different dates warning if relevant
            if (length(.pkgenv[['hogdays']])>1) {
                    msg<-paste0(msg, 
                                    "Across groundhog.library() calls you have used different dates (",
                                    paste0(.pkgenv[['hogdays']],collapse=' , '),
                                   ") that is a possible root cause for this conflict, ",
                                   "consider using the same date throughout."
                                )
                          
                  } #End if multiple dates
        
        #Continue
          msg <- paste0(msg, " \n ",
                      "You can unload all packages by restarting the R session. \n ",
                      restart.text(),
                      type.ok)
          
      
        #Show message
          answer<-infinite.prompt(format.msg(msg),"ok")
          exit()
          
        }
          
        
  #--------------------------             
  #Conflict 4: Dependency conflicts with previously loaded groundhog package  
  #          (ignore.dep() is a solution)

              
          conflict4.TF <-   snowball$pkg           %in% gs$pkg           &    #pkg requested before
                           !snowball$pkg_vrs_repos %in% gs$pkg_vrs_repos &    #but not same vrs or repository 
                           !snowball$pkg %in% ignore.deps                   #and we are not asked to ignore this conflict
        
            #vector with T/F for each pkg having been previosly loaded in different version with groundhog
             #(this includes the pkg requested now, but that was taken care of in conflict #3)
            
            
          if   (sum(conflict4.TF>0))
          {
            
          #Character with list of packages that need to be ignored
            need.to.ignore <- pasteQC(snowball$pkg[conflict4.TF])  #pasteQC(), Utils.R function #31
    
          #Start saying there is a conflict
            msg <- paste0(text.not.loaded,
                          "Another version of a package needed to load '",requested_pkg_vrs,"' ",
                          "was previously loaded with groundhog. ")
              
                   
          #Add different dates warning if relevant
            if (length(.pkgenv[['hogdays']])>1) {
                          msg<-paste0(msg,
                              "Across groundhog.library() calls you have used different dates (",
                                  paste0(.pkgenv[['hogdays']],collapse=' , '),
                              ") that is possible root cause of this conflict.")
                          
                  } #End if  
          
          ##Request restart   
            msg<-paste0(msg, " \n There are two alternative solutions: \n ",
                        
                             " 1) Restart the R Session and modify your script to avoid the other version being loaded in the first place \n ",
                             restart.text(),
                        
                             " 2) Explicitly allow the already loaded version of the package ",
                             "to be tolerated, by adding the 'ignore.deps' options, running: \n ",
                             "    `groundhog.library('",requested_pkg , "','", date, "', ignore.deps=c('", need.to.ignore ,"')",
                             " \n \n ", 
                             type.ok)
                          
            answer <- infinite.prompt(format.msg(msg),c("ok","quit()"))
            if (answer=="quit()") quit()
            exit()
            }
 
          
          
  #--------------------------         
 
  #Conflict 5: Any pkg in snowball was already loaded, different version, not with groundhog
    
      conflict5.TF  <- snowball$pkg %in% active$pkg &           #pkg we want is active 
                       !snowball$pkg_vrs %in% active$pkg_vrs &  #but a different version
                       !snowball$pkg %in% ignore.deps          #and did not ask to ignore it

      
	
    #If no conflict, early return
      if (sum(conflict5.TF)==0) return(invisible(TRUE))
           

    #Install the snowball
       install.snowball(snowball,date=date,install.only = TRUE, skip.remotes=FALSE)
       
    #Localize the snowball
       localize.snowball(snowball)   
         
    #Message that a restart is needed 
        txt<-paste0(text.not.loaded,
                    "Some of the packages needed to load '",requested_pkg_vrs,"' conflicted with packages ",
                    "already in your R session. The conflict has been resolved, but you will need to ",
                    "restart the R session and re-run groundhog.library('",requested_pkg,"','",date,"') ",
                    "to complete the process. \n ",
                    restart.text()
                    )
          answer<-infinite.prompt(format.msg(txt),c("quit()","stop"),must.restart = TRUE) #'stop' is for debugging, not meant to be selected by users
          if (answer=='quit()') quit()
          exit()
  
} # End function

