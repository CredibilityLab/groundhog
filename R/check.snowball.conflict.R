
# If a package in the to be loaded/installed snowball conflicts with an already loaded 
# package, informative feedback is provided




check.snowball.conflict <- function(snowball, pkg.requested, force.install, ignore.deps, date) {

  
   #1 Update snowball and groundhog.session dataframes
  
      #1.1 Fill repos column for snowball (CRAN vs Remote, this is NOT the install from, it is where package exists)
          snowball$repos <- get.repos_from.snowball(snowball)  #function utils.R #40
          snowball$pkg_vrs_repos <- paste0(snowball$pkg_vrs, snowball$repos)
          
      #1.2 Get requested pkg (vector)
          
          requested_pkg_vrs <- snowball$pkg_vrs[snowball$pkg %in% pkg.requested]
          requested_pkg     <- snowball$pkg[snowball$pkg %in% pkg.requested]
          requested_repos   <- snowball$repos[snowball$pkg %in% pkg.requested]
          requested_pkg_vrs_repos <- snowball$pkg_vrs_repos[snowball$pkg %in% pkg.requested]
          
          
      #1.4 Short name for session 
        gs <- .pkgenv[['groundhog.session_df']]
              
              #this is a dataframe with variables: pkg, vrs, pkg_vrs, repos, time, requested
              #it is loaded with zzz and updated in groundhog.library.single() and ''.remote()
              #time is when the pkg was loaded, and requested is TRUE for actively asked for package.
        
        
      #1.5 Create unique identifier  #will compare these to #1.4
          gs$pkg_vrs_repos      <- paste0(gs$pkg_vrs,gs$repos)
          
        
          
   #2 Active packages
          active <- get.active()
  
  
   #3 DROPPED
    
   #4 CONFLICTS
     #Do first the conflicts for remotes, so that the general advice to just restart R is not given for them
          
          
      #DROPPED SINCE WE DO THIS IN BACKGROUND - Conflict 1: force Install (is any package that needs to be *installed*  loaded)
          
          
      #Conflict 2: Same remote, different date
      #Conflict 3: Requested package was previously loaded with groundhog but different version or repository
      #Conflict 4: Dependency conflicts with previously loaded groundhog package
      #Conflict 5: Any pkg in snowball was already loaded, different version, not with groundhog
        
    #Text reused in several separate messages
      text.not.loaded <- paste0("\n  ** Needed packages are installed, but restarting R is necessary to load them. **")
      text.restart    <- paste0("Please restart your R Session (in R Studio SHFT-CTRL/CMD-F10) ",
                                "and run the groundhog.library() command again.")
    #--------------------------             
    #Conflict 2: Same remote, different date
        
        pkg.conflict_remote_date <- snowball$pkg[ (snowball$pkg %in% .pkgenv[['remotes_df']]$pkg) & (!date %in%  .pkgenv[['remotes_df']]$date) ]
        
        
        if (length(pkg.conflict_remote_date)>0)
        {
          msg <- paste0("The following packages were previously loaded from a non-CRAN repository, ",
                "using a date other than '",date,"': ",pasteQC(pkg.conflict_remote_date),". ",
                "This creates a potential package version conflict that is not detectable ",
                "by version number. To avoid this conflict you may need to modify ",
                "your script ensuring the same date is used for every groundhog.library() call. ",
                text.restart)
          
          message1(msg)
          stop(text.not.loaded)

        }

       
  #--------------------------               
  #Conflict 3 - a requested.pkg was previously loaded with groundhog but different version or repository
        for (k in 1:length(requested_pkg))  
        {      
        if   (requested_pkg[k]           %in% gs$pkg           &     #requested pkg was already loaded with groundhog
             !requested_pkg_vrs_repos[k] %in% gs$pkg_vrs_repos )     #but not same vrs or repository 
            
           {
      #Note: conflict 2 already took care of same pkg_vrs_repos, but different date for remotes, which could be a conflict too.
              
            
      #Start message flagging problem
          msg<-paste0("Another version of '", requested_pkg[k] , "', was previously loaded with groundhog in this R session. ")
          
        #Add different dates warning if relevant
            if (length(.pkgenv[['hogdays']])>1) {
                    msg<-paste0(msg, "\n",
                                    "Across groundhog.library() calls you have used different dates:\n(",
                                    pasteQC(.pkgenv[['hogdays']]),").\nThat is a likely root cause for this conflict, ",
                                   "consider using the same date throughout."
                                )
                          
                  } #End if multiple dates
        
        #Continue
          message1(msg)
          message1(text.restart)
          stop(text.not.loaded)

        }
        }
          
        
  #--------------------------             
  #Conflict 4: Dependency conflicts with previously loaded groundhog package  
  #          (ignore.dep() is a solution)

              
          conflict4.TF <-   snowball$pkg           %in% gs$pkg           &    #pkg requested before
                           !snowball$pkg_vrs_repos %in% gs$pkg_vrs_repos &    #but not same vrs or repository 
                           !snowball$pkg %in% ignore.deps                  #and we are not asked to ignore this conflict
        
            #vector with T/F for each pkg having been previously loaded in different version with groundhog
             #(this includes the pkg requested now, but that was taken care of in conflict #3)
            
            
          if   (sum(conflict4.TF>0))
          {
            
          #Character with list of packages that need to be ignored
            need.to.ignore <- pasteQC(snowball$pkg[conflict4.TF])  #pasteQC(), Utils.R function #31
    
          #Start saying there is a conflict
            msg <- paste0("Another version of a needed package was previously loaded with groundhog.")
              
                   
          #If multiple hogdays used add explanation
            if (length(.pkgenv[['hogdays']])>1) { 
                          msg<-paste0(msg, "\n",
                                     "Across groundhog.library() calls you have used different dates (",
                                     pasteQC(.pkgenv[['hogdays']]),
                                      ") that is a possible root cause for this conflict.")
                          
                  } #End if more than 1 hogday 
          
          ##Add ignore deps
            msg <- paste0(msg, "\n",
                          "You can either restart the R Session and avoid the conflict, or rely on the 'ignore.deps()' option to ignore it.")
            
          #Show message
            message1(msg)
            stop(" ** Restart R and re-run the groundhog.library() command **")
            
            
            }
 
          
          
  #--------------------------         
  #MOST LIKELY CONFLICT:
  #Conflict 5: Any pkg in snowball was already loaded, different version, not with groundhog
    
      conflict5.TF  <- snowball$pkg %in% active$pkg &           #pkg we want is active 
                       !snowball$pkg_vrs %in% active$pkg_vrs &  #but a different version
                       !snowball$pkg %in% ignore.deps          #and did not ask to ignore it

      
	
    #If no conflict, early return
      if (sum(conflict5.TF)==0) return(invisible(TRUE))
           

       
    #Message that a restart is needed 
        msg <-"All needed packages are installed, but some have conflicting versions already loaded in your R session."
        message1(msg)
        message1(text.restart)
        stop(text.not.loaded)
        
} # End function

