


#1 BEFORE
#2 Conflicts that stop script before installing even
    #2.1 Conflict 1: Same remote, different date
    #2.2 Conflict 2: a requested.pkg was previously loaded with groundhog but different version or repository
    #2.3 Conflict 3


  #Before downloading packages, check if there is a *groundhog induced* conflict
    check.conflict.before<-function(snowball, pkg.requested, ignore.deps, date)
    {
      
     #1 Short name for session.snowbals
        ss <- .pkgenv[['session.snowballs']]
      
    #2 Check conflicts before only if groundhog has been used already and session.snowballs[] has rows
      if (nrow(ss)>0)
        {
          
    #3 F10
      f10 = ifelse(interactive(), "\n(in R Studio run CMD/CTRL-SHFT-F10 to restart R session.) ","")
      stop.txt = "\n\n ** Conflict with previously loaded packages **"
      
      
    #4 Prepare variables
        #4.1 Include repo (CRAN vs Remote) in snowballl
              snowball$repos <- get.repos_from.snowball(snowball)  #function utils.R #40
              snowball$pkg_vrs_repos <- paste0(snowball$pkg_vrs, snowball$repos)
              
        #4.2 Get requested pkg (vector)
              
              requested_pkg_vrs <- snowball$pkg_vrs[snowball$pkg %in% pkg.requested]
              requested_pkg     <- snowball$pkg[snowball$pkg %in% pkg.requested]
              requested_repos   <- snowball$repos[snowball$pkg %in% pkg.requested]
              requested_pkg_vrs_repos <- snowball$pkg_vrs_repos[snowball$pkg %in% pkg.requested]
              
                   
                  #this is a dataframe with variables: pkg, vrs, pkg_vrs, repos, time, requested
                  #it is loaded with zzz and updated in groundhog.library() 
            #-- (used to be updated with .single and single.remote())
                  #time is when the pkg was loaded, and requested is TRUE for actively asked for package.
            
        #4.3 Create unique identifier  #will compare these to #1.4
              ss$pkg_vrs_repos      <- paste0(ss$pkg_vrs , ss$repos)
              
        #4.4 Active packages 
              active <- .pkgenv[['active']]  #this was captured right before installing packages in groundhog.library()
      
        
              
       #--------------------------             
    #2 Checking specific Conflicts
          
          
      #2.1 Conflict 1: Same remote, different date
        pkg.conflict_remote_date <- snowball$pkg[ (snowball$pkg %in% .pkgenv[['remotes_df']]$pkg) & (!date %in%  .pkgenv[['remotes_df']]$date) ]
        
        
        if (length(pkg.conflict_remote_date)>0)
        {
          message1("Some packages being requested were previously loaded from a non-CRAN repository, \n",
                "using a date other than '",date,"': ",pasteQC(pkg.conflict_remote_date),".\n",
                "Please modify your script to use the same date across groundhog.library() calls.\n",
                "To unload packages restart your R session ",f10)
          message(stop.txt)
          exit()

        }

       
    #-----------------------------------------------------------------------------
          
      #2.2 Conflict 2 - a *requested.pkg* was previously loaded with groundhog but different version or repository
        for (k in 1:length(requested_pkg))  
        {      
        if   (requested_pkg[k]           %in% ss$pkg           &     #requested pkg was already loaded with groundhog
             !requested_pkg_vrs_repos[k] %in% ss$pkg_vrs_repos )     #but not same vrs or repository 
            
           {
      #Note: conflict 1 already took care of same pkg_vrs_repos, but different date for remotes, which could be a conflict too.
              
            
      #Start message flagging problem
          msg<-paste0("Another version of '", requested_pkg[k] , "', was previously loaded with groundhog in this R session. ")
          
        #Add different dates warning if relevant
            if (length(.pkgenv[['hogdays']])>1) {
                    msg<-paste0(msg, "\n",
                                    "Across groundhog.library() calls you have used different dates:\n(",
                                    pasteQC(.pkgenv[['hogdays']]),")."
                                )
                          
                  } #End if multiple dates
        
        #Message
          msg<-paste0(msg, "\nChange the requested date and/or restart R session to unload conflicting packages." , f10)
          message1(msg)
          message(stop.txt)
          exit()

        } #End conflict 2.2
          
          
    #-----------------------------------------------------------------------------
          
    #Conflict 3 A *dependency* was loaded for a different or repos date (add 'ignore.deps' option)
    conflict3.TF <-   snowball$pkg           %in% ss$pkg           &    #pkg requested before
                      !snowball$pkg_vrs_repos %in% ss$pkg_vrs_repos &    #but not same vrs or repository 
                      !snowball$pkg %in% ignore.deps                  #and we are not asked to ignore this conflict
        
            #vector with T/F for each pkg having been previously loaded in different version with groundhog
             #(this includes the pkg requested now, but that was taken care of in conflict #3)
            
            
          if   (sum(conflict3.TF>0))
          {
            
          #Character with list of packages that need to be ignored
            need.to.ignore <- pasteQC(snowball$pkg[conflict3.TF])  #pasteQC(), Utils.R function #31
    
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
            msg<-paste0(msg,"\nThe conflicting packages are:",need.to.ignore)
            msg<-paste0(msg,"\nTo unload packages you need to restart the R Session",f10)
          #Show message
            message1(msg)
            message(stop.txt)
            exit()
          
            }
    }#End conflict 3
    
          
          
    
      #Conflict 4 - *Any* conflict on a non-interactive session (because we cannot request after installation they restart in  a forceful way)
      
        if (interactive()==FALSE)
        {
        pkg.conflict <- active$pkg[pactive$pkg %in% snowball.all$pkg & !active$pkg_vrs %in% snowball.all$pkg_vrs]
        
        if (length(pkg.conflict) >0)
        {
          msg<-paste0("Some of the packages you need to load have other versions already loaded.",
                      "You need to either start a new R session to unload them or explicitly ignore ",
                      "those conflicts by adding the package names in the `ignore.deps`",
                      "argument of the groundhog.library() call.",
                      "The packages with a conflict are: ", 
                      pastcQC(pkg.conflict))
          message1(msg)
          message(stop.txt)
          exit()
          }
        }
    
        } #ENd if nrow(ss)>0
        } #End check.before
              
              
    
          
######################################################################################################################################################
  
  #--------------------------             
  #3 Conflict after
          
      check.conflict.after<-function(snowball, pkg.requested, ignore.deps, date)
      {
      #F10 msg
        f10 = ifelse(interactive(), "\n(in R Studio run CMD/CTRL-SHFT-F10 to restart R session.) ","")
        
      
        active <- .pkgenv[['active']]  #this was captured right before installing packages in groundhog.library()
      #Any pkg in snowball was already loaded, different version, not with groundhog (with groundhog checked before installing)
    
      conflict.TF  <- snowball$pkg %in% active$pkg &           #pkg we want is active 
                       !snowball$pkg_vrs %in% active$pkg_vrs &  #but a different version
                       !snowball$pkg %in% ignore.deps          #and did not ask to ignore it

      
	
    #If no conflict, early return
      if (sum(conflict.TF)==0) return(invisible(TRUE))
           
       
    #Message that a restart is needed 
        message1("There is a conflict between requested and loaded packages.")
        msg = paste0("Please restart your R session and rerun the groundhog.library() command.",f10)
         
        infinite.prompt(msg, "uncle",must.restart=TRUE)
        
    } # End function
