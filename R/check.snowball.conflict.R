

#BEFORE
 #1) Prepare variables
 #2) Possible conflicts
    # 2.1) A *requested.pkg* was previously loaded with groundhog but different version or repository
    # 2.2) A *dependency* was loaded for a different repos or date 
    # 2.3) *Any* conflict on a non-interactive session (because we cannot request after installation they restart in  a forceful way)

#3 AFTER
    # Any pkg in snowball was already loaded, different version, not with groundhog (with groundhog checked before installing)




#---------------------------------------------------------------------------------------------


  #1 BEFORE
    #Before downloading packages, check if there is a *groundhog induced* conflict
    check.conflict.before<-function(snowball, pkg.requested, ignore.deps, date)
    {

     #1.1 Short name for session.snowballs
        ss <- .pkgenv[['session.snowballs']]
      
    #1.2 Check conflicts before only if groundhog has been used already and session.snowballs[] has rows
      if (nrow(ss)>0)
        {
          
    #1.3 F10
      f10 = ifelse(interactive(), "\n(in R Studio run CMD/CTRL-SHFT-F10 to restart R session).","")
      stop.txt = "\n\n ** Conflict with previously loaded packages **"
      
      
    #1.4 Prepare variables
        #1.4.1 Include repo (CRAN vs Remote) in snowballl
              snowball$repos <- get.repos_from.snowball(snowball)  #function utils.R #40
              snowball$pkg_vrs_repos <- paste0(snowball$pkg_vrs, snowball$repos)
              
        #1.4.2 Get requested pkg (vector)
              
              requested_pkg_vrs <- snowball$pkg_vrs[snowball$pkg %in% pkg.requested]
              requested_pkg     <- snowball$pkg[snowball$pkg %in% pkg.requested]
              requested_repos   <- snowball$repos[snowball$pkg %in% pkg.requested]
              requested_pkg_vrs_repos <- snowball$pkg_vrs_repos[snowball$pkg %in% pkg.requested]
              
                   
                  #this is a dataframe with variables: pkg, vrs, pkg_vrs, repos, time, requested
                  #it is loaded with zzz and updated in groundhog.library() 
            #-- (used to be updated with .single and single.remote())
                  #time is when the pkg was loaded, and requested is TRUE for actively asked for package.
            
        #1.4.3 Create unique identifier  #will compare these to #1.4
              ss$pkg_vrs_repos      <- paste0(ss$pkg_vrs , ss$repos)
              
        #1.4.4 Active packages 
              active <- .pkgenv[['active']]  #this was captured right before installing packages in groundhog.library()
      
        
              
  #-----------------------------------------------------
              
  #2 possible conflicts
      #Conflict 1 - a *requested.pkg* was previously loaded with groundhog but different version or repository
        for (k in 1:length(requested_pkg))  
        {      
        if   (requested_pkg[k]           %in% ss$pkg           &     #requested pkg was already loaded with groundhog
             !requested_pkg_vrs_repos[k] %in% ss$pkg_vrs_repos )     #but not same vrs or repository 
            
           {

            
          #Start message flagging problem
              msg<-paste0("Another version of '", requested_pkg[k] , "' was previously loaded with groundhog in this R session.")
              
            #Add different dates warning if relevant
                if (length(.pkgenv[['hogdays']])>1) {
                        msg<-paste0(msg, "\n",
                                        "This may be because across groundhog.library() calls you have used different dates:\n(",
                                        pasteQC(.pkgenv[['hogdays']]),")."
                                )
                          
                  } #End if multiple dates
        
        #Message
          msg<-paste0(msg, "\nTo unload all packages restart your R session." , f10)
          gstop(msg) #utils #51

        } #End conflict 2.2
          
          
    #-----------------------------------------------------------------------------
          
    #Conflict 2 A *dependency* was loaded for a different repos or date 
    conflict2.TF <-   snowball$pkg           %in% ss$pkg           &     #pkg requested before
                      !snowball$pkg_vrs_repos %in% ss$pkg_vrs_repos &    #but not same vrs or repository 
                      !snowball$pkg %in% ignore.deps                     #and we are not asked to ignore this conflict
        
            #vector with T/F for each pkg having been previously loaded in different version with groundhog
             #(this includes the pkg requested now, but that was taken care of in conflict #1)
            
            
          if   (sum(conflict2.TF>0))
          {
            
          #Character with list of packages that need to be ignored
            need.to.ignore <- pasteQC(snowball$pkg[conflict2.TF])  #pasteQC(), Utils.R function #31
            .pkgenv[['conflicts']] <- need.to.ignore
            .view.conflicts <<- view.conflicts() #utils #57
            
            #used pasteQC to make it a bit more difficult to copy-paste into ignore.deps the name of the variable
			      #avoid users doing ignore.deps=.view.conflicts for it would make for less reproducible code

          #Start saying there is a conflict
            msg <- paste0("Another version of a needed package was previously loaded with groundhog.")
              
                   
          #If multiple hogdays used add explanation
            if (length(.pkgenv[['hogdays']])>1) { 
                          msg<-paste0(msg, "\n",
                                     "Across groundhog.library() calls you have used different dates (",
                                     pasteQC(.pkgenv[['hogdays']]),
                                      "), \nthat is the likely cause of this conflict.\n",
                      "You may either keep the date of this groundhog.library() call and\n",
                      "unload previosly loaded packages, or modify the date so that it matches\n",
                      "a previously used date.\n")
              
              } #End if more than 1 hogday  
              
            #Message on ignore deps
              #msg<-paste0(msg,"\nYou may skip this check using the `ignore.deps` option.")
              
            #Show some or all          
              if (length(snowball$pkg[conflict2.TF])<=5) {
                msg<-paste(msg,"\nThe packages creating the conflict are:",need.to.ignore)

              } else {
                msg<-paste(msg,"\nThere are more than 5 conflicting packages, view them with: `.view.conflicts`")
              }
            #To unload
            msg<-paste0(msg, "\nYou may avoid this conflict by restarting the R session (see also `ignore.deps` option)",f10)
            
          #Show message
            gstop(msg) #utils #51
          
            }
    }#End conflict 2
    
    #--------------------------------------      
          
    
      #Conflict 3 - *Any* conflict on a non-interactive session (because we cannot request after installation they restart in  a forceful way)
      
        if (interactive()==FALSE)
        {
        pkg.conflict <- active$pkg[ active$pkg %in% snowball$pkg & 
                                   !active$pkg_vrs %in% snowball$pkg_vrs & 
                                    !active$pkg %in% ignore.deps]
        
        if (length(pkg.conflict) >0)
        {
          msg<-paste0("Some of the packages you need to load have other versions already loaded.",
                      "You need to either start a new R session to unload them or explicitly ignore ",
                      "those conflicts by adding the package names in the `ignore.deps`",
                      "argument of the groundhog.library() call.",
                      "The packages with a conflict are: ", 
                      snowball$pkg[conflict2.TF])
            
			gstop(msg) #utils #51

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
        f10 = ifelse(interactive(), "\n(in R Studio  CMD/CTRL-SHFT-F10 restarts the R session.) ","")
        
      
        active <- .pkgenv[['active']]  #this was captured right before installing packages in groundhog.library()
      #Any pkg in snowball was already loaded, different version, not with groundhog (with groundhog checked before installing)
    
      conflict.TF  <- snowball$pkg %in% active$pkg &           #pkg we want is active 
                       !snowball$pkg_vrs %in% active$pkg_vrs &  #but a different version
                       !snowball$pkg %in% ignore.deps          #and did not ask to ignore it

      
	
    #If no conflict, early return
      if (sum(conflict.TF)==0) return(invisible(TRUE))
           
       
    #Message that a restart is needed 
      #If installation just happened
      msg <- ''
      if (sum(snowball$installed==FALSE)>0) {
        message2( "Installation succesfull.")
        }
        
        msg <- paste0(msg, 
                  "Other versions of the requested packages are already loaded,\n",
                  " please restart the R session and re-run groundhog.library()",f10)
      
        infinite.prompt(msg, "uncle",must.restart=TRUE)
        
        
      
      } # End function
