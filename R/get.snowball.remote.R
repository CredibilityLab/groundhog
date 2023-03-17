
#Script modifies cran.toc to include the remote package and its remote dependencies, and then
#generates a snowball using the default get.snowball() function
#

  get.snowball.remote<-function(pkg,date,remote_id, usr,include.suggests, force.install)
  {

    
        #Validate date and include suggests
            validate.TF(include.suggests)
            validate.date(date)

  #1  If snowball already exists early return it  
    
        #Path to snowball
            snowball_dir <- paste0(get.groundhog.folder() , '/snowballs_v2/' , remote_id )
           
             if (include.suggests==FALSE) snowball_file <- paste0(usr ,"_", pkg , "_" ,  gsub( "-", "_" , date) , '.rds')  
             if (include.suggests==TRUE)  snowball_file <- paste0(usr ,"_", pkg , "_" ,  gsub( "-", "_" , date) , '_with_suggests.rds')  
  
            snowball_path <- file.path(snowball_dir, snowball_file)
      
        #Create snowball directory if it does not exist
            if (!file.exists(snowball_dir)) dir.create(snowball_dir,recursive=TRUE, showWarnings = FALSE)  
            
        #If snowball file exists, return it
            if (file.exists(snowball_path)) {
                snowball <- readRDS(snowball_path)
                return(snowball)
            } 

  #2 Verify clone exists and is up to date 
       valid_clone <- validate.clone_date(pkg, date,remote_id ,usr)
    
  #3 Get baton (information on remote and its remote dependencies)  'get.baton.R'
       baton<-get.baton(pkg,date,remote_id,usr,include.suggests)
       
          #This will include sha values and dependencies
       
  #3.5 Verify requested name of package was obtained
       if (!pkg %in% baton$rows.toc$Package) {
         
         #Last row in the baton has name of DESCRIPTION pkg on its own
           pkg_self_name <- baton$rows.toc[1,]$Package
         
         txt<-paste0(
                 "The package you requested, '" , usr , "/" , pkg , "'\n",
                 "appears to have a different name on ", remote_id,".\n",
                 "You could try running:  groundhog.library('",usr , "/" , pkg_self_name,"' , '",date,"')\n",
        	     "Type 'OK' to confirm you have read this message.")
         answer <- infinite.prompt(format.msg(txt),'ok')
         exit()
       }
       
       
      
  #4 Modify cran toc removing other versions of the remote packages, and adding new ones with date 1970-01-01
    #load cran.toc locally  
      cran.toc <- .pkgenv[['cran.toc']]
    
    #drop the remote packages from cran.toc
      cran.toc <- subset(cran.toc, !cran.toc$Package %in% baton$rows.toc$Package)
      
    #add the remote versions
      cran.toc <- rbind(baton$rows.toc,  cran.toc)

    #Copy changes to the environmental variable  
      .pkgenv[['cran.toc']] <- cran.toc
      
      
  #5 Get snowball from modified cran.toc
      snowball <- get.snowball(pkg,date,include.suggests=include.suggests, force.install=force.install)
      

  #6 Modify snowball's row with remote packages
      
      #6.0 Check if the remote packages have been installed
         #how many remote files
          n.remotes <- length(baton$rows.toc$Package)
          
        #make vector where they are all not- installed
          remote.installed=rep(FALSE,n.remotes)
          
        #Now check if a installed.packages() detects it in that folder
          
          for (j in 1:n.remotes)  
          {
            if (nrow(utils::installed.packages(baton$installation_path[j]))>0) {
              remote.installed[j] <- TRUE
            }#End if installed
            }#End for loop
                            
      
      #6.1 Which packages in snowball are remote?
          kj <- match(snowball$pkg, baton$rows.toc$Package  )  
          
          #kj is a vector indcating the match between row k in snowball and row j in baton
          #NA--> not in baton, kj>0 is the jth row in baton matching the kth row in snowball
      
      #6.3 Add sha and usr to snowball
          snowball$sha=snowball$usr=NA

      #6.4 Loop and if it is not NA replace various values of the snowball
        for (k in 1:nrow(snowball))
        {
        if (!is.na(kj[k])) {
          #If there is a row in baton matching the snowball's pkg, replace these:
            snowball$from[k]              <- baton$install.from[kj[k]]
            snowball$installation.path[k] <- baton$installation_path[kj[k]]  
            snowball$sha[k]               <- baton$sha[kj[k]]
            snowball$usr[k]               <- baton$usr[kj[k]]
            snowball$installed[k]         <- remote.installed[kj[k]]  #computed in #5.1
            
            #kj[k] is it kth element in kj, thus it is the j'th row in baton we are getting
            
            
        }}

          
        return(snowball)
  }