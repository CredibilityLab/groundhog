
  groundhog.library.single.remote <-  function(pkg, date,  quiet.install ,  include.suggests , 
    ignore.deps, force.source , force.install )
      { 
      #0 Note that Date and R validated in groundhog.library()
      #1 Process pkg-->usr, remote_id
          pkg_list<-make.pkg_list(pkg)
          usr <- pkg_list$usr
          remote_id <- pkg_list$remote_id
          pkg <- pkg_list$pkg
      
      
        #0.5 Check if snowball for this package exists (snowball.rds), 
    
      #If the snowball exists, the pkg should be installed already pkg_sha, can load the snowball
      #If the snowball does not exist {
      
      #1 Verify clone exists and is up to date 
          valid_clone <- validate.clone_date(pkg, date,remote_id ,usr)
    
      #2 Get baton (information on remote and its remote dependencies)  'get.baton.R'
          baton<-get.baton(pkg,date,remote_id,usr)
          
      #3 Modify cran toc removing other versions of the remote packages, and adding new ones with date 1970-01-01
        #load cran.toc locally  
          cran.toc <- .pkgenv[['cran.toc']]
        
        #drop the remote packages from cran.toc
          cran.toc <- subset(cran.toc, !Package %in% baton$rows.toc$Package)
          
        #add the remote versions
          cran.toc <- rbind(baton$rows.toc,  cran.toc)

        #Copy changes to the environmental variabe  
          .pkgenv[['cran.toc']] <- cran.toc
          
          
      #4 Get snowball from modified cran.toc
          snowball <- get.snowball(pkg,date)
          
    
      #5 Modify snowball's row swith remote packages
          #5.0 Check if the remote packages have been installed
             #how many remote files
              n.remotes <- length(baton$rows.toc$Package)
              
            #make vector where they are all not- installed
              remote.installed=rep(FALSE,n.remotes)
              
            #Now check if a installed.packages() detects it in that folder
              
              for (j in 1:n.remotes)  
              {
                if (nrow(installed.packages(baton$installation_path))>0) {
                  remote.installed[j] <- TRUE
                }#End if installed
                }#End for loop
                                
          
          
          
          #5.1 Which packages in snowball are remote?
              kj <- match(snowball$pkg, baton$rows.toc$Package  )  
              
              #kj is a vector indcating the match between row k in snowball and row j in baton
              #NA--> not in baton, kj>0 is the jth row in baton matching the kth row in snowball
          
          #5.3 Add sha and usr to snowball
              snowball$sha=snowball$usr=NA

          #5.4 Loop and if it is not NA replace variuos values of the snowball
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
              
      #6 install snowball
          install.snowball(snowball, date)
    
      #7 Load snowball
          load.snowball(snowball,ignore.deps)
            
  } #End of function

