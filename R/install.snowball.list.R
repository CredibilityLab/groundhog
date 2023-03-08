

  install.snowball.list<-function(pkg, snowball.list, date,cores)
  {
    
    #0 Logging
      #Directory path
        log_path = paste0(get.groundhog.folder(),"/installation_logs")
        dir.create(log_path,showWarnings = FALSE, recursive = TRUE)
        
      #File paths 
        installation_log_path      <-  file.path(log_path,  "full_log.txt")
        installation_progress_path <-  file.path(log_path, "summary_log.txt")

      #0.1 Delete them so we get a new log
        unlink(installation_log_path)
        unlink(installation_progress_path)

        
#----------------------------------------------------
        
    #1 Empty cran, mran, and source storms
            
          #take the first snowball found, and empty it, to use as template
           empty.snowball <- snowball.list[[1]][FALSE,]
           storm.cran = storm.mran = storm.source = snowball.all = empty.snowball
              
   #2 Loop over snowballs making cran, mran, and source subsets, & snowball.all
            for (k in 1:length(snowball.list))
            {
                sk <- snowball.list[[k]]   #short name
                
              #The three storms, pkgs not yet installed, not already in the storm, and not base
                storm.cran   <- rbind(storm.cran ,  sk[sk$from=='CRAN'   & sk$installed==FALSE & !(sk$pkg %in% storm.cran$pkg)   & !(sk$pkg %in% base_pkg()),])
                storm.mran   <- rbind(storm.mran ,  sk[sk$from=='MRAN'   & sk$installed==FALSE & !(sk$pkg %in% storm.mran$pkg)   & !(sk$pkg %in% base_pkg()),])
                storm.source <- rbind(storm.source ,sk[sk$from=='source' & sk$installed==FALSE & !(sk$pkg %in% storm.source$pkg) & !(sk$pkg %in% base_pkg()),])
                
              #Snowball.all
                snowball.all <- rbind(snowball.all, snowball.list[[k]])
            }
          
    #3 Prepare all paths
          for (pathk in snowball.all$installation.path) {
            if (!file.exists(pathk))  {
              dir.create(pathk,recursive=TRUE,showWarnings = FALSE)
            }
            }
    
           
    #4 Add paths not already in the libpaths
             
        #Existing and new
          old <- .libPaths()
          new <- snowball.all$installation.path
          missing  <- new [!new %in% old]
        
        
          if (length(missing)>0)
          {
          .libPaths(c(missing, old))
          }
  
            #note: usually all paths will already be there, having been added in groundhog.library()
            #      but, if running install.snowball.list() in background, the libpath is empty, so we add them just for the installation
          
#----------------------------------------------------

           
    #5 Message # of packages to install
           n.tot    <- nrow(snowball.all)
           n.install<- nrow(storm.cran)+nrow(storm.mran)+nrow(storm.source)

           
           if (n.install>0)
             {
             message2("A total of ",n.tot," packages need to be loaded; ", n.install," of which need to be installed.")
            }
           
           # 
           if (n.install==0)
           {
           #base.library.snowball.list(snowball.list)  #Utils.R #47
           return(invisible(snowball.all))

           }
#------------------------------------------------------------------------ 
           
  #6 Install CRAN & MRAN binaries       
              
           
  #6.1 Make URLs
    r <- getOption("repos")
    cran.server <- r["CRAN"]
    os <- Sys.info()["sysname"]
    
    
    #Windows
     cran.url=mran.url=binaries.url=''
       if (os=='Windows') {
            if (nrow(storm.cran)>0)  cran.url<-paste0(cran.server , 'bin/windows/contrib/', get.r.majmin() , "/" , storm.cran$pkg_vrs ,".zip" )
            if (nrow(storm.mran)>0)  mran.url<-paste0("https://cran.microsoft.com/snapshot/", storm.mran$MRAN.date, "/bin/windows/contrib/" ,get.r.majmin(), "/" ,storm.mran$pkg_vrs ,".zip" )
            binaries.url <- c(cran.url, mran.url)
            }

    
    #MAC <<<Pending>>>
    
         if (os=='Darwin')
         {
           message('line 152 in new.groundhog.library(*), need to setup mac OS detection')
           exit()
         }
     
     
    if (cores>1) 
    {
    message1("    Groundhog installs packages in parallel to save time.") 
    message1("    To force sequential installation set `cores=1` in groundhog.library().")
    }

  #6.3 drop empty binaries ('')
       binaries.url<- binaries.url[binaries.url !=''] 
           
       #if any left, install them
         if (length(binaries.url)>0)
         {
         
  #6.4 Give message about number to install
      cran.time <-  ceiling(length(cran.url)/cores)*3    #say each binary in cran takes 3 seconds
      mran.time <-  ceiling(length(mran.url)/cores)*10   #say each binary in mran takes 10 seconds
      binary.time <- cran.time + mran.time + 4           #fixed cost of 4 seconds to install binaries
      now      <- format(Sys.time(), "%H:%M")
 
      message1("Will now attempt installing " , length(binaries.url) , " packages from binary files.")

      if (binary.time>=40) message1("This will take roughly ", binary.time," seconds (time now: ",now,").")
      if (binary.time<40)  message1("This will probably take less than 1 minute (time now: ",now,").")

          
  #6.5 Parallel processing
    if (cores>1)
    {

  #6.5 make the cluster
        cores1 <- min(cores,4) #prevent overburdening with multiple parallel request
        CL.1 <- parallel::makeCluster(getOption("cl.cores", cores1),outfile=installation_log_path)
        
        #Make all package functions available
         parallel::clusterExport(CL.1,
                    unclass(lsf.str(envir = asNamespace("groundhog"), all = TRUE)),
                    envir = as.environment(asNamespace("groundhog")))
                     
      
  #6.5 Parallel loop installing binaries
      #Run the loop
          res=parallel::parLapply(CL.1 , binaries.url , install.binary) #see install.on.R
                  
      #Kill the cluster
          parallel::stopCluster(CL.1)

    } #End if parallel processing
           
           
  #6.6 Serial processing
      if (cores==1) 
      {
        
        #loop installing
          for (urlk in binaries.url)
          {
          install.binary(urlk)   #see install.on.R
          }
      }
           
  } #End if binaries exist
#--------------------------
                      
 #7 Install Source        
    
                  
  #7.0 Any package not successfully installed from binary, gets to source
          
     #All storms in one data.frame
          storm.full <- rbind(storm.cran, storm.mran, storm.source)
       
      #See what's installed from snowstorm full     
            ip<-data.frame(installed.packages(c(storm.cran$installation.path, storm.mran$installation.path)))              
  
      #Everything not there, added to storm source
            #missing subset
              storm.cran.missing <- storm.cran[!storm.cran$pkg_vrs %in% paste0(ip$Package,"_",ip$Version),] 
              storm.mran.missing <- storm.mran[!storm.mran$pkg_vrs %in% paste0(ip$Package,"_",ip$Version),] 

              
            #Count missing, show warning if any
              n.missing <- nrow(storm.cran.missing)+nrow(storm.mran.missing)
              if (n.missing>0) {
                msg  = paste0("Groundhog says: There are ",n.missing," packages that did not install succesfully.\n",
                              "Will try to install from source files instead (a slower process)")
                message(msg)
           }
  
      #add missing to source                     
              storm.source <- rbind(storm.source, storm.cran.missing, storm.mran.missing)
            
  #7.1 Early return if nothing left to install
            if (nrow(storm.source)==0) return(invisible(snowball.all))
              
   #7.2 Get dependencies dep12 to make snowflakes            
              
          #start empty
            dep12=data.frame(pkg=character() , dep2=character())
                  
          #populate it 
            for (pkgk in storm.source$pkg)
                {
                if (!pkgk %in% dep12$pkg) 
                    {
                    dep12.k <- get.all.dependencies(pkgk, date)
                    dep12.k <- dep12.k [!dep12.k$pkg %in% dep12$pkg,]
                    dep12 <- rbind(dep12, dep12.k)
                    }
                  }
                      
            #Drop base packages
              dep12<-dep12[!dep12$pkg %in% base_pkg() & !dep12$dep2 %in% base_pkg(), ]
          
    #7.3 Break storm into independent snowflakes
            k=0
            snowflakes<-list()
            while (nrow(dep12) > 0) {
                
                #Counter
                  k <- k + 1
                
                #Find dependencies without dependencies  TRUE/FALSE vector
                  indep.rows <- !(dep12$dep2 %in% dep12$pkg)
                
                #Make snowflake #k
                  indepk <- unique(as.character(dep12$dep2[indep.rows]))
                  snowflakes[[k]] <- indepk
                  
                # Drop those rows from both
                  dep12 <- dep12[!indep.rows, ]
                  
                # Safety valve in case loop impossible to end
                  if (k == 50000) {
                    break
                }
              }
                
              #Add the requested packages, if they are not already in some snowflake
                snowflakes[[k+1]]<-pkg[!pkg %in% unlist(snowflakes)]
                
    #7.4 Drop already istalled packages from the snowflakes
            for (k in 1:length(snowflakes))
            {
              
              snowflakes[[k]] <-snowflakes[[k]] [snowflakes[[k]] %in% snowball.all$pkg[snowball.all$installed==FALSE]  ]
              
            }
                
           snowflakes <- snowflakes[lapply(snowflakes,length)>0]     
                
    #7.5 Make the URLs for downloading sources
        ap_source <- get.current.packages("source")
        storm.source$url<-''
              
          for (k in 1:nrow(storm.source))
          {
           if (storm.source$pkg_vrs[k] %in% ap_source$pkg_vrs) 
            {
            #If it is current, get from page with all sources
            storm.source$url[k] <- paste0(cran.server , "/src/contrib/" ,storm.source$pkg_vrs[k] , ".tar.gz")
            
           } else { 
             
          #If it is not current, use archive
            storm.source$url[k] <- paste0(cran.server , "/src/contrib/Archive/" ,storm.source$pkg[k] , "/" ,  storm.source$pkg_vrs[k] , ".tar.gz")
            
            } #End if not current
          }   #End loop looking for URL
                 
        
   #7.6 make the cluster
         
    #7.7 Loop over snowflakes 
           
     
            for (k in 1:length(snowflakes))
            {  
            CL.2 <- parallel::makeCluster(getOption("cl.cores", cores),outfile=installation_log_path)
          
            #Parallel loop installing source
             parallel::clusterExport(CL.2, 
                                  unclass(lsf.str(envir = asNamespace("groundhog"), all = T)),
                                   envir = as.environment(asNamespace("groundhog")))
                         
                              #exporting all function to cluster, solution from 
                              #https://stackoverflow.com/questions/67595111/r-package-design-how-to-export-internal-functions-to-a-cluster
       
            
            #Get the URL for all pkgs in snowflake 'k'
              source.url<-storm.source$url[storm.source$pkg %in% snowflakes[[k]] ]
                    
            #Sort them in decreasing installation time
              resort <- rank(-storm.source$installation.time[storm.source$pkg %in% snowflakes[[k]] ],ties='random')
              source.url <- source.url[order(resort)]
                    
            
            #Get the URL for all pkgs in snowflake 'k'
                source.path  <- storm.source$installation.path[storm.source$pkg %in% snowflakes[[k]] ]
                
            #Feedback
              message.batch.installation.feedback(source.url, storm.source, snowflakes,  k ,cores ) #utils 48

                
            #Install snowflake k
              parallel::parLapply(CL.2 , source.url , install.source)  #install.one.R has this function 'install.source 
            
             #Kill the cluster
               parallel::stopCluster(CL.2)   
              
              #Localize
              localize.snowball(storm.source [storm.source$pkg %in% snowflakes[[k]],])
         
                                   
            } #Loop installing snowflakes
              
         
            
         
          
        return(invisible(snowball.all))
              
      }