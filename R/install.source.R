

  install.source <- function(snowball,date,cores)
  {
     
     
    #0 Add all paths so that it is found when attempted
      .libPaths(unique(snowball$installation.path))
    
    #0.5 Parallel installation if more than 1 core and more than 1 package
      batches.failed <- 0 #flag turns to 1 if batches attempted and fail                      


    #1 Keep only source  packages that are not yet installed
        snowball <- snowball[snowball$from  %in% c('source', 'github','gitlab') & snowball$installed==FALSE, ]
        
    #1.5 early return if nothing to install
          snowball$success = snowball$installed
          if (nrow(snowball)==0) return(snowball)
        
    #2 Get URL for installing each source
        
        #2.1 Source files available as current pkgs (not in the archive)
            ap_source <- get.current.packages("source")
        
        #2.2 Basis of URL
           repos <- as.character(getOption("repos"))
           
        #2.3 Add url for source
          snowball$source_url <- ifelse(snowball$pkg_vrs %in% ap_source$pkg_vrs, 
                      paste0(repos , "/src/contrib/" ,snowball$pkg_vrs , ".tar.gz"),
                      paste0(repos , "/src/contrib/Archive/" ,snowball$pkg , "/" ,  snowball$pkg_vrs , ".tar.gz"))
      
        #2.4 Modify default URL if a package is remote
          
          #Check if snowball includes remote packages (it would have the column usr & sha)
            if ("sha" %in% names(snowball))
            {
              #Find the remotes and instead of URL provide info remote::usr_pkg to install from clone
                for (k in 1:nrow(snowball))
                {
                
                #If a sha is found, it's remote
                  if (!is.na(snowball$sha[k]))
                  {
                    #We figure out github vs gitlab from installation path  
                      remote_id <- ''
                      if  (regexpr('/_github/', snowball$installation.path[k])[[1]] > 0) remote_id <- 'github'
                      if  (regexpr('/_gitlab/', snowball$installation.path[k])[[1]] > 0) remote_id <- 'gitlab'
                      
                    #Then provide the 'url' (a ";;" separated string with all the fields needed to provide clone installation instructions)
                      snowball$source_url <- ifelse(!is.na(snowball$sha), 
                                                  paste0("remote::",remote_id,"::",snowball$usr,"::",snowball$pkg,"::",date,"::",snowball$sha),
                                                  snowball$source_url)
                                                  
                      #Note: this modified URL is then read by install.one.source (install.one.R) to install from clone  
                      
                  } #End if sha found
                }   #End loop remote
            }       #End if one remote at least 
          

        #2.5 Start message
          message2("Will now install ",nrow(snowball), " packages from source")
    

      if (cores>1 & nrow(snowball)>1)
      {
        
          #3.0 Log path
              log_cores_path <- paste0(get.groundhog.folder(),"/batch_installation_console.txt")
              log_path         <- paste0(get.groundhog.folder(),"/batch_installation_log.txt")
              dir.create(dirname(log_path),recursive = TRUE,showWarnings = FALSE)
            
              #Clear the log path
              unlink(log_path)
              
          #3.0.5 Message  
              message1("Will rely on `",cores, "` core processors for faster in-parallel installation")
              message1("To force sequential installation set option `cores=1` in groundhog.library() ")
        
          #3.1 Get snowflakes (snowball broken into parallel installable parts)
              snowflakes <- get.snowflakes (snowball, date) #get.snowflakes.R
            
            
          #3.2 Loop over snowflakes
              
              for (k in 1:length(snowflakes))
              {  
                cluster_id <- parallel::makeCluster(getOption("cl.cores", min(cores,length(snowflakes[[k]]))),outfile=log_cores_path)
            
            #3.3 Inner parallel loop with pkgs from source
                parallel::clusterExport(cluster_id, 
                           unclass(utils::lsf.str(envir = asNamespace("groundhog"), all = TRUE)),
                           envir = as.environment(asNamespace("groundhog")))
                           
                  #exporting all function to cluster, solution from 
                  #https://stackoverflow.com/questions/67595111/r-package-design-how-to-export-internal-functions-to-a-cluster
                
            #3.4 Get snowball subset for this snowflake
                snowball.k <- snowball[snowball$pkg %in% snowflakes[[k]], ]
                      
            #3.5 Sort snowball.k by decreasing installation time
                #Feedback
                  message.batch.installation.feedback(snowball,snowflakes,k,cores) #message.batch.installation.feedback.R
                  
                  
                  #utils 48 in 'parallel groundhog' and utils #44
      
            #3.6 Install the snowflake
                  parallel::parLapply(cluster_id , snowball.k$source_url , install.one)  #install.one.R has this function 'install.source 
              
            #3.7  Kill the cluster
                  parallel::stopCluster(cluster_id)   
                  
            #3.8 Check that everything was installed.
                  ip <- data.frame(utils::installed.packages(),stringsAsFactors = FALSE,row.names = NULL)
                  ip$pkg_vrs = paste0(ip$Package,"_",ip$Version)
                  snowflake.pkg_vrs <- snowball$pkg_vrs[snowball$pkg %in% snowflakes[[k]] ]
                  missing.pkg_vrs <-  snowflake.pkg_vrs[!snowflake.pkg_vrs %in% ip$pkg_vrs]
                  
                    
                  #Localize pkgs in snowball that succeeded to be installed 
                   localize.snowball(snowball [(snowball$pkg %in% snowflakes[[k]]) & 
                                               (snowball$pkg_vrs %in% ip$pkg_vrs), ])
                  
                   
                
            #3.9 If it was not, try sequentially
                  if (length(missing.pkg_vrs)>0) {
                    
            
                    
                    #Message end batch installation
                      msg=paste("Installation of ",pasteQC(missing.pkg_vrs)," failed.\n",
                              "Will attempt installing sequentially")
                    
                      message(msg)
                    
                    #Flag that batch installation failed
                      batches.failed <- 1
                    
                    #recheck if packages are installed or not
                      snowball$installed <- snowball$pkg_vrs %in% ip$pkg_vrs
                    
                    break
                  }
                  
                  
                
            } #Loop installing snowflakes
              
          } #End parallel installation
    
          
          
    #4 Sequential installation
      if (cores==1 | nrow(snowball)==1 | batches.failed==1)
      {
          
          #4.2 Start clock for install feedback
              start.time=Sys.time()
            
          #4.3 Loop over individual packages
              for (k in 1:length(snowball$source_url))
              { 
              #Feedback
                installation.feedback(k, date, snowball, start.time) 

              #Install
                install.one (snowball$source_url[k])       
              
                
              #Verify installation
                  ip <- data.frame(utils::installed.packages(),stringsAsFactors = FALSE,row.names = NULL)
                  ip$pkg_vrs = paste0(ip$Package,"_",ip$Version)
                  if (!snowball$pkg_vrs[k] %in% ip$pkg_vrs) {
                    msg=paste0("The package ",snowball$pkg_vrs[k]," failed to install.")
                    
                    #r tools?
                     if (get.os()=='windows')
                      {
                       make<-Sys.which("make")
                        if (nchar(make)<2) {
                          paste0(msg,"\n",
                               "It seems you do not have R Tools installed which may explain the problem.\n",
                               "Check out https://groundhogr.com/rtools/")
                          } #End no R Tools
                          }#End windows
                        
                    
                    gstop(msg)
                    
                  }
         
              
              #localize
                localize.snowball(snowball [k,])
              
              }  #End install loop
      
    } #End sequential install
          
          
  }#End function install.source()