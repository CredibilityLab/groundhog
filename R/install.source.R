


  install.source <- function(snowball,date,cores)
  {
    
    #If default -1, then go with total -2
    if (cores==-1) cores <- parallel::detectCores()-2
    
    #0 Add all paths so that it is found when attempted
      .libPaths(unique(snowball$installation.path))

    #1 Keep only source  packages that are not yet installed
        snowball <- snowball[snowball$from  =='source' & snowball$installed==FALSE, ]
        
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
      
          
        #2.4 message
          
          message1("Will now install ",nrow(snowball), " packages from source")
    
   #3 Parallel installation
      if (cores>1)
      {
        
        
          #3.0 Log path
              log_cores_path <- paste0(get.groundhog.folder(),"/batch_installation_console.txt")
              log_path         <- paste0(get.groundhog.folder(),"/batch_installation_log.txt")
              dir.create(dirname(log_path),recursive = TRUE,showWarnings = FALSE)
            
              #Clear the log path
              unlink(log_path)
              
          #3.0.5 Message  
              message1("Will rely on `",cores, "` core processors for faster in-parallel installation")
              message1("To force sequential installation add option `cores=1` to your groundhog.library() call")
        
          #3.1 Get snowflakes (snowball broken into parallel installable parts)
              snowflakes <- get.snowflakes (snowball, date) #get.snowflakes.R
            
            
          #3.2 Loop over snowflakes
              for (k in 1:length(snowflakes))
              {  
                cluster_id <- parallel::makeCluster(getOption("cl.cores", min(cores,length(snowflakes[[k]])),outfile=log_cores_path)
            
            #3.3 Inner parallel loop with pkgs from source
                parallel::clusterExport(cluster_id, 
                           unclass(lsf.str(envir = asNamespace("groundhog"), all = TRUE)),
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
                  parallel::parLapply(cluster_id , snowball.k$source_url , install.one.source)  #install.one.R has this function 'install.source 
              
            #3.7  Kill the cluster
                  parallel::stopCluster(cluster_id)   
                
            #3.7 Localize so that future snowflakes find these packages
                localize.snowball(snowball [snowball$pkg %in% snowflakes[[k]],])
                
                
            } #Loop installing snowflakes
              
          } #End parallel installation
    
          
          
    #4 Sequential installation
      if (cores==1)
      {
          #4.1 Message  
              message1("Because you set cores=1, will install sequentially")
              message1("For faster installation drop the `cores=1` option in your groundhog.library() call")
        
          #4.2 Start clock for install feedback
              start.time=Sys.time()
            
          #4.3 Loop over individual packages
              for (k in 1:length(snowball$source_url))
              { 
              #Feedback
                installation.feedback(k, date, snowball, start.time) 

              #Install
                install.one.source (snowball$source_url[k])       
              
              #localize
                localize.snowball(snowball [k,])
              
              }  #End install loop
      
    } #End sequential install
          
          
  }#End function install.source()