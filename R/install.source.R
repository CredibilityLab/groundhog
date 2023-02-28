

#This should be called install.snowball.source.parallel()
#Another could just be install.snowball.source.sequential()

#Both should be run before any loading


cores.minus.1=4
snowball=groundhog::get.snowball('rio','2022-07-01')
snowball$from='source'
snowball$installed=FALSE
date='2022-07-01'

#snowball here is really a combination of snowballs into a bigger one

  install.source = function(snowball,date,cores=cores.minus.1)
  {
    
        #0 Add all paths so that it is found when attempted
          .libPaths(unique(snowball$installation.path))
  
        #1 Keep only source  packages that are not yet installed
          snowball <- snowball[snowball$from  =='source' & snowball$installed==FALSE, ]
    
        #2 Get URL for installing each source
            
            #2.1 Source files available as current pkgs (not in the archive)
                ap_source <- get.current.packages("source")
            
            #2.2 Basis of URL
              repos     <- as.character(getOption("repos"))
               
            #2.3 Add url for source
              snowball$source_url <- ifelse(snowball$pkg_vrs %in% ap_source$pkg_vrs, 
                          paste0(repos , "/src/contrib/" ,snowball$pkg_vrs , ".tar.gz"),
                          paste0(repos , "/src/contrib/Archive/" ,snowball$pkg , "/" ,  snowball$pkg_vrs , ".tar.gz"))
          
              
        #3 Get snowflakes (snowball broken into parallel installable parts)
            snowflakes <- get.snowflakes (snowball, date) 
          
          
        #4 Install snowflakes
        
          #4.1 Loop over snowflakes
            for (k in 1:length(snowflakes))
            {  
              CL.2 <- parallel::makeCluster(getOption("cl.cores", cores.minus.1),outfile='c:/temp/install_log_2023_02_26')
          
          #4.2 Inner parallel loop with pkgs from source
              parallel::clusterExport(CL.2, 
                         unclass(lsf.str(envir = asNamespace("groundhog"), all = T)),
                         envir = as.environment(asNamespace("groundhog")))
                         
                #exporting all function to cluster, solution from 
                #https://stackoverflow.com/questions/67595111/r-package-design-how-to-export-internal-functions-to-a-cluster
              
          #4.3 Get snowball subset for this snowflake
              snowball.k <- snowball[snowball$pkg %in% snowflakes[[k]], ]
                    
          #4.4 Sort snowball.k by decreasing installation time
              snowball.k <- snowball.k [order(-snowball.k$installation.time) ,]
                
              #Feedback
              #message.batch.installation.feedback(source.url, storm.source, snowflakes,  k ,cores ) 
                #utils 48 in 'parallel groundhog' and utils #44

                #4.5 Install the snowflake
                    parallel::parLapply(CL.2 , snowball.k$source_url , install.one.source)  #install.one.R has this function 'install.source 
            
            #5  Kill the cluster
                parallel::stopCluster(CL.2)   
              
          #6 Localize so that future snowflakes find these packages
              localize.snowball(snowball [snowball$pkg %in% snowflakes[[k]],])
              
              
          } #Loop installing snowflakes
              
  }
            
           
      snowball=get.snowball('rio','2022-05-09')
      snowball$from='source'
      date='2022-05-09'
      install.source(snowball,'2022-05-09')