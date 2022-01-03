
    
#Obtain the sha associated with teh commite for a remote (gitlab, github)
    
    get.sha <- function(git_usr_pkg, groundhog.day)
    {
        #1 Local parameters
          #1.1 Groundhog time (Unix time for 1st sec of next day)
            groundhog.time <-  as.numeric(as.POSIXct(as.Date(groundhog.day)+1, format="%Y-%m-%d"))
      
          #1.2 remote_id
            remote_id <- get.remote_id(git_usr_pkg)
            
          #1.3 usr_pkg
            usr_pkg <- get.usr_pkg(git_usr_pkg)
   
          #1.4
            rds_path <- get.sha_time.rds_path(git_usr_pkg)         
        
            
  #2 Initiate empty sha_time data.frame
        sha_time <-data.frame(sha=character(0),time=numeric(0))
        rds.exists <- 0
        know_first_commit <- FALSE  #Dummy to record if we need to go back and check again for older commits

  #3 Read existing file if any 
      if (file.exists(rds_path)) {
      
        
      #3.1 Read
          sha_list <- readRDS(rds_path)
          sha_time         <- sha_list$sha_time            #The data_frame with commits sha and time
          time_saved       <- sha_list$time_saved           #When was the rds file saved
          know_first_commit <- sha_list$know_first_commit  #First commit ever if we know it
          
          
      #3.2 If we sandwich the groundhog.time, return groundhog.sha
          if (time_saved  >= groundhog.time & groundhog.time >= min(sha_time$time))  {
             
              #Time of last commit which comes before 1st second of day after groundhog.day
                 time.k = max(sha_time$time[sha_time$time <= groundhog.time])
                 
              #Find the sha associated with it, there could be ties, in which case we go with the first of them [1] (most recent)
                 groundhog.sha <- sha_time[which(sha_time$time==time.k),]$sha[1]
                 
              #Early return
                 return(groundhog.sha)
                 
            }  #End if we have found it already
          
     
    } #End if rds exists
            
                   
  #4 Try to update the sha_time file
      if (remote_id=='github') sha_time <- update.github_sha_time(git_usr_pkg , groundhog.day)
      if (remote_id=='gitlab') sha_time <- update.gitlab_sha_time(git_usr_pkg)
        
  #5 Try again to return a sha
      
        if (min(sha_time$time) <= groundhog.time) {
             #Find the biggest time that is smaller than groundhog.time
                  time.k = max(sha_time$time[sha_time$time<=groundhog.time])
                  
              #Find the sha associated with it, there could be ties, in which case we go with the first of them [1] (most recent)
                  groundhog.sha <- sha_time[which(sha_time$time==time.k),]$sha[1]
              #Early return
                  return(groundhog.sha)
            }  #End if we have found it already
          
  #6 if we did not find it
          if (min(sha_time$time)>groundhog.time) {
            date0=as.Date(as.POSIXct(min(sha_time$time), origin="1970-01-01"))
            exit("\nThe groundhog day you entered '",groundhog.day, "', is earlier than\n",
              "when '",usr_pkg,"' was  first available on GitHub ('first commit: ", date0,"')") 
           return(invisible(FALSE))

           }
    }#End function         
            
    
    

    
