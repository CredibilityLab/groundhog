
#Update the sha_time file for a github pacakge, visits the API for github and gets commit times and sha for specific files
#
#Function 1: reads a single page of output (JSON)
#Function 2: loop over 1 until we obtain the commit we need

#--------------------------------------------------------------------------\\

#Function 1 - Read 1 page of github commits
       import.github_sha_time.k <-function(link) 
        {
        #1. Read the page
            json=tryCatch(fromJSON(link), error = function(e) {
                  exit('groundhog says:\nunable to connect to GitHub to obtain commits information using the URL: "',link, '"')
                  })
            
        #2. Extract two variables, sha and time
          sha_time.k <- data.frame(
                time = timestamp.to.time(json$commit$committer$date),
                sha  = json$sha
                )
          
        #3. Output the dataframe
           return(sha_time.k)
       }
         
       
#--------------------------------------------------------------------------\\
 
#Function 2: loop over Function 1 until we obtain the commit we need or learn it does not exist
    #Note: groundhog.day  is included to guide how long to search. Github's API outputs in batches of 100 and it is a 
    #bit slow to process with jsonlite, so we do not get older sha's than necessary, stop when we read groundhog.time
    
      update.github_sha_time <- function(pkg, groundhog.day, remote_id='github', usr)
      {
      
      #0 Set local parameters
        #0.1 Groundhog time (Unix time for 1st sec of next day)
          groundhog.time <-  as.numeric(as.POSIXct(as.Date(groundhog.day)+1, format="%Y-%m-%d"))
        
        #0.2 Shorter vars
            usr_pkg     <- paste0(usr , '/' , pkg)
            git_usr_pkg <- paste0(remote_id, "::", usr , '/' , pkg)
        
      #1 Path to .rds file 
          rds_path <- get.sha_time.rds_path(pkg, remote_id, usr)         
          
      #2 Ensure directory exists
         if (!file.exists(dirname(rds_path))) dir.create(dirname(rds_path),showWarnings = FALSE,recursive=TRUE)
          
      #3 Initiate empty sha_time data.frame
          #3.0 Empty data_frame
            sha_time <-data.frame(sha=character(0),time=numeric(0))
          
          #3.1 Assume rds does not exist
            rds.exists <- FALSE
          
          #3.2 Don't know the first commit
            know_first_commit <- FALSE  
          

      #4 Read existing sha_time file if any if the date_range works, output it 
          if (file.exists(rds_path)) {
            
          #4.0 Read the list,
                sha_list <- readRDS(rds_path)
                sha_time         <- sha_list$sha_time           #1. The data_frame with commits sha and time
                time_saved       <- sha_list$time_saved         #2. When was the rds file saved
                know_first_commit <- sha_list$know_first_commit #3. TRUE/FALSE whether first commit ever is known (in GitHub results are shown 100 at a time, so we may not have already collected the first commit)
                rds.exists <- TRUE
                
          #4.1 If we sandwich the groundhog.time, already have the sha_time file we need
                if (time_saved  >= groundhog.time & groundhog.time >= min(sha_time$time))  {
                   return(sha_time)
                }
                
  
      #<NOTE> if we get passed 4.1 it means our sha_time file, if any, does not sandwich the groundhog day
            
      #5 Early return if we know already that the date is too early    
          if (know_first_commit==TRUE && min(sha_time$time)>groundhog.time)
          {
            date0=as.Date(as.POSIXct(min(sha_time$time), origin="1970-01-01"))
              message("\nThe groundhog day you entered '", groundhog.day, "', is earlier than\n",
              "when '", usr_pkg , "' was first available on " , remote_id , 
              " (first commit: '", date0,"').\n\n",
              "This conclusion is based on data downloaded from ", remote_id, " in the past, \n",
              "if you believe it is incorrect, you can refresh that information by running:\n",
              "refresh_commits('" , git_usr_pkg , "')")
              exit()
          } #End if known first commit
                
          } #End if file already exists
           
           
      #6.0 Update sha_time
        github_url = paste0('https://api.github.com/repos/' , usr_pkg , '/commits?per_page=100&sha=')
    
      #7 Ensure jsonlite is available
          if (!"jsonlite" %in% .packages()) {
            message1("The package 'jsonlite' is needed in order to get information from GitHub.\nWill load via groundhog")
            groundhog.library('jsonlite' , groundhog.day)
            }
        
      #8 User feedback 
        message1('Groundhog is obtaining from GitHub information on saved edits ("commits") for "' , usr_pkg , '"')
        message1('     --> from: "' , github_url , '"')
        
      #9 Loop reading commit times
        for (k in 1:5000)  #Exit with break, 5000 is the limit number of pages to read
        {
          
        #9.1 Start on first page with &sha='' unless we are looking for older than oldest date on file
          if (k==1)   sha_start <- ''
          if (k==1 & (rds.exists==TRUE && max(sha_time$time) < groundhog.time)) sha_start <- sha_time[nrow(sha_time),]$sha

        #9.2 After k=1 use the last one in most recent search
          if (k>1) sha_start <- sha_time.k[nrow(sha_time.k),]$sha
          
        #9.3 Add sha_start to URL
           github_url.k <- paste0(github_url, sha_start)
           
        #9.4 Get the new commits and times
          sha_time.k <- import.github_sha_time.k(github_url.k)
       
        #9.5 Add to existing sha_time
          sha_time<- rbind(sha_time, sha_time.k)
          
          
        #9.7 Compute # of rows and minimum time to see if we end the loop
           #if there is one result, it is the lats
              if (nrow(sha_time.k)==1) {
                  know_first_commit <- TRUE   #Make note we came to the first commit
                  break                       #Break the loop
                }
          
           #If max> groundhog> min, we found it
              if (min(sha_time$time) < groundhog.time & max(sha_time$time)>groundhog.time) break 
          
           #If  groundhog>max -->no changes between our most recent record and groundhog day
              if (max(sha_time$time) < groundhog.time) break
          
          
        #9.6 Feedback
          message1('      ...',(k-1)*100 + nrow(sha_time.k),' commits processed') #100 commits from past pages and the length of this sha_time
        
        }#ENd of for 1:5000 loop
       
  
      #5 Save rds file with list of obtained results
        sha_list <- list('time_saved'=date.to.time(Sys.time()),  #Time when rds was saved (more robust than metadata)
                         'know_first_commit'=know_first_commit,  #TRUE/FALSE for whether we have gotten to the first commit
                         'sha_time'=sha_time)                    #Data frame with commits
       
         saveRDS(sha_list, rds_path, version=2)                 #Version 2 for backwards compatibility with earlier R
          
      #6 Return sha_time
         return(sha_time)
         
         
    } #End of function
         
      