
#Obtain from gitlab the commit SHA and times to install specific versions

  update.gitlab_sha_time <- function(git_usr_pkg)
  {
  #0 Error message to be shown if something goes wrong
      error_msg <- paste0('groundhog says:\nunable to refresh commits page from Gitlab for "',git_usr_pkg,'" cannot proceed.')
   
  #1. Get the git package name (repo/pkg), dropping gitlab:: or http://
      usr_pkg <- get.usr_pkg(git_usr_pkg)
     
  #2. URL for gitlab main page for this package
      
      #The generic URL
        url_main.page <-paste0('https://gitlab.com/' , usr_pkg, '/commits' )
      
      
      #Read the page with robust attempt that gives warning
        main.page <- try_readLines(url_main.page,n=300)       #at most 300 lines for speed
      #Collapse to single row of text
        main.page <- paste0(main.page,collapse='')
      
  #3. Find URL to .atom list of commits
      #3.1  Find line of text with the link to the .atom formatted commits page 
          link.pos1 <- strpos1("atom+",main.page)[1]+8
          link.pos2 <- strpos1("format=atom",main.page)[1]+10
          commits.line  <- substr(main.page, link.pos1, link.pos2)
    
      #3.2 Find the link in that line
          commits.pos1  <- strpos1(usr_pkg, commits.line)
          commits.pos2  <- strpos1('=atom', commits.line)+10
          commits.url   <- paste0('https://gitlab.com/', substr(commits.line, commits.pos1, commits.pos2))
      
      
      #3.3 user feedback  
        message1('Groundhog is obtaining from Gitlab information on saved edits ("commits") for "' , usr_pkg , '"')
        message1('     --> from: "' , commits.url , '"')
   
      
          
  #4. Read all the commits into a single line 
      atom.page <- try_readLines(commits.url)
      atom.page <- paste0(atom.page,collapse='')
      
  #4 Get sha values
      #4.1 the shas (they are between  <id></id> in the atom.page )
        id.pos1 <- as.numeric(strpos1('<id>',  atom.page)) + 4
        id.pos2 <- as.numeric(strpos1('</id>', atom.page) -1)
        
      #4.2 #If the numbers do not match, or if there is only 1, exit()
        n1 <- length(id.pos1)
        n2 <- length(id.pos2)
        if (n1!=n2 | n1<1 ) {
          message(error_msg, '(error 1)')
          exit()
        }
        
      #4.3 Extract the url_with_sha
        url_with_sha <- substring(atom.page , id.pos1, id.pos2)
      
      #4.4 Just the sha
        #Function to get last part of a string
          get_last <-function(x) {
              split.x <- strsplit(x,"/")[[1]]
              n <- length(split.x)
              return(split.x[n])
          } 
          
        #Apply the fucntion to all rows obtained
          sha  <- sapply(url_with_sha,get_last,USE.NAMES = FALSE)
        
  #5 Read the timestamps 
    #5.1 Read
      time.pos1 <- as.numeric(strpos1('<updated>', atom.page)+9 )
      time.pos2 <- as.numeric(strpos1('</updated>', atom.page)-1 )
    
    #5.2 If the numbers do not match, or if there is only 1, exit()
        n3 <- length(time.pos1)
        n4 <- length(time.pos2)
        if (n3!=n4 | n3<1 | n1!=n3 ) {
          message(error_msg, '(error 2)')
          exit()
          }
      
    #5.3 Get the time-stamps
      timestamp <- substring(atom.page , time.pos1 , time.pos2)
    
    #5.4 Convert to unix time
      time <- timestamp.to.time(timestamp)
      
  #6 Combine time and sha          
      sha_time <- data.frame(time=time,sha=sha)
      
  #7 Drop if sha does not have 40 characters (the first commit is ot 'head')
      sha_time <- subset(sha_time, nchar(sha)==40)
      
  #8 If none left, error
      if (nrow(sha_time)==0) {
        message(error_msg, '(error 3)')
        exit()
      }
  
  #9 Save the sha_time  (in gitlab always get all commits, so know the first commit is hard-coded TRUE)
      #9.1 Make list
        sha_list <- list(time_saved=date.to.time(Sys.time()), 
                      know_first_commit = TRUE,  
                      sha_time = sha_time)
      #9.2 Set the path
        rds_path <- get.sha_time.rds_path(git_usr_pkg)
        
      #9.3 Save it
        #Ensure directory exists
          dir_path <- dirname(rds_path)
          if (!file.exists(dir_path)) {
            dir.create(dir_path,showWarnings = FALSE,recursive = TRUE)
          }
        #Save
          saveRDS(sha_list, rds_path)
            
    return(sha_time)
          
} #End of Function 
       

    #Example
    #update.gitlab_sha_time('https://gitlab.com/jimhester/covr')
       