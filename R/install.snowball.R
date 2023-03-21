
  install.snowball <- function(snowball, date, cores,skip.remotes=FALSE) {
   
      date=as.Date(date)
    
    #0 Drop repeat packages
      snowball<-snowball[!duplicated(snowball$pkg),]
      
      
    #1 Install all binaries
      snowball.binaries <- install.binaries(snowball)
  
    #2 If any binary failed, change `from` in snowball to 'source'
      if (sum(snowball.binaries$success==FALSE) > 1) {
        pkg.failed <- snowball.binaries[snowball.binaries$success==FALSE, ]$pkg
        snowball$from <- ifelse(snowball$pkg %in% pkg.failed  ,
                                  'source', 
                                   snowball$from)
        }  
    
    #3 Install source
        install.source(snowball, date, cores)
        
        
     #4 update groundhog.installed.rds 
        
        #Which files were installed
          snowball.new <- snowball[snowball$installed==FALSE,]
        
        #If none, early return
          if (nrow(snowball.new)==0) return(invisible(snowball))
          
        #Make a df with pkg_vrs and time
            
            new.df <-data.frame(pkg_vrs = paste0(snowball.new$pkg ,"_", snowball.new$vrs),
                               installation.path =snowball.new$installation.path, 
                               time    = as.numeric(Sys.time()))
      
        #Load the existing one  
            old.df <-read.local.rds("groundhog.installed.rds")
          
        #Update
          if (nrow(old.df)==0) updated.df <- new.df
          if (nrow(old.df)> 0) updated.df <- rbind(old.df , new.df)
          
        #Save it
          save.local.rds(updated.df , 'groundhog.installed.rds')

        
    
        }