
  install.snowball <- function(snowball, date, cores=-1,skip.remotes=FALSE) {
   
      date=as.Date(date)
    
   
      
      
    #1 Drop repeat packages
      snowball<-snowball[!duplicated(snowball$pkg),]
      
      
    #2 Install all binaries
      snowball.binaries <- install.binaries(snowball,cores)
  
    #3 If any binary failed, change `from` in snowball to 'source'
      if (sum(snowball.binaries$success==FALSE) > 1) {
        pkg.failed <- snowball.binaries[snowball.binaries$success==FALSE, ]$pkg
        snowball$from <- ifelse(snowball$pkg %in% pkg.failed  ,
                                  'source', 
                                   snowball$from)
        }  
    
    #4 Install source
        install.source(snowball, date, cores)
        
     }