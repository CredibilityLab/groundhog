
  install.snowball <- function(snowball, date, cores=-1,skip.remotes=FALSE) {
   
      date=as.Date(date)
    
   
      
      
    #1 Drop repeat packages
      snowball<-snowball[!duplicated(snowball$pkg),]
      
      
    #2 Install all binaries
      snowball.binaries <- install.binaries(snowball,cores)

      
    #3 If any binary failed, change `from` in snowball to 'source'
      if (sum(snowball.binaries$success==FALSE) > 0) {
        pkg.failed <- snowball.binaries[snowball.binaries$success==FALSE, ]$pkg
        snowball$from <- ifelse(snowball$pkg %in% pkg.failed  ,
                                  'source', 
                                   snowball$from)
        }  
    
    #4 Install source
        install.source(snowball, date, cores)
        
    #5 Update groundhog cache incrementally after installation
        #Get installation paths for packages that were just installed
        if (nrow(snowball) > 0 && "installation.path" %in% names(snowball)) {
          #Get unique installation paths (these are the package directories like R-4.5/curl_6.2.3)
          installed_paths <- unique(snowball$installation.path)
          
          #Don't filter by file.exists() - compute.ip.for.paths() will handle missing paths
          #The path might not exist immediately after installation due to timing, but
          #we should still try to compute IP for it (it will return empty if path doesn't exist)
          if (length(installed_paths) > 0) {
            #Compute IP for these specific installation paths
            #installed.packages() can handle individual package directories
            new_ip <- compute.ip.for.paths(installed_paths)
            
            #Add to cache incrementally (this will add new packages and update existing ones)
            if (nrow(new_ip) > 0) {
              add.packages.to.ip.cache('groundhog', new_ip)
            }
          }
        }
        
     }