#New function to load packages

  load.cran.toc <-function(update.toc = FALSE) {
    
    #0 Get filename for gran
      gran.filename <- get.gran.filename()
  
          #note: this looks something like windows42.rds. For <3.1 and for unix, it is ""  
          #Utils #67
    
    #1 Files to load cran.toc, cran.times and gran.toc if it exists
        files.rds = c('cran.toc.rds' , 'cran.times.rds' )
        if (gran.filename!='') files.rds <- c(files.rds, gran.filename)

    #2 Early return if all exist and update=FALSE
        if (!is.null(.pkgenv[['cran.toc']]) &                        #we got CRAN toc
            !is.null(.pkgenv[['cran.times']]) &                      #we got cran times
            (!is.null(.pkgenv[['gran.toc']]) | gran.filename=='') &   #we got GRAN or it does not exist    
            update.toc==FALSE)                                           #we are not updating
          {
          return(invisible(TRUE)) 
          }
            
    #3 URLs
        #3.1 cran.toc
          cran.toc.URL.g   <- "https://groundhogR.com/cran.toc.rds"
          cran.toc.URL.w   <- "http://s3.wasabisys.com/groundhog/cran.toc.rds"  
          
       #3.2 cran.toc
          cran.times.URL.g <- "https://groundhogR.com/cran.times.rds"
          cran.times.URL.w <- "http://s3.wasabisys.com/groundhog/cran.times.rds"  
 
       #3.3 GRAN
          gran.URL.g      <- paste0("https://groundhogR.com/gran.toc/",     gran.filename)
          gran.URL.w      <- paste0("https://gran.groundhogr.com/toc/",gran.filename)  
          
    #4 Local paths
          gf <- get.groundhog.folder()
          cran.toc.path   <- file.path(gf , 'cran.toc.rds')    
          cran.times.path <- file.path(gf , 'cran.times.rds')    
          gran.toc.path   <- file.path(gf ,  gran.filename)    
          
    #5 Download if they do not exist or if we are updating
          #CRAN TOC
            if (!file.exists(cran.toc.path) | update.toc==TRUE) {
              message1("Downloading database with information for all CRAN packages ever published")
                download.toc(cran.toc.URL.g , cran.toc.URL.w,  cran.toc.path) 
              
                  #`download.toc(url1, url2, path) -----  Utils.R #68, tries URL1, then URl2 upon failure, and saves to path
            }
            
          #CRAN TIMES
            if (!file.exists(cran.times.path) | update.toc==TRUE) {
              message1("Downloading database with installation times for all source packages on CRAN")

              download.toc(cran.times.URL.g , cran.times.URL.w,  cran.times.path)
            }
            
          #GRAN
            if (gran.filename!='')
              {
              if (!file.exists(gran.toc.path) | update.toc==TRUE) {
                  message1("Downloading database will URLs for relevant binaries on GRAN")

                download.toc(gran.URL.g , gran.URL.w,  gran.toc.path)
                 }  #End download if it dooes not exist locally
                 } #End check if GRAN exist for this R version

    #6 Load them
        .pkgenv[['cran.toc']]   <- try(readRDS(cran.toc.path),silent=TRUE)
        .pkgenv[['cran.times']] <- try(readRDS(cran.times.path),silent=TRUE)
      
      #GRAN if it exists  
      if (gran.filename!='') {
        .pkgenv[['gran.toc']]   <- try(readRDS(gran.toc.path),silent=TRUE)
      }
        
    #7 Verify RDS files can be read
        #7.1 cran toc
          if (as.character(class(.pkgenv[['cran.toc']] ))=='try-error') {
              message1("groundhog says: the file cran.toc.rds seems to be corrupted, will update it.")
              download.toc(cran.toc.URL.g , cran.toc.URL.w,  cran.toc.path) 
              .pkgenv[['cran.toc']]   <- try(readRDS(cran.toc.path))
              } 
          
        #7.2 cran times
          if (as.character(class(.pkgenv[['cran.times']] ))=='try-error') {
              message1("groundhog says: the file cran.times.rds seems to be corrupted, will update it.")
              download.toc(cran.times.URL.g , cran.times.URL.w,  cran.times.path)
             .pkgenv[['cran.times']]   <- try(readRDS(cran.times.path))
            } 
        
        #7.3 GRAN toc
        
        if (gran.filename!='' && as.character(class(.pkgenv[['gran.toc']] ))=='try-error') {
            message1("groundhog says: the file gran.times.rds seems to be corrupted, will update it.")
            download.toc(gran.URL.g , gran.URL.w,  gran.toc.path)
           .pkgenv[['gran.toc']]   <- try(readRDS(gran.toc.path))
           }  
        
       
        #7.4 Check again 
          if (as.character(class(.pkgenv[['cran.toc']] ))=='try-error')   gstop("Could not read 'cran.toc.rds")
          if (as.character(class(.pkgenv[['cran.times']] ))=='try-error') gstop("Could not read 'cran.times.rds")
          if (as.character(class(.pkgenv[['gran.toc']] ))=='try-error')   gstop("Could not read 'gran.toc.rds")
          
       #7.5 delete cache if update=true
          if (update.toc==TRUE) {
            
            cache_path=get.cache_path()
            if (file.exists(cache_path)) unlink(cache_path)
            
          }
         
  }
  