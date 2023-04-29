#New function to load packages

  load.cran.toc <-function(update.toc = FALSE) {
    
    #0 Get filename for gran
      gran.filename <- get.gran.filename()
  
          #note: this looks something like windows42.rds. For <3.1 and for unix, it is ""  
    
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
          cran.toc.URL.w   <- "https://s3.wasabisys.com/groundhog/cran.toc.rds"  
          
       #3.2 cran.toc
          cran.times.URL.g <- "https://groundhogR.com/cran.times.rds"
          cran.times.URL.w <- "https://s3.wasabisys.com/groundhog/cran.times.rds"  
 
       #3.3 GRAN
          gran.URL.g      <- paste0("https://groundhogR.com/gran.toc/",     gran.filename)
          gran.URL.w      <- paste0("https://gran.groundhogR.com/gran.toc/",gran.filename)  
          
    #4 Local paths
          gf <- get.groundhog.folder()
          cran.toc.path   <- file.path(gf , 'cran.toc.rds')    
          cran.times.path <- file.path(gf , 'cran.times.rds')    
          gran.toc.path   <- file.path(gf ,  gran.filename)    
          
    #5 Download if they do not exist or if we are updating
          #CRAN TOC
            if (!file.exists(cran.toc.path) | update.toc==TRUE) {
                download.toc(cran.toc.URL.w , cran.toc.URL.g,  cran.toc.path) 
              
                  #`download.toc(url1, url2, path) -----  Utils.R #68, tries URL1, then URl2 upon failure, and saves to path
            }
            
          #CRAN TIMES
            if (!file.exists(cran.times.path) | update.toc==TRUE) {
              download.toc(cran.times.URL.w , cran.times.URL.g,  cran.times.path)
            }
            
          #GRAN
            if (gran.filename!='')
              {
              if (!file.exists(gran.toc.path) | update.toc==TRUE) {
              download.toc(gran.URL.w , gran.URL.g,  gran.toc.path)
                 }  #End download if it dooes not exist locally
                 } #End check if GRAN exist for this R version

    #6 Load them
        .pkgenv[['cran.toc']]   <- readRDS(cran.toc.path)
        .pkgenv[['cran.times']] <- readRDS(cran.times.path)
      
      #GRAN if it exists  
      if (gran.filename!='') {
        .pkgenv[['gran.toc']]   <- readRDS(gran.toc.path)
      }
  }
  