#Load all databases, optional parameter update.toc leads to downloading from groundhogr.com as wasaby as backup




load.cran.toc <- function(update.toc = FALSE) {
  
  #1. URL with rds files
    groundhogR.url <- "https://groundhogR.com/"
    wasabi.url     <- "https://s3.wasabisys.com/groundhog/"  
  
  
  #2. Local paths 
      gf <- get.groundhog.folder()

    #Ensure directory for groundhog exists
      dir.create(gf, showWarnings = FALSE, recursive = TRUE) 
  
    #Names of files
      files.rds = c('cran.toc.rds' , 'cran.times.rds' , 'missing.mran.dates.rds' )


  #3 Loop checking file exist, downloading if necessary, and assigning to .pkgenv
          for (rdsk in files.rds)
          {
          #See if file exists
            in.path <- file.exists(file.path(gf ,rdsk))                      #in groundhog.folder()
            #in.pkg  <-file.exists(system.file(rdsk, package = "groundhog"))  #in inst folder for groundhog
			in.pkg <- FALSE
          #Case 1: in.pkg but not in path, copy it locally (but not being updated, to avoid wasteful copy )
            if (in.path==FALSE & in.pkg==TRUE & update.toc==FALSE) {
              file.copy  (system.file(rdsk, package = "groundhog") , file.path(gf, rdsk))
              }
            
          #Case 2:  neither exists or if was asked for update, download
            in.path <- file.exists(file.path(gf ,rdsk))              #redo the check to catch errors

            
            if ((in.path==FALSE & in.pkg==FALSE) | update.toc==TRUE) {
            #Download groundhog
                dl <- try(utils::download.file(paste0(groundhogR.url, rdsk), file.path(gf, rdsk) , mode = "wb", method = "libcurl" ))
            
                #If download failed, try  wasabi's backup
                  if (dl!=0) {
                      dl2 <- try(utils::download.file(paste0(wasabi.url, rdsk), file.path(gf, rdsk), mode = "wb", method = "libcurl" ))
                      if (dl2!=0) stop('Error.\nGroundhog says: could not download "', rdsk, "'")
                    }
              } #End case 2
            
          #Read it locally, dropping the .rds part of the name ('cran.toc.rds' -> 'cran.toc')
              dfk <- gsub(".rds","",rdsk)
             .pkgenv[[dfk]] <- readRDS(file.path(gf,rdsk))
          }
    
  
 
  #5 Return TRUE
    invisible(TRUE)
}
