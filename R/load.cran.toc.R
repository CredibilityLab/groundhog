
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
      files.rds = c('cran.toc.rds' , 'cran.times.rds' )

    #Empty gran file so when linux checks whether it exists it does not check this one.
      gran.file.rds =''
      
    #Add gran.toc if mac or windows after version 3.1
      os <- get.os()
      if (os %in% c('windows','mac', 'mac_arm')) {
        
          #User has this R version  
            r.version    <- get.r.majmin()
            
          #Drop the period
            rv <- sub("\\.","",r.version)
  
          #rds with gran toc 
            gran.file.rds <- paste0(os, rv,'.rds')
            
          #use mac for mac_arm for R<4.14
            if (os=='mac_arm' & as.numeric(r.version)<4.1) {
                  gran.file.rds <- paste0('mac', rv,'.rds')
                }
            
          #Add gran.toc only if R=3.1 or newer
            if (as.numeric(r.version)>3.1) {
              files.rds = c(files.rds, gran.file.rds)
              }
          
      }#End os is mac or windows
      
  #3 Loop checking file exist, downloading if necessary, and assigning to .pkgenv
          for (rdsk in files.rds)
          {
          #See if file exists
            in.path <- file.exists(file.path(gf ,rdsk))                      #in groundhog.folder()
            in.pkg  <- file.exists(system.file(rdsk, package = "groundhog"))  #in inst folder for groundhog
          
          #Case 1: in.pkg but not in path, copy it locally (but not being updated, to avoid wasteful copy )
            if (in.path==FALSE & in.pkg==TRUE & update.toc==FALSE) {
              file.copy  (system.file(rdsk, package = "groundhog") , file.path(gf, rdsk))
              }
            
          #Case 2: in both but the package's is newer (this happens when updating groundhog version)
              if (in.path && in.pkg && update.toc==FALSE)
                {
                #Time of both files
                  time.path <- file.info(file.path(gf ,rdsk))$mtime
                  time.pkg <- file.info(system.file(rdsk, package = "groundhog"))$mtime
                

                #If pkg is newer, copy it
                if (time.path<time.pkg)
                  {
                  file.copy  (system.file(rdsk, package = "groundhog") , file.path(gf, rdsk), overwrite = TRUE)
                 } #End if package's version is newer
              }   #End if both files exist
              
          #Case 3:  neither exists or if was asked for update, download
            in.path <- file.exists(file.path(gf ,rdsk))              #redo the check to catch errors

            
            if ((in.path==FALSE & in.pkg==FALSE) | update.toc==TRUE) {
              
            #Download toc files
             #GET URL
                #cran.toc, times
                  if (rdsk != gran.file.rds) {
                    url.g <- paste0(groundhogR.url, rdsk)
                    url.w <- paste0(wasabi.url,     rdsk)
                  }
                
                #gran.toc 
                  if (rdsk == gran.file.rds) {
                    url.g <- paste0(groundhogR.url, "gran.toc/", rdsk)
                    url.w <- paste0("http://gran.groundhogr.com/toc/", rdsk)
  
                  }
                
              
              #R Version After 3.4
               if (getRversion()>"3.4") {
                dl <- try(utils::download.file(url.w , file.path(gf, rdsk) , mode = "wb", method = "libcurl" ))
            
                #If download failed, try  wasabi's backup
                  if (dl!=0) {
                      dl2 <- try(utils::download.file(url.g , file.path(gf, rdsk), mode = "wb", method = "libcurl" ))
                      if (dl2!=0) stop('Error.\nGroundhog says: could not download "', rdsk, "'")
                    } #End if download failed  from groundhogR.com
               } #ENd if version 3.4
              
               #R Version before 3.4
               if (getRversion()<"3.4") {
                dl <- try(utils::download.file(url.w, file.path(gf, rdsk) , mode = "wb" ))
            
                #If download failed, try  wasabi's backup
                  if (dl!=0) {
                      dl2 <- try(utils::download.file(url.g , file.path(gf, rdsk), mode = "wb" ))
                      if (dl2!=0) stop('Error.\nGroundhog says: could not download "', rdsk, "'")
                    } #End if download failed from groundhogR.com
               } #ENd if version 3.4
              
              } #End case 2
            
          #Read it locally, dropping the .rds part of the name ('cran.toc.rds' -> 'cran.toc')
              #CRAN and Times already have the name, simply drop .rds     
              if (rdsk != gran.file.rds) {
                 dfk <- gsub(".rds","",rdsk)
              } 

             #GRAN has an os specific name, let's change it to gran.toc
              if (rdsk == gran.file.rds) {
                 dfk <- 'gran.toc'
               } 
            
             .pkgenv[[dfk]] <- readRDS(file.path(gf,rdsk))
          }
    
  
 
  #5 Return TRUE
    invisible(TRUE)
}

