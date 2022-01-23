# Load `cran.toc`
#
# Loads three dataframes, when not available locally they are downloaded, or when update.toc=TRUE.
# cran.toc: contains all CRAN packages, with their dependencies and publication date.
# missing.mran.dates: dates when the microsoft backup of CRAN did not run and thus should not be used
# install.times:     how long it takes to install from source a given pkg

load.cran.toc <- function(update.toc = FALSE) {
  
  #1. URL with rds files
    groundhogR.url <- "https://groundhogR.com/"
    wasabi.url     <- "https://s3.wasabisys.com/groundhog/"  #backup where rds files are also saved
  
  
  #2. Local paths 
    groundhog.folder <- get.groundhog.folder()

    #Ensure directory for groundhog exists
      dir.create(groundhog.folder, showWarnings = FALSE) 
  
    #Paths to databases:
      toc.path   <- file.path(groundhog.folder, "cran.toc.rds")
      times.path <- file.path(groundhog.folder, "cran.times.rds")
      mran.path  <- file.path(groundhog.folder, "missing.mran.dates.rds")

  #3 Decide if local or updating
  if (update.toc == FALSE) {

        #3.1 Ensure copy exists in groundhog folder  (otherwise copy from pkg installation folder)
          if (!file.exists(toc.path))    file.copy  (system.file("cran.toc.rds", package = "groundhog") , toc.path)
          if (!file.exists(times.path))  file.copy  (system.file("cran.times.rds", package = "groundhog") , times.path)
          if (!file.exists(mran.path))   file.copy  (system.file("missing.mran.dates.rds", package = "groundhog") , mran.path)
    
        #3.2 UPDATE databases if needed
            } else {
      
        #3.3 Try groundhogr.com
            dl_times <- try(download.file(paste0(groundhogR.url, "cran.times.rds"),         times.path, mode = "wb", method = "libcurl" ))
            dl_toc   <- try(download.file(paste0(groundhogR.url,   "cran.toc.rds"),           toc.path, mode = "wb", method = "libcurl"))
            dl_mran  <- try(download.file(paste0(groundhogR.url,  "missing.mran.dates.rds"), mran.path, mode = "wb", method = "libcurl"))
    
      
        #3.4 If that did not work, try wasabi's backup
          if (dl_times!=0) dl_times <- try(download.file(paste0(wasabi.url, "cran.times.rds"),        times.path, mode = "wb", method = "libcurl" ))
          if (dl_toc!=0)   dl_toc   <- try(download.file(paste0(wasabi.url, "cran.toc.rds"),          toc.path, mode = "wb", method = "libcurl" ))
          if (dl_mran!=0)  dl_mran  <- try(download.file(paste0(wasabi.url, "missing.mran.dates.rds"), mran.path, mode = "wb", method = "libcurl" ))
            
            
        #3.5 Check download errors
             if (any(inherits(dl_times, "try-error"), inherits(dl_toc, "try-error"), inherits(dl_mran, "try-error"))) {
                return(invisible(FALSE))
              }
            
     } #End if-else (if updating)
    
  #4 Read RDS  files
      cran.times <- readRDS(times.path)
      cran.toc <- readRDS(toc.path)
      missing.mran.dates <- readRDS(mran.path)
      
  #5 Assign pkgnev
    .pkgenv[["cran.times"]]         <- cran.times
    .pkgenv[["cran.toc"]]           <- cran.toc
    .pkgenv[["missing.mran.dates"]] <- missing.mran.dates

 
 
  #7 Return TRUE
  invisible(TRUE)
}
