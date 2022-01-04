#Function which checks if local files for databases exists and downloads them if not
#or if the update=TRUE parameter is set.


  load.databases <- function(update=FALSE)
  {
  
  #1 Directories on servers where .rds files are saved
      groundhogR.url <- "https://groundhogR.com/"
      wasabi.url     <- "https://s3.wasabisys.com/groundhog/"  #backup where rds files are also saved
  
  #2 Get local paths
      groundhog.folder <- get.groundhog.folder()
      if (!file.exists(groundhog.folder)) dir.create(groundhog.folder, showWarnings = FALSE, recursive = TRUE) 
      
      toc.path   <- file.path(groundhog.folder, "cran.toc.rds")
      times.path <- file.path(groundhog.folder, "cran.times.rds")
      mran.path  <- file.path(groundhog.folder, "missing.mran.dates.rds")


  #3 Download if files are not present or if update was selected
      
          if (!file.exists(toc.path) | !file.exists(times.path) | !file.exists(mran.path) | update==TRUE) 
            {
            
          #Try groundhogr.com
            dl_times <- try(download.file(paste0(groundhogR.url, "cran.times.rds"),         times.path, mode = "wb", method = "libcurl" ))
            dl_toc <- try(download.file(paste0(groundhogR.url,   "cran.toc.rds"),           toc.path, mode = "wb", method = "libcurl"))
            dl_mran <- try(download.file(paste0(groundhogR.url,  "missing.mran.dates.rds"), mran.path, mode = "wb", method = "libcurl"))

          #Try in wasabi if needed
            if (inherits(dl_times,'try-error'))  dl_times <- try(download.file(paste0(wasabi.url, "cran.times.rds"),         times.path, mode = "wb", method = "libcurl" ))
            if (inherits(dl_toc,'try-error'))    dl_toc   <- try(download.file(paste0(wasabi.url, "cran.toc.rds"),           toc.path, mode = "wb", method = "libcurl" ))
            if (inherits(dl_mran,'try-error'))   dl_mran  <- try(download.file(paste0(wasabi.url, "missing.mran.dates.rds"), mran.path, mode = "wb", method = "libcurl" ))
          
          #note: download.file==0 when operation succeeds.
      
          #Die if any are still not downloaded
            if (any(inherits(dl_times, "try-error"), inherits(dl_toc, "try-error"), inherits(dl_mran, "try-error"))) {
              message2()
              message1("Unable to download needed files for groundhog. Please check if you are\n",
                       "connected to the internet. If after checking http://groundhogr.com/troubleshoot\n",
                       "the problem persist, please get in touch\nhttps://github.com/CredibilityLab/groundhog")
              message('\n >> error downloading groundhog databases.')
              return(invisible(FALSE))

            
          } #End if downloads failed
          } #End if downloads should be attempted
      
    #4 Load databases to environment
      .pkgenv[["cran.times"]]         <- readRDS(times.path)
      .pkgenv[["full_toc"]]           <- readRDS(toc.path)
      .pkgenv[["missing.mran.dates"]] <- readRDS(mran.path)

      
    return(invisible(TRUE))
  }