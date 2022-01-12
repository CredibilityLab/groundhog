# Load `cran.toc`
#
# Loads three dataframes, when not available locally they are downloaded, or when update.toc=TRUE.
# cran.toc: contains all CRAN packages, with their dependencies and publication date.
# missing.mran.dates: dates when the microsoft backup of CRAN did not run and thus should not be used
# install.times:     how long it takes to install from source a given pkg

load.cran.toc <- function(update.toc = FALSE) {
  
  #URL with rds files
    groundhogR.url <- "https://groundhogR.com/"
    wasabi.url     <- "https://s3.wasabisys.com/groundhog/"  #backup where rds files are also saved
  
  
  #Local groundhog
    groundhog.folder <- get.groundhog.folder()

  #Ensure directory for groundhog exists
    dir.create(groundhog.folder, showWarnings = FALSE) 

  #Paths to databases:
    toc.path <- file.path(groundhog.folder, "cran.toc.rds")
    times.path <- file.path(groundhog.folder, "cran.times.rds")
    mran.path <- file.path(groundhog.folder, "missing.mran.dates.rds")

  #JUST LOAD
  if (update.toc == FALSE) {

    # TOC
    if (file.exists(toc.path)) {
      cran.toc <- readRDS(toc.path)
    } else {
      cran.toc <- readRDS(system.file("cran.toc.rds", package = "groundhog"))
    }

    # Move the cran.toc outside the function space, to global environment
    .pkgenv[["cran.toc"]] <- cran.toc

    # Times
    if (file.exists(times.path)) {
      cran.times <- readRDS(times.path)
    } else {
      cran.times <- readRDS(system.file("cran.times.rds", package = "groundhog"))
    }

    .pkgenv[["cran.times"]] <- cran.times


    # MRAN missing dates
    if (file.exists(mran.path)) {
      missing.mran.dates <- readRDS(mran.path)
    } else {
      missing.mran.dates <- readRDS(system.file("missing.mran.dates.rds", package = "groundhog"))
    }

    .pkgenv[["missing.mran.dates"]] <- missing.mran.dates
    } else {
      
    #UPDATE FILE, THEN LOAD
      
    #If updating, try groundhogr.com
      dl_times <- try(download.file(paste0(groundhogR.url, "cran.times.rds"),         times.path, mode = "wb", method = "libcurl" ))
      dl_toc <- try(download.file(paste0(groundhogR.url,   "cran.toc.rds"),           toc.path, mode = "wb", method = "libcurl"))
      dl_mran <- try(download.file(paste0(groundhogR.url,  "missing.mran.dates.rds"), mran.path, mode = "wb", method = "libcurl"))


    #If that did not work, try wasabi's backup
      if (dl_times!=0) dl_times <- try(download.file(paste0(wasabi.url, "cran.times.rds"),        times.path, mode = "wb", method = "libcurl" ))
      if (dl_toc!=0)   dl_toc   <- try(download.file(paste0(wasabi.url, "cran.toc.rds"),          toc.path, mode = "wb", method = "libcurl" ))
      if (dl_mran!=0)  dl_mran  <- try(download.file(paste0(wasabi.url, "missing.mran.dates.rds"), mran.path, mode = "wb", method = "libcurl" ))
      
    #Read local files
      cran.times <- readRDS(times.path)
      cran.toc <- readRDS(toc.path)
      missing.mran.dates <- readRDS(mran.path)

    .pkgenv[["cran.times"]] <- cran.times
    .pkgenv[["cran.toc"]] <- cran.toc
    .pkgenv[["missing.mran.dates"]] <- missing.mran.dates

    if (any(inherits(dl_times, "try-error"), inherits(dl_toc, "try-error"), inherits(dl_mran, "try-error"))) {
      return(invisible(FALSE))
    }
  }

  invisible(TRUE)
}
