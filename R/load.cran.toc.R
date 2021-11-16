# Load `cran.toc`
#
# Load a `data.frame` listing all CRAN packages, with their dependencies and
# publication date.

load.cran.toc <- function(update.toc = FALSE) {
  #0 Main Paths
      groundhogR.url <- "https://groundhogR.com/"
      groundhog.folder <- get.groundhog.folder()

  # 1 Ensure directory for groundhog exists
      dir.create(groundhog.folder, showWarnings = FALSE) # Create if does not exist

  # 2 Paths two databases (toc and times:
  # LOCAL
      toc.path <-   file.path(groundhog.folder, "cran.toc.rds")
      times.path <- file.path(groundhog.folder, "cran.times.rds")
      mran.path <-  file.path(groundhog.folder, "missing.mran.dates.rds")

      
  #3 UPDATE IF REQUESTED
    if (update.toc==TRUE)
    {
    #3.1 Download 
      #3.1.1 Agent for downloading
         agent <- paste0("R/", R.version$major , ".", R.version$minor, " (",.Platform$OS.type,")")
              # Create simpler standardized agent ID 
              #     (because Unix machines without r-studio give a user agent which is blocked by the groundhogr.com server)
              #     the simplified syntax used is "R/4.1.2 (windows)"
      
      #3.1.2 URL paths
        times.url <- paste0(groundhogR.url, "cran.times.rds")
        toc.url  <-  paste0(groundhogR.url, "cran.toc.rds")
        mran.url <-  paste0(groundhogR.url, "missing.mran.dates.rds")
        
      #3.1.3 Attempt downloading
    		dl_times <-  try(download.file(times.url , times.path, mode = "wb", method = "libcurl", headers = c("User-Agent" = agent)))
    		dl_toc   <-  try(download.file(toc.url   , toc.path,   mode = "wb", method = "libcurl", headers = c("User-Agent" = agent)))
    		dl_mran  <-  try(download.file(mran.url  , mran.path,  mode = "wb", method = "libcurl", headers = c("User-Agent" = agent)))
    }
      
      
  #4 Read the rds files from groundhog_folder, if they exist, else use files originally shipped with groundhog package 
    #4.1 TOC 
      if (file.exists(toc.path)) {
          cran.toc <- readRDS(toc.path)
          } else {
          cran.toc <- readRDS(system.file("cran.toc.rds", package = "groundhog"))
          }

    # 4.2 Times
      if (file.exists(times.path)) {
        cran.times <- readRDS(times.path)
      } else {
        cran.times <- readRDS(system.file("cran.times.rds", package = "groundhog"))
      }
  
    #4.3  MRAN missing dates
        if (file.exists(mran.path)) {
          missing.mran.dates <- readRDS(mran.path)
        } else {
          missing.mran.dates <- readRDS(system.file("missing.mran.dates.rds", package = "groundhog"))
        }

  #5 Move the rds files to package environment
    .pkgenv[["cran.toc"]] <- cran.toc
    .pkgenv[["cran.times"]] <- cran.times
    .pkgenv[["missing.mran.dates"]] <- missing.mran.dates
  
  
  invisible(TRUE)
}
