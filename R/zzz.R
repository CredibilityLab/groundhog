.mismatch.warning <- new.env(parent = emptyenv())

.pkgenv <- new.env(parent = emptyenv())

#'
.onLoad <- function(libname, pkgname) {
  .pkgenv[["supportsANSI"]] <- Sys.getenv("TERM") %in% c("xterm-color", "xterm-256color", "screen", "screen-256color")
  
  
  
  #Message shown when loading the package
      #Generate suggested date
        r.using.full <- get.rversion() # Get current
        r.using.major <- R.version$major
        r.using.minor <- strsplit(R.version$minor, "\\.")[[1]][1]
        R.toc <- toc("R") # Get R toc
        R_vrs <- grep(paste0("^", r.using.major, ".", r.using.minor), R.toc$Version, value = TRUE)
        R1 <- R_vrs[1]
        R.date <- subset(R.toc,Version==R1)$Published
        suggested.date <- min(R.date+20 , Sys.Date()-2)
    
    message2("groundhog says [using:'R-" , r.using.full , "']") 
    message1(
            "Packages downloaded with groundhog are saved to '",get.groundhog.folder(),"'\n",
            "You may change that with: set.groundhog.folder('<path>')\n\n"
            )
           
     }

#Default parameters
  #Dependencies loaded for version of R being used
    .pkgenv$current.deps=c("Rcpp", "RcppArmadillo", "BH", "RcppEigen", "StanHeaders", 
                           "RcppParallel", "RcppProgress")

    
#' @importFrom utils packageVersion compareVersion
.onAttach <- function(libname, pkgname) {

  groundhog.version_cran <- tryCatch(
    as.character(readLines("http://groundhogr.com/groundhog_version.txt")),
    warning = function(w) NULL,
    error = function(e) NULL
  )

  # isTRUE() is necessary here because this will return logical(0) if the pkg
  # is not on CRAN, or if working offline (current.packages is NULL in this case).
  groundhog.version_using <- as.character(packageVersion("groundhog"))
  if (isTRUE(groundhog.version_cran > groundhog.version_using)) {
    packageStartupMessage(
      'groundhog says: "OUTDATED"\nYou are using version ', groundhog.version_using, " and the current version is ",
      groundhog.version_cran, '. Please update by running: \ninstall.packages("groundhog")'
    )
  }
  
  
}
