#' Get date of binary package-version availability for current R version, on  MRAN
#'
#' @inheritParams get.R.pkg.date
#'
#' @seealso [get.version()] for obtaining a package version from a given date
#'
#' @inherit get.R.pkg.date return
#'
# @examples
# \dontrun{
# groundhog:::get.date.for.install.binary("magrittr_1.0.1")
# }
#'
get.date.for.install.binary <- function(pkg_vrs) {
  binary.date <- as.DateYMD("1970-01-01")
  # R being used
  r.using.full <- get.rversion() # Get current
  r.using.major <- R.version$major
  r.using.minor <- strsplit(R.version$minor, "\\.")[[1]][1]

  #1 Find R1 and R2 (first and last dates with the current vesion of R (ignoring patch)
  
    #1.1 Get all R releases
      R.toc <- toc("R") # Get R toc

    #1.2  Find all R versions with same major and minor versions as currently using
      R_vrs <- grep(paste0("^", r.using.major, ".", r.using.minor), R.toc$Version, value = TRUE)

    #1.3 Get the first and last one
      R1=R_vrs[1]
      R2=R_vrs[length(R_vrs)]
      
    #1.4 Get those dates
      date.R1=R.toc[match(R1,R.toc$Version),]$Published
      date.R2=R.toc[match(R2,R.toc$Version),]$Published

  #2 Fine pkg1 and pkg2 (first and last dates with the desired package version
    #2.1 Get all package releases
      pkg=get.pkg(pkg_vrs)
      pkg.toc <- toc(pkg) 

    #2.2 Match the date of desired, and grab that and next release as date.pkg1, date.pkg2
      vrs=get.vrs(pkg_vrs)
      date.pkg1 <- pkg.toc[match(vrs,pkg.toc$Version),]$Published    #date when desired version was released
      date.pkg2 <- pkg.toc[match(vrs,pkg.toc$Version)+1,]$Published  #date when next version was released
      
      #If no next release, use 'today' minus two days
      if (is.na(date.pkg2)) {
          date.pkg2 <- Sys.Date()-2
          }
      
  #3 The date to use is day prior to either the new package or the new R release
      binary.date <- min(date.R2 - 1, date.pkg2 -1)
      
  return(as.DateYMD(binary.date))
}
