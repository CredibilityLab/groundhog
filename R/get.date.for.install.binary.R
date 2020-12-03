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
      R.k1=match(R1,R.toc$Version)  #kth observation with 1st date
      R.k2=match(R2,R.toc$Version)  #kth with last
        
      #First R date based on match of minor  
        date.R1=R.toc[R.k1,]$Published
      #Second is either the next version of R, or the current day
        
        #If this is NOT teh most recent R, look at the next one
        if (R.k2<nrow(R.toc)) 
            {
            date.R2=R.toc[R.k2 + 1,]$Published  
        #If it is the most recent, look at today
            } else {
            date.R2=Sys.Date() -2  
            }
  #2 Find pkg1 and pkg2 (first and last dates with the desired package version)
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
      
      
      
  #3 Find range of values when package and R version match
      #3.1 Start period
        D1 <- max(date.R1, date.pkg1)
        D2 <- min(date.R2, date.pkg2)
      
      #3.2 If there is no overlap, ends before it starts, January 1
        if (D2<=D1) return(as.DateYMD("1970-01-01"))
   
      #3.3 Available dates
          available.dates <- D1:D2
      
      #3.4 Drop MRAN missing dates
          missing.mran.dates <- .pkgenv[["missing.mran.dates"]] 
          available.dates <- available.dates[!available.dates %in% missing.mran.dates]
      
      #3.5 Use week before end, unless it is too early
          n=length(available.dates)
          date.median <- available.dates[ceiling(n/2)]
          date.1week  <- max(available.dates[n-7], date.median)
          binary.date=max(date.median,date.1week)
          
      
  return(as.DateYMD(binary.date))
}
