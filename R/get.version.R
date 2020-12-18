#' Get version from date
#'
#' @inheritParams get.snowball
#' @param patch Either `"current"` for the exact current version of the `pkg`
#'   at the specified date, or `"max"` for the version with the same major and
#'   minor but maximal patch version numbers.
#'@return most recent package version available on requested date (if source 
#'  and binary package versions do not match, the most recent of the two versions is returned.
#'
#' @seealso [get.R.pkg.date()] for the opposite operation: determining date
#'   for a given version.
#'
# @examples
# \dontrun{
# groundhog:::get.version("magrittr", "2018-02-12")
# }
#'
get.version <- function(pkg, date, patch = c("current", "max")) {

  patch <- match.arg(patch)

  # 1. Get toc
    dfk <- toc(pkg)
    cran.toc <- .pkgenv[["cran.toc"]]

  # 2.2 Check if date request comes after first date for that package
    if (dfk$Published[1] > date) {
      message2()
      message1("According to our records, the package: '", pkg, "' was not yet available on CRAN on '", date, "'")
      exit()
    }
    
  # 2.3 Update cran.toc if  requested date comes before most up to date date
      #Get most recent package date
        last.toc.date <- max(cran.toc$Published, na.rm = TRUE)
      
      #If requested date is after update, and check again
      if (date > last.toc.date) {
    	    update_cran.toc_if.needed(date)
          cran.toc <- .pkgenv[["cran.toc"]]
          last.toc.date <- max(cran.toc$Published, na.rm = TRUE)
          if (date > last.toc.date) {
            exit(
                "The index of CRAN pacakges ends on ", last.toc.date,
                " which is before the date you entered: ", date, ".\n"
                )
            
              } #End second 'if' current date>cran
          } #End first 'if' 

  
  # 3 Get version
    version.k <- max(which(dfk$Published < date)) # Position of last package available before the date
    current.version <- dfk$Version[version.k]
  
   
  #4 Output version
  if (patch == "current") {
    return(current.version)
  } else if (patch == "max") {
    nopatch <- gsub("^(\\d+)\\.(\\d+).*", "\\1.\\2", current.version)
    return(dfk$Version[max(which(gsub("^(\\d+)\\.(\\d+).*", "\\1.\\2", dfk$Version)==nopatch))])
  }
}
