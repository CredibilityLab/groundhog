# 2.3 Get version from date
get.version <- function(pkg, date, current.deps = c("Rcpp")) {
  # 1. Get toc
  dfk <- toc(pkg)
  # 2 Validate
  # 2.1 Check if Package exists in our records
  if (nrow(dfk) == 0) {
    cat2()
    cat1(paste0("groundhog.library() does not have the package ", pkg, " indexed so it cannot install or use it. Note: It only has CRAN packages"))
    stop("")
  }
  # 2.2 Check if date request comes after first date for that package
  if (dfk$Published[1] > date) {
    cat2()
    cat1(paste0("According to our records, the package: '", pkg, "' was not yet available on CRAN on '", date, "'"))
    stop("")
  }
  # 2.3 Check if date requested comes before most up to date date
  last.toc.date <- max(cran.toc$Published, na.rm = TRUE)
  if (date > last.toc.date) {
    stop(
      "groundhog.library() index of packages ends on  ", last.toc.date,
      " which is before the date you entered:", date, ".\nThe index updates automatically ",
      "every time you call groundhog.library()"
    )
  }
  # 3 If pkg is in current.deps, deliver version for current version of R
  if (pkg %in% current.deps) {
    using.r.rdate <- get.rdate() # date of R being used
    version.k <- max(which(dfk$Published < using.r.rdate)) # which kth date of pkg matches it
    vrs <- dfk[version.k, ]$Version # get kth version
    return(vrs)
  }


  # 4 Get version
  version.k <- max(which(dfk$Published < date)) # Position of last package available before the date
  return(dfk$Version[version.k]) # Return that kth element
}
