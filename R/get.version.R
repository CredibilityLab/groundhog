#' Get version from date
#'
#' @inheritParams get.snowball
#' @param patch Either `"current"` for the exact current version of the `pkg`
#'   at the specified date, or `"max"` for the version with the same major and
#'   minor but maximal patch version numbers.
#'
# FIXME: add @return
#'
#' @seealso [get.R.pkg.date()] for the opposite operation: determining date
#'   for a given version.
#'
#' @examples
#' \donttest{
#' groundhog:::get.version("magrittr", "2018-02-12")
#' }
#'
get.version <- function(pkg, date, current.deps = c("Rcpp", "RcppArmadillo", "BH", "RcppEigen", "StanHeaders", "RcppParallel", "RcppProgress"), patch = c("current", "max")) {

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
  # 2.3 Check if date requested comes before most up to date date
  last.toc.date <- max(cran.toc$Published, na.rm = TRUE)
  if (date > last.toc.date) {
    exit(
      "groundhog.library() index of packages ends on ", last.toc.date,
      " which is before the date you entered: ", date, ".\n",
      "The index updates automatically "
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
  current.version <- dfk$Version[version.k]

  if (patch == "current") {
    return(current.version)
  } else if (patch == "max") {
    nopatch <- gsub("^(\\d+)\\.(\\d+).*", "\\1.\\2", current.version)
    return(dfk$Version[max(which(gsub("^(\\d+)\\.(\\d+).*", "\\1.\\2", dfk$Version)==nopatch))])
  }
}
