#' Get date of binary package-version availability for current R version, on  MRAN
#'
#' @inheritParams get.R.pkg.date
#'
#' @seealso [get.version()] for obtaining a package version from a given date
#'
#' @inherit get.R.pkg.date return
#'
#' @examples
#' \dontrun{
#' groundhogR:::get.date.for.install.binary("magrittr_1.0.1")
#' }
#'
get.date.for.install.binary <- function(pkg_vrs) {
  binary.date <- as.DateYMD("1970-01-01")
  # R being used
  r.using.full <- get.rversion() # Get current
  r.using.major <- R.version$major
  r.using.minor <- strsplit(R.version$minor, "\\.")[[1]][1]

  # Get R Toc
  R.toc <- toc("R") # Get R toc

  # Find all R versions with same major and minor versions as currently using
  R_vrs <- grep(paste0("^", r.using.major, ".", r.using.minor), R.toc$Version, value = TRUE)

  # Look through them starting with the most recent one
  R_vrs <- rev(R_vrs)

  # And put the currently used version first
  R_vrs <- c(r.using.full, R_vrs[R_vrs != r.using.full])

  for (v in R_vrs) {
    binary.date <- get.R.pkg.date(pkg_vrs, v)
    if (binary.date > "1970-01-01") {
      break
    }
  }

  # If date if from before MRAN's first day, 2014-09-17, go to 1970
  if (binary.date < "2014-09-17") {
    binary.date <- as.DateYMD("1970-01-01")
  }

  return(as.DateYMD(binary.date))
}
