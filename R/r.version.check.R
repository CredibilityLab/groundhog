#' Compare \R versions
#'
#' Compare the current R version to the \R version at a given `date`.`
#'
#' @inheritParams get.version
#'
#' @return a named `list` with the following elements:
#' \describe{
#'   \item{r.using.full}{\R version currently in use}
#'   \item{r.using.major}{Major version number of the \R version currently in use}
#'   \item{r.using.minor}{Minor version number of the \R version currently in use}
#'   \item{r.using.majmin}{The Major.minor version number of the \R version currently in use}
#'   \item{r.using.patch}{Patch version number of the \R version currently in use}
#'   \item{r.need.full}{\R version currently in use at `date`}
#'   \item{r.need.major}{Major version number of the \R version in use at `date`}
#'   \item{r.need.minor}{Minor version number of the \R version in use at `date`}
#'   \item{r.using.majmin}{The Major.minor version number of the \R version currently in use}
#'   \item{r.need.patch}{Version number of the \R version in use at `date`, with
#'   the maximum patch version available today.}
#' }
#'
# @examples
# \donttest{
# groundhog:::r.version.check("2018-02-12")
# }
#'
r.version.check <- function(date) {
  # Using
  r.using.major <- as.numeric(R.version$major)
  r.using.minor <- as.numeric(strsplit(R.version$minor, "\\.")[[1]][1])
  r.using.patch <- as.numeric(strsplit(R.version$minor, "\\.")[[1]][2])
  r.using.majmin <- paste0(r.using.major, ".", r.using.minor)
  r.using.full <- paste0(r.using.majmin, ".", r.using.patch)
  # need
  r.need.full <- get.version("R", date, patch = "max")
  r.need.split <- strsplit(r.need.full, "\\.")[[1]]
  r.need.major <- as.numeric(r.need.split[1])
  r.need.minor <- as.numeric(r.need.split[2])
  r.need.majmin <- paste0(r.need.major, ".", r.need.minor)
  # r.need.patch <- as.numeric(r.need.split[3])
  r.need.patch <- r.need.split[3] # version 2.15.1-w generates NA by coercion

  return(namedList(
    r.using.full, r.using.major, r.using.minor, r.using.majmin, r.using.patch,
    r.need.full, r.need.major, r.need.minor, r.need.majmin, r.need.patch
  ))
}

