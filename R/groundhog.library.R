#' 8  Final function:   groundhog.library()
#'
#' @inheritParams install.snowball
#' @param current.deps Dependencies that will install current version regardless
#'   of date.
#' @param ignore.package.conflicts Logical (defaults to `FALSE`). With `TRUE`,
#'   if an already attached package will be installed, it is detached, but all
#'   depednecies are left installed. With default, `FALSE`, script stops and
#'   asks for session restart.
#'
#' @importFrom utils capture.output
#'
#' @export
#'
groundhog.library <- function(
  pkg, date,
  quiet.install = TRUE,
  plot.console = TRUE,
  include.suggests = FALSE,
  current.deps = c("Rcpp"),
  ignore.package.conflicts = FALSE,
  force.source = FALSE,
  force.install = FALSE)
{

  # 8.2 Update cran.toc() if needed for entered date (#2.12)
  update_cran.toc_if.needed(date)

  # 8.3 Check for r.mismatch (#2.13)
  check.mismatch.R(date)

  # 8.4 Get vrs
  vrs <- get.version(pkg, date)
  pkg_vrs <- paste0(pkg, "_", vrs)

  # 8.5 GET SNOWBALL (#6)
  snowball <- get.snowball(pkg, date, include.suggests)

  # 8.6 CHECK FOR CONFLICT SNOWBALL <->ACTIVE PACKAGES
  if (!ignore.package.conflicts) {
    check.snowball.conflict(snowball)
  }

  # 8.7 Install pacakges if needed
  install.snowball(pkg, date, include.suggests, force.install = force.install,
                   force.source = force.source, plot.console = plot.console,
                   quiet.install = quiet.install)

  # 8.8 Do library() call for pkg_vrs
  library(pkg, character.only = TRUE)

  # 8.9  verify success
  message2()

  # load active packagse
  active <- paste0(capture.output(utils::sessionInfo()), collapse = ",")
  # Found there?
  if (grepl(pkg_vrs, active)) {
    message1("Succesfully loaded ", pkg_vrs, " and its ", nrow(snowball) - 1,
             " dependencies.")
  } else {
    message1("FAILED to load ", pkg_vrs)
  }
}
