#' 8  Final function:   groundhog.library()
#'
#' @inheritParams install.snowball
#' @param ignore.package.conflicts Logical (defaults to `FALSE`). With `TRUE`,
#'   if an already attached package will be installed, it is detached, but all
#'   depednecies are left installed. With default, `FALSE`, script stops and
#'   asks for session restart.
#'
#' @return a character vector containing all active packages for the session,
#'   with their version number, under the format `pkg_vrs`.
#'
#' @examples
#' groundhog.library("magrittr", "2018-02-12")
#' @importFrom utils capture.output
#'
#' @export
#'
groundhog.library <- function(
                              pkg, date,
                              quiet.install = TRUE,
                              include.suggests = FALSE,
                              current.deps = c("Rcpp", "RcppArmadillo", "BH", "RcppEigen", "StanHeaders", "RcppParallel", "RcppProgress"),
                              ignore.package.conflicts = FALSE,
                              force.source = FALSE,
                              force.install = FALSE) {

  # 1 Initial check, if pacakge already attached stop package is already attached
  if (pkg %in% names(utils::sessionInfo()$otherPkgs)) {
    message1("A version of the package '", pkg, "' is already loaded.")
    return(invisible(get.active()$pkg_vrs))
  }

  # Check if using R that's from a version PRIOR to that current for the desired date (prior to current release)
  # e.g., using R-3.3.3 for "2020-01-05"

  rv <- r.version.check(date) # Get version of r being used and needed
  r.toc <- toc("R") # Get all versions of R
  r.using.k <- match(rv$r.using.full, r.toc$Version) # Which
  r.need.k <- match(rv$r.need.full, r.toc$Version)

  if (r.using.k < r.need.k) {
    message2()
    message(
      "You are using R-", rv$r.using.full, " and the current R version for the data you entered:",
      "'", date, "' was R-", rv$r.need.full, ".\n",
      "To ensure reproducibility you must use groundhog.library() with a version of R ",
      "that is as least\nas recent as the entered date (i.e., R>=", rv$r.need.full, ")\n",
      "\n\n   ----------------- Package '", pkg, "' NOT LOADED ----------------"
    )
    exit()
  }


  # Grab .libpaths()
  orig_lib_paths <- .libPaths()
  on.exit(.libPaths(orig_lib_paths))

  # 8.2 Update cran.toc() if needed for entered date (#2.12)
  update_cran.toc_if.needed(date)

  # If package name was given using non-standard evaluation (i.e., unquoted)
  pkg <- as.character(substitute(pkg))

  # 8.4 Get vrs
  vrs <- get.version(pkg, date, current.deps = current.deps)
  pkg_vrs <- paste0(pkg, "_", vrs)


  # 8.5 GET SNOWBALL
  snowball <- get.snowball(pkg, date, include.suggests, current.deps = current.deps)




  # 8.6.4 CHECK FOR CONFLICT SNOWBALL <->ACTIVE PACKAGES
  if (!ignore.package.conflicts) {
    check.snowball.conflict(snowball, force.install)
  }


  # 8.6.5 message if installation will be necessary
  need.to.install.total <- sum(!snowball$installed)
  if (need.to.install.total > 0) {
    message2()
    message1(
      "Loading ", pkg_vrs, " requires loading ", nrow(snowball), " packages, of which ",
      need.to.install.total, " will need to be installed."
    )
  }
  # 8.7 Install pacakges if needed
  install.snowball(pkg, date, include.suggests,
    force.install = force.install,
    force.source = force.source,
    quiet.install = quiet.install,
    current.deps = current.deps
  )

  # 8.8 Do library() call for pkg_vrs
  library(pkg, character.only = TRUE, lib.loc = .libPaths()[!.libPaths() %in% orig_lib_paths]) # Exclude from search the default library


  # 8.9  Draft success/failure messags
  # look at loaded packages
  loaded_pkg_vrs <- get.active()$pkg_vrs

  # 8.9.1 If succesfull
  if (pkg_vrs %in% loaded_pkg_vrs) {
    # Success on package
    msg <- c(">Successfully loaded ", pkg_vrs)
    # Add dependencies, if any
    # if (nrow(snowball) > 1) {
    # msg <- paste0(msg, " and its ", nrow(snowball) - 1, " dependencies.")
    # } # End if it has dependencies
  } # ENd if succesfull

  # 8.9.2 If failure
  if (!pkg_vrs %in% loaded_pkg_vrs) {
    msg <-c("FAILED to load ", pkg_vrs)
  }

  # 9 Show messages
  # 9.1 Show message2() only if nothing was installed (otherwise the header is already there from the installation lines)
  if (all(!snowball$installed)) {
    message2()
  }

  # Show message
  message1(msg)


  # 10 output
  invisible(loaded_pkg_vrs)
}
