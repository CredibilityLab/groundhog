#' Install packages as available on set date -  groundhog.library()
#'
#' @inheritParams install.snowball
#' @param ignore.package.conflicts Logical (defaults to `FALSE`). With `TRUE`,
#'   if an already attached package will be installed, it is detached, but all
#'   dependencies are left installed. With default, `FALSE`, script stops and
#'   asks for session restart.
#'
#' @return a character vector containing all active packages for the session,
#'   with their version number, under the format `pkg_vrs`.
#'
#' @examples
#' \dontrun{
#' groundhog.library("magrittr", "2018-02-12")
#' }
#'
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

  # If package name was given using non-standard evaluation (i.e., unquoted)
  pkg <- as.character(substitute(pkg))

  # 1 Initial check, if package already attached stop package is already attached
  #if (pkg %in% names(utils::sessionInfo()$otherPkgs)) {
    #message1("A version of the package '", pkg, "' is already loaded.")
    #return(invisible(get.active()$pkg_vrs))
  #}
  
  #1 initial check,  stop if same version
    #1.1 Get version of requested package
        vrs <- get.version(pkg, date, current.deps = current.deps)
        pkg_vrs <- paste0(pkg, "_", vrs)
        
    #1.2 Get pkg_vrs of attached packages
        attached.pkg <- names(utils::sessionInfo()$otherPkgs)
        
        attached.vrs <- packageVersion(attached.pkg)
        attached.pkg_vrs <- paste0(attached.pkg, "_" , attached.vrs)
      
    #1.3 If requested pkg_vrs is there, stop
         if (pkg_vrs  %in% attached.pkg_vrs) {
            message1("groundhog says: The package you requested ('", pkg, "_", vrs, "') is already loaded.")
            return(invisible(get.active()$pkg_vrs))
         }
        
         if (pkg  %in% attached.pkg) {
           message2()
            message1("You requested '", pkg, "_", vrs, "', but another version of that package is already loaded.\nYou can restart the R session to load a different version (in R Studio CTRL-SHIFT-F10)")
            return(invisible(get.active()$pkg_vrs))
          }
    
  # Check if using R that's from a version PRIOR to that current for the desired date (prior to current release)
  # e.g., using R-3.3.3 for "2020-01-05"

  update_cran.toc_if.needed(date = date)

  rv <- r.version.check(date) # Get version of r being used and needed

  if (package_version(rv$r.using.majmin) < package_version(rv$r.need.majmin)) {
    message2()
    message(
      "You are using R-", rv$r.using.full, " and the current R version for the data you entered:",
      "'", date, "' was R-", rv$r.need.majmin, ".\n",
      "To ensure reproducibility you must use groundhog.library() with a version of R ",
      "that is as least\nas recent as the entered date (i.e., R>=", rv$r.need.majmin, ")\n",
      "\n\n   ----------------- Package '", pkg, "' NOT LOADED ----------------"
    )
    exit()
  }

  # Grab .libpaths()
  orig_lib_paths <- .libPaths()
  .libPaths("")  #actively remove default library path
  on.exit(.libPaths(orig_lib_paths))

  # 8.2 Update cran.toc() if needed for entered date (#2.12)
  update_cran.toc_if.needed(date)

  # 8.4 Get vrs
  vrs <- get.version(pkg, date, current.deps = current.deps)
  pkg_vrs <- paste0(pkg, "_", vrs)

  # 8.5 GET SNOWBALL
  snowball <- get.snowball(pkg, date, include.suggests, current.deps = current.deps)


  # 8.6.4 CHECK FOR CONFLICT SNOWBALL <->ACTIVE PACKAGES
  if (!ignore.package.conflicts) {
    check.snowball.conflict(snowball, force.install)  #This was stop processing of request and ask for SHFT/CTRL-F10
    #unload.conflicts(snowball)                         #This unloads packages in conflict
    
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
  # 8.7 Install packages if needed
  
  install.snowball(pkg, date, include.suggests,
    force.install = force.install,
    force.source = force.source,
    quiet.install = quiet.install,
    current.deps = current.deps
  )

  # 8.9  Draft success/failure messages
  # look at loaded packages
  loaded_pkg_vrs <- get.active()$pkg_vrs

  # 8.9.1 If successful
  if (pkg_vrs %in% loaded_pkg_vrs) {
    # Success on package
    msg <- c(">groundhog says: successfully loaded '", pkg_vrs,"'.")
  } else {
    msg <-c("groundhog says: FAILED to load '", pkg_vrs,"'.")
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
