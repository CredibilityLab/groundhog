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
                              current.deps = .pkgenv$current.deps,  #set in zzz.R
                              ignore.package.conflicts = FALSE,
                              force.source = FALSE,
                              force.install = FALSE) {

  # If package name was given using non-standard evaluation (i.e., unquoted)
  pkg <- as.character(substitute(pkg))

  
  #1 initial check,  stop if same version
      active=get.active()
  
    #1.1 Get version of requested package
        vrs <- get.version(pkg, date, current.deps = current.deps)
        pkg_vrs <- paste0(pkg, "_", vrs)
        
    #1.2 Stop if  pkg_vrs already attached
        attached.list=utils::sessionInfo()$otherPkgs
        attached.pkg <- names(attached.list)
        attached.vrs <- lapply(attached.list, function(x) x$Version)
        attached.pkg_vrs <- paste0(attached.pkg, "_", attached.vrs) 
        
        if (pkg_vrs %in% attached.pkg_vrs) {
            message1("groundhog says: the package you requested ('", pkg, "_", vrs, "') is already attached")
            return(invisible(active$pkg_vrs))

        }
    #1.3 Attach if package is loaded but not attached
        if (pkg_vrs %in% active$pkg_vrs)
        {
          attachNamespace(pkg)
          message1("groundhog says: the package you requested ('", pkg, "_", vrs, "') was loaded, now it is also attached")
          return(invisible(active$pkg_vrs))

        }
      
 
  # Check if using R that's from a version PRIOR to that current for the desired date (prior to current release)
  # e.g., using R-3.3.3 for "2020-01-05"

  #2 Check R version
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

  #3 Grab .libpaths()
    orig_lib_paths <- .libPaths()
    .libPaths("")  #actively remove default library path
    on.exit(.libPaths(orig_lib_paths))

  #4 Update cran.toc() if needed for entered date 
    update_cran.toc_if.needed(date)

  #5 Get vrs
    vrs <- get.version(pkg, date, current.deps = current.deps)
    pkg_vrs <- paste0(pkg, "_", vrs)

  #6 GET SNOWBALL
    snowball <- get.snowball(pkg, date, 
                             include.suggests=include.suggests, 
                             force.source=force.source,  
                             current.deps = current.deps)
    


  #7 CHECK FOR CONFLICT SNOWBALL <->ACTIVE PACKAGES
    if (!ignore.package.conflicts) {
      check.snowball.conflict(snowball, force.install)  #This was stop processing of request and ask for SHFT/CTRL-F10
      #unload.conflicts(snowball)                         #This unloads packages in conflict
      
    }

  #8 message if installation will be necessary
    need.to.install.total <- sum(!snowball$installed)
    if (need.to.install.total > 0) {
      message2()
      message1(
        "Loading ", pkg_vrs, " requires loading ", nrow(snowball), " packages, of which ",
        need.to.install.total, " will need to be installed."
              )
      

  }
  #9 Install packages if needed
  
  install.snowball(snowball, 
    date=date,
    force.install = force.install,
    force.source = force.source,
    quiet.install = quiet.install
    )
  
  #10 Load packages & attach the requested package
    n=nrow(snowball)
   for (k in 1:n)
    {
    .libPaths(c(.libPaths(), snowball$installation.path[k] ))
    loadNamespace(snowball$pkg[k], lib.loc = snowball$installation.path[k])
    if (k==n)   attachNamespace(snowball$pkg[k])
  }

  #11 Success/failure message
    #11.1 look at loaded packages
      loaded_pkg_vrs <- get.active()$pkg_vrs

    #11.2 
    #if (pkg_vrs %in% loaded_pkg_vrs) {
      if (all(snowball$pkg_vrs %in% loaded_pkg_vrs)) {
        message1("groundhog says: successfully loaded '", pkg_vrs,"'.")
        } else {
        message("groundhog says: FAILED to load '", pkg_vrs,"'.")
      }

      
      
    
  #12 output
    invisible(loaded_pkg_vrs)
  }

