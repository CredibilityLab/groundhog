#' Install snowball
#'
#' Install given `pkg` with the version from `date`, making sure the order in
#' which dependencies are installed doesn't generate conflicts.
#'
#' @inheritParams get.snowball
#' @inheritParams install.source
#' @inheritParams installation.feedback
#' @param force.install Logical (defaults to `FALSE`). If `TRUE`, even if
#'   package is found for this R-build, it will reinstall it.
#'
#' @inherit install.source return
#'
#' @examples
#' \donttest{
#' groundhogR:::install.snowball("magrittr", "2018-02-12", include.suggests = FALSE)
#' }
#'
#' @seealso [get.snowball()] to determine in which order packages are installed
#'
#' @importFrom utils install.packages
#'
install.snowball <- function(pkg, date, include.suggests, force.install = FALSE,
                             force.source = FALSE, quiet.install = TRUE, current.deps = c("Rcpp", "RcppArmadillo", "BH", "RcppEigen", "StanHeaders", "RcppParallel", "RcppProgress")) {

  # 0 Get the snowball
  snowball <- get.snowball(pkg, date, include.suggests, force.install, current.deps)

  # 0.1 pkg_vrs for target package
  pkg_vrs <- as.character(snowball[nrow(snowball), "pkg_vrs"])

  # 1. FORCE INSTALL
  if (any(snowball$installed) & force.install) {
    # Subset of packages that are installed
    snowball.installed <- snowball[snowball$installed, ]
    # Get their path
    snowball.installed$paths <- mapply(get.installed_path, snowball.installed$pkg, snowball.installed$vrs)
    # Delete the paths
    unlink(snowball.installed$paths, recursive = TRUE, force = TRUE)
    snowball$installed <- FALSE
  } # End #1 force

  # 2. FORCE SOURCE
  if (force.source || .Platform$pkgType == "source") {
    # If the user chooses it or we the platform doesn't support binary packages,
    # we get it from source
    snowball$from <- "source"
  }

  # 3 INSTALLATION LOOP

  start.time <- Sys.time() # When loops starts, to compute actual completion time
  for (k in seq_len(nrow(snowball))) {
    # 3.0 Assign path
    lib.k <- as.character(snowball[k, "installation.path"])

    # 3.1 Install if needed
    if (!snowball[k, "installed"]) {

      # 3.2 Feedback to user on time and pogresss
      installation.feedback(k, date, snowball, start.time)

      # 3.3 Shorter variable names
      pkg.k <- snowball[k, "pkg"]
      vrs.k <- snowball[k, "vrs"]
      pkg_vrs.k <- snowball[k, "pkg_vrs"]
      from.k <- snowball[k, "from"]
      mran.date.k <- snowball[k, "MRAN.date"]

      # 3.4 Create directory
      dir.create(lib.k, recursive = TRUE, showWarnings = FALSE)

      # 3.5 INSTALL K FROM CRAN
      if (from.k == "CRAN") {
        message1("\n>Installing Binary from CRAN")
        install.packages(pkg.k, dependencies = FALSE, lib = lib.k, repos = "https://cloud.r-project.org", type = "both", quiet = quiet.install, keep_outputs = file.path(get.groundhog.folder(), "logs/"))
      }

      # 3.6 INSTALL K FROM MRAN
      if (from.k == "MRAN") {
        message1("\n>Attempting to install binary from MRAN\n")
        install.packages(pkg.k, lib = lib.k, type = "binary", repos = paste0("https://mran.microsoft.com/snapshot/", mran.date.k, "/"), dependencies = FALSE, quiet = quiet.install, keep_outputs = file.path(get.groundhog.folder(), "logs/"))
      }


      # 3.7 INSTALL K FROM SOURCE IF SUPPOSED TO, OR I STILL NOT INSTALLED
      if (from.k == "source" | !is.pkg_vrs.installed(pkg.k, vrs.k)) {
        message1("\n>Attempting to install from source")

        # 1) Try CRAN
        # If current version
        if ((pkg_vrs.k %in% .pkgenv[["current.packages"]]$pkg_vrs) == TRUE) {
          url1 <- paste0("https://cran.r-project.org/src/contrib/", pkg_vrs.k, ".tar.gz")
          install.packages(url1, repos = NULL, lib = lib.k, type = "source", dependencies = F, quiet = quiet.install)
        }

        # If archive version
        if ((pkg_vrs.k %in% .pkgenv[["current.packages"]]$pkg_vrs.k) == FALSE) {
          url2 <- paste0("https://cran.r-project.org/src/contrib/Archive/", pkg.k, "/", pkg_vrs.k, ".tar.gz")
          install.packages(url2, repos = NULL, lib = lib.k, type = "source", dependencies = F, quiet = quiet.install)
        }

        # 2) Try MRAN
        # Attempt 3 overall, MRAN around selected date, current tarballs path
        if (is.pkg_vrs.installed(pkg.k, vrs.k) == FALSE) # if still not installed
          {
            # Set date for mran, midpoint of available dates around requested date  +-10 days,
            date <- as.Date(date) # Ensure formated as date
            max.date <- min(Sys.Date() - 2, date + 10) # use as highest possible mran date, 2 days ago
            mran.date <- get.available.mran.date(date - 10, max.date) # Function get.available.mran.date() in utils.R

            # Try path for current packages
            url3 <- paste0("https://cran.microsoft.com/snapshot/", mran.date, "/src/contrib/", pkg_vrs.k, ".tar.gz")
            install.packages(url3, repos = NULL, lib = lib.k, type = "source", dependencies = F, quiet = quiet.install)
          }
        # Attempt 4 overall, MRAN around selected date, Archive tarballs path
        if (is.pkg_vrs.installed(pkg.k, vrs.k) == FALSE) # if still not installed
          {
            url4 <- paste0("https://cran.microsoft.com/snapshot/", mran.date, "/src/contrib/Archive/", pkg.k, "/", pkg_vrs.k, ".tar.gz")
            install.packages(url4, repos = NULL, lib = lib.k, type = "source", dependencies = F, quiet = quiet.install)
          }

        # Attempt 5, MRAN around Publish date, current path
        if (is.pkg_vrs.installed(pkg.k, vrs.k) == FALSE) # if still not installed
          {
            # Set date for mran, midpoint of available dates around requested date  +-10 days,
            toc.pkg <- toc(pkg)
            date.Published <- as.Date(subset(toc.pkg, Version = vrs)$Published) + 2
            max.date.published <- min(Sys.Date() - 2, date.Published + 20) # Use as highest possible mran published date, 2 days ago
            mran.published.date <- get.available.mran.date(date.Published, max.date.published) # Function get.available.mran.date() in utils.R
            # Treat as current
            url5 <- paste0("https://cran.microsoft.com/snapshot/", mran.published.date, "/src/contrib/", pkg_vrs.k, ".tar.gz")
            install.packages(url5, repos = NULL, lib = lib.k, type = "source", dependencies = F, quiet = quiet.install)
          }

        # Attempt 6, MRAN around Publish date, Archive path
        if (is.pkg_vrs.installed(pkg.k, vrs.k) == FALSE) # if still not installed
          {
            # If fails try archive
            url6 <- paste0("https://cran.microsoft.com/snapshot/", mran.published.date, "/src/contrib/", pkg_vrs.k, ".tar.gz")
            install.packages(url6, repos = NULL, lib = lib.k, type = "source", quiet.install = quiet, dependencies = F, quiet = quiet.install)
          }
      }
      # install.source(pkg_vrs.k, lib.k, date, quiet.install = quiet.install)  #used to call an external function


      # 3.8 VERIFY INSTALL
      now.installed <- is.pkg_vrs.installed(pkg.k, vrs.k)

      # 3.8.5 If not success, try source again, forcing download of file
      # if (!now.installed) {
      #  install.source(pkg_vrs.k, lib.k, date, force.download = TRUE, quiet.install = quiet.install)
      # }
      # 2020 05 14 Commented out as it makes failure too slow, and unlikely that tarball is incorrectly downloaded.

      # 3.8.6 Verify install again
      # now.installed <- is.pkg_vrs.installed(pkg.k, vrs.k)

      # 3.9 Installation failed
      if (!now.installed) {

        # If using different R version, attribute to that:
        rv <- r.version.check(date)
        if ((rv$r.using.minor != rv$r.need.minor) | (rv$r.using.major != rv$r.need.major)) {
          message1() # to skip a line
          message2()
          message("Installation failed!")
          message1(
            "A likely culprit is that there is a discrepancy between the R Version you are using R-", rv$r.using.full,
            "\nand the one that matches the date (", date, ") you entered: R-", rv$r.need.full, ".\n",
            "Many packages cannot be installed in newer versions of R.\n",
            "Either use a newer version of the package, or follow the instructions below to run the older version of R."
          )


          # Instructions for switching R to specific date (separate function)
          msg.R.switch(date)
        }

        # R TOOLS CHECK
        # WINDOWS and missing R Tools
        if (Sys.info()["sysname"] == "Windows" & Sys.which("make") == "") {
          # Draft message
          msg <- paste0(
            "\n***RTOOLS ALERT***\nYou need 'R Tools' to install packages from source (e.g., when installing\n",
            "older versions of a package in newer versions of R). R Tools was not found (note that different\n",
            "version of R Tools are needed for different versions of R. You may dowload R Tools from:\n",
            "https://cran.r-project.org/bin/windows/Rtools/ \n\n "
          )
          # print it
          message1(msg)
        } # End of if make==""




        # Stop the script
        message("\n\n\n----------------   The package ", pkg_vrs, " did NOT install.  Read above for details  -----------------")
        opt <- options(show.error.messages = FALSE)
        on.exit(options(opt))
        stop()
      }

      # Installation succeded
      if (now.installed) {
        # message2()
        message1(pkg_vrs.k, " installed succesfully. Saved to: ", lib.k)
        # delete temporary renamed foler if they were created
      }
    } # End of check for whetehr already installed

    # Add to libpath, unless it is the one to be installed
    .libPaths(c(lib.k, .libPaths()))
  } # End loop install


  invisible(NULL)
} # End install.snowball()
