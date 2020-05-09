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
                             force.source = FALSE, quiet.install = TRUE, current.deps = "Rcpp") {

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
    # Rename the paths so they are not found and installation takes place
    snowball.installed$paths_rename <- paste0(snowball.installed$paths, "_forcing.reinstall") # new names
    file.rename(snowball.installed$paths, snowball.installed$paths_rename) # assign them
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
        install.packages(pkg.k, dependencies = FALSE, lib = lib.k, repos = "https://cloud.r-project.org", type = "both", quiet = quiet.install, keep_outputs = file.path(get.groundhog.folder(), "logs/"))
        message1("Installing Binary from CRAN")
      }

      # 3.6 INSTALL K FROM MRAN
      if (from.k == "MRAN") {
        # Try MRAN 1
        install.packages(pkg.k, lib = lib.k, type = "binary", repos = paste0("https://mran.microsoft.com/snapshot/", mran.date.k, "/"), dependencies = FALSE, quiet = quiet.install, keep_outputs = file.path(get.groundhog.folder(), "logs/"))
        message1("Attempt #1 to install binary from from MRAN")
        # Try MRAN 2: If still not installed, try day after (some MRAN days are missing)
        if (!is.pkg_vrs.installed(pkg.k, vrs.k)) {
          install.packages(pkg.k, lib = lib.k, type = "binary", repos = paste0("https://mran.microsoft.com/snapshot/", mran.date.k + 1, "/"), dependencies = FALSE, quiet = quiet.install, keep_outputs = file.path(get.groundhog.folder(), "logs/"))
          message1("Attempt #2 to install binary from from MRAN")
        }
        # Try MRAN 3: If still not installed, try 2 days earlier
        if (!is.pkg_vrs.installed(pkg.k, vrs.k)) {
          install.packages(pkg.k, lib = lib.k, type = "binary", repos = paste0("https://mran.microsoft.com/snapshot/", mran.date.k - 2, "/"), dependencies = FALSE, quiet = quiet.install, keep_outputs = file.path(get.groundhog.folder(), "logs/"))
          message1("Attempt #3 to install binary from from MRAN")
        }
      } # end MRAN


      # 3.7 INSTALL K FROM SOURCE IF SUPPOSED TO, OR I STILL NOT INSTALLED
      if (from.k == "source" | !is.pkg_vrs.installed(pkg.k, vrs.k)) {
        install.source(pkg_vrs.k, lib.k, date, quiet.install = quiet.install)
        message1("Attempting to install from source")
      }

      # 3.8 VERIFY INSTALL
      now.installed <- is.pkg_vrs.installed(pkg.k, vrs.k)

      # 3.8.5 If not success, try source again, forcing download of file
      if (!now.installed) {
        install.source(pkg_vrs.k, lib.k, date, force.download = TRUE, quiet.install = quiet.install)
      }

      # 3.8.6 Verify install again
      now.installed <- is.pkg_vrs.installed(pkg.k, vrs.k)

      # 3.9 Installation failed
      if (!now.installed) {

        # If using different R version, attribute to that:
        rv <- r.version.check(date)
        if ((rv$r.using.minor != rv$r.need.minor) | (rv$r.using.major != rv$r.need.major)) {
          message2()
          message1(
            "As mentioned before, there is a discrepancy between the R Version you are using R-", rv$r.using.full,
            "\nand the one that matches the date (", date, ") you entered: R-", rv$r.need.full, ". ",
            "It was worth a try \nbut the install did not work. Follow the instructions below to",
            " try this script in the correct R version."
          )


          # Long instructions written above
          msg.R.switch(date)
        }

        # PENDINGrename back the flders taht were renamed
        #

        # Stop the script
        message2("\n\n\n--------------------   The package ", pkg_vrs, " did NOT install.-----------------")
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
