#' Install package from source
#'
#' @inheritParams get.R.pkg.date
#' @inheritParams get.dependencies
#' @param quiet.install Logical (defaults to `TRUE`).Run [install.packages()]
#'   with `quiet = TRUE`?
#' @param lib character. Path where to install the package.
#' @param force.download logical (defaults to `FALSE`). If the tarball
#'   containing the package source code already exists locally, should it be
#'   downloaded again?
#'
#' @inherit utils::install.packages return
#'
#' @importFrom utils download.file install.packages
#'
install.source <- function(pkg_vrs, lib, date, force.download = FALSE, quiet.install = FALSE) {
  # 6.1 Preliminaries
  pkg <- get.pkg(pkg_vrs)
  vrs <- get.vrs(pkg_vrs)

  # 6.2 Paths
  tarball.name <- paste0(pkg_vrs, ".tar.gz") # Name of tarball
  tarball.dir <- file.path(get.groundhogr.folder(), "_tarballs") # Folder to save it
  tarball.path <- file.path(tarball.dir, tarball.name) # Path to tarball itself

  # 6.3 Ensure folder exists for tarball saving exists
  dir.create(tarball.dir, showWarnings = FALSE, recursive = TRUE)

  # 6.4 Download tarball if needed
  if (!file.exists(tarball.path) | force.download) {
    # 6.4.2 Find tarball in CRAN, based on whether it is current or not current
    if (pkg_vrs %in% current.packages$pkg_vrs) {
      file.url <- paste0("https://cran.r-project.org/src/contrib/", pkg_vrs, ".tar.gz")
    } else {
      file.url <- paste0("https://cran.r-project.org/src/contrib/Archive/", pkg, "/", pkg_vrs, ".tar.gz")
    }

    down.tarball.attempt <- try(download.file(file.url, destfile = tarball.path))


    # 6.4.3 If downloading fails, try from MRAN on date requested as current, then that date as archive, then first day on MRAN as current, then archive
    if (inherits(down.tarball.attempt, "try-error")) {
      down.tarball.attempt <- try(download.file(paste0("https://cran.microsoft.com/snapshot/", date, "/src/contrib/", pkg_vrs, ".tar.gz"), destfile = tarball.path))
    }
    if (inherits(down.tarball.attempt, "try-error")) {
      down.tarball.attempt <- try(download.file(paste0("https://cran.microsoft.com/snapshot/", date - 2, "/src/contrib/Archive/", pkg, "/", pkg_vrs, ".tar.gz"), destfile = tarball.path))
    }
    if (inherits(down.tarball.attempt, "try-error")) {
      down.tarball.attempt <- try(download.file(paste0("https://cran.microsoft.com/snapshot/", date - 2, "/src/contrib/", pkg_vrs, ".tar.gz"), destfile = tarball.path))
    }
    if (inherits(down.tarball.attempt, "try-error")) {
      down.tarball.attempt <- try(download.file(paste0("https://cran.microsoft.com/snapshot/", date + 1, "/src/contrib/Archive/", pkg, "/", pkg_vrs, ".tar.gz"), destfile = tarball.path))
    }
    if (inherits(down.tarball.attempt, "try-error")) {
      down.tarball.attempt <- try(download.file(paste0("https://cran.microsoft.com/snapshot/", date + 1, "/src/contrib/", pkg_vrs, ".tar.gz"), destfile = tarball.path))
    }
    if (inherits(down.tarball.attempt, "try-error")) {
      down.tarball.attempt <- try(download.file(paste0("https://cran.microsoft.com/snapshot/", date, "/src/contrib/Archive/", pkg, "/", pkg_vrs, ".tar.gz"), destfile = tarball.path))
    }
    if (inherits(down.tarball.attempt, "try-error")) {
      down.tarball.attempt <- try(download.file(paste0("https://cran.microsoft.com/snapshot/2014-09-18/src/contrib/", pkg_vrs, ".tar.gz"), destfile = tarball.path))
    }
    if (inherits(down.tarball.attempt, "try-error")) {
      down.tarball.attempt <- try(download.file(paste0("https://cran.microsoft.com/snapshot/2014-09-18/src/contrib/Archive/", pkg, "/", pkg_vrs, ".tar.gz"), destfile = tarball.path))
    }
  }

  if (!file.exists(tarball.path)) {
    message1(
      "could not find the tarball file for package ",
      pkg, " version:", vrs, " in CRAN (", file.url, "). \nMaybe you are offline?."
    )
    stop("Installation failed.")
  } else {
    # Create the folder
    dir.create(lib, showWarnings = FALSE, recursive = TRUE)
    # Install the package
    install.packages(tarball.path, type = "source", lib = .pkgenv[["libPaths"]], destdir = lib, quiet = quiet.install, dependencies = FALSE, repos = NULL, INSTALL_opts = "--no-staged-install")
  } # End if success

} # End of install.source
