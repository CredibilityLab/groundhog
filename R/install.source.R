# 6 Install from source
#' @importFrom utils download.file install.packages

install.source <- function(pkg_vrs, lib, date, force.download = FALSE, quiet = FALSE) {
  # 6.1 Preliminaries
  pkg <- get.pkg(pkg_vrs)
  vrs <- get.vrs(pkg_vrs)

  # 6.2 Paths
  tarball.name <- paste0(pkg_vrs, ".tar.gz") # Name of tarball
  tarball.dir <- paste0(groundhogR.folder, "/_tarballs") # Folder to save it
  tarball.path <- paste0(tarball.dir, "/", tarball.name) # Path to tarball itself

  # 6.3 Ensure folder exists for tarball saving exists
  dir.create(tarball.dir, showWarnings = FALSE, recursive = TRUE)

  # 6.4 Download tarball if needed
  if (!file.exists(tarball.path) | force.download) {
    # 6.4.1 Set URL for download
    toc.pkg <- toc(pkg) # get toc() to see if we have the most current version

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

  # 6.5 Success downloading?
  # yes--> install
  if (file.exists(tarball.path)) {
    # Create the folder
    dir.create(lib, showWarnings = FALSE, recursive = TRUE)
    # Install the package
    install.packages(tarball.path, type = "source", lib = lib, dependencies = FALSE, repos = NULL, INSTALL_opts = "--no-staged-install")
  } # End if success


  # 6.6 no--> convey bad news
  if (!file.exists(tarball.path)) {
    message1(
      "could not find the tarball file for package ",
      pkg, " version:", vrs, " in CRAN (", file.url, "). \nMaybe you are offline?."
    )
    stop("Installation failed.")
  }
} # End of install.source
