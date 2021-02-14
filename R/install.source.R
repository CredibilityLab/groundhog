#' Install package from source
#'
#' @noRd
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
  tarball.dir <- file.path(get.groundhog.folder(), "_tarballs") # Folder to save it
  tarball.path <- file.path(tarball.dir, tarball.name) # Path to tarball itself

  # 6.3 Ensure folder exists for tarball saving exists
  dir.create(tarball.dir, showWarnings = FALSE, recursive = TRUE)

  # 6.4 Download tarball from CRAN
  if (!file.exists(tarball.path) | force.download) {
    # 6.4.2 Find tarball in CRAN, based on whether it is current or not current
    if (pkg_vrs %in% .pkgenv[["current.packages"]]$pkg_vrs) {
      file.url <- paste0("https://cran.r-project.org/src/contrib/", pkg_vrs, ".tar.gz")
    } else {
      file.url <- paste0("https://cran.r-project.org/src/contrib/Archive/", pkg, "/", pkg_vrs, ".tar.gz")
    }

    down.tarball.attempt <- try(download.file(file.url, destfile = tarball.path))


    # 6.4.3 If CRAN fails, try MRAN around date requested as current, if that fails, date around publication
    # The interval is +-10 days, but first we check +10 days is not past today
    date <- as.Date(date)
    max.date <- min(Sys.Date() - 2, date + 10) # use as highest possible mran date, 2 days ago
    mran.date <- get.available.mran.date(as.Date(date) - 10, max.date) # Function get.available.mran.date() in utils.R

    # Attempt source on mran.date
    if (inherits(down.tarball.attempt, "try-error")) {
      down.tarball.attempt <- try(download.file(paste0("https://cran.microsoft.com/snapshot/", mran.date, "/src/contrib/", pkg_vrs, ".tar.gz"), destfile = tarball.path, mode = "wb"))
    }
    if (inherits(down.tarball.attempt, "try-error")) {
      down.tarball.attempt <- try(download.file(paste0("https://cran.microsoft.com/snapshot/", mran.date, "/src/contrib/Archive/", pkg, "/", pkg_vrs, ".tar.gz"), destfile = tarball.path, mode = "wb"))
    }


    # If still no success, try the days following it publication
    toc.pkg <- toc(pkg)
    date.Published <- subset(toc.pkg, Version = vrs)$Published + 2
    if (date.Published < Sys.Date() - 2) {
      max.date.published <- min(Sys.Date() - 2, date.Published + 20) # use as highest possible mran published date, 2 days ago
      mran.published.date <- get.available.mran.date(date.Published, max.date.published) # Function get.available.mran.date() in utils.R


      # Attempt source on first day on MRAN
      if (inherits(down.tarball.attempt, "try-error")) {
        down.tarball.attempt <- try(download.file(paste0("https://cran.microsoft.com/snapshot/", mran.published.date, "/src/contrib/Archive/", pkg, "/", pkg_vrs, ".tar.gz"), destfile = tarball.path, mode = "wb"))
      }
      if (inherits(down.tarball.attempt, "try-error")) {
        down.tarball.attempt <- try(download.file(paste0("https://cran.microsoft.com/snapshot/", mran.published.date, "/src/contrib/", pkg_vrs, ".tar.gz"), destfile = tarball.path, mode = "wb"))
      }
    } # End if it was published more than 2 days ago
  }

  if (!file.exists(tarball.path)) {
    message1(
      "could not find the tarball file for package ",
      pkg, " version:", vrs, " in CRAN (", file.url, "). \nMaybe you are offline?."
    )
    exit("Installation failed.")
  } else {
    # Create the folder
    dir.create(lib, showWarnings = FALSE, recursive = TRUE)
    # Install the package
    install.packages(tarball.path, type = "source", lib = lib, quiet = quiet.install, dependencies = FALSE, repos = NULL)
  } # End if success
} # End of install.source
