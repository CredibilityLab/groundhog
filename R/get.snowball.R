#' Figure out in which order dependencies should be installed to avoid conflicts
#'
#' @inheritParams get.all.dependencies
#' @param force.install Logical (defaults to `FALSE`). If `TRUE`, even if
#'   package is found for this R-build, it will reinstall it.
#' @param force.source Logical (defaults to `FALSE`). If `TRUE`,` will skip CRAN
#'   and MRAN attempts, download tar.gz, and install from it.
#' @param current.deps Dependencies that will install current version regardless
#'   of date.
#'
#' @importFrom utils tail
get.snowball <- function(pkg, date, include.suggests = FALSE, force.source = FALSE, current.deps = "Rcpp") {

  # 1) Get dependencies
  dep12 <- get.all.dependencies(pkg, date)

  # 2) process dep12  (topographical sort) to get order of installation
  k <- 0 # counter of iterations attempting to sort
  indep <- c()
  # In each loop we take all dependencies without dependencies and add them to the sequence of installation
  # Do until all dependencies have been assigned an order (so dep12 is empty)
  while (nrow(dep12) > 0) {
    k <- k + 1
    indep.rows <- !(dep12$dep2 %in% dep12$pkg) ## Find dependecies without dependencies  TRUE/FALSE vector
    # Add those dependencies to the list of independencies
    indepk <- unique(as.character(dep12$dep2[indep.rows]))
    indep <- c(indep, indepk)
    # Drop those rows from both
    dep12 <- dep12[!indep.rows, ]
    # Safety valve in case loop impossible to end
    if (k == 50000) {
      break
    }
  }

  # 3) Add pkg at the end
  snowball.pkg <- c(indep, pkg)

  # 4) Get the version of each package
  snowball.vrs <- vapply(snowball.pkg, get.version, date, current.deps, FUN.VALUE = character(1)) # current.deps replaces the version of those dep with the version that's current when installing
  snowball.pkg_vrs <- paste0(snowball.pkg, "_", snowball.vrs)

  # 5 Snowball table, with installed | CRAN | MRAN | TARBALL | INSTALLATION TIME
  snowball.installed <- mapply(is.pkg_vrs.installed, snowball.pkg, snowball.vrs) # 5.1 Installed?  TRUE/FALSE
  snowball.CRAN <- (snowball.pkg_vrs %in% current.packages$pkg_vrs) & (max(toc("R")$Version) == get.rversion()) # 5.2 Binary in CRAN and using most recent R TRUE/FALSE
  snowball.MRAN.date <- as.Date(sapply(snowball.pkg_vrs, get.date.for.install.binary), origin = "1970-01-01") # 5.3 Binary date in MRAN?
  snowball.MRAN.date <- as.DateYMD(snowball.MRAN.date)
  snowball.MRAN <- !snowball.MRAN.date == "1970-01-01"
  snowball.from <- ifelse(snowball.MRAN, "MRAN", "source") # MRAN if available, if not source
  snowball.from <- ifelse(snowball.CRAN, "CRAN", snowball.from) # Replace MRAN if CRAN is available and using most recent version of R

  # Installation time from source
  snowball.time <- round(mapply(get.installation.time, snowball.pkg, snowball.vrs), 0)

  # Adjust installation time
  adjustment <- 2.5 # install times in CRAN's page are systematically too long, this is an initial adjustment factor
  snowball.time <- pmax(round(snowball.time / adjustment, 0), 1)

  # Installation path
  # Only r.path portion differs across where the package is obtained
  r.path <- vapply(seq_along(snowball.pkg), function(k) {
    switch(
      snowball.from[k],
      "MRAN" = get.version("R", snowball.MRAN.date[k]),
      "CRAN" = max(toc("R")$Version),
      "source" = get.rversion()
    )
  }, character(1))

  # Vector with paths
  snowball.installation.path <- paste0(.pkgenv[["groundhogR.folder"]], "/R-", r.path, "/", snowball.pkg_vrs)


  # data.frame()
  snowball <- data.frame(snowball.pkg, snowball.vrs, snowball.pkg_vrs, # Identify pkg
    snowball.installed, # Installed?
    snowball.from, # Where to install from
    snowball.MRAN.date, # MRAN date, in case MRAN is tried
    snowball.time, # time to install
    snowball.installation.path,
    stringsAsFactors = FALSE
  ) # directory to save the binary package to
  # how long will it take

  names(snowball) <- c("pkg", "vrs", "pkg_vrs", "installed", "from", "MRAN.date", "installation.time", "installation.path")


  return(snowball)
}
