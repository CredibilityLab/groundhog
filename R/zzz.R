.mismatch.warning <- new.env(parent = emptyenv())

.pkgenv <- new.env(parent = emptyenv())

#' @importFrom utils packageVersion
#'
.onLoad <- function(libname, pkgname) {
  .pkgenv[["supportsANSI"]] <- Sys.getenv("TERM") %in% c("xterm-color", "xterm-256color", "screen", "screen-256color")

  # 1.2 Get available packages to see if each attempted to install is new
  current.packages <- as.data.frame(
    available.packages(contriburl = contrib.url(repos = "https://cran.r-project.org/"))[, c(1, 2)],
    stringsAsFactors = FALSE
  )
  current.packages$pkg_vrs <- paste0(current.packages$Package, "_", current.packages$Version)

  .pkgenv[["current.packages"]] <- current.packages

  groundhog_cran <- current.packages$Version[current.packages$Package == "groundhogR"]

  # isTRUE() is necessary here because this will return logical(0) is the pkg
  # is not on CRAN
  if (isTRUE(package_version(groundhog_cran) > packageVersion("groundhogR"))) {
    packageStartupMessage(
      "A more recent version of groundhogR is available. Please install it by ",
      'running install.packages("groundhogR") and then restart your R session.'
    )
  }
}
