.mismatch.warning <- new.env(parent = emptyenv())

.pkgenv <- new.env(parent = emptyenv())

#'
.onLoad <- function(libname, pkgname) {
  .pkgenv[["supportsANSI"]] <- Sys.getenv("TERM") %in% c("xterm-color", "xterm-256color", "screen", "screen-256color")

}

#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname) {

  current.packages <- try(get.current.packages("source"), silent = TRUE)

  groundhog_cran <- current.packages$Version[current.packages$Package == "groundhog"]

  # isTRUE() is necessary here because this will return logical(0) is the pkg
  # is not on CRAN, or if where working offline (current.packages is NULL in
  # this case).
  if (isTRUE(package_version(groundhog_cran) > packageVersion("groundhog"))) {
    packageStartupMessage(
      "A more recent version of groundhog is available. Please install it by ",
      'running install.packages("groundhog") and then restart your R session.'
    )
  }
}
