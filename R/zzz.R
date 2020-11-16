.mismatch.warning <- new.env(parent = emptyenv())

.pkgenv <- new.env(parent = emptyenv())

#'
.onLoad <- function(libname, pkgname) {
  .pkgenv[["supportsANSI"]] <- Sys.getenv("TERM") %in% c("xterm-color", "xterm-256color", "screen", "screen-256color")

}

#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname) {

  current.packages <- tryCatch(
    get.current.packages(getOption("pkgType")),
    warning = function(w) NULL,
    error = function(e) NULL
  )

  groundhog_cran <- current.packages$Version[current.packages$Package == "groundhog"]

  # isTRUE() is necessary here because this will return logical(0) if the pkg
  # is not on CRAN, or if working offline (current.packages is NULL in this case).
  if (isTRUE(package_version(groundhog_cran) > packageVersion("groundhog"))) {
    packageStartupMessage(
      "A more recent version of groundhog is available. Please install it by ",
      'running install.packages("groundhog").'
    )
  }
}
