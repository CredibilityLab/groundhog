.mismatch.warning <- new.env(parent = emptyenv())

.pkgenv <- new.env(parent = emptyenv())

#'
.onLoad <- function(libname, pkgname) {
  .pkgenv[["supportsANSI"]] <- Sys.getenv("TERM") %in% c("xterm-color", "xterm-256color", "screen", "screen-256color")
}

#' @importFrom utils packageVersion compareVersion
.onAttach <- function(libname, pkgname) {

  groundhog.version_cran <- tryCatch(
    as.character(readLines("http://groundhogr.com/groundhog_version.txt")),
    warning = function(w) NULL,
    error = function(e) NULL
  )

  # isTRUE() is necessary here because this will return logical(0) if the pkg
  # is not on CRAN, or if working offline (current.packages is NULL in this case).
  groundhog.version_using <- as.character(packageVersion("groundhog"))
  if (isTRUE(groundhog.version_cran > groundhog.version_using)) {
    packageStartupMessage(
      'groundhog says: "OUTDATED"\nYou are using version ', groundhog.version_using, " and the current version is ",
      groundhog.version_cran, '. Please update by running: \ninstall.packages("groundhog")'
    )
  }
}
