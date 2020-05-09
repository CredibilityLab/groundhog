.mismatch.warning <- new.env(parent = emptyenv())

.pkgenv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  .pkgenv[["supportsANSI"]] <- Sys.getenv("TERM") %in% c("xterm-color", "xterm-256color", "screen", "screen-256color")

  # 1.2 Get available packages to see if each attempted to install is new
  current.packages <- as.data.frame(
    available.packages(contriburl = contrib.url(repos = "https://cran.r-project.org/"))[, c(1, 2)]
  )
  current.packages$pkg_vrs <- paste0(current.packages$Package, "_", current.packages$Version)

  .pkgenv[["current.packages"]] <- current.packages
}
