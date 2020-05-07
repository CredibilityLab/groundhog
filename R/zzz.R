.mismatch.warning <- new.env(parent = emptyenv())

.pkgenv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  .pkgenv[["supportsANSI"]] <- Sys.getenv("TERM") %in% c("xterm-color", "xterm-256color", "screen", "screen-256color")
}
