#' Get package search path
#'
#' Determine paths to search for an already installed package (versions of same
#' minor, in reverse chronological order).
#'
#' @inheritParams get.dependencies
#' @param vrs character. Version of the package to install.
#'
#' @return the path where the package from a specific version is/will be
#'   installed.
#'
#' @examples
#' \donttest{
#' groundhog:::get.pkg_search_paths("magrittr", "1.0.1")
#' }
#'
get.pkg_search_paths <- function(pkg, vrs) {

  rv <- as.character(getRversion())

  # Get rid of patch version number
  rv <- gsub("\\.\\d+(-w)?$", "", rv)

  # paths
  pkg_search_paths <- paste0(get.groundhog.folder(), "/R-", rv, "/", pkg, "_", vrs)

  # Ensure directories exist
  return(pkg_search_paths)
}
