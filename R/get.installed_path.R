#' Check if package version is installed for current or past R versions,
#'
#' @inheritParams get.pkg_search_paths
#'
#' @return If the package with the specific version number is installed, the
#'   installation path is returned. Otherwise, an empty string (`""`) is
#'   returned.
#'
# @examples
# \dontrun{
# groundhog:::get.installed_path("magrittr", "1.5")
# }
#'
get.installed_path <- function(pkg, vrs) {
  # Get full paths
  pkg_search_path <- get.pkg_search_paths(pkg, vrs)

  installed_path <- find.package(pkg, pkg_search_path, quiet = TRUE)

  if (length(installed_path) == 0) {
    return("")
  } else {
    return(pkg_search_path)
  }

}
