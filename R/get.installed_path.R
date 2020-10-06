#' Check if package version is installed for current or past R versions,
#'
#' @inheritParams get.pkg_search_paths
#'
#' @return If the package with the specific version number is installed, the
#'   installation path is returned. Otherwise, an empty string (`""`) is
#'   returned.
#'
# @examples
# \donttest{
# groundhog:::get.installed_path("magrittr", "1.5")
# }
#'
get.installed_path <- function(pkg, vrs) {
  # Get full paths
  pkg_search_paths <- get.pkg_search_paths(pkg, vrs)

  # Search for package till found
  installed_path <- ""
  for (pathk in pkg_search_paths) {
    # If directory and file exists, test if package is installed, if file does not exist; it is not installed anyway
    if (file.exists(pathk)) {
      # See if there isa  package there
      df.pkg <- as.data.frame(installed.packages(lib.loc = pathk)) # Note: in {groundhog} each package version gets a 'library' within the R version
      if (nrow(df.pkg) > 0) {
        # If there is, check its version (is unlikely but possible for wrong version to get installed (e.g., wrong Published.date in DESCRIPTION
        # Get installed version
        installed_version <- df.pkg$Version[df.pkg$Package == pkg]
        # If different from desired, delete folder
        if (installed_version != vrs) {
          unlink(pathk, recursive = TRUE)
        }
        # If not different, it is installed output path
        if (installed_version == vrs) {
          installed_path <- pathk
          break
        } # End if correct version installed
      } # If file exists identified by possible search path
    } # End if found
  } # End:   for (pathk in
  return(installed_path)
}
