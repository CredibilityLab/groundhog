# Check if package version is installed for current or past R versions,


get.installed_path <- function(pkg, vrs) {
  
  pkg_search_path <- get.pkg_search_paths(pkg, vrs)

  installed_path <- find.package(pkg, pkg_search_path, quiet = TRUE)

  if (length(installed_path) == 0) {
    return("")
  } else {
    return(pkg_search_path)
  }

}
