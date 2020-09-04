get.current.packages <- function(type) {

  # Get available packages to see if each attempted to install is new
  current.packages <- as.data.frame(
    available.packages(contriburl = contrib.url(repos = "https://cloud.r-project.org/", type = type))[, c(1, 2)],
    stringsAsFactors = FALSE
  )
  current.packages$pkg_vrs <- paste0(current.packages$Package, "_", current.packages$Version)

  return(current.packages)

}

