get.current.packages <- function() {

  # Get available packages to see if each attempted to install is new
  current.packages <- as.data.frame(
    available.packages(contriburl = contrib.url(repos = "https://cran.r-project.org/"), type = "binary")[, c(1, 2)],
    stringsAsFactors = FALSE
  )
  current.packages$pkg_vrs <- paste0(current.packages$Package, "_", current.packages$Version)

  .pkgenv[["current.packages"]] <- current.packages

  return(TRUE)

}

