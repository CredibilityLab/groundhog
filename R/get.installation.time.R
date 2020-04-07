#' Get installation time
#'
#' Get installation time for a given `pkg` and `vrs`. This is estimated from
#' CRAN data about installation time on their build servers.
#'
#' @inheritParams get.pkg_search_paths
#'
#' @return the required time (in seconds) to install the `pkg` with the specific
#'   `vrs`.
#'
#' @examples
#' \donttest{
#' get.installation.time("magrittr", "1.5")
#' }
#'
get.installation.time <- function(pkg, vrs) {

  cran.times <- .pkgenv[["cran.times"]]

  dfk <- cran.times[cran.times$pkg_vrs == paste0(pkg, "_", vrs), ] # subset of package
  if (nrow(dfk) == 1) {
    return(dfk$installation.time) # lookup installation times
  } else {
    return(180)
  } # if not found, assume 3 minutes
}
