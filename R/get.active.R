#' Get active packages
#'
#' Get currently active (=loaded) packages in the session, with their version
#'
#' @return a `data.frame` with two columns:
#' \describe{
#'   \item{pkg}{the package name}
#'   \item{pkg_vrs}{the package name and its version, separated by an
#'   underscore}
#' }
#'
#' @examples
#' \donttest{
#' groundhog:::get.active()
#' }
#'
#' @importFrom utils installed.packages
#'
get.active <- function() {
  loaded.list <- utils::sessionInfo()$loadedOnly # pkgs in name space
  attached.list <- utils::sessionInfo()$otherPkgs # pkgs in attached
  active.pkg <- c(names(loaded.list), names(attached.list)) # Get names of active packages
  active.vrs <- c(lapply(loaded.list, function(x) x$Version), lapply(attached.list, function(x) x$Version))
  active.pkg_vrs <- paste0(active.pkg, "_", active.vrs) # merge to pkg_vrs

  # Drop those in base R
  active.in.base <- active.pkg %in% installed.packages(priority = "base")
  active.pkg <- active.pkg[!active.in.base]
  active.vrs <- active.vrs[!active.in.base]
  active.pkg_vrs <- active.pkg_vrs[!active.in.base]

  df <- data.frame(active.pkg, active.pkg_vrs, stringsAsFactors = FALSE)
  names(df) <- c("pkg", "pkg_vrs")
  df
}
