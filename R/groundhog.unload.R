#' Load `cran.toc`
#'
#' Unload a package upon request, unloading all its parents (reverse-dependencies)
#'
#' @param pkg character, name of package to be unoaded
#'
#' @return (invisibly) `TRUE`/`FALSE` depending on the success/failure of this
#'   operation.
#'
#'
# @examples
# \donttest{
# groundhog:::groundhog.unload('rio')
# }

