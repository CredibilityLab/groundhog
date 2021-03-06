#' Show CRAN publication dates for all versions of a given package
#'
#' @param pkg (required) package name
#' @param dependencies logical (defaults to `FALSE`). Should the output contain
#'    package dependencies (`Imports`, `Depends` and `Suggests`) for `pkg`.
#'
#' @return a `data.frame` where each row corresponds to one version of `pkg`, a date column contains the publication date,
#' and when `dependecies`=TRUE, columns show package dependencies over time as well.
#'
#' @examples
#' \dontrun{
#' toc("R")
#' toc("magrittr")
#' toc("rio",dependencies = TRUE)
#' }
#'
#' @export
toc <- function(pkg, dependencies = FALSE) {

  if (is.null(.pkgenv[["cran.toc"]])) {
    load.cran.toc(update.toc = FALSE)
  }
  cran.toc <- .pkgenv[["cran.toc"]]

  if (dependencies) {
    output <- cran.toc[cran.toc$Package == pkg, c("Version", "Published", "Imports", "Depends", "Suggests", "LinkingTo")]
  } else {
    output <- cran.toc[cran.toc$Package == pkg, c("Version", "Published")]
  }

  if (nrow(output) == 0) {
    message2()
    message1(
      "There is no package '", pkg, "' in our database of all CRAN packages ever posted.\n",
      "   Keep in mind that:\n",
      "    1. package names are cAsE seNsiTive\n",
      "    2. Only CRAN packages can be loaded via groundhog (not github or bioconductor)"
    )
    exit()
  }


  output <- output[order(output$Published), ]
  rownames(output) <- NULL

  return(output)
}
