#' Show table of contents (toc) (package versions and publication dates) for a pkg, sorted chronologically
#' @export
toc <- function(pkg, dependencies = FALSE) {

  if (is.null(.pkgenv[["cran.toc"]])) {
    load.cran.toc(update.toc = FALSE)
  }
  cran.toc <- .pkgenv[["cran.toc"]]

  if (dependencies) {
    output <- cran.toc[cran.toc$Package == pkg, c("Version", "Published", "Imports", "Depends", "Suggests")]
  } else{
    output <- cran.toc[cran.toc$Package == pkg, c("Version", "Published")]
  }

  if (nrow(output) == 0) {
    message2()
    message1(
      "There is no package '", pkg, "' in our database of all CRAN packages ever posted.\n",
      "   Keep in mind that:\n",
      "    1. package names are cAsE seNsiTive\n",
      "    2. The package name needs to be in quotes: e.g., toc('groundhogR') \n",
      "    3. Only CRAN packages can be loaded via groundhogR"
    )
    stop()
  }

  output <- output[order(output$Published), ]
  return(output)
}
