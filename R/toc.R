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

  #Load full_toc (cran.toc plus possible remotes)
  full_toc <- get.full_toc()  
  
      #Note 1: parameters pkg & date are optional, without them we don't actively re-create baton
      #Note 2: if the .env version exists, it gives that one, which is faster, if it is not availble, it loads from cran.toc.rds

  if (dependencies==TRUE) {
    output <- full_toc[full_toc$Package == pkg, c("Version", "Published", "Imports", "Depends", "Suggests", "LinkingTo")]
  } else {
    output <- full_toc[full_toc$Package == pkg, c("Version", "Published")]
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
