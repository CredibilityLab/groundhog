#' Show joint table of contents (toc) for multiple packages
#'
#' @param pkgs character vector containing the package names.
#' @param date1,date2 date range to consider (in the format "%Y-%m-%d").
#'
#' @return A `data.frame` with 3 columns:
#' \describe{
#'   \item{Version}{The package version number}
#'   \item{Published}{The date at which the specific version was published}
#'   \item{Package}{The package name}
#' }
#'
#' @seealso [toc()] for the same function for a single package.
#'
#' @examples
#' 
#' \dontrun{
#' cross.toc(c("magrittr", "R"))
#' cross.toc(c("magrittr", "rlang"), date1 = "2012-02-01", date2 = "2020-02-01")
#' }
#'
#' @export
#'
cross.toc <- function(pkgs, date1 = "1970-1-1", date2 = Sys.Date()) {
  #Load cran.toc
    full_toc <- get.full_toc(date=date2)
  
  toc.all <- full_toc[full_toc$Package %in% pkgs, c("Version", "Published", "Package")]
  toc.all <- toc.all[order(toc.all$Published), ]
  # date subset
  toc.all <- toc.all[toc.all$Published > date1 & toc.all$Published < date2, ]
  rownames(toc.all) <- NULL

  return(toc.all)
}
