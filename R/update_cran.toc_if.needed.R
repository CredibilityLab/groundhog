#' Update `cran.toc` if needed
#'
#' Update `cran.toc` if it's not recent enough to include the required date.
#'
#' @param date required date, in the format "%Y-%m-%d" (ISO 8601 convention)
#'
#' @return `TRUE` or `FALSE` depending on whether `cran.toc` was updated or not.
#'
#' @seealso [load.cran.toc()]
#'
# @examples
# \donttest{
# groundhog:::update_cran.toc_if.needed("2020-03-01")
# }
update_cran.toc_if.needed <- function(date) {
  # 1 Format entered date by user
  date <- as.DateYMD(date)

  # Stop if date is in the future
  if (date > Sys.Date() - 2) {
    message2()
    message1(
      "Groundhog's database is updated multiple times a day, but, to ensure",
      " reproducibility of your script, given time zone differences and ",
      "delays in updating different CRAN mirrors, don't use a date more ",
      "recent than two days ago, (i.e., the most recent date you may use ",
      "today with groundhog is:'", format(Sys.Date() - 2), "')."
    )
    exit("################### invalid date ##########################")
  }

  # 2 Load cran.toc if not yet loaded
  if (is.null(.pkgenv[["cran.toc"]])) {
    load.cran.toc(update.toc = FALSE)
  }

  cran.toc <- .pkgenv[["cran.toc"]]

  # 3 If user wants  a newer date than available, or if their version of R is newer than that in cran.toc.rds - update it.
  cran.toc$Published <- as.DateYMD(cran.toc$Published) # Convert cran.toc $Published, to a date variable
  max.date <- max(cran.toc$Published) # Most recent date in cron

  # 4 Compare most recent to entered date
  if (max.date < date) {
    message2()
    message1(
      "The date you entered, '", format(date), "', requires updating  your local database with the list of all CRAN package-versions (cran.toc.rds), \n",
      "because it goes only until ", format(max.date), ". That file is being updated now."
    )
    # Update the database
    return(load.cran.toc(TRUE))
  }

  # 5 Also update if the  version of R being used is newer than that in cran.toc.rds
  # Get verison of R being used
  R.using <- get.rversion()
  tocR <- toc("R")
  # If not in cran.toc, update
  if (!R.using %in% tocR$Version) {
    message2()
    message1(
      "The file cran.toc.rds,  with the list of CRAN package-versions, is older than the version of R you ",
      "are using. That file is being updated now."
    )

    return(load.cran.toc(TRUE))
  } else {
    return(FALSE)
  }
}
