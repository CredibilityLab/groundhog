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
#' @examples
#' update_cran.toc_if.needed("2020-03-01")
#'
update_cran.toc_if.needed <- function(date) {
  # 1 Format entered date by user
  date <- as.DateYMD(date)

  # Stop if date is in the future
  if (date > Sys.Date() - 2) {
    message2()
    message1(
      "GroundhogR's database is updated multiple times a day, but, to ensure",
      " reproducibility of your script, given time zone differences and ",
      "delays in updating different CRAN mirrors, don't use a date more ",
      "recent than two days ago, (i.e., the most recent date you may use ",
      "today with groundhogR is:'", Sys.Date() - 2, "')."
    )
    stop()
  }

  # 2 Load cran.toc if not yet loaded
  if (is.null(.pkgenv[["cran.toc"]])) {
    load.cran.toc(update.toc = FALSE)
  }

  cran.toc <- .pkgenv[["cran.toc"]]

  # 3 If user wants  a newer date than available, update it.
  cran.toc$Published <- as.DateYMD(cran.toc$Published) # Convert cran.toc $Published, to a date variable
  max.date <- max(cran.toc$Published) # Most recent date in cron

  # 4 Compare most recent to entered date
  if (max.date < date) {
    message2()
    message1(
      "The date you entered, '", date, "', requires updating your database with the list of CRAN package versions, \n",
      " for it goes only until ", max.date, ".The udate is happening as you read this. "
    )
    # Update the database
    load.cran.toc(TRUE)
    return(TRUE)
  } else {
    return(FALSE)
  }
}
