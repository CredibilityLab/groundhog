#' update cran.toc() if needed
#' @param date In the format "%Y-%m-%d" (ISO 8601 convention)
update_cran.toc_if.needed <- function(date) {
  # 1 Format entered date by user
  date <- as.DateYMD(date)

  # 2 Load cran.toc if not yet loaded
  if (!exists("cran.toc")) load.cran.toc(update.toc = FALSE)

  # 3 If user wants  a newer date than available, update it.
  cran.toc$Published <- as.DateYMD(cran.toc$Published) # Convert cran.toc $Published, to a date variable
  max.date <- max(cran.toc$Published) # Most recent date in cron

  # 4 Compare most recent to entered date
  if (max.date < date) {
    cat2()
    cat1(paste0(
      "The date you entered, '", date, "', requires updating your database with the list of CRAN package versions, \n",
      " for it goes only until ", max.date, ".The udate is happening as you read this. "
    ))

    # Stop if date is in the future
    msg.future.date <- paste0(
      "GroundhogR's database is updated multiple times a day, but, to ensure reproducibility of your script,",
      "given time zone differences and delays in updating different CRAN mirrors, don't use a date more recent than ",
      "two days ago, (i.e., the most recent date you may use today with groundhogR is:'", Sys.Date() - 2, "')."
    )
    if (date > Sys.Date() - 2) {
      cat2()
      cat1(msg.future.date)
      stop()
    }
    # Update the database
    load.cran.toc(TRUE)
  } # End if desired date is after most recent date
} # End 2.12
