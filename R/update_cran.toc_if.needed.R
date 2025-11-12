# Update `cran.toc` if it's not recent enough to include the required date.
#
# @param date required date, in the format "%Y-%m-%d" (ISO 8601 convention)
#
# @return `TRUE` or `FALSE` depending on whether `cran.toc` was updated or not.
#
#
# Example:  
#groundhog:::update_cran.toc_if.needed("2020-03-01")


update_cran.toc_if.needed <- function(date) {
  
  
  # 1 Format entered date by user
    date <- as.DateYMD(date)
  
  # Guard: Check if we've already processed a date >= this date in this session
  # This prevents redundant calls when get.version() is called multiple times
  # If we've checked a newer date, we don't need to check older dates again
  if (!is.null(.pkgenv[["last_update_check_date"]])) {
    last_checked_date <- .pkgenv[["last_update_check_date"]]
    if (date <= last_checked_date) {
      return(FALSE)
    }
    # If new date is greater, we'll proceed and update the stored date at the end
  }

  # Stop if date is in the future
      if (date > Sys.Date() - 2) {
        msg<-paste0(
          "To ensure reproducibility of your script, given timezone differences and\n",
          "delays in updating different CRAN mirrors, don't use a date more\n",
          "recent than two days ago: ", format(Sys.Date() - 2), ".")
		  gstop(msg) #util #51
      }

  # 2 Load cran.toc if not yet loaded
      if (is.null(.pkgenv[["cran.toc"]])) {
        load.cran.toc(update.toc = FALSE)
      }
    
      cran.toc <- .pkgenv[["cran.toc"]]
      
      
      
# 3 If user wants  a newer date than available, or if their version of R is newer than that in cran.toc.rds - update it.
  cran.toc$Published <- as.DateYMD(cran.toc$Published) # Convert cran.toc $Published, to a date variable
  max.date <- max(cran.toc$Published) # Most recent date in CRAN

  # 4 Compare most recent to entered date
  if (max.date < date) {
    message2()
    message1(
      "The date you entered, '", format(date), "' requires updating your local database\n",
      "with the list of all CRAN package-versions (cran.toc.rds)"
      )
    # Update the database
      result <- load.cran.toc(TRUE)
      # Save that we've checked this date (only if it's greater than previously stored)
      if (is.null(.pkgenv[["last_update_check_date"]]) || date > .pkgenv[["last_update_check_date"]]) {
        .pkgenv[["last_update_check_date"]] <- date
      }
      return(result)
  }

  # 4 Update if the  version of R being used is newer than that in cran.toc.rds AND cran toc is older than 2 days
      # Get version of R being used
        R.using <- get.rversion()
        tocR <- toc("R")
      # If not in cran.toc, and the cran.toc date is more than 2 days old
        if (!R.using %in% tocR$Version && max.date<  Sys.Date()-2) {
        message2()
        message1(
          "The local database with all package-versions available on CRAN \n",
		      "is older than the version of R you are using. That file is being updated now."
        )

        result <- load.cran.toc(TRUE)
        # Save that we've checked this date (only if it's greater than previously stored)
        if (is.null(.pkgenv[["last_update_check_date"]]) || date > .pkgenv[["last_update_check_date"]]) {
          .pkgenv[["last_update_check_date"]] <- date
        }
        return(result)
  } else {
    # Save that we've checked this date (only if it's greater than previously stored, even if no update was needed)
    if (is.null(.pkgenv[["last_update_check_date"]]) || date > .pkgenv[["last_update_check_date"]]) {
      .pkgenv[["last_update_check_date"]] <- date
    }
    return(FALSE)
  }
}
