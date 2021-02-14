# Check mismatch between \R versions
#
# Warn if there is a mismatch between the current and required \R version
#
#
#seealso [r.version.check()] to get a named list containing the version
# numbers of the \R version currently in use and required.
#
check.mismatch.R <- function(date) {
  # Get versions of R being used a needed
  rv <- r.version.check(date)

  # If major or minor do not match, give warning
  if (rv$r.using.minmaj != rv$r.need.minmaj) {

    # Check if have not warned for the last hour
    now <- Sys.time()

    r.mismatched.last.time <- tryCatch(
      get("r.mismatched.last.time", envir = .mismatch.warning),
      error = function(e) NULL
    )

    # If there is not previous time, assign it a full year to trigger the warning
    if (is.null(r.mismatched.last.time)) {
      since.warning <- 60 * 365
    } else {
      # Time since last warning.
      since.warning <- as.numeric(round(difftime(now, r.mismatched.last.time, units = "mins"), 0))
    }

    # If more than a 60 minute gap give full warning
    if (since.warning > 60) {
      message2()
      message1("---------------- R Version check ----------------")
      message1(
        "You are using  R : '", rv$r.using.full, ", but for the date you entered (", date, "),\n",
        "the current R was: '", rv$r.need.full, "'"
      )
      message1(
        "This version mismatch has two consequences:\n",
        "1) Some of the packages you install, and their dependencies, probably will take longer to install,\n",
        "possibly *minutes* longer.\n",
        "2) There is some chance the original code won't run in the newer R version"
      )

      message2("\nBottom line")
      message1(
        "It's probably worth continuing as-is, and if you get tired of waiting for packages to install,\nor get errors trying to run ",
        "it, then you try it with the appropriate version of R. \nRunning older R is actually quite easy. To get instructions, execute:\nmsg.R.switch('", date, "') "
      )
      assign("r.mismatched.last.time", Sys.time(), envir = .mismatch.warning)

      message1("\n\nRegardless of your choice, this warning will not appear again within the next hour")
      quit.menu(date = date)
    } # End if more than 60 minutes since last warning

    # If less than 60 minute, short version
    if (since.warning <= 60) {
      message2()
      message1(
        "R Version mismatch. Using: R-", rv$r.using.full, " but on '", date, "' it was : R-", rv$r.need.full,
        ".\n Full warning shown earlier (scroll up), won't be shown again for the next ", 60 - since.warning, " minutes."
      )
    } # End if recently warned
  } # END warning check
} # End of function 2.13
