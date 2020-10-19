#' Installation feedback
#'
#' Print progress about the package installation process (number of packages
#' already installed or remaining, time left, etc.)
#'
#' @inheritParams r.version.check
#' @inheritParams estimate.seconds.left
#'
installation.feedback <- function(k, date, snowball, start.time) {

  # Get R being used and needed
  rv <- r.version.check(as.DateYMD(date))

  # How much time so far in this installation process
  time.so.far <- as.numeric(difftime(Sys.time(), start.time, units = "secs"))

  # Shorter variables
  N <- nrow(snowball)
  pkg_vrs <- snowball[N, "pkg_vrs"]

  # Estimate of time
  seconds.left <- estimate.seconds.left(k, start.time, snowball)
  finish.time.estimate <- format(Sys.time() + seconds.left, "%H:%M")
  finish.time.max <- format(Sys.time() + seconds.left * 3, "%H:%M")


  # Show feedback
  msg.header <- paste0("\n\n\ngroundhog.library(), says [using R-", get.rversion(), "]: Installing '", snowball[k, "pkg_vrs"], "', package #", k, " out of ", N, " needed for '", pkg_vrs, "'")
  msg <- c(
    "> The time now is ", format(Sys.time(), "%H:%M"), ", and we quite roughly estimate the",
    "  process to end around ", finish.time.estimate, "\n",
    "> It is unlikely to finish after ", finish.time.max, "\n",
    "> These estimates will be revised after each package installs,\n",
    "  but they will remain noisy estimates."
  )

  # Add msg if R mismatch and recent enough for groundhog
  if (rv$r.using.majmin != rv$r.need.majmin & date > "2014-09-18") {
    msg <- c(
      msg,
      "\n> Installation is slow because you are using R-", get.rversion(), "\n",
      "> If you run this script with the R version current for the date you entered: '", date, "', i.e., R-", get.version("R", date), ",\n",
      "  the entire installation would take about a few minutes at most.\n",
      "> Instructions for running other versions of R on demand:  https://groundhogR.com/many"
    )
  } # End if R mismach

  # Add message if installing from source taht output has been supressed
  if (snowball[k, "from"] == "source") {
    msg <- c(
      msg,
      "\n\n> When installing a package from source, abundant and fast-speed output is generated \n",
      " flooding the console where these messages are printed. Thus, groundhog.library() supresses\n",
      " such output. You may run groundhog.library() with the option 'quiet=FALSE' to display all output."
    ) # End of message
  } # End of if source file



  message2(msg.header)
  message1(msg)
}
