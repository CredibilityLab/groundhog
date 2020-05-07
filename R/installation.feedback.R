#' Installation feedback
#'
#' Print progress about the package installation process (number of packages
#' already installed or remaining, time left, etc.)
#'
#' @inheritParams r.version.check
#' @inheritParams estimate.seconds.left
#'   about the installation process be printed in the plot console.
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
  msg.header <- paste0("\n\n\ngroundhog.library() says: Installing '", snowball[k, "pkg_vrs"], "', package #", k, " out of ", N, " needed for '", pkg_vrs, "'")
  msg <- paste0(
    "> The time now is ", format(Sys.time(), "%H:%M"), ", and we quite roughly estimate the ",
    "process to end around ", finish.time.estimate, "\n",
    "> It is unlikely to finish after ", finish.time.max, "\n",
    "> These estimates will be revised after each package installs, but they will remain noisy estimates.\n"
  )

  # Add msg if R mismatch
  if (rv$r.using.major != rv$r.need.major | rv$r.using.minor != rv$r.need.minor) {
    msg <- paste0(
      msg,
      "> Installation is slow because you are using R-", get.rversion(), "\n",
      "> If you run this script with the R version available on the date you entered: '", date, "', i.e., R-", get.version("R", date), ",\n",
      "   the entire installation would take about a minute or two.\n",
      "> Instructions for running older version of R:  https://grondhogR.com/olderR"
    )
  }
  message2(msg.header)
  message1(msg)
}
