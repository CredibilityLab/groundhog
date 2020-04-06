#' Installation feedback
#'
#' Print progress about the package installation process (number of packages
#' already installed or remaining, time left, etc.)
#'
#' @inheritParams r.version.check
#' @inheritParams estimate.seconds.left
#' @param plot.console logical (defaults to `TRUE`). Should detailed feedback
#'   about the installation process be printed in the plot console.
#'
# FIXME: add @return
#'
installation.feedback <- function(k, date, snowball, start.time, plot.console = TRUE) {

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

  # If first one being installed, show dots
  if (k == 1 | time.so.far < 5 & plot.console) {
    plot(c(.25, .5, .75), c(.5, .5, .5), cex = 10, pch = 16, col = "red", xlim = c(0, 1), xaxt = "n", xlab = "**LOADING**", ylab = "", yaxt = "n")
    Sys.sleep(.75)
  }
  # Show plot.console feedback
  if (plot.console) {
    msg.plot <- paste0(
      "> groundhog.library() is in the process of installing '", pkg_vrs, "'.\n\n",
      "> '", pkg_vrs, "' depends on ", N - 1, " other packages which will be installed if needed as well.\n",
      "> We are now installing **", snowball[k, "pkg_vrs"], "**, #", k, " out of the ", N, " packages total:\n\n",
      "> The time now is ", format(Sys.time(), "%H:%M"), ", and we quite roughly estimate the ",
      "process to end around ", finish.time.estimate, "\n",
      "> It is unlikely to finish after ", finish.time.max, "\n",
      "> These estimates will be revised after each package installs, but they will remain noisy estimates.\n\n\n"
    )


    # Add msg if R mismatch
    if (rv$r.using.major != rv$r.need.major | rv$r.using.minor != rv$r.need.minor) {
      msg.plot. <- paste0(
        "> Installation is slow because you are using R-", get.rversion(), "\n",
        "> If you run this script with the R version available on '", date, "', i.e., on R-", get.version("R", date), ",\n",
        "   the entire installation would take about a minute or two.\n",
        "> Instructions for running older version of R:  http://tiny.cc/SwitchR"
      )
    }

    cat1.plot(msg.plot)
  } # ENd if plot.console==T
  # Show cat 1 feedback
  message2()
  message1(
    "Installing package #", k, " out of ", nrow(snowball), " needed.\npackage:",
    snowball[k, "pkg_vrs"], "'.\n\n"
  )
}
