#' Estimate seconds left in install
#'
#' @param k numeric. Current step in the snowball process.
#' @param snowball the result of [get.snowball()].
#' @param start.time the result of [Sys.time()] when the installation process
#'   started
#'
#' @return the remaining time left in seconds
#'
estimate.seconds.left <- function(k, start.time, snowball) {
  # Time assigned to binary installs
  time.per.binary <- 25 # assumed 25 seconds per binary
  # Time so far
  time.so.far <- as.numeric(difftime(Sys.time(), start.time, units = "secs"))

  # Total time estimate
  estimated.total.source <- round(sum(snowball[snowball$from == "source" & !snowball$installed, ]$installation.time), 0) # For source, use estimated install time
  estimated.total.binary <- time.per.binary * sum(snowball$from != "source" & !snowball$installed) # For binary, assume 25 seconds per package that is not installed
  estimated.total <- estimated.total.source + estimated.total.binary

  # subset of snowball packages left
  N <- nrow(snowball)
  snowball.left <- snowball[k:N, ]

  estimated.left.source <- round(sum(snowball.left[snowball.left$from == "source" & !snowball.left$installed, ]$installation.time), 0)
  estimated.left.binary <- time.per.binary * sum(snowball.left$from != "source" & !snowball.left$installed) # For binary, assume 5 seconds per package that is not installed
  estimated.left <- estimated.left.source + estimated.left.binary

  if (k == 1 | time.so.far < 30) {
    return(estimated.total)
  }
  # If not the first package, adjust estimate
  if (k > 1) {
    actual.past <- round(as.numeric(Sys.time() - start.time, units = "secs"), 0)
    estimated.past.source <- sum(snowball$installation.time[1:(k - 1)][snowball$from[1:(k - 1)] == "source" & !snowball$installed[1:(k - 1)]])
    estimated.past.binary <- time.per.binary * sum(snowball.left$from != "source" & !snowball.left$installed) # For binary, assume 5 seconds per package that is not installed
    estimated.past <- estimated.past.source + estimated.past.binary

    progress <- estimated.past / estimated.total

    ae.ratio <- (actual.past / estimated.past) * progress + 1 * (1 - progress) # The ratio of actual/adjustment is a weighted average of just 1 (use estimate as is), and the empirical reatio so far in this intallation progress so fart
    return(ae.ratio * estimated.left)
  }
}
