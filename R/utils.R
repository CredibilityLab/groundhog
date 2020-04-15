# 1. Preliminaries
chooseCRANmirror(graphics = FALSE, ind = 1)

# 1.2 Get available packages to see if each attempted to install is new
current.packages <- data.frame(available.packages())[, c(1, 2)]
current.packages$pkg_vrs <- paste0(current.packages$Package, "_", current.packages$Version)

# 2.0.5 Parse pkg_vrs into pkg and vrs
get.pkg <- function(x) substr(x, 1, regexpr("_", basename(x)) - 1)
get.vrs <- function(x) substr(x, regexpr("_", basename(x)) + 1, nchar(x))

#' Is pkg_vrs installed (within same R-minor version)?
#'
#' @inheritParams get.installed_path
is.pkg_vrs.installed <- function(pkg, vrs) {
  (get.installed_path(pkg, vrs) %in% get.pkg_search_paths(pkg, vrs))
}

#' Format Y-M-D as date
#'
#' @param x character string containing the date in the format "%Y-%m-%d"
#'
as.DateYMD <- function(x) as.Date(x, format = "%Y-%m-%d")

# 2.2  R Being used
# 2.2.1 R Date
get.rdate <- function() {
  r.current <- R.version$version.string
  date <- paste0(R.version$year, "-", R.version$month, "-", R.version$day)
  return(as.DateYMD(date))
}

# 2.2.2 R Version
get.rversion <- function() {
  r.version <- paste0(R.version$major, ".", R.version$minor)
  return(r.version)
}

message1 <- function(..., domain = NULL, appendLF = TRUE) {

  if (.pkgenv[["supportsANSI"]]) {
    message(c("\033[36m", ..., "\033[0m"), domain = domain, appendLF = appendLF)
  } else {
    message(..., domain = NULL, appendLF = TRUE)
  }

}
message2 <- function(..., domain = NULL, appendLF = TRUE) {

  msg <- list(...)
  if (length(msg)==0) {
    msg <- c("groundhog.library() says [using R-", get.rversion(), "]:")
  }

  if (.pkgenv[["supportsANSI"]]) {
    message(c("\033[1;36m", msg, "\033[0m"), domain = domain, appendLF = appendLF)
  } else {
    message(msg, domain = NULL, appendLF = TRUE)
  }

}
# 2.8 Automatically name elements in list with name of the objects in the list
# https://stackoverflow.com/questions/16951080/can-lists-be-created-that-name-themselves-based-on-input-object-names
#' @importFrom stats setNames
namedList <- function(...) {
  L <- list(...)
  snm <- sapply(substitute(list(...)), deparse)[-1]
  if (is.null(nm <- names(L))) {
    nm <- snm
  }
  if (any(nonames <- nm == "")) {
    nm[nonames] <- snm[nonames]
  }
  setNames(L, nm)
}

# 2.10 Quit menu
quit.menu <- function(date) {
  message1(
    "Type 'Q', 'quit' or 'stop' to stop the script.\nAnything else to continue"
  )
  x <- readline("")
  if (tolower(x) %in% c("q", "quit", "stop")) {
    message2()
    message1("You typed ", x, " so script stops...")
    msg.R.switch(date)
    stop("---")
  } # End if quit

  message1("You typed '", x, "' the script continues...")

} # End quit.menu

# 2.18 Plot console
#' @importFrom graphics par plot segments text
cat1.plot <- function(x) {
  # Get existing margins to return to them after console use
  old.par <- par(mar = c(.25, .25, .25, .25))
  # Return margins to defaults on exit
  on.exit(par(old.par))

  # Catch user's attention
  #    plot(c(.25,.5,.75),c(.5,.5,.5),cex=10,pch=16,col='red',xlim=c(0,1))
  # Sys.sleep(.75)
  # Set no margins
  plot(1, 1, col = "white", xaxt = "n", yaxt = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1))
  text(.5, 1, "groundhogR's Console", font = 2, col = "cyan4")
  text(0, .85, adj = c(0, 1), x, font = 1, cex = .9, col = "cyan4")
  segments(x0 = 0, x1 = .4, y1 = .15, y0 = .15, col = "cyan4")
  text(0, .1, font = 1, pos = 4, cex = .75, col = "cyan4", "You can avoid this console by running:\ngroundhog.library(..., plot.console=FALSE)")

}
