#' Get dependencies for one package
#'
#' Get direct dependencies for one package `pkg`, for the version on CRAN at the
#' given `date`.
#'
#' @param pkg character. Name of the package to install
#' @param date date (in the format "%Y-%m-%d"), Required date for the package.
#' @param include.suggests Logical (defaults to `FALSE`. Should suggested
#'   packages be installed?
#'
#' @return A character vector containing the package dependencies for `pkg`, for
#'   the version on CRAN at `date.`
#'
#' @examples
#' \donttest{
#' groundhog:::get.dependencies("magrittr", "2018-02-12", include.suggests = TRUE)
#' }
#'
#' @seealso [get.all.dependencies()] for recursive (=indirect) dependencies
#'
#' @importFrom utils installed.packages
#'
get.dependencies <- function(pkg, date, include.suggests = FALSE) {
  if (is.null(.pkgenv[["cran.toc"]])) {
    load.cran.toc(update.toc = FALSE)
  }
  cran.toc <- .pkgenv[["cran.toc"]]

  # Get version from date
  vrs <- get.version(pkg, date)
  # Get dependencies if version exists
  row <- cran.toc[cran.toc$Package == pkg & cran.toc$Version == vrs, c("Imports", "Depends", "Suggests", "LinkingTo")] # row in mastertoc
  dep <- c(row$Imports, row$Depends, row$LinkingTo) # merge
  if (include.suggests) {
    dep <- c(dep, row$Suggests) # add 'Suggests' dependencies if requested
  }

  dep <- unlist(strsplit(dep, ",")) # turn to array
  dep <- dep[dep != ""] # drop empty values
  dep <- dep[dep != "R"] # drop R as a dependency
  dep <- dep[!dep %in% installed.packages(priority = "base")] # base dependencies from R
  return(dep)
} # End get.dependencies()

#' Get recursive dependencies
#'
#' Get all recursive (=indirect) dependencies for one package `pkg`, for the
#' version on CRAN at the given `date`.
#'
#' @inheritParams get.dependencies
#'
#' @return a `data.frame` where the first column (`pkg`) contains non-terminal
#'   dependencies and the second column (`dep2` contains a dependencies of the
#'   package in column `pkg`.
#'
#' @examples
#' \donttest{
#' groundhog:::get.all.dependencies("magrittr", "2018-02-12", include.suggests = TRUE)
#' }
#'
#' @seealso [get.dependencies()] for direct dependencies only
#'
get.all.dependencies <- function(pkg, date, include.suggests = FALSE) {
  # 5.1. Populate the starting point with this package and its dependencies
  # [a] Vector with pending deps, for looping install
  pending <- get.dependencies(pkg, date, include.suggests = include.suggests) # include.suggests=TRUE means that suggested dependencies and their depdencies are installed.

  # [b] dep12: data.frame with two columns, pkg-left, dependency-right, for snowball loading
  dep12 <- data.frame(pkg = as.character(), dep2 = as.character())

  if (length(pending > 0)) {
    dep12 <- data.frame(pkg = pkg, dep2 = pending)
  }

  # 5.2 Loop over pending, adding to dep12, and both adding and subtracting from pending till it's empty
  k <- 1
  while (length(pending) != 0) {
    # 5.3 Grab the first among the pending dependencies
    depk <- pending[1]

    # 5.4 Get its dependencies
    pendingk <- get.dependencies(pkg = depk, date = date, include.suggests = FALSE) # NEVER include suggested deps for deps

    # 5.5 Drop depk from pending
    pending <- pending[-match(depk, pending)]

    # 5.6 if pendingk not empty, update pending and dep12
    if (length(pendingk) > 0) {
      # [a] Process pendingk prior to adding to pending()
      # drop empty
      # FIXME: this line should not be necessary since there should be not
      # context where an empty element is returned
      pendingk <- pendingk[pendingk != ""]
      # Already processed dropped
      already.processed <- pendingk %in% dep12[, 1] # identify in pending those already processed
      pendingk.net <- pendingk[!already.processed] # drop them
      pending <- unique(c(pending, pendingk.net)) # Unique so that if we add somethign already pending we don't add it
      pending <- pending[pending != ""] # drop empty

      # Add pendingk.net to dep12 if any
      if (length(pendingk.net) > 0) {
        dep12k <- data.frame(pkg = depk, dep2 = pendingk.net)
        dep12 <- rbind(dep12, dep12k)
      }
    } # End 5.5 if some new dependencies to add

    k <- k + 1
    if (k > 50000) break # In case the loop does not converge to a stable dataframe
  } # End while
  return(dep12 = dep12)
} # End function get.all.dependencies
