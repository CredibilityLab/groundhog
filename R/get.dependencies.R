#' Get dependencies for one package
#'
#' Get direct dependencies for one package `pkg`, for the version on CRAN at the
#' given `date`.
#'
#' @param pkg character. Name of the package to install
#' @param date date (in the format "%Y-%m-%d"), Required date for the package.
#' @param include.suggests Logical (defaults to `FALSE`. Should suggested
#'   packages be installed?
#' @return A character vector containing the package dependencies for `pkg`, for
#'   the version on CRAN at `date.`
#'
# @examples
# \donttest{
# groundhog:::get.dependencies("magrittr", "2018-02-12", include.suggests = TRUE)
# }
#'
#' @seealso [get.all.dependencies()] for recursive (=indirect) dependencies
#'
get.dependencies <- function(pkg, date, include.suggests = FALSE) {

  update_cran.toc_if.needed(date = date)
  cran.toc <- .pkgenv[["cran.toc"]]

  # Get version from date
  vrs <- get.version(pkg, date)

  # Get dependencies if version exists
  row <- cran.toc[cran.toc$Package == pkg & cran.toc$Version == vrs, c("Imports", "Depends", "Suggests", "LinkingTo")]
  dep <- c(row$Imports, row$Depends, row$LinkingTo) # merge
  if (include.suggests) {
    dep <- c(dep, row$Suggests) # add 'Suggests' dependencies if requested
  }
  dep <- unlist(strsplit(dep, ","))  # turn to array
  dep <- dep[!dep %in% base_pkg()]   # base dependencies from R
  dep <- unique(dep)                 # some dependencies can be listed both in Imports and LinkingTo, keep only one

  #Drop non-cran
  non.cran <- dep[!dep %in% cran.toc$Package]
  dep <- dep[!dep %in% non.cran]

  #Give warning (commented out because this code is executed twice within a groundhog.library() call, and so it duplicates the warning)
  #if (length(non.cran)>0) {   #This only considers non-cran dependencies which are not the packaae of interest, to avoid double reporting it
  #  message2("groundhog.library() Warning: Missing dependencies *??!")
  # message1("The following dependencies: '",non.cran,"' were not found on CRAN and their installation will not be attempted.")
  # }

  # These steps are normally taken care of server side but let's stay on the
  # safe side
  dep <- dep[dep != ""] # drop empty values
  dep <- dep[dep != "R"] # drop R as a dependency
  dep <- dep[is.na(dep)] # drop NA

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
# @examples
# \donttest{
# groundhog:::get.all.dependencies("magrittr", "2018-02-12", include.suggests = TRUE)
# }
#'
#' @seealso [get.dependencies()] for direct dependencies only
#'
get.all.dependencies <- function(pkg, date, include.suggests = FALSE) {


  # 5.1. Populate the starting point with this package and its dependencies
  # [a] Vector with pending deps, for looping install
  pending <- get.dependencies(pkg, date, include.suggests = include.suggests) # include.suggests=TRUE means that suggested dependencies and their dependencies are installed.

  if (length(pending) == 0) {
    return(data.frame(pkg = character(), dep2 = character()))
  }

  # [b] dep12: data.frame with two columns, pkg-left, dependency-right, for snowball loading
  dep12 <- data.frame(pkg = pkg, dep2 = pending)

  # 5.2 Loop over pending, adding to dep12, and both adding and subtracting from pending till it's empty
  k <- 1
  while (length(pending) != 0) {
    # 5.3 Grab the first among the pending dependencies
    depk <- pending[1]

    # 5.4 Get its dependencies
    pendingk <- get.dependencies(pkg = depk, date = date, include.suggests = FALSE) # Note: never include suggested deps for deps

    # 5.5 Drop depk from pending
    pending <- pending[-match(depk, pending)]

    # 5.6 if pendingk not empty, update pending and dep12
    if (length(pendingk) > 0) {
      # [a] Process pendingk prior to adding to pending()
      # Already processed dropped
      already.processed <- pendingk %in% dep12[, 1] # identify in pending those already processed
      pendingk.net <- pendingk[!already.processed] # drop them
      pending <- unique(c(pending, pendingk.net)) # Unique so that if we add somethign already pending we don't add it

      # Add pendingk.net to dep12 if any
      if (length(pendingk.net) > 0) {
        dep12k <- data.frame(pkg = depk, dep2 = pendingk.net)
        dep12 <- rbind(dep12, dep12k)
      }
    } # End 5.5 if some new dependencies to add

    k <- k + 1
    if (k > 50000) break # In case the loop does not converge to a stable dataframe
  } # End while
  return(dep12)
} # End function get.all.dependencies
