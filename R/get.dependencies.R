#' Get dependencies for one package on a given date
#'
#' Get direct dependencies for one package `pkg`. 
#' For CRAN packages it uses the date when it was published on CRAN
#' For GitHub and Gitlab package it is based on the last commit 
#' pushed on, or prior to the the `date`.


  get.dependencies <- function(pkg, date, include.suggests = FALSE) {

    full_toc <- get.full_toc()
  
      
  
  #1 If a base R package, return empty character vector (cannot look up in cran.toc.rds for it is not on cran)
    if (pkg %in% base_pkg()) {
      return(as.character())
    }
  
  #2 If a remote pkg (github, gitlab) read the 
    remote <- FALSE
    if (basename(pkg) != pkg) remote <- TRUE
    
    if (remote==TRUE) {
      pkg=git_usr_pkg_sha;
      
      
      
    }
    
  # Get version from date
      vrs <- get.version(pkg, date)

  # Get dependencies if version exists
      row <- full_toc[full_toc$Package == pkg & full_toc$Version == vrs, c("Imports", "Depends", "Suggests", "LinkingTo")]
      dep <- c(row$Imports, row$Depends, row$LinkingTo) # merge
      if (include.suggests) {
        dep <- c(dep, row$Suggests) # add 'Suggests' dependencies if requested
      }
      dep <- unlist(strsplit(dep, ","))  # turn to array
      dep <- unique(dep)                 # some dependencies can be listed both in Imports and LinkingTo, keep only one

  #Drop non-cran
    non.cran <- dep[!dep %in% full_toc$Package]
    dep <- dep[!dep %in% non.cran]


  # These steps are normally taken care of server side but let's stay on the safe side
    dep <- dep[dep != ""] # drop empty values
    dep <- dep[dep != "R"] # drop R as a dependency
    dep <- dep[!is.na(dep)] # drop NA

  return(dep)
} # End get.dependencies()

#' Get recursive dependencies
#'
#' Get all recursive (=indirect) dependencies for one package `pkg`, for the
#' version on CRAN at the given `date`.
#'
#'
#' @return a `data.frame` where the first column (`pkg`) contains non-terminal
#'   dependencies and the second column (`dep2` contains a dependencies of the
#'   package in column `pkg`.
#' @noRd
# @examples
# \dontrun{
# groundhog:::get.all.dependencies("magrittr", "2018-02-12", include.suggests = TRUE)
# }
#'
#seealso [get.dependencies()] for direct dependencies only

get.all.dependencies <- function(pkg, date, include.suggests = FALSE) {
  


  # 5.1. Populate the starting point with this package and its dependencies
  # [a] Vector with pending deps, for looping install
  pending <- get.dependencies(pkg, date, include.suggests) # include.suggests=TRUE means that suggested dependencies and their dependencies are installed.
                                                           
  if (length(pending) == 0) {
    return(data.frame(pkg = character(), dep2 = character(),stringsAsFactors = FALSE))
  }

  # [b] dep12: data.frame with two columns, pkg-left, dependency-right, for snowball loading
  dep12 <- data.frame(pkg = pkg, dep2 = pending, stringsAsFactors = FALSE)

  # 5.2 Loop over pending, adding to dep12, and both adding and subtracting from pending till it's empty
  k <- 1
  while (length(pending) != 0) {
    # 5.3 Grab the first among the pending dependencies
    depk <- pending[1]

    # 5.4 Get its dependencies
    pendingk <- get.dependencies(pkg = depk, date = date, include.suggests = FALSE) # Note: never include suggested deps for deps

    # 5.5 Drop depk from pending
    pending <- pending[-match(depk, pending)]
    
    #5.6 Add new pairs to  dep12 
      if (length(pendingk) > 0) {
        dep12k <- data.frame(pkg = depk, dep2 = pendingk)
        dep12 <- rbind(dep12, dep12k)
        }

    #5.7 Add new dependencies to pending 
    if (length(pendingk) > 0) {
      already.processed <- pendingk %in% dep12[, 1] # identify in pending those already processed
      pendingk.net <- pendingk[!already.processed] # drop them
      pending <- unique(c(pending, pendingk.net)) # Unique so that if we add something already pending we don't add it
    } # End 5.5 if some new dependencies to add
    
    k <- k + 1
    if (k > 50000) break # In case the loop does not converge to a stable dataframe
  } # End while
  return(dep12)
} # End function get.all.dependencies
