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
#' @noRd
# @examples
# \dontrun{
# groundhog:::get.dependencies("magrittr", "2018-02-12", include.suggests = TRUE)
# }
#'
#' @noRd

get.dependencies <- function(pkg, date, include.suggests = FALSE) {

  update_cran.toc_if.needed(date = date) #also loads the cran.toc if not yet loaded
  cran.toc <- .pkgenv[["cran.toc"]]

  
  #If a base R package, return empty character vector (cannot lookup in cran.toc.rds for it is not on cran)
    if (pkg %in% base_pkg()) {
      return(as.character())
    }
  
  # Get version from date
  vrs <- get.version(pkg, date)

  # Get dependencies if version exists
  row <- cran.toc[cran.toc$Package == pkg & cran.toc$Version == vrs, c("Imports", "Depends", "Suggests", "LinkingTo")]
  dep <- c(row$Imports, row$Depends, row$LinkingTo) # merge
  if (include.suggests) {
    dep <- c(dep, row$Suggests) # add 'Suggests' dependencies if requested
  }
  dep <- unlist(strsplit(dep, ","))  # turn to array
  #dep <- dep[!dep %in% base_pkg()]   # base dependencies from R
  dep <- unique(dep)                 # some dependencies can be listed both in Imports and LinkingTo, keep only one

  #Drop non-cran
  non.cran <- dep[!dep %in% cran.toc$Package]
  dep <- dep[!dep %in% non.cran]

  
  # These steps are normally taken care of server side but let's stay on the
  # safe side
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


  #1 If saved before, load it
        #Path to dep12
            dep12_dir <- paste0(get.groundhog.folder() , '/dep12' )
            
        #With and without suggests are different files
            if (include.suggests==FALSE) dep12_file <- paste0(pkg , "_" ,  gsub( "-", "_" , date) , '.rds')  
            if (include.suggests==TRUE)  dep12_file <- paste0(pkg , "_" ,  gsub( "-", "_" , date) , '_with_suggests.rds')  
        
            
        #Final path    
            dep12_path <- file.path(dep12_dir, dep12_file)
            
        #Create directory if it does not exist
            if (!file.exists(dep12_dir)) dir.create(dep12_dir,recursive=TRUE, showWarnings = FALSE)  
            
        #If dep12 has been saved, load and return it
            if (file.exists(dep12_path)) {
                dep12 <- readRDS(dep12_path)
                return(dep12)
              } 
            
  
      # 2. Populate the starting point with this package and its dependencies
      # [a] Vector with pending deps, for looping install
      pending <- get.dependencies(pkg, date, include.suggests = include.suggests) # include.suggests=TRUE means that suggested dependencies and their dependencies are installed.
    
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
            dep12k <- data.frame(pkg = depk, dep2 = pendingk, stringsAsFactors=FALSE)
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
      
      #Save copy
        saveRDS(dep12, dep12_path, version=2)
        
      return(dep12)
} # End function get.all.dependencies

#' Get recursive dependencies for multiple packages (batch processing)
#'
#' Resolves dependencies for all packages together, avoiding redundant work
#' when packages share dependencies.
#'
#' @param pkg character vector. Names of packages to process
#' @param date date (in the format "%Y-%m-%d"), Required date for the packages.
#' @param include.suggests Logical (defaults to `FALSE`. Should suggested
#'   packages be installed?
#' @return a `data.frame` where the first column (`pkg`) contains non-terminal
#'   dependencies and the second column (`dep2`) contains a dependency of the
#'   package in column `pkg`. This includes dependencies for all requested packages.
#' @noRd

get.all.dependencies.batch <- function(pkg, date, include.suggests = FALSE) {
  
  # Initialize combined dep12 dataframe
  dep12.all <- data.frame(pkg = character(), dep2 = character(), stringsAsFactors = FALSE)
  
  # Track which packages have been processed (to avoid redundant work)
  processed <- character()
  
  # Start with all requested packages as pending
  pending <- unique(pkg)
  
  # Process all packages and their dependencies
  while (length(pending) > 0) {
    # Get next package to process
    pkgk <- pending[1]
    pending <- pending[-1]
    
    # Skip if already processed
    if (pkgk %in% processed) next
    
    # Check if this is a base package
    if (pkgk %in% base_pkg()) {
      processed <- c(processed, pkgk)
      next
    }
    
    # Check cache first
    dep12_dir <- paste0(get.groundhog.folder(), '/dep12')
    if (include.suggests == FALSE) {
      dep12_file <- paste0(pkgk, "_", gsub("-", "_", date), '.rds')
    } else {
      dep12_file <- paste0(pkgk, "_", gsub("-", "_", date), '_with_suggests.rds')
    }
    dep12_path <- file.path(dep12_dir, dep12_file)
    
    if (file.exists(dep12_path)) {
      # Load from cache (cached dep12 contains full transitive dependency graph)
      dep12k <- readRDS(dep12_path)
    } else {
      # Resolve dependencies recursively for this package
      # For requested packages, use include.suggests; for dependencies, never include suggests
      use_suggests <- include.suggests && (pkgk %in% pkg)
      
      # Get direct dependencies first
      deps <- get.dependencies(pkgk, date, include.suggests = use_suggests)
      
      if (length(deps) == 0) {
        dep12k <- data.frame(pkg = character(), dep2 = character(), stringsAsFactors = FALSE)
      } else {
        # Start with direct dependencies
        dep12k <- data.frame(pkg = pkgk, dep2 = deps, stringsAsFactors = FALSE)
        
        # Recursively resolve dependencies (similar to get.all.dependencies)
        pending_deps <- deps
        while (length(pending_deps) > 0) {
          depk <- pending_deps[1]
          pending_deps <- pending_deps[-match(depk, pending_deps)]
          
          # Get dependencies of this dependency (never include suggests for deps)
          pendingk <- get.dependencies(pkg = depk, date = date, include.suggests = FALSE)
          
          if (length(pendingk) > 0) {
            dep12k_new <- data.frame(pkg = depk, dep2 = pendingk, stringsAsFactors = FALSE)
            dep12k <- rbind(dep12k, dep12k_new)
            
            # Add new dependencies to pending if not already processed
            already_in_graph <- pendingk %in% dep12k$pkg
            pendingk.net <- pendingk[!already_in_graph]
            pending_deps <- unique(c(pending_deps, pendingk.net))
          }
        }
      }
      
      # Save to cache if directory exists
      if (!file.exists(dep12_dir)) {
        dir.create(dep12_dir, recursive = TRUE, showWarnings = FALSE)
      }
      if (nrow(dep12k) > 0) {
        saveRDS(dep12k, dep12_path, version = 2)
      }
    }
    
    # Add to combined dep12
    if (nrow(dep12k) > 0) {
      dep12.all <- rbind(dep12.all, dep12k)
      
      # Add new dependencies to pending (if not already processed or pending)
      # The cached dep12 already contains transitive deps, so we get all unique deps
      new_deps <- unique(dep12k$dep2)
      new_deps <- new_deps[!new_deps %in% c(processed, pending)]
      new_deps <- new_deps[!new_deps %in% base_pkg()]  # Skip base packages
      pending <- c(pending, new_deps)
    }
    
    # Mark as processed
    processed <- c(processed, pkgk)
  }
  
  # Remove duplicates
  dep12.all <- dep12.all[!duplicated(dep12.all), ]
  
  return(dep12.all)
} # End function get.all.dependencies.batch