#' Get dependencies for multiple packages simultaneously
#'
#' Get direct dependencies for multiple packages `pkgs`, for the version on CRAN at the
#' given `date`. This function processes all packages simultaneously and merges their dependencies.
#'
#' @param pkgs character vector. Names of the packages to get dependencies for
#' @param date date (in the format "%Y-%m-%d"), Required date for the packages.
#' @param include.suggests Logical (defaults to `FALSE`). Should suggested
#'   packages be included?
#' @return A character vector containing the unique package dependencies for all packages in `pkgs`, for
#'   the version on CRAN at `date`.
#' @noRd

get.dependencies.set <- function(pkgs, date, include.suggests = FALSE) {
  
  # Validate inputs
  if (length(pkgs) == 0) {
    return(character())
  }
  
  # Get dependencies for each package
  all_deps <- character()
  for (pkg in pkgs) {
    deps <- get.dependencies(pkg, date, include.suggests = include.suggests)
    all_deps <- c(all_deps, deps)
  }
  
  # Return unique dependencies
  return(unique(all_deps))
} # End get.dependencies.set()

#' Get recursive dependencies for multiple packages simultaneously
#'
#' Get all recursive (=indirect) dependencies for multiple packages `pkgs`, for the
#' version on CRAN at the given `date`. This function processes all packages simultaneously
#' and merges their dependency graphs.
#'
#' @param pkgs character vector. Names of the packages to get dependencies for
#' @param date date (in the format "%Y-%m-%d"), Required date for the packages.
#' @param include.suggests Logical (defaults to `FALSE`). Should suggested
#'   packages be included?
#' @return a `data.frame` where the first column (`pkg`) contains non-terminal
#'   dependencies and the second column (`dep2`) contains a dependency of the
#'   package in column `pkg`. This includes all dependencies from all packages in `pkgs`.
#' @noRd

get.all.dependencies.set <- function(pkgs, date, include.suggests = FALSE) {
  
  # Validate inputs
  if (length(pkgs) == 0) {
    return(data.frame(pkg = character(), dep2 = character(), stringsAsFactors = FALSE))
  }
  
  # Get all dependencies for all packages and merge them
  dep12_combined <- data.frame(pkg = character(), dep2 = character(), stringsAsFactors = FALSE)
  
  # Get dependencies for each package (individual packages are already cached by get.all.dependencies)
  for (pkg in pkgs) {
    dep12_pkg <- get.all.dependencies(pkg, date, include.suggests = include.suggests)
    if (nrow(dep12_pkg) > 0) {
      dep12_combined <- rbind(dep12_combined, dep12_pkg)
    }
  }
  
  # Remove duplicates
  if (nrow(dep12_combined) > 0) {
    dep12_combined <- unique(dep12_combined)
  }
  
  return(dep12_combined)
} # End function get.all.dependencies.set

#'  Generates dataframe with all dependencies needed to install multiple packages, in the order they will be loaded
#'
#'@param pkgs character vector, names of target packages to load (and install if needed)
#'@param date character string  (yyyy-mm-dd), or date value, with the date which determines the 
#'version of the packages, and all dependencies, to be loaded (and installed if needed).
#'@param include.suggests logical, defaults to `FALSE`. When set to `TRUE`, includes
#'   dependencies classified in the DESCRIPTION file as `suggested`.
#'@param force.install logical, defaults to `FALSE`. When set to `TRUE`, the column `installed`
#'in the generated snowball is set FALSE for all packages, causing them to be installed even if already installed.
#'@return a dataframe with all packages that need to be installed, their version , whether they are installed, where 
#'to obtain them if not locally available (CRAN vs MRAN), which date to use for MRAN, 
#'installation time from source (in seconds), and local path for storage
#' @examples
#' \dontrun{
#' get.snowball.set(c("rio", "dplyr"), "2020-07-12")
#'}
#' @export

get.snowball.set <- function(pkgs, date, include.suggests=FALSE, force.install=FALSE) {
  
  # Validate inputs
    if (length(pkgs) == 0) {
      stop("pkgs must be a non-empty character vector")
    }
  
  # Remove duplicates and validate
   pkgs <- unique(pkgs)
  
  # Validate date and include suggests
    date <- as.DateYMD(date)
    validate.TF(include.suggests)
    validate.date(date)
  
  # Ensure pkgs are loaded
    load.cran.toc()
  
  # Message when creating new snowball (shown before dependency resolution)
    pkgs_str <- paste(pkgs, collapse = "', '")
    message1("You had not previously loaded these packages for '",date,"'.\n",
             "Will find which versions are needed for that day.")
    
  # 1 Get dependencies for all packages
   dep12 <- get.all.dependencies.set(pkgs, date, include.suggests = include.suggests)
  
  # 2 Produce dep12 (topological sort) to get order of installation
    k <- 0 # counter of iterations attempting to sort
    indep <- c()
  
  # In each loop we take all dependencies without dependencies and add them to the sequence of installation
  # Do until all dependencies have been assigned an order (so dep12 is empty)
    while (nrow(dep12) > 0) {
      k <- k + 1
      indep.rows <- !(dep12$dep2 %in% dep12$pkg) ## Find dependencies without dependencies  TRUE/FALSE vector
      
      # Add those dependencies to the list of independencies
      indepk <- unique(as.character(dep12$dep2[indep.rows]))
      indep <- c(indep, indepk)
      # Drop those rows from both
      dep12 <- dep12[!indep.rows, ]
      # Safety valve in case loop impossible to end
      if (k == 50000) {
        break
      }
    }
  
  # 2.5 Drop base packages for those will be loaded explicitly by library() command
   indep <- indep[!indep %in% base_pkg()]
  
  # 3) Add all target packages at the end (in the order they were provided, but remove duplicates)
  # Remove any packages that are already in indep
    pkgs_to_add <- pkgs[!pkgs %in% indep]
    snowball.pkg <- c(indep, pkgs_to_add)
    
  # 4) Get the version of each package
    snowball.vrs <- c()
    for (k in 1:length(snowball.pkg)) {
      snowball.vrs[k] <- as.character(get.version(snowball.pkg[k], date = date))
    }
    snowball.pkg_vrs <- paste0(snowball.pkg, "_", snowball.vrs)
  
  # 5 Snowball table with packages to be installed & necessary attributes (location, binary / source, etc)
  
  # Installed?
    ip.groundhog <- get.ip('groundhog')
    loans <- get.loans(verfiy.package.exists = TRUE)
    
  # If the pkg is found in either the local or groundhog folder, deem this TRUE
    snowball.installed <- snowball.pkg_vrs %in% c(ip.groundhog$pkg_vrs, loans$pkg_vrs)
    
  # Over-rule it if requested to install all
    if (force.install == TRUE) {
      snowball.installed <- FALSE
    }
  
  # Vector with paths
    snowball.installation.path <- mapply(get.pkg_search_paths, snowball.pkg, snowball.vrs)
    
  # No need to find the locations if everything is already installed. This also
  # allows us to run offline in easy cases
    if (all(snowball.installed)) {
      return(
        data.frame(
          "pkg" = snowball.pkg,
          "vrs" = snowball.vrs,
          "pkg_vrs" = snowball.pkg_vrs,
          "installed" = TRUE,
          "from" = '',
          "GRAN.date" = as.Date('1970-01-01'),
          "installation.time" = 0,
          "installation.path" = snowball.installation.path,
          stringsAsFactors = FALSE
        )
      )
    }
  
  os <- get.os()
  if (os != 'other') {
    # For windows and mac get binaries information
    snowball.CRAN <- snowball.pkg_vrs %in% get.current.packages("binary")$pkg_vrs
    snowball.GRAN.date <- as.Date(sapply(snowball.pkg_vrs, get.gran.binary.date, date = date), origin = "1970-01-01")
    snowball.GRAN.date <- as.DateYMD(snowball.GRAN.date)
    
    snowball.GRAN <- snowball.GRAN.date != "1970-01-01"
    snowball.from <- ifelse(snowball.GRAN, "GRAN", "source")
    snowball.from <- ifelse(snowball.CRAN, "CRAN", snowball.from)
  } else {
    # If os IS 'other'
    n <- length(snowball.pkg_vrs)
    snowball.CRAN <- rep(FALSE, n)
    snowball.from <- rep('source', n)
    snowball.GRAN.date <- rep(as.Date("1970-01-01"), n)
  }
  
  # Installation time from source
  snowball.time <- round(mapply(get.installation.time, snowball.pkg, snowball.vrs), 0)
  
  # Adjust installation time
  adjustment <- 2.5
  snowball.time <- pmax(round(snowball.time / adjustment, 0), 1)
  
  # 6 data.frame()
  snowball <- data.frame(
    "pkg" = snowball.pkg,
    "vrs" = snowball.vrs,
    "pkg_vrs" = snowball.pkg_vrs,
    "installed" = snowball.installed,
    "from" = snowball.from,
    "GRAN.date" = snowball.GRAN.date,
    "installation.time" = snowball.time,
    "installation.path" = snowball.installation.path,
    stringsAsFactors = FALSE
  )
  
  # 7 Base packages are always installed
  snowball$installed <- ifelse(snowball$pkg %in% base_pkg(), TRUE, snowball$installed)
  
  return(snowball)
}

