#'  Generates dataframe with all dependencies needed to install a package, in the order they will be loaded
#'
#'@param pkg character string, name of target package to load (and install if needed), 
#'@param date character string  (yyyy-mm-dd), or date value, with the date which determines the 
#'version of the package, and all dependencies, to be loaded (and installed if needed).
#'@param include.suggests logical, defaults to `FALSE`. When set to `TRUE`, includes
#'   dependencies classified in the DESCRIPTION file as `suggested`.
#'@param force.install logical, defaults to `FALSE`. When set to `TRUE`, the column `installed`
#'in the generated snowball is set FALSE for all packages, causing them to be installed even if already installed.
#'@return a dataframe with all packages that need to be installed, their version , whether they are installed, where 
#'to obtain them if not locally available (CRAN vs MRAN), which date to use for MRAN, 
#'installation time from source (in seconds), and local path for storage
#' @examples
#' \dontrun{
#' get.snowball("rio", "2020-07-12")
#'}
#' @export
#' 

######################################################################################
#Outline
#
#0  If snowball already exists early return it  
#1 Get dependencies
#2 Produce  dep12  (topographical sort) to get order of installation
#3 Add pkg at the end
#4 Get the version of each package
#5 Make Snowball table
#6 Turn to data frame 
######################################################################################


get.snowball <- function(pkg, date, include.suggests=FALSE, force.install=FALSE, quiet=FALSE) {

    #Validate date and include suggests
            date <- as.DateYMD(date)
            validate.TF(include.suggests)
            validate.date(date)

    #Ensure pkgs are loaded
        load.cran.toc()
  
    #0  If snowball already exists early return it  
    
        #Path to snowball
            snowball_dir <- paste0(get.groundhog.folder() , '/snowballs_v2' )
            
        #Snowball with and without suggests are different snowballs, so two possible rds files for any given pkg date
            if (include.suggests==FALSE) snowball_file <- paste0(pkg , "_" ,  gsub( "-", "_" , date) , '.rds')  
            if (include.suggests==TRUE)  snowball_file <- paste0(pkg , "_" ,  gsub( "-", "_" , date) , '_with_suggests.rds')  
        
            
        #Final path    
            snowball_path <- file.path(snowball_dir, snowball_file)
            
        #Create snowball directory if it does not exist
            if (!file.exists(snowball_dir)) dir.create(snowball_dir,recursive=TRUE, showWarnings = FALSE)  
            
        #If snowball has been saved, load and return it, but delete it so that if it fails next time it won't be here
            if (file.exists(snowball_path)) {
              #Load
                snowball <- readRDS(snowball_path)
                
              #Delete
                unlink(snowball_path)
                
              #Update if necessary if a borrowed package from groundhog-library to local-library goes missing
                
                #Check for lost packages
                    loans.all   <- get.loans(verfiy.package.exists=FALSE)
                    loans.still <- get.loans(verfiy.package.exists=TRUE)  
                    loans.lost  <- loans.all[!loans.all$md5 %in% loans.still$md5,]
                    
                    
                #Update snowball setting lost packages (if any) as not installed
                    if (nrow(loans.lost)>0)
                    {
                    snowball$installed <- ifelse(snowball$pkg_vrs %in% loans.lost$pkg_vrs &
                                                 !snowball$pkg_vrs %in% get.ip("groundhog")$pkg_vrs 
                                                   , FALSE, snowball$installed)
                    
                    #If a pkg is lost, we force installed=FALSE, otherwise we keep what it is (which should be TRUE)
                    #since we only save a snowball upon installing successfully all of it
                    
                      
                    }

                
                
                return(snowball)
              } 
         
            
  # Note about saving (shown after all dependency messages, only once per groundhog.library call)
    if (!quiet && (is.null(.pkgenv[['snowball.note.shown']]) || .pkgenv[['snowball.note.shown']] == FALSE)) {
        message2("We will save the information below for next time you load those same package versions")
        .pkgenv[['snowball.note.shown']] <- TRUE
    }
  
               
  # Message when creating new snowball (shown before dependency resolution)
        if (!quiet) {
          message1("Finding the version of all packages that '", pkg, "' needs for requested date: '", date, "'")
        }
            
  # 1 Get dependencies
  dep12 <- get.all.dependencies(pkg, date, include.suggests = include.suggests)
  

 

  # 2 Produce  dep12  (topographical sort) to get order of installation
  
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

    #2.5 Drop base packages for those will be loaded explicitly by library() command
      indep <- indep[!indep %in% base_pkg()]
      
    
    # 3) Add pkg at the end
      snowball.pkg <- c(indep, pkg)


  # 4) Get the version of each package
    #snowball.vrs <- vapply(snowball.pkg, get.version, date, FUN.VALUE = character(1)) 
    snowball.vrs<-c()
    for (k in 1:length(snowball.pkg)) {
      snowball.vrs[k]=as.character(get.version(snowball.pkg[k],date=date))
      }
    snowball.pkg_vrs <- paste0(snowball.pkg, "_", snowball.vrs)

    
    
  # 5 Snowball table with packages to be installed & necessary attributes (location, binary / source, etc)

  #Installed?
      ip.groundhog <- get.ip('groundhog')                    #utils #58
      loans<-get.loans(verfiy.package.exists=TRUE)  #utils #59: get data.frame with borrowed packages 
                                                             #           that are still found in local library

    #If the pkg is found in either the local or groundhog folder, deem this TRUE    
      snowball.installed <- snowball.pkg_vrs %in% c(ip.groundhog$pkg_vrs, loans$pkg_vrs) 
      

  #Over-rule it if requested to install all
    if (force.install==TRUE) snowball.installed < -FALSE
    
  # Vector with paths
    snowball.installation.path <- mapply(get.pkg_search_paths, snowball.pkg, snowball.vrs)

  # No need to find the locations if everything is already installed. This also
  # allows us to run offline in easy cases
  if (all(snowball.installed)) {
    return(
      data.frame(
        "pkg" = snowball.pkg,
        "vrs" = snowball.vrs,
        "pkg_vrs" = snowball.pkg_vrs,       # Identify pkg
        "installed" = TRUE,                  # Installed?
        "from" = '',                         # Where to install from
        "GRAN.date" = as.Date('1970-01-01'), # GRAN date
        "installation.time" = 0,             # time to install
        "installation.path" = snowball.installation.path,
        stringsAsFactors = FALSE
      )
    )
  }

  os <- get.os()
  if (os!='other') {
    
  #For windows and mac get binaries information
    snowball.CRAN <- snowball.pkg_vrs %in% get.current.packages("binary")$pkg_vrs
    snowball.GRAN.date <- as.Date(sapply(snowball.pkg_vrs, get.gran.binary.date , date=date), origin = "1970-01-01") # 5.3 Binary date in GRAN?
    snowball.GRAN.date <- as.DateYMD(snowball.GRAN.date)

    snowball.GRAN <- snowball.GRAN.date != "1970-01-01"
    snowball.from <- ifelse(snowball.GRAN, "GRAN", "source")      # GRAN if available, if not source
    snowball.from <- ifelse(snowball.CRAN, "CRAN", snowball.from) # Replace GRAN if CRAN is available and using most recent version of R
  } else {
    
  #If os IS 'other'
    n <- length(snowball.pkg_vrs)
    snowball.CRAN <- rep(FALSE, n)
    snowball.from <- rep('source', n)
    snowball.GRAN.date <- rep(as.Date("1970-01-01"),n)
    }
  
  # Installation time from source
  snowball.time <- round(mapply(get.installation.time, snowball.pkg, snowball.vrs), 0)

  # Adjust installation time
  adjustment <- 2.5 # install times in CRAN's page are systematically too long, this is an initial adjustment factor
  snowball.time <- pmax(round(snowball.time / adjustment, 0), 1)

#6 data.frame()
  snowball <- data.frame(
    "pkg" = snowball.pkg,
    "vrs" = snowball.vrs,
    "pkg_vrs" = snowball.pkg_vrs,         # Identify pkg
    "installed" = snowball.installed,     # Installed?
    "from" = snowball.from,               # Where to install from
    "GRAN.date" = snowball.GRAN.date,     # GRAN date, in case GRAN is tried
    "installation.time" = snowball.time,  # time to install
    "installation.path" = snowball.installation.path,
    stringsAsFactors = FALSE
  )

#7 Base packages are always installed
  snowball$installed <- ifelse(snowball$pkg %in% base_pkg(), TRUE,snowball$installed)

  return(snowball)
}

#' Get snowballs for multiple packages (batch processing)
#'
#' Processes multiple packages together, resolving shared dependencies once
#' and avoiding redundant work.
#'
#' @param pkg character vector. Names of packages to process
#' @param date date (in the format "%Y-%m-%d"), Required date for the packages.
#' @param include.suggests logical, defaults to `FALSE`. When set to `TRUE`, includes
#'   dependencies classified in the DESCRIPTION file as `suggested`.
#' @param force.install logical, defaults to `FALSE`. When set to `TRUE`, the column `installed`
#'   in the generated snowball is set FALSE for all packages.
#' @return a list with two elements:
#'   - `snowballs`: named list of snowball data.frames, one per requested package
#'   - `all`: combined snowball data.frame with all unique packages
#' @noRd

get.snowballs.batch <- function(pkg, date, include.suggests = FALSE, force.install = FALSE) {
  
  # Validate inputs
  date <- as.DateYMD(date)
  validate.TF(include.suggests)
  validate.date(date)
  
  # Ensure packages are loaded
  load.cran.toc()
  
  # Remove duplicates
  pkg <- unique(pkg)
  
  # 1. Check for cached snowballs first
  snowball.list <- list()
  pkg_to_process <- character()
  snowball_dir <- paste0(get.groundhog.folder(), '/snowballs_v2')
  
  if (!file.exists(snowball_dir)) {
    dir.create(snowball_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  for (pkgk in pkg) {
    if (include.suggests == FALSE) {
      snowball_file <- paste0(pkgk, "_", gsub("-", "_", date), '.rds')
    } else {
      snowball_file <- paste0(pkgk, "_", gsub("-", "_", date), '_with_suggests.rds')
    }
    snowball_path <- file.path(snowball_dir, snowball_file)
    
    if (file.exists(snowball_path)) {
      # Load cached snowball
      snowball.k <- readRDS(snowball_path)
      unlink(snowball_path)  # Delete as per current logic
      
      # Update if necessary if a borrowed package goes missing
      loans.all <- get.loans(verfiy.package.exists = FALSE)
      loans.still <- get.loans(verfiy.package.exists = TRUE)
      loans.lost <- loans.all[!loans.all$md5 %in% loans.still$md5, ]
      
      if (nrow(loans.lost) > 0) {
        snowball.k$installed <- ifelse(snowball.k$pkg_vrs %in% loans.lost$pkg_vrs &
                                        !snowball.k$pkg_vrs %in% get.ip("groundhog")$pkg_vrs,
                                        FALSE, snowball.k$installed)
      }
      
      snowball.list[[pkgk]] <- snowball.k
    } else {
      pkg_to_process <- c(pkg_to_process, pkgk)
    }
  }
  
  # 2. If all were cached, return early
  if (length(pkg_to_process) == 0) {
    snowball.all <- merge.snowballs(snowball.list)
    return(list(snowballs = snowball.list, all = snowball.all))
  }
  
  # 3. Show note about saving (only once)
  if (is.null(.pkgenv[['snowball.note.shown']]) || .pkgenv[['snowball.note.shown']] == FALSE) {
    message2("We will save the information below for next time you load those same package versions")
    .pkgenv[['snowball.note.shown']] <- TRUE
  }
  
  # 4. Single message for all packages being processed
  if (length(pkg_to_process) == 1) {
    message1("Finding the version of all packages that '", pkg_to_process[1], 
             "' needs for requested date: '", date, "'")
  } else {
    message1("Finding the version of all packages that ", 
             paste(pkg_to_process, collapse = ", "), 
             " need for requested date: '", date, "'")
  }
  
  # 5. Batch dependency resolution - resolve all dependencies at once
  all_dep12 <- get.all.dependencies.batch(pkg_to_process, date, include.suggests)
  
  # 6. Extract unique packages and get versions in batch
  all_unique_pkgs <- unique(c(pkg_to_process, all_dep12$pkg, all_dep12$dep2))
  all_unique_pkgs <- all_unique_pkgs[!all_unique_pkgs %in% base_pkg()]
  
  # Batch version lookup - create version cache
  version_cache <- new.env()
  for (pkgk in all_unique_pkgs) {
    vrs <- as.character(get.version(pkgk, date = date))
    assign(pkgk, vrs, envir = version_cache)
  }
  
  # Helper function to get cached version
  get.version.cached <- function(pkgk) {
    if (exists(pkgk, envir = version_cache)) {
      return(get(pkgk, envir = version_cache))
    }
    return(as.character(get.version(pkgk, date = date)))
  }
  
  # 7. Build snowballs for each requested package
  for (pkgk in pkg_to_process) {
    snowball.k <- build.snowball.from.dep12(pkgk, all_dep12, get.version.cached, date, 
                                            include.suggests, force.install)
    snowball.list[[pkgk]] <- snowball.k
  }
  
  # 8. Merge all snowballs efficiently
  snowball.all <- merge.snowballs(snowball.list)
  
  return(list(snowballs = snowball.list, all = snowball.all))
}

#' Build a snowball data.frame from dependency graph for a single package
#'
#' @param pkg character. Package name
#' @param all_dep12 data.frame. Combined dependency graph for all packages
#' @param get.version.fun function. Function to get version (cached version lookup)
#' @param date date. Requested date
#' @param include.suggests logical. Whether to include suggests
#' @param force.install logical. Whether to force reinstall
#' @return snowball data.frame
#' @noRd

build.snowball.from.dep12 <- function(pkg, all_dep12, get.version.fun, date, 
                                       include.suggests, force.install) {
  
  # Filter dep12 to only include dependencies for this package
  # Start with direct dependencies
  dep12.pkg <- all_dep12[all_dep12$pkg == pkg, ]
  
  # Add transitive dependencies (dependencies of dependencies)
  if (nrow(dep12.pkg) > 0) {
    deps_to_add <- dep12.pkg$dep2
    while (length(deps_to_add) > 0) {
      new_deps <- all_dep12[all_dep12$pkg %in% deps_to_add, ]
      if (nrow(new_deps) > 0) {
        # Add new dependencies that aren't already in dep12.pkg
        existing <- paste(dep12.pkg$pkg, dep12.pkg$dep2, sep = "|")
        new_rows <- new_deps[!paste(new_deps$pkg, new_deps$dep2, sep = "|") %in% existing, ]
        if (nrow(new_rows) > 0) {
          dep12.pkg <- rbind(dep12.pkg, new_rows)
          deps_to_add <- unique(new_rows$dep2)
        } else {
          deps_to_add <- character()
        }
      } else {
        deps_to_add <- character()
      }
    }
  }
  
  # 2. Topological sort to get order of installation
  k <- 0
  indep <- c()
  dep12 <- dep12.pkg
  
  while (nrow(dep12) > 0) {
    k <- k + 1
    indep.rows <- !(dep12$dep2 %in% dep12$pkg)
    indepk <- unique(as.character(dep12$dep2[indep.rows]))
    indep <- c(indep, indepk)
    dep12 <- dep12[!indep.rows, ]
    if (k == 50000) break
  }
  
  # Drop base packages
  indep <- indep[!indep %in% base_pkg()]
  
  # Add pkg at the end
  snowball.pkg <- c(indep, pkg)
  
  # Get versions using cached function
  snowball.vrs <- vapply(snowball.pkg, get.version.fun, FUN.VALUE = character(1))
  snowball.pkg_vrs <- paste0(snowball.pkg, "_", snowball.vrs)
  
  # Check if installed
  ip.groundhog <- get.ip('groundhog')
  loans <- get.loans(verfiy.package.exists = TRUE)
  snowball.installed <- snowball.pkg_vrs %in% c(ip.groundhog$pkg_vrs, loans$pkg_vrs)
  
  if (force.install == TRUE) snowball.installed <- FALSE
  
  # Get installation paths
  snowball.installation.path <- mapply(get.pkg_search_paths, snowball.pkg, snowball.vrs)
  
  # Early return if all installed
  if (all(snowball.installed)) {
    return(data.frame(
      "pkg" = snowball.pkg,
      "vrs" = snowball.vrs,
      "pkg_vrs" = snowball.pkg_vrs,
      "installed" = TRUE,
      "from" = '',
      "GRAN.date" = as.Date('1970-01-01'),
      "installation.time" = 0,
      "installation.path" = snowball.installation.path,
      stringsAsFactors = FALSE
    ))
  }
  
  # Get binary/source information
  os <- get.os()
  if (os != 'other') {
    snowball.CRAN <- snowball.pkg_vrs %in% get.current.packages("binary")$pkg_vrs
    snowball.GRAN.date <- as.Date(sapply(snowball.pkg_vrs, get.gran.binary.date, date = date), 
                                   origin = "1970-01-01")
    snowball.GRAN.date <- as.DateYMD(snowball.GRAN.date)
    snowball.GRAN <- snowball.GRAN.date != "1970-01-01"
    snowball.from <- ifelse(snowball.GRAN, "GRAN", "source")
    snowball.from <- ifelse(snowball.CRAN, "CRAN", snowball.from)
  } else {
    n <- length(snowball.pkg_vrs)
    snowball.CRAN <- rep(FALSE, n)
    snowball.from <- rep('source', n)
    snowball.GRAN.date <- rep(as.Date("1970-01-01"), n)
  }
  
  # Installation time
  snowball.time <- round(mapply(get.installation.time, snowball.pkg, snowball.vrs), 0)
  adjustment <- 2.5
  snowball.time <- pmax(round(snowball.time / adjustment, 0), 1)
  
  # Create data.frame
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
  
  # Base packages are always installed
  snowball$installed <- ifelse(snowball$pkg %in% base_pkg(), TRUE, snowball$installed)
  
  return(snowball)
}

#' Efficiently merge multiple snowball data.frames
#'
#' @param snowball.list list. List of snowball data.frames
#' @return combined snowball data.frame with unique packages
#' @noRd

merge.snowballs <- function(snowball.list) {
  if (length(snowball.list) == 0) {
    return(data.frame())
  }
  if (length(snowball.list) == 1) {
    return(snowball.list[[1]])
  }
  
  # Combine all snowballs
  snowball.all <- do.call(rbind, snowball.list)
  
  # Remove duplicates (keep first occurrence)
  snowball.all <- snowball.all[!duplicated(snowball.all$pkg_vrs), ]
  
  return(snowball.all)
}
