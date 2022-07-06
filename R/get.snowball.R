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
#5 Make Snowball table with: installed | CRAN | MRAN | TARBALL | INSTALLATION TIME
#6 Turn to data frame 
######################################################################################


get.snowball <- function(pkg, date, include.suggests=FALSE, force.install=FALSE) {

    #Validate date and include suggests
            date <- as.DateYMD(date)
            validate.TF(include.suggests)
            validate.date(date)

  
    #0  If snowball already exists early return it  
    
        #Path to snowball
            snowball_dir <- paste0(get.groundhog.folder() , '/snowballs' )
            
        #Snowball with and without suggests are different snowballs, so two possible rds files for any given pkg date
            if (include.suggests==FALSE) snowball_file <- paste0(pkg , "_" ,  gsub( "-", "_" , date) , '.rds')  
            if (include.suggests==TRUE)  snowball_file <- paste0(pkg , "_" ,  gsub( "-", "_" , date) , '_with_suggests.rds')  
        
            
        #Final path    
            snowball_path <- file.path(snowball_dir, snowball_file)
            
        #Create snowball directory if it does not exist
            if (!file.exists(snowball_dir)) dir.create(snowball_dir,recursive=TRUE, showWarnings = FALSE)  
            
        #If snowball has been saved, load and return it, but delete it so that if it fails next time it won't be here
            if (file.exists(snowball_path)) {
                snowball <- readRDS(snowball_path)
                unlink(snowball_path)
                return(snowball)
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

    
    
  # 5 Snowball table, with installed | CRAN | MRAN | TARBALL | INSTALLATION TIME

  # - Install from CRAN if possible
  # - else, install from MRAN if possible
  # - else, install from source

  #Installed?
    snowball.installed <- mapply(is.pkg_vrs.installed, snowball.pkg, snowball.vrs)

  #Over-rule it if requested to install all
    if (force.install==TRUE) snowball.installed<-FALSE
    
  # Vector with paths
    snowball.installation.path <- mapply(get.pkg_search_paths, snowball.pkg, snowball.vrs)

  # No need to find the locations if everything is already installed. This also
  # allows us to run offline in easy cases
  if (all(snowball.installed)) {
    return(
      data.frame(
        "pkg" = snowball.pkg,
        "vrs" = snowball.vrs,
        "pkg_vrs" = snowball.pkg_vrs, # Identify pkg
        "installed" = TRUE, # Installed?
        "from" =  NA_character_, # Where to install from
        "MRAN.date" = NA_character_, # MRAN date, in case MRAN is tried
        "installation.time" = NA_real_, # time to install
        "installation.path" = snowball.installation.path,
        stringsAsFactors = FALSE
      )
    )
  }

  
    snowball.CRAN <- snowball.pkg_vrs %in% get.current.packages("binary")$pkg_vrs
    snowball.MRAN.date <- as.Date(sapply(snowball.pkg_vrs, get.date.for.install.binary,date=date), origin = "1970-01-01") # 5.3 Binary date in MRAN?
    snowball.MRAN.date <- as.DateYMD(snowball.MRAN.date)

    snowball.MRAN <- snowball.MRAN.date != "1970-01-01"
    snowball.from <- ifelse(snowball.MRAN, "MRAN", "source")      # MRAN if available, if not source
    snowball.from <- ifelse(snowball.CRAN, "CRAN", snowball.from) # Replace MRAN if CRAN is available and using most recent version of R

  
  # Installation time from source
  snowball.time <- round(mapply(get.installation.time, snowball.pkg, snowball.vrs), 0)

  # Adjust installation time
  adjustment <- 2.5 # install times in CRAN's page are systematically too long, this is an initial adjustment factor
  snowball.time <- pmax(round(snowball.time / adjustment, 0), 1)

#6 data.frame()
  snowball <- data.frame(
    "pkg" = snowball.pkg,
    "vrs" = snowball.vrs,
    "pkg_vrs" = snowball.pkg_vrs, # Identify pkg
    "installed" = snowball.installed, # Installed?
    "from" = snowball.from, # Where to install from
    "MRAN.date" = snowball.MRAN.date, # MRAN date, in case MRAN is tried
    "installation.time" = snowball.time, # time to install
    "installation.path" = snowball.installation.path,
    stringsAsFactors = FALSE
  )


  return(snowball)
}
