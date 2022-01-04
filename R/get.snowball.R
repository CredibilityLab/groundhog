#'  Generates dataframe with all dependencies needed to install a package, in the order they will be loaded
#'
#'@param pkg character string, name of target package to load (and install if needed), 
#'@param date character string  (yyyy-mm-dd), or date value, with the date which determines the 
#'version of the package, and all dependencies, to be loaded (and installed if needed).
#'@param include.suggests logical, defaults to `FALSE`. When set to `TRUE`, includes
#'   dependencies classified in the DESCRIPTION file as `suggested`.
#'@param force.source logical (defaults to `FALSE`). When set to `TRUE`, will not attempt 
#'   installing binary from CRAN or MRAN and instead download source file and install it.
#'@return a dataframe with all packages that need to be installed, their version , whether they are installed, where 
#'to obtain them if not locally available (CRAN vs MRAN), which date to use for MRAN, 
#'installation time from source (in seconds), and local path for storage
#' @examples
#' \dontrun{
#' get.snowball("rio", "2020-07-12")
#'}
#' @export

get.snowball <- function(pkg, date, include.suggests = FALSE, force.source = FALSE) {

  #0 Standardize the name of the pkg to be just a package name even if a remote was entered
      remote_id   <- get.remote_id(pkg)  #c('cran','gitbut', 'gitlab') 
      git_usr_pkg <- standardize.git_usr_pkg(pkg)    #note, this takes the syntax 'github::usr/pkg' but if pkg is a cran package, then git_usr_pkg=pkg
      pkg         <- basename(git_usr_pkg)           #extract just the pkg name from remote packages
      
  

  #1 Paths to load/save snowball 
       #snowballs directory
            snowballs_dir <- paste0(get.groundhog.folder(), '/snowballs/')
            if (!file.exists(snowballs_dir)) dir.create(snowballs_dir,recursive = TRUE, showWarnings = FALSE)
                
        #path to this snowball
            #Default name
              snowball_name <-  pkg  
              
            #Change for snowballs for remote packages
              if (remote_id != 'cran') snowball_name <-  gsub("/|::", "_" , git_usr_pkg)
        
            snowball_path <-  paste0(snowballs_dir, snowball_name , "_",  gsub("-", "_", date),'.rds')
              #example: 'c:/groundhog_folder/snowballs/github_crsh_papaja_2021_05_01.rds'
            
            
      
            
  #2 If snowball exists locally, load it        
            if (file.exists(snowball_path)) {
              snowball <- readRDS(snowball_path)
              return(snowball)
              }
        
            
        
  #3 Load toc
    # toc is the table of contents with packages' publish dates and dependencies
    # by default it is cran.toc.rds, but get.toc() modifies it to include the necessary remotes 
    # when asking for a remote package)
    
    #full_toc <- get.full_toc(pkg=git_usr_pkg, date=date)  #see get.toc.R  
                                       #here we pass on the full git_pkg_usr so that get.toc() knows whether it is remote
      full_toc<-get.full_toc()
    
  
  # 3 Get dependencies
    dep12 <- get.all.dependencies(pkg, date, include.suggests = include.suggests)   #here we pass on just pkg becuase toc will have the remote version 

  
    
  # 4 process dep12  (topographical sort) to get order of installation
    
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
  # 5 Add pkg at the end
      snowball.pkg <- c(indep, pkg)

  # 5 Get the version of each package
      snowball.vrs <- vapply(snowball.pkg, get.version, date, FUN.VALUE = character(1)) 
      snowball.pkg_vrs <- paste0(snowball.pkg, "_", snowball.vrs)

  # 6 Snowball table, with installed | CRAN | MRAN | TARBALL | INSTALLATION TIME
      #Column 1: installed? TRUE/FALSE
        snowball.installed <- mapply(is.pkg_vrs.installed, snowball.pkg, snowball.vrs)   #TRUE/FALSE
    
      #Column 2: paths where packages are (to be) installed
        snowball.installation.path <- mapply(get.pkg_search_paths, snowball.pkg, snowball.vrs)

        #Modification for remote pkgsand possible remote dependencies
          if (remote_id!='cran') {
            baton <- get.baton(git_usr_pkg, date)
            k.remote <- match(baton$remote.pkg_lib$remote.pkg , snowball.pkg)
            snowball.installation.path[k.remote] <- baton$remote.pkg_lib$lib
           }

    
    
  #7 Quicker snowball output if all packages are already installed.
      # No need to find the locations if everything is already installed. This also
      # allows us to run offline in easy cases
      if (all(snowball.installed)) {
        snowball <- data.frame(
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
        return(snowball)
      } #End if all packages are installed.

  
  #8 Figure out where to install from 
        
    snowball.CRAN <- snowball.pkg_vrs %in% get.current.packages("binary")$pkg_vrs
    snowball.MRAN.date <- as.Date(sapply(snowball.pkg_vrs, get.date.for.install.binary), origin = "1970-01-01") #Binary date in MRAN?
    snowball.MRAN.date <- as.DateYMD(snowball.MRAN.date)

    # IF force.source==TRUE then all packages will come from source, else, figure out where from
    if (force.source) {
      snowball.from <- rep_len("source", length(snowball.pkg))
    } else {
      snowball.MRAN <- snowball.MRAN.date != "1970-01-01"
      snowball.from <- ifelse(snowball.MRAN, "MRAN", "source") # MRAN if available, if not source
      snowball.from <- ifelse(snowball.CRAN, "CRAN", snowball.from) # Replace MRAN if CRAN is available and using most recent version of R
    }
  
  #9 Installation time from source
      snowball.time <- round(mapply(get.installation.time, snowball.pkg, snowball.vrs), 0)

    # Adjust installation time
        adjustment <- 2.5 # install times in CRAN's page are systematically too long, this is an initial adjustment factor
        snowball.time <- pmax(round(snowball.time / adjustment, 0), 1)

  #10 Put snowball columns into a data.frame()
    snowball <- data.frame(
      "pkg" = snowball.pkg,
      "vrs" = snowball.vrs,
      "pkg_vrs" = snowball.pkg_vrs,                        # Identify pkg_vrs
      "installed" = snowball.installed,                    # Installed?
      "from" = snowball.from,                              # Where to install from (CRAN, MRAN, source, github, gitlab)
      "MRAN.date" = snowball.MRAN.date,                    # MRAN date, in case MRAN is tried
      "installation.time" = snowball.time,                 # time to install
      "installation.path" = snowball.installation.path,
      'sha'='',                                            #sha  for remotes
      'usr'='',                                            #user for remotes         
      stringsAsFactors = FALSE
      )
    
  #11 Modify snowball for remote packages
      if (remote_id!='cran')  #Note: remote_id is an attribute of the target package
            {
            #Navigate description files of all remote packages to update the snowball
              baton <- get.baton(git_usr_pkg, date)  #see process.remote.all()
        
            #which kth pkg in the snowball are remote?
              k.remote <- match(baton$remote.pkg_lib$remote.pkg , snowball.pkg)

            #Modify them
              snowball[k.remote,]$from <- baton$install.from
              snowball[k.remote,]$MRAN.date <- as.Date('1970-01-01')
              snowball[k.remote,]$sha <- baton$sha
              snowball[k.remote,]$usr <- baton$usr
          }
    
  #12 Save the snowball so taht future calls skip all the steps
    saveRDS(snowball, snowball_path, version=2)
    
    
  return(snowball)
}
