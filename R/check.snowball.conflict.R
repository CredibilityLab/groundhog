#' Check snowball conflict
#'
#' @inheritParams estimate.seconds.left
#' @inheritParams install.snowball
#' @inheritParams groundhog.library
#' @param ignore.deps optional character vector containing dependencies which 
#'   may mismatch those implied by the entered date and be tolerated. This will
#'   prevent the installation to stop and request restarting the R session for 
#'   specified dependencies.

#'
check.snowball.conflict <- function(snowball, force.install, ignore.deps, date) {
  #1 Get sets of packages that are treated differently 
      #1.1. Ignore conflicts 
          ignore.deps=c("testthat", "rstudioapi", ignore.deps)  
      
      #1.2 REcommended 
            #Deal with potential conflicts with non explicitly acknowledged dependencies with recommended packages
          ip <- data.frame(utils::installed.packages())
          recommended.pkg <- ip[ip$Priority=="recommended",]$Package
    
      #1.3 Active packages
        active <- get.active()
    
  #2 short name for package being installed/loaded
    requested_pkg_vrs <- snowball$pkg_vrs[length(snowball$pkg_vrs)]

  #3 Check if any package that needs to be installed are loaded; separate check from below because even SAME version created conflict
  if (force.install) {
    conflict.pkg <- (snowball$pkg %in% active$pkg) 
    if (any(conflict.pkg)) {
      message2()
      message(
        "You selected 'force.install=TRUE' but the following packages that would be installed\n",
        "are currently loaded:", paste0(snowball$pkg[conflict.pkg], collapse = ",  "),
        "\n\nYou need to restart your R session to carry out the installation.\n",
        "(in R Studio press: CTRL/CMD-SHIFT-F10 to do so)"
      
      )
      exit()
    } # End conflict found for forced install
  } # End check force install

  
  
    
  #4 Compare sets 
    #Compare already active package and package_version to find conflicts
      conflict.needed <- "" # Assume nothing is in conflict
      
    #These are packages that are needed and have a conflict with an active one
      conflict.needed     <- snowball$pkg_vrs[!(snowball$pkg_vrs %in% active$pkg_vrs) & (snowball$pkg %in% active$pkg) & (!snowball$pkg %in% ignore.deps)]
      conflict.needed.pkg <- snowball$pkg    [!(snowball$pkg_vrs %in% active$pkg_vrs) & (snowball$pkg %in% active$pkg) & (!snowball$pkg %in% ignore.deps)]
      conflict.needed <- sort(conflict.needed)
      
      
   #5 R Studio .rmd work-around: actively unload pacakges they automatically load
      
      #Try to unload knitr and xfun if loaded and mismatch, workaround R STudio automatically loading them for .rmd files
     
      #Packages groundhog tries to unload if there is a mismatch
          unload.pkg.all <- c("knitr" , 'xfun')
          
      #Keep those in the conflict list, if any
          unload.pkg.all <- unload.pkg.all[unload.pkg.all %in% conflict.needed.pkg]
          
      #If any are there loop, unload, recheck
            if (length(unload.pkg.all > 0))
            {
            #Loop over them trying to unload them
                for (pk in unload.pks.all) tryCatch(unloadNamespace(pk))
                      
            #Return conflict check
                active <- get.active()
                
                #Compare already active package and package_version to find conflicts
                    conflict.needed <- "" # Assume nothing is in conflict
          
                #These are packages that are needed and have a conflict with an active one
                    conflict.needed <- snowball$pkg_vrs[!(snowball$pkg_vrs %in% active$pkg_vrs) & (snowball$pkg %in% active$pkg) & (!snowball$pkg %in% ignore.deps)]
                    conflict.needed <- sort(conflict.needed)
            }
      
      
     
    #6 These are packages that are active and have a conflict with a needed one (do not include packages in ignore.deps)
      conflict.active     <- active$pkg_vrs[!(active$pkg_vrs %in% snowball$pkg_vrs) & (active$pkg %in% snowball$pkg) & (!active$pkg %in% ignore.deps)]
      conflict.active.pkg <- active$pkg    [!(active$pkg_vrs %in% snowball$pkg_vrs) & (active$pkg %in% snowball$pkg) & (!active$pkg %in% ignore.deps)]
      conflict.active     <- sort(conflict.active)

      
      
      
  #7 Generate variables with counts and list of packages in conflict
    n.conflict <- length(conflict.needed)
    n.needed <- nrow(snowball)

  #8 Paste the package(s), which are vectors, into a string
    conflict.needed <- paste(conflict.needed, collapse = ",  ") # put a , between packages
    conflict.active <- paste(conflict.active, collapse = ",  ")


  #9 If different # of packages match pkg vs pkg_vrs, we have same packages  different vrs: stop
  if (conflict.needed != "") {
    message2()
    message1(n.conflict, " of the ", n.needed, " packages needed by '", requested_pkg_vrs, "' are currently loaded,",
             " but not with the version that is needed.\n",
            "Loaded: ",conflict.active,"\n",
            "Needed: ",conflict.needed,"\n\n",
            "To solve this: restart the R session. Note: you will need to do 'library(groundhog)' again.\n\n",
            "In R Studio press: CTRL/CMD-SHIFT-F10")
     message("The package '", requested_pkg_vrs,"' was *NOT* loaded")
     
    
  #10 If in the conflict we have a recommended package, special instructions
     #10.1 Set of recommended files with a conflict
          conflict.recommended <- conflict.active.pkg [conflict.active.pkg %in% recommended.pkg]
          n.cr <- length(conflict.recommended)
      #10.2
       if (n.cr>0) {
            message("\n\n       IMPORTANT: if you see this message after having restarted your R session,\n", 
                    "       it means one of the packages you are loading does not properly reference\n",
                    "       a package it needs (in its 'DESCRIPTION' file).\n",
                    "       To solve this, you need to restart the session again, CTRL-SHIFT-F10,\n",
                    "       then run the code below **BEFORE** loading the packages you want to load:")
          
              for (k in 1:n.cr) {
                  message2("\n       groundhog.library('" , conflict.recommended[k] , "','" , date , "')" )
                } #End of loop over conflicts with recommended files.
        
      }
  #11 Return
     invisible(list(packages.needed=conflict.needed, packages.active=conflict.active))
    exit()
  } # End if some conflict found
} # End function
