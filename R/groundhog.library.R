#' Install packages as available on set date -  groundhog.library()
#'
#' @inheritParams install.snowball
#' @param ignore.deps optional character vector containing dependencies which 
#'   may mismatch those implied by the entered date and be tolerated. This will
#'   prevent the installation to stop and request restarting the R session for 
#'   specified dependencies.
#' @param pkg character string dependencies which 
#'
#' @return a character vector containing all active packages for the session,
#'   with their version number, under the format `pkg_vrs`.
#'
#' @examples
#' \dontrun{
#' groundhog.library("magrittr", "2018-02-12")
#' }
#'
#' @importFrom utils capture.output
#'
#' @export
#'
groundhog.library <- function(
                              pkg, date,
                              quiet.install = TRUE,
                              include.suggests = FALSE,
                              current.deps = .pkgenv$current.deps,  #set in zzz.R
                              ignore.deps = c(),
                              ignore.package.conflicts = FALSE,
                              force.source = FALSE,
                              force.install = FALSE) {

  # If package name was given using non-standard evaluation (i.e., unquoted)
  pkg <- as.character(substitute(pkg))

  
  #1 initial check,  stop if same version
      active=get.active()
  
    #1.1 Get version of requested package
        vrs <- get.version(pkg, date, current.deps = current.deps)
        pkg_vrs <- paste0(pkg, "_", vrs)
        
    #1.2 Stop if  pkg_vrs already attached
        attached.list=utils::sessionInfo()$otherPkgs
        attached.pkg <- names(attached.list)
        attached.vrs <- lapply(attached.list, function(x) x$Version)
        attached.pkg_vrs <- paste0(attached.pkg, "_", attached.vrs) 
        
        if (pkg_vrs %in% attached.pkg_vrs) {
            message1("groundhog says: the package you requested ('", pkg, "_", vrs, "') is already attached")
            return(invisible(active$pkg_vrs))

        }
    #1.3 Attach if package is loaded but not attached
        if (pkg_vrs %in% active$pkg_vrs)
        {
          attachNamespace(pkg)
          message1("groundhog says: the package you requested ('", pkg, "_", vrs, "') was loaded, now it is also attached")
          return(invisible(active$pkg_vrs))

        }
      
 
  # Check if using R that's from a version PRIOR to that current for the desired date (prior to current release)
  # e.g., using R-3.3.3 for "2020-01-05"

  #2 Check R version
    rv <- r.version.check(date) # Get version of r being used and needed
    
    #2.1 Is date for a later major R? STOP

    if (package_version(rv$r.using.majmin) < package_version(rv$r.need.majmin)) {
      message2()
      message(
        "You are using R-", rv$r.using.full, " and the current R version for the data you entered:",
        "'", date, "' was R-", rv$r.need.majmin, ".\n",
        "To ensure reproducibility you must use groundhog.library() with a version of R ",
        "that is as least\nas recent as the entered date (i.e., R>=", rv$r.need.majmin, ")\n",
        "\n\n   ----------------- Package '", pkg, "' NOT LOADED ----------------"
      )
      exit()
    }
    
    
    #2.2 Is date for a previous major R? Warn
    
      if (package_version(rv$r.using.majmin) > package_version(rv$r.need.majmin)) {
          
         #Path to text file to keep track if warning already shown
            cookie_path <- paste0(get.groundhog.folder(),"/warning_r_mismatch.txt")
        
          #How long since last warning?
            since_warning <- 25  #assume 25 hours, i.e., show warnings
            if (file.exists(cookie_path)) since_warning <- difftime(Sys.time(),file.info(cookie_path)$ctime,units='hours')
            
          #If >24 show warnings
          if (since_warning>24)
          {
          message2()
          message1(
            "You are using R-", rv$r.using.full, ", but on (","'", date, "') the current version was R-", rv$r.need.majmin, ".\n",
            "Across different R versions, the same code can give different results or not\n",
            "work at all. You may want to either change the date you entered, or the version of R you use.\n",
            " - To change a date, choose something after '",get.r.majmin.release(),"'\n",
            " - Or use R-",rv$r.need.full, "  (instructions for running older R versions: http://groundhogr.com/many)\n\n")
          message1("Type 'OK' to continue anyway")
          text <- readline()
            
          #If they press stop, don't load/install package
            if (text!='ok' & text!="OK" & text!="'ok'") {
              message("You did not type OK, so it stopped.")
              message("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
              exit()
              }
            message1("OK. We will continue. This warning will not be shown again within 24 hours")
            
            #Save file to indicate this warning was shown (but only if they don't say stop, will show again if they say stop)
                write("1",cookie_path)
                
          } #Showed warnings
            
          
          }
      
          
          

  #3 Grab .libpaths()
    orig_lib_paths <- .libPaths()
    .libPaths("")  #actively remove default library path
    on.exit(.libPaths(orig_lib_paths))

  #4 Update cran.toc() if needed for entered date 
    update_cran.toc_if.needed(date)

  #5 Get vrs
    vrs <- get.version(pkg, date, current.deps = current.deps)
    pkg_vrs <- paste0(pkg, "_", vrs)

  #6 GET SNOWBALL
    snowball <- get.snowball(pkg, date, 
                             include.suggests=include.suggests, 
                             force.source=force.source,  
                             current.deps = current.deps)
    


  #7 CHECK FOR CONFLICT SNOWBALL <->ACTIVE PACKAGES
      check.snowball.conflict(snowball, force.install,ignore.deps,date)  
    
      
    

  #8 message if installation will be necessary
    need.to.install.total <- sum(!snowball$installed)
    if (need.to.install.total > 0) {
      message2()
      message1(
        "Loading ", pkg_vrs, " requires loading ", nrow(snowball), " packages, of which ",
        need.to.install.total, " will need to be installed."
              )
      

  }
  #9 Install packages if needed
  
  install.snowball(snowball, 
    date=date,
    force.install = force.install,
    force.source = force.source,
    quiet.install = quiet.install
    )
  
  #10 Load packages & attach the requested package
    n=nrow(snowball)
   for (k in 1:n)
    {
    .libPaths(c(.libPaths(), snowball$installation.path[k] ))
    loadNamespace(snowball$pkg[k], lib.loc = snowball$installation.path[k])
    if (k==n)   attachNamespace(snowball$pkg[k])
  }

  #11 Success/failure message
    #11.1 look at loaded packages
      active <- groundhog:::get.active()
      
    #11.2 Message
        #Package not loaded
            if (!pkg %in% active$pkg) 
                  {
                  message("groundhog says: FAILED to load '", pkg_vrs,"'")
                  }
      
        #Unexpected version loaded
            loaded_pkg_vrs <- active[active$pkg==pkg,]$pkg_vrs
            if ((pkg %in% active$pkg) & (!pkg_vrs %in% active$pkg_vrs)) 
                  {
                  
                  message("groundhog says: WARNING, loaded unexpected version of '", pkg, "'\n",
                         "expected: ''", pkg_vrs, "\n",
                         "loaded  : ''", pkg_vrs, "\n"
                    )
                  }
    if (pkg_vrs %in% loaded_pkg_vrs) {
      message1("groundhog says: successfully loaded '", pkg_vrs,"'.")
        } else {
        
      }

      
      
    
  #12 output
    invisible(loaded_pkg_vrs)
  }

