
# Package environment
        .pkgenv <- new.env(parent = emptyenv())


# installed base packages
        .pkgenv[['base_pkg']] <- data.frame(utils::installed.packages(priority = 'base'),row.names=NULL,stringsAsFactors = FALSE)$Package
      
        

  .onLoad <- function(libname, pkgname) {
  
    #1 pkgenv values
    
          #1.1 Empty paths for groundhog loaded packages
            .pkgenv[['groundhog.paths']] <- c(character())
              
          
          #1.2 Packages that have been already localized
            .pkgenv[['localized']] <- c()
          
          #1.3 Dataframe with snowballs loaded this session (used to check for groundhog induced conflicts)
           .pkgenv[['session.snowballs']] <- data.frame(pkg=character(),
                                                              vrs=character(), 
                                                              pkg_vrs=character(), 
                                                              repos=character(), 
                                                              time=numeric(),
                                                              sha=character(), 
                                                              requested=logical())
    
          #1.4 Remotes loaded
             .pkgenv[['session.remotes_df']] <- data.frame(remote_id = character() ,  usr=character() , pkg=character() , date=character())
            
          #1.5 Setup pkg variable values
              .pkgenv[["supportsANSI"]] <- Sys.getenv("TERM") %in% c("xterm-color", "xterm-256color", "screen", "screen-256color")
              .pkgenv[['default_libpath']] <-  .libPaths()
              .pkgenv[['hogdays']] <-c()
              .pkgenv[['conflicts']] <- ''
              
          
      
    #2 Hidden variables in local environment
        .available.restore.points <<- get.restore.points()
        .view.conflicts <<- view.conflicts.function()
    
        
    #5 Verify a mirror has been set    
      set.default.mirror() #Function 36 -  utils.R
  
    #6 Check if new version of groundhog exists, if we have not checked today yet
       if (check.consent(ask=FALSE)==TRUE) check.groundhog.version(min.days=1) #Function 42  -  utils.R
          
    #7 Load cran toc if available
       if (check.consent(ask=FALSE)==TRUE) load.cran.toc()
      
 
    #8 Delete purge subfolder with to-be-deleted pkgs (put here when using copy-and-delete method)
      #purge for >=3.0.0
         purge_path <- paste0(.libPaths()[1],"/_purge")
          if (dir.exists(purge_path)) try(unlink(purge_path,recursive=TRUE))
         
      #purge <3.0.0
         try(purge_v2.2()) #Utils #66

   
  
    } #End of onLoad


#7. Attaching 
    #' @importFrom utils packageVersion compareVersion
     
    .onAttach <- function(libname, pkgname) {
      

     #Startup msgs
      #7.1 Version and URL for website
          packageStartupMessage ("Attached: 'Groundhog' (Version: ",packageVersion('groundhog'),  ")") 
          packageStartupMessage ("Tips and troubleshooting: https://groundhogR.com")

    #While developing:
     # packageStartupMessage ("#######################################################\n",
     #                         "This DEV version: 2023 04 28 - 15:24 (Barcelona time)")

      
      
  } #End on attach
    
    
      
 