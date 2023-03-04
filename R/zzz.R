
# Package environment
        .pkgenv <- new.env(parent = emptyenv())


# installed base packages
        .pkgenv[['base_pkg']] <- data.frame(utils::installed.packages(priority = 'base'))$Package
      
        

  .onLoad <- function(libname, pkgname) {
  

    #---VARIABLES---
    
      
      #1 Empty paths for groundhog loaded packages
        .pkgenv[['groundhog.paths']] <- c(character())
      
          
      
      #3 Packages that have been already localized
        .pkgenv[['localized']] <- c()
      
        
      #4 Dataframe with groundhog package this session, saving every pkg loaded with groundhog.
        .pkgenv[['groundhog.session_df']] <- data.frame(pkg=character(),
                                                        vrs=character(), 
                                                        pkg_vrs=character(), 
                                                        repos=character(), 
                                                        requested=logical(),
                                                        time=numeric())
        
      #5 Setup pkg variable values
          .pkgenv[["supportsANSI"]] <- Sys.getenv("TERM") %in% c("xterm-color", "xterm-256color", "screen", "screen-256color")
          .pkgenv[['default_libpath']] <-  .libPaths()
          .pkgenv[['groundhog_loaded_pkgs']] <- c()
    
    
      #6 Delete to be purged packages, if any (put here with disable.packages())
          packages_df <- get.packages_df() #utils.R #Function 33
          purge_df <- subset(packages_df, packages_df$purged==TRUE)
          if (nrow(purge_df)>0) unlink(purge_df$path , recursive = TRUE)   
    
          
      #7 Verify a mirror has been set    
        set.default.mirror() #Function 36 -  utils.R

         
    } #End of onLoad


    
#2. Attaching 
    #' @importFrom utils packageVersion compareVersion
     
    .onAttach <- function(libname, pkgname) {
      
  
      #1 Report versions being used
          groundhog.version_using <- as.character(packageVersion("groundhog"))
          r.using.full= get.rversion() 
          packageStartupMessage ("Loaded 'groundhog' (version:",packageVersion('groundhog'),  ")") 
          packageStartupMessage ("Tips and troubleshooting: https://groundhogR.com")
          
      #2 check for need to update groundhog
        check.groundhog.version(min.days=0) #Function 42  -  utils.R
  
        } #End on attach
    
    
      
 