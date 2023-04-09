
# Package environment
        .pkgenv <- new.env(parent = emptyenv())


# installed base packages
        .pkgenv[['base_pkg']] <- data.frame(utils::installed.packages(priority = 'base'),row.names=NULL)$Package
      
        

  .onLoad <- function(libname, pkgname) {
  
    
    
    #1 pkgenv values
    
          #1.1 Empty paths for groundhog loaded packages
            .pkgenv[['groundhog.paths']] <- c(character())
              
          
          #1.2 Packages that have been already localized
            .pkgenv[['localized']] <- c()
          
          #1.3 Dataframe with snowballs loaded this session
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
              .pkgenv[['acceptable.option.names']] <- c('os','download.sequentially')
              

    #2 Hidden variables in local environment
        .available.restore.points <<- get.restore.points()
        .view.conflicts <<- ""
        
    #3 Delete to be purged packages, if any (put here with disable.packages())
        packages_df <- get.packages_df() #utils.R #Function 33
        purge_df <- subset(packages_df, packages_df$purged==TRUE)
        if (nrow(purge_df)>0) unlink(purge_df$path , recursive = TRUE)   
  
  
    #4 Verify a mirror has been set    
      set.default.mirror() #Function 36 -  utils.R
  
  
    #5 Check if new version of groundhog exists, if we have not checked today yet
       check.groundhog.version(min.days=1) #Function 42  -  utils.R
        
  
  
    } #End of onLoad


#2. Attaching 
    #' @importFrom utils packageVersion compareVersion
     
    .onAttach <- function(libname, pkgname) {
         
      #1 Report versions being used
          groundhog.version_using <- as.character(packageVersion("groundhog"))
          r.using.full= get.rversion() 
          packageStartupMessage ("Loaded 'groundhog' (version:",packageVersion('groundhog'),  ")") 
          packageStartupMessage ("Tips and troubleshooting: https://groundhogR.com")
          
  
        } #End on attach
    
    
      
 