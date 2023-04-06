
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
              

    #2 Available restore points
        .available.restore.points <<- get.restore.points()
        
    #3 Delete to be purged packages, if any (put here with disable.packages())
        packages_df <- get.packages_df() #utils.R #Function 33
        purge_df <- subset(packages_df, packages_df$purged==TRUE)
        if (nrow(purge_df)>0) unlink(purge_df$path , recursive = TRUE)   
  
    #4 restore library complete message
        restore_dir <- paste0(get.groundhog.folder(),"/restore_points/", get.r.majmin())
        restore_cookie <- file.path(restore_dir , "restore_pending_restart_cookie.rds")  #see restore.library() #9
        if (file.exists(restore_cookie)) {
          restore_cookie_time <- readRDS(restore_cookie)
          #If a restore was requested within 1 hour, show message to confirm completed
          #more than 1 hour probabably means they went away and came back and msg would be confusing
          
            if (as.numeric(Sys.time())-restore_cookie_time < 1*60*60)
              {
              message2('Restore library process completed.')
            }
          #Delete cookie
          unlink(restore_cookie)
          
        }
        
    #5 Verify a mirror has been set    
      set.default.mirror() #Function 36 -  utils.R
  
    #6 Check if new version of groundhog exists, if we have not checked today yet
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
    
    
      
 