
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
      
      #4 Dataframe with snowballs loaded this session
     .pkgenv[['session.snowballs']] <- data.frame(pkg=character(),
                                                        vrs=character(), 
                                                        pkg_vrs=character(), 
                                                        repos=character(), 
                                                        time=numeric(),
                                                        sha=character(), 
                                                        requested=logical())

     
     
       #5 Remotes loaded
         .pkgenv[['session.remotes_df']] <- data.frame(remote_id = character() ,  usr=character() , pkg=character() , date=character())
        
      #5 Setup pkg variable values
          .pkgenv[["supportsANSI"]] <- Sys.getenv("TERM") %in% c("xterm-color", "xterm-256color", "screen", "screen-256color")
          .pkgenv[['default_libpath']] <-  .libPaths()
          .pkgenv[['hogdays']] <-c()
          .pkgenv[['acceptable.option.names']] <- c('os','download.sequentially')
          

          
    
      #6 Delete to be purged packages, if any (put here with disable.packages())
          packages_df <- get.packages_df() #utils.R #Function 33
          purge_df <- subset(packages_df, packages_df$purged==TRUE)
          if (nrow(purge_df)>0) unlink(purge_df$path , recursive = TRUE)   
    
          
      #7 Verify a mirror has been set    
        set.default.mirror() #Function 36 -  utils.R
        
       #8 If background file script for installing snowball.list has not been copied, copy it
          back_path<- file.path(get.groundhog.folder(), "background_install.snowball.list.R")
          if (!file.exists(back_path) & system.file("background_install.snowball.list.R", package = "groundhog")!='')
            {
            file.copy  (system.file("background_install.snowball.list.R", package = "groundhog") , back_path)
          }

       #9 Read manually set options
          #Path where option files are saved
            options_path <-  paste0(path.expand("~"), "/R_groundhog/options/")
            
          #If path exists, read its contents
            if (file.exists(options_path)) {
              
                #List of file paths as vector
                    options_files <- list.files(options_path,full.names = TRUE)
                  
                #Loop reading those files
                    for (fk in options_files) {
                      
                      #Assign to .pkgenv
                      .pkgenv[[basename(fk)]] <- scan(fk,what='character',quiet=TRUE)
                      
                     
                    }
                    
              } #End if file found

                  
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
    
    
      
 