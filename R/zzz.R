
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
              
          
      
    #2 Hidden variables in local environment (need to equate to functions in zzz to avoid error on check)
        .available.restore.points <<- get.restore.points()   #Utils.R #55
        .view.conflicts <<- view.conflicts.function()        #Utils.R #57
    
        
    #5 Verify a mirror has been set    
      set.default.mirror() #utils.R  #36
  
    #6 Some checks after consent given
         if (check.consent(ask=FALSE)==TRUE) 
         {
          #6.1 New groundhog?
            check.groundhog.version(min.days=1) #Utils.R #42

          #6.2 R too new?
            #check_R_old_enough()
           
          #6.3 cran toc available?
             load.cran.toc()
         }
 
    #7 Delete purge subfolder with to-be-deleted pkgs (put here when using copy-and-delete method)
      #purge for >=3.0.0
         purge_path <- paste0(.libPaths()[1],"/_purge")
         if (dir.exists(purge_path)) try(unlink(purge_path,recursive=TRUE))
         
      #purge <3.0.0
         try(purge_v2.2()) #Utils #66

   
         
  
    } #End of onLoad


#8. Attaching 
    #' @importFrom utils packageVersion compareVersion
     
    .onAttach <- function(libname, pkgname) {
      

     #Startup msgs
      #7.1 Version and URL for website
          packageStartupMessage ("Attached: 'Groundhog' (Version: ",packageVersion('groundhog'),  ")") 
          packageStartupMessage ("Tips and troubleshooting: https://groundhogR.com")

    #While developing:
          packageStartupMessage ("#######################################################\n",
                               "This DEV version: 2023 05 05 - 15:29 (Barcelona time)")

      
      
  } #End on attach
    
    
      
 