
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
       check.groundhog.version(min.days=1) #Function 42  -  utils.R
          
    #7 Load cran toc if available
       if (check.consent(FALSE)==TRUE) load.cran.toc()
  
    } #End of onLoad


#7. Attaching 
    #' @importFrom utils packageVersion compareVersion
     
    .onAttach <- function(libname, pkgname) {
      

     #Startup msgs
      #7.1 Version and URL for website
          packageStartupMessage ("Attached: 'Groundhog' (Version: ",packageVersion('groundhog'),  ")") 
          packageStartupMessage ("Tips and troubleshooting: https://groundhogR.com")


      #7.2  Default date suggestion
         # suggested default date on the year when R is released and next year, no default otherwise
          if (check.consent(ask=FALSE)==TRUE)
          {
            R.year <- as.numeric(format(get.r.majmin.release(),"%Y"))
            today.year <- as.numeric(format(Sys.Date()-2,"%Y"))      #subtract 2 days so that it is treated like last year till Jan 3rd
            today.month <- as.numeric(format(Sys.Date(),"%m"))

          #Assume we will not recommend anything
              default.date <- NULL

          #1st year of R version, after June 1st, year/06/01
              if (today.year==R.year & today.month>=6) default.date <- paste0(today.year,"/06/01")
          #2st year of R version, year/01/01
              if (today.year==R.year+1)                default.date <- paste0(today.year,"/01/01")

          if (!is.null(default.date)) {
              packageStartupMessage ("--> Suggested default groundhog day: '",default.date,"'")
              }
          } #End if consent given before
          

          
       
          

packageStartupMessage ("##########################################\n This version: 2023 04 17 - 15:15")

  } #End on attach
    
    
      
 