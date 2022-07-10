
#Create package environment
  .pkgenv <- new.env(parent = emptyenv())

#Empty paths for groundhog loaded packages
  .pkgenv[['groundhog.paths']] <- c(character())

#installed base packages
  .pkgenv[['base_pkg']] <- data.frame(utils::installed.packages(priority = 'base'))$Package
    

#Packages that have been already localized
  .pkgenv[['localized']] <- c()

  
#Dataframe with groundhog package this session, saving every pkg loaded with groundhog.
  .pkgenv[['groundhog.session_df']] <- data.frame(pkg=character(),
                                                  vrs=character(), 
                                                  pkg_vrs=character(), 
                                                  repos=character(), 
                                                  requested=logical(),
                                                  time=numeric())
  

#Check support of colors? (legacy function perhaps)
  .onLoad <- function(libname, pkgname) {
  

    #Setup pkg variable values
    .pkgenv[["supportsANSI"]] <- Sys.getenv("TERM") %in% c("xterm-color", "xterm-256color", "screen", "screen-256color")
    .pkgenv[['default_libpath']] <-  .libPaths()
    .pkgenv[['groundhog_loaded_pkgs']] <- c()

    
              
    #Delete to be purged packages, if any (put here with disable.packages())
      packages_df <- get.packages_df() #utils.R #Function 33
      purge_df <- subset(packages_df, packages_df$purged==TRUE)
      if (nrow(purge_df)>0) unlink(purge_df$path , recursive = TRUE)   
    
         
    } #End of onLoad


    
#2. Attaching 
    #' @importFrom utils packageVersion compareVersion
     
    .onAttach <- function(libname, pkgname) {
      
      
      
      #1. Check if consent to write to local folder used for groundhog files has been given
        main_folder <-  paste0(path.expand("~"), "/R_groundhog")
        consent <- (file.exists(main_folder))
        
      #2. If no consent, ask for it
         if (consent == FALSE) {
            message("groundhog needs authorization to save files to  '",main_folder, "'\n",
                                  "Enter 'OK' to provide authorization")
                                  
            answer <- readline()
            answer <- gsub("'", "", answer)  #kill the ' if entered

            if (toupper(answer)=="OK") {
              consent <- TRUE
              }
              } #End if consent == FALSE
        
      #3. It not given, die
          if (consent == FALSE)
          {
          message("You did not say 'OK'\nIf you run 'groundhog.library()' you will be asked again")
          }
        
      #4. If consent given, proceed
          if (consent == TRUE)
          {
              #Create the folder (when running groundhog.library() this will signal consent was given)
                dir.create(main_folder, showWarnings = FALSE, recursive = TRUE)
        
            
      #5 Load toc files
          load.cran.toc()    

      #6 Report versions being used
          groundhog.version_using <- as.character(packageVersion("groundhog"))
          r.using.full= get.rversion() 
          packageStartupMessage ("Loaded 'groundhog' (version:",packageVersion('groundhog'),  ") using R-" ,r.using.full) 
          packageStartupMessage ("Tips and troubleshooting: https://groundhogR.com")
        
           
      #7 check for need to update groundhog
        check.groundhog.version(min.days=0) #Function 42  -  utils.R
        
      #8 Verify a mirror has been set    
        set.default.mirror() #Function 36 -  utils.R

 
      } #End if consent==TRUE
    } #End on attach
    
    
      
 