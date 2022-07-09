
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
    #2.1 Show message of library path 
    #' @importFrom utils packageVersion compareVersion
    .onAttach <- function(libname, pkgname) {
      
      #Consent previously?
        main_folder <-  paste0(path.expand("~"), "/R_groundhog")
        consent <- (file.exists(main_folder))
        
       #Check if folder exists, or ask for consent otherwise
         if (consent == FALSE) {
            message("groundhog needs authorization to save files to  '",main_folder, "'\n",
                                  "Enter 'OK' to provide authorization")
                                  
            answer <- readline()
            answer <- gsub("'", "", answer)  #kill the ' if entered

            if (toupper(answer)=="OK") {
              consent <- TRUE
              }
              } #End if consent == FALSE
        
      # If No consent, die
          if (consent == FALSE)
          {
          message("You did not say 'OK'\nIf you run 'groundhog.library()' you will be asked again")
          }
        
      #Proceed only if consent exists
          if (consent == TRUE)
          {
              #Create the folder (when running groundhog.library() this will signal consent was given)
                dir.create(main_folder, showWarnings = FALSE, recursive = TRUE)
               
                
      #Load toc files
          load.cran.toc()    

        #Report versions being used
          groundhog.version_using <- as.character(packageVersion("groundhog"))
          r.using.full= get.rversion() 
          packageStartupMessage ("Loaded 'groundhog' (version:",packageVersion('groundhog'),  ") using R-" ,r.using.full) 
          packageStartupMessage ("Tips and troubleshooting: https://groundhogR.com")
        
           
    #2.2 check for update
    # isTRUE() is necessary here because this will return logical(0) if the pkg
    # is not on CRAN, or if working offline (current.packages is NULL in this case).
      #Try to read from groundhogr.com   
          groundhog.version_cran <- tryCatch(
          as.character(readLines("https://groundhogr.com/groundhog_version.txt")),
          warning = function(w) NULL,
          error = function(e) NULL
        )
       
    #Get majmin
      if (!is.null(groundhog.version_cran)) {
          gv.using <- as.numeric(strsplit(groundhog.version_using, "\\.")[[1]])
          gv.cran  <- as.numeric(strsplit(groundhog.version_cran, "\\.")[[1]])
          gv.using.majmin <- 10000*gv.using[1] + gv.using[2]
          gv.cran.majmin <-  10000*gv.cran[1]  + gv.cran[2]
    
    
        if (isTRUE(gv.cran.majmin > gv.using.majmin)) {
          packageStartupMessage(
            "\n\n\n",
            "          OUTDATED GROUNDHOG\n",
            "            You are using version  '" , groundhog.version_using, "\n",
            "            The current version is '" , groundhog.version_cran, "'\n\n",
            "            You can read about the changes here: https://groundhogr.com/changelog\n\n",
            "Please update by running: \ninstall.packages('groundhog')"
            )
            }  #End mismatch in version
          } #End if !is.null()
          } #ENd if consent==TRUE
        
        
        
     #Verify a mirror has been set    
      set.default.mirror() #Function 36

 
     
      } #End on attach
    
    
      
 