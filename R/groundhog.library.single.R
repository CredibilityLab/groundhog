#The groundhog.library() command runs validation and then loops over this function to actually
#load and install the called upon pacakge
#
#Until version 1.4.0 this was part of a single loop inside groundhog.library() but it was split into
#a separate function.
##################################################################################################

  groundhog.library.single <-  function(pkg, date,  quiet.install ,  include.suggests , ignore.deps, force.source , force.install )
      { 
    
        
  #0 Date and R validated in groundhog.library()
      
    
  #1 Get pkg_vrs
      vrs     <- get.version(pkg, date)
      pkg_vrs <- paste0(pkg, "_", vrs)
  
      
  #2 Validate pkg
      validate.pkg_vrs(pkg, vrs, date, ignore.deps)
                      
          
  #3 Update cran.toc() if needed for entered date 
      update_cran.toc_if.needed(date)

  #4 GET SNOWBALL
      snowball <- get.snowball(pkg, date, include.suggests=include.suggests, force.source=force.source)
    
  #5 Set path to find installed packages during installation .libpaths()
      
      #Grab existing path(s)
         orig_lib_paths <- .libPaths()
         
      #actively remove default library paths to prevent loading packages from local library
 	      .libPaths("")  
 	      
 	    #Assign the set of paths to be used as libraries
 	      #Create directories if they don't exist, otherwise libpath won't create it
 	        for (pathk in snowball$installation.path) {
 	          dir.create(pathk, recursive = TRUE, showWarnings = FALSE)
 	          }
 	      
 	      #Add all paths
 	       .libPaths(snowball$installation.path)
 	      
 	      #return to default path upon exiting
           on.exit(.libPaths(orig_lib_paths))
 	      

  #6 CHECK FOR CONFLICT SNOWBALL <->ACTIVE PACKAGES
      check.snowball.conflict(snowball, force.install,ignore.deps,date)  
    

  #7 message if installation will be necessary
    need.to.install.total <- sum(!snowball$installed)
    if (need.to.install.total > 0) {
      message2()
      message1(
        "Loading ", pkg_vrs, " requires loading ", nrow(snowball), " packages, of which ",
        need.to.install.total, " will need to be installed."
              )
    }
    
  #8 Install packages if needed
    install.snowball(snowball, 
      date=date,
      force.install = force.install,
      force.source = force.source,
      quiet.install = quiet.install
      )
  
  #10 Load packages & attach the requested package
   n <- nrow(snowball)
   
   #10.1 Load the cran.toc
      cran.toc <- .pkgenv[["cran.toc"]]
      cran.toc.snowball <- cran.toc [paste0(cran.toc$Package,"_",cran.toc$Version) %in% snowball$pkg_vrs, ]
      
   #10.2 Get the needed DEPEND dependencies so that they are attached
      attach.all <- unique(unlist(strsplit(cran.toc.snowball$Depends, ",")))
  
  #10.4 add package itself to attach list
      attach.all <- c(attach.all, pkg)
      
   #10.5 Add to path and attach if needed
       for (k in 1:n)
        {
        
      #Load the package and put it on the library path in case there is a reference to it like a library call.
        if (!snowball$pkg[k] %in% base_pkg())
          {
          loadNamespace(snowball$pkg[k], lib.loc = snowball$installation.path[k])
          }

        if (snowball$pkg[k] %in% attach.all)
          { 
            attached.so_far <- (.packages())
            if (!snowball$pkg[k] %in% attached.so_far) 
            {
            message1("Attaching ",snowball$pkg_vrs[k])
            attachNamespace(snowball$pkg[k])           
            } #End if not attached
          } #End if it is in the to-attach list
      
        } #End for loop

  #11 Success/failure message
    #11.1 look at loaded packages
      active <- get.active()
      
    #11.2 Message
        #Package not loaded
            if (!pkg %in% active$pkg) 
                  {
                  message("groundhog says: FAILED to load '", pkg_vrs,"'")
                  }
      
        #Unexpected version loaded
            loaded_pkg_vrs <- active[active$pkg==pkg,]$pkg_vrs
            if ((pkg %in% active$pkg) & (!pkg_vrs %in% active$pkg_vrs) & (!pkg %in% ignore.deps))
                  {
                  message("groundhog says: WARNING, loaded unexpected version of '", pkg, "'\n",
                         "expected: '", pkg_vrs, "'\n",
                         "loaded  : '", active$pkg_vrs[active$pkg %in% pkg], "'\n"
                    )
                  }
   
      
    if (pkg %in% base_pkg()) {
      message("Note: the package '", pkg, "' is part of base R.\n",
              "The version included in base R is always loaded, regardless of the date entered.")
    }
 
  
  } #ENd of groundhog.library.single() function

