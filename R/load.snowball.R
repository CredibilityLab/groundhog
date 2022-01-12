#Load imports, attach DEPENDS and requested package 
   
  load.snowball <- function(snowball,  ignore.deps)
  {
      
    #0 load full_toc
		  cran.toc <- .pkgenv[["cran.toc"]]  
	
    #1 Size of snowball 
      n <- nrow(snowball)
   
    #1.5 pkg_Vrs being loaded
      pkg_vrs <- snowball[n,]$pkg_vrs
      pkg <-snowball[n,]$pkg
      
   #2 Subset the full_toc 
      cran.toc.snowball <- cran.toc [paste0(cran.toc$Package,"_",cran.toc$Version) %in% snowball$pkg_vrs, ]
      
   #3 Get the needed DEPEND dependencies so that they are attached
      attach.all <- unique(unlist(strsplit(cran.toc.snowball$Depends, ",")))
  
   #4 add package itself to attach list
      attach.all <- c(attach.all, pkg)
      
   #5 Add to path and attach if needed
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

  #6 Success/failure message
    #6.1 look at loaded packages
      active <- get.active()
      
   
    #6.3 If package did not succeed to load, message
            if (!pkg %in% active$pkg) 
                  {
                  exit("groundhog says: FAILED to load '", pkg_vrs,"'")
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
            
    #Return vector with attached & remote packages added from this snowball
      snowball.remotes <- snowball[snowball$sha!='',]
      snowball.remotes$attached <- (snowball.remotes$pkg %in% attach.all)

      return(snowball.remotes)
  } #ENd of function