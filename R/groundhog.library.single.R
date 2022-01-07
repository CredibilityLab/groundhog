#The groundhog.library() command runs validation and then loops over this function to actually
#load and install the called upon pacakge
#
#Until version 1.4.0 this was part of a single loop inside groundhog.library() but it was split into
#a separate function.
##################################################################################################

  groundhog.library.single <-  function(pkg, date,  quiet.install ,  include.suggests , 
    ignore.deps, force.source , force.install , tolerate.R.version )
      { 
    
      #0. Don't try to load 'groundhog'
        if ("groundhog" == pkg) {
          message("Error. May not use groundhog.library() to load groundhog.\n",
                  "To load the version of groundhog available on '", date, "', please use:\n",
                  "meta.groundhog('" ,  date , "')"
                   )
          exit()
        } #End if groundhog is a package being called

  
    #1.0 initial check,  stop if same version
      active=get.active()
      
  
    #1.1 Get version of requested package
        vrs <- get.version(pkg, date)
        pkg_vrs <- paste0(pkg, "_", vrs)
      

    #1.2 Stop if  pkg_vrs already attached
        attached.list= utils::sessionInfo()$otherPkgs
        attached.pkg <- names(attached.list)
        attached.vrs <- lapply(attached.list, function(x) x$Version)
        
      #Add base packages  
        attached.base.pkg <- utils::sessionInfo()$basePkgs
        attached.base.vrs <- as.character(sapply(attached.base.pkg, get.version, date)) 
        attached.pkg <- c(attached.pkg, attached.base.pkg)
        attached.vrs <- c(attached.vrs, attached.base.vrs)
        attached.pkg_vrs <- paste0(attached.pkg , "_" , attached.vrs)
        
        if (pkg_vrs %in% attached.pkg_vrs) {
            message1("groundhog says: the package you requested ('", pkg_vrs, "') is already attached.")
            return(invisible(active$pkg_vrs))
            
        }

  
        
    #1.2.5 Attach if package is loaded but not attached
        if (pkg_vrs %in% active$pkg_vrs)
        {
          attachNamespace(pkg)
          message1("groundhog says: the package you requested ('", pkg, "_", vrs, "') was loaded, now it is also attached")
          return(invisible(active$pkg_vrs))

        }
   
   #1.3 Mismatched package already attached  
         if ((pkg %in% attached.pkg) &  (!pkg_vrs %in% attached.pkg_vrs)) {
            message1(
                    "groundhog says: another version of '", pkg,"' is already attached ('", active$pkg_vrs[active$pkg==pkg],"').\n",
                    "To solve this: restart the R session. Note: you will need to do 'library(groundhog)' again.\n\n",
                    "In R Studio press: CTRL/CMD-SHIFT-F10"
                    )
          message("\nThe package '", pkg_vrs,"' was *NOT* attached")
          return(invisible(active$pkg_vrs))
         }
        
    
    #1.5 Attach mismatched version if ignore.deps is loaded but not attached (common scenario, trying to attach knitr in .rmd file)
       if ((pkg %in% active$pkg) & (!pkg_vrs %in%  active$pkg_vrs) & (pkg %in% ignore.deps))
        {
         #Recommended
             ip <- data.frame(utils::installed.packages(),stringsAsFactors = FALSE)
             recommended.pkgs <- unique(subset(ip, ip$Priority=="recommended")$Package) #unique because there may be two versions of the same package in different libraries
         
          attachNamespace(pkg)
          message1("groundhog says: succesfully attached '" , pkg , "'")
          message("\ngroundhog warning:\n", 
                   "'", pkg, "' was already loaded, and it is now attached,\n",
                  "BUT the loaded version ('" , active$pkg_vrs[active$pkg==pkg] , "') does not match the version for ",
                   "'" , date, "' ('", pkg_vrs , "').\n",
                  "To attach the desired version you can try restarting the R session.\n\n",
                  "In R Studio press: CTRL/CMD-SHIFT-F10\n\n",
                  "Note that it is possible that this package is being loaded automatically from your local\n",
                  "library. In this case, after restarting the session the problem will persist.\n",
                  "You can ignore this problem and tolerate lack of version control for the involved packages.\n", 
				  "You can also prevent this problem by uninstalling the package from your non-groundhog library\n",
				  "running: remove.packages('", pkg ,"'), but if R Studio is using the package (e.g., the\n",
				  "'knit' button requires 'knitr' outside of groundhog), you may need to rely on R rather than\n",
				  "R Studio for that disabled functionality."
				 )
		
          
        #Explain the recommended issue if appropriate
          if (pkg %in% recommended.pkgs) {
              message(
                    "The package in question '", pkg , "', is a 'recommended' package which makes removing it from the\n",
                    "local (non-groundhog) library potentially more problematic; this is why this conflict\n",
                    "is tolerated by groundhog, producing a warning rather than an error."
                    )
            }
          return(invisible(active$pkg_vrs))

        }   
        
        
    
    # Check if using R that's from a version PRIOR to that current for the desired date (prior to current release)
    # e.g., using R-3.3.3 for "2020-01-05"

    #2 Check R version
      rv <- r.version.check(date) # Get version of r being used and needed
    
    #If using version of R too advance for groundhog
      R.toc <- toc("R")
      if (package_version(max(R.toc$Version)) < package_version(rv$r.using.majmin))
          {
          #two days ago
            two.days.ago <- Sys.Date()-2
          #update temporarily cran.toc.if needed
            max.date <- (max(.pkgenv[["cran.toc"]]$Published))
            if (max.date < two.days.ago) load.cran.toc(update.toc = TRUE)
            rv <- r.version.check(date) 
            if (package_version(max(R.toc$Version)) < package_version(rv$r.using.majmin))
              {
              message("groundhog says:")
              message("The version of R you are using, 'R-" , rv$r.using.full, "' is not in the groundhog database.")
              message("You can temporarily add it. This is a work-around intended for testing groundhog with R versions\n",
                      "that have not yet been released. You should not consider this script reproducible.\n",
                      "To write reproducible R code please use an already released version of R instead. \n\n",
                      "Type OK to temporarily add ", rv$r.using.full , " to your local groundhog database\n",
                      " (it will be added as if it were released 2 days ago, and when you restart the R session it will be removed).")
              text <- readline("Enter OK to add, anything else to stop >")
              
              #If say OK, add row to database temporarily
                if (tolower(text)=="ok")
                {
                  row <- data.frame(Package="R",Version=rv$r.using.full,Published = two.days.ago, Imports="",Depends="",Suggests="",LinkingTo="")
                  .pkgenv[["cran.toc"]] <- rbind(.pkgenv[["cran.toc"]], row)
                } else {
              #Else, end
                  message("Your typed '" , text, "!=OK, groundhog.library() request will terminate here.")
                  exit()
                 } #end else
              } #End if version not in cran.toc even after update
            }   #End if version not in cran.toc before update
    
    #2.1 Is date for a later major R? STOP

    if ((package_version(rv$r.using.majmin) < package_version(rv$r.need.majmin)) &  rv$r.using.full!=tolerate.R.version) 
      {
      message2()
      message(
        "You are using R-", rv$r.using.full, " and the current R version for the date you entered:",
        "'", date, "' was R-", rv$r.need.majmin, ".\n",
        "It is recommended that you use the matching version of R, but you may bypass this error message\n",
        "by adding: `tolerate.R.version='",rv$r.using.full,"'` as an option to your groundhog.library() call.",
        "\n\n   ----------------- Package '", pkg, "' NOT LOADED ----------------"
      )
      exit()
    }
    
        
    #2.1.5 throw warning if using R that is too old, but explicitly allowingg via command 
    if ((package_version(rv$r.using.majmin) < package_version(rv$r.need.majmin)) & 
        (rv$r.using.majmin==tolerate.R.version | rv$r.using.full==tolerate.R.version)) {
          message2()
          message(
           "You are using R-", rv$r.using.full, " and the current R version for the data you entered:",
           "'", date, "' was R-", rv$r.need.majmin, ".\n",
           "Usually this results in an error and groundhog stops processing the request, but\n",
           "you are receiving only a warning because you explicitly allowed version 'R-", tolerate.R.version, "'.\n"
          )
            
            
         }

        
    
    #2.2 Is date for a previous major R? Warn
    
      if (package_version(rv$r.using.majmin) > package_version(rv$r.need.majmin)) {
          
         #Path to text file to keep track if warning already shown
            cookie_path <- paste0(get.groundhog.folder(),"/warning_r_mismatch.txt")
        
          #Path to full text of code submitted that we amay want to execute (code interrupted by prompt)
            all.text_path <-paste0(get.groundhog.folder(),"/all_text_mismatch_r.txt")
        
            
          #How long since last warning?
            since_warning <- 31  #assume 30 minutes , i.e., show warnings
            if (file.exists(cookie_path)) since_warning <- difftime(Sys.time(),file.info(cookie_path)$mtime,units='mins')
			      
          #If >24 show warnings
          if (since_warning>30)
          {
          #Update cookie to indicate warning has been shown now
            unlink(cookie_path)

          #Show warning

          message2()
          message1(
            "You are using R-", rv$r.using.full, ", but on ","'", date, "' the current version was R-", rv$r.need.majmin, ".\n",
            "Old packages can take longer to install, and old code in general and old packages\n",
            "in particular can give different results, or not run at all, in newer R versions.\n",
            "You may want to either change the date you entered or the version of R you use.\n",
            " - To change the date, choose something after '",get.r.majmin.release(),"'\n",
            " - For instructions to run older versions of R (e.g. R-",rv$r.need.full, "), see http://groundhogr.com/many\n\n")
          
          #While loop for readline to avoid submitted code to be interpreted as the answer
            len.answer <- 0
            text <-''
            j <- 1 #counter of times message is shown
            while (strip.prompt(text)!="x" & strip.prompt(text)!="ok")  
            {
              prompt.text <- paste0("Type 'OK' to ignore this warning about the date for {", pkg , "}, or type 'X' to stop >")
              text <- readline(prompt.text)
              if (strip.prompt(text) !="ok" & strip.prompt(text) != "x") {
                    
                  message(j , ") You answered: '", text , "'") 
                  message("   To ensure you are actively answering, only 'OK' and 'X' are accepted as responses\n")
                  j<-j+1 #Add to counter of msgs rejected
                } #end if
            } #End while
          
            
          #If they press stop, don't load/install package
            if (strip.prompt(text)=="x") {
              message("You typed 'X'; the attempt to load '",pkg,"' has stopped.")
              exit()
              } else {
              message1("OK. We will continue. This warning will not be shown again within 30 minutes.\n")
              write("1",cookie_path)
            
              } #End else
          } #Showed warnings
        
          
          }
      
       
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

