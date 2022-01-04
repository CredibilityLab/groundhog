# The groundhog.library() command runs validation and then calls this function (looping if more than 1 pkg was called)
# to install and load them

#OUTLINE
 #1 Validate date
 #2 Validate pkg_vrs
 #3 Validate R
 #4 Get Snowball
 #5 Set path to find installed packages during installation .libpaths()
 #6 CHECK FOR CONFLICT SNOWBALL <->ACTIVE PACKAGES
 #7 message if installation will be necessary
 #8 Install packages if needed
 #9 Dropped
 #10 Load packages & attach the requested package
     #10.1 Load the cran.toc
     #10.2 Get the needed DEPEND dependencies so that they are attached
     #10.4 add package itself to attach list
     #10.5 Add to path and attach if needed
 #11 Success/failure message
     #11.1 look at loaded packages
     #11.2 Message

#############################################################################



  groundhog.library.single <-  function(pkg, date,  quiet.install ,  include.suggests , 
     ignore.deps, force.source , force.install , tolerate.R.version )
      { 
    
    #0 Make pkg.list is a standardized notation for when pkg is the package name vs usr/pkg, vs git::usr/pkg (e.g. pkg.list$usr_pkg)
        pkg_list<-make.pkg_list(pkg)
    
    #1 Validate date (done in groundhog.library())
    
    #2 If remote update cran toc
        remote_id <-get.remote_id(pkg)
        if (remote_id=='cran')                    full_toc <- get.full_toc()
        if (remote_id %in% c('gitlab','github'))  full_toc <- get.full_toc(pkg_list$git_usr_pkg , date , refresh=TRUE)  
        #get.full_toc.R delivers cran.toc.rds, updated if needed, and with remotes associated with pkg if any
      

      #On exit, reload the saved databases, this way changes to full_toc with remotes are removed after the package is installed.
       on.exit(load.databases())  #From load.databases.R
    
       
    #2 Validate R version
          validate_R(pkg=pkg, date=date, tolerate.R.version=tolerate.R.version)
          # Function in validate_R.R checking if the version of R being used exists and is consistent
          # with the groundhog.day.
 
      
    #2.5 Process remotes
            git_usr_pkg <- pkg  #assume it is cran, so git_usr_pkg = pkg, change if it is remote
          
        #Standardize the name of the pkg to be just a package name even if a remote was entered
            remote_id <- get.remote_id(pkg)  #c('cran','github', 'gitlab') 
            if (remote_id!='cran') {
              git_usr_pkg <- standardize.git_usr_pkg(pkg)    #note, this takes the syntax 'github::usr/pkg' but if pkg is a cran package, then git_usr_pkg=pkg
              pkg         <- basename(git_usr_pkg)           #extract just the pkg name from remote packages
              baton <- get.baton(git_usr_pkg, date)          #see process.remote.all.R, get.baton is an alias
            }
          
        
          
    #3 Validate pkg_vrs 
          validate.pkg_vrs(pkg, date, ignore.deps)  
          # Function in validate.pkg_vrs.R. It checks if attached or loaded 
          # version exists and whether it matches the requested version.
          # exiting the request if a problem is caught.
  
		      vrs <- get.version(pkg, date)
		      pkg_vrs<-paste0(pkg,"_",vrs)
             
    #4 Load/Create snowball 
          snowball <- get.snowball(git_usr_pkg, date, include.suggests=include.suggests, force.source=force.source)
    
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
  
 
  #9 Load the snowball
    load.snowball(snowball, ignore.deps=ignore.deps) 
    #This is a function on its own so that the groundhog.github() can rely on it too
 
  
  } #ENd of groundhog.library.single() function

