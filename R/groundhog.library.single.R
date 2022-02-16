
  groundhog.library.single <-  function(pkg, date,  quiet.install ,  include.suggests , ignore.deps, force.source , force.install )
  { 
    
        
  #0 Date and R validated in groundhog.library()
  
  #1 Get pkg_vrs
      vrs     <- get.version(pkg, date)
      pkg_vrs <- paste0(pkg, "_", vrs)
  
      
  #2 Validate pkg
      validation<-validate.pkg_vrs(pkg, vrs, date, ignore.deps)
      if (validation=='already_attached') return(TRUE)
          
      
  #3 Update cran.toc() if needed for entered date 
      update_cran.toc_if.needed(date)

      
  #4 GET SNOWBALL
      snowball <- get.snowball(pkg=pkg , date=date , include.suggests=include.suggests, force.install=force.install)
    
      if (force.source==TRUE)  snowball$from='source'
      if (force.install==TRUE) snowball$installed=FALSE
      
      
  #5 CHECK FOR CONFLICT SNOWBALL <-> AVAILABLE PACKAGES
      check.snowball.conflict(snowball, force.install,ignore.deps,date)  
    
        
  #6 message if installation will be necessary
    need.to.install.total <- sum(!snowball$installed)
    if (need.to.install.total > 0) {
      message2()
      message1("Loading ", pkg_vrs, " requires loading ", nrow(snowball), " packages, of which ",need.to.install.total, " will need to be installed.")
      } #End 7
    
    
  #7 Install packages if needed, add and groundhog libpaths for each package
    install.snowball(snowball,       date=date,      force.install = force.install, force.source = force.source, quiet.install = quiet.install)
  
 
  #8 Attach it
        base.library(pkg, character.only=TRUE)

      #and add it to the libpath       
       .libPaths(c(.libPaths(), snowball$installation.path[nrow(snowball)]))
      
  #9 Verify (check target is attached and full snowball pkg_vrs is loaded or in libpath)
     verified <- verify.snowball.loaded(snowball, ignore.deps)  
 
      
  #10 If verified and no ignore deps used, save snowball
     if (verified==TRUE) { 
     
      #10.1 Update  what is installed in the snowball
          ip<-data.frame(utils::installed.packages(snowball$installation.path))
          snowball$installed <- snowball$pkg %in% ip$Package
          
			 #10.2 Path to snowball
							snowball_dir <- paste0(get.groundhog.folder() , '/snowballs' )
							snowball_file <- paste0(pkg , "_" ,  gsub( "-", "_" , date) , '.rds')  
							snowball_path <- file.path(snowball_dir, snowball_file)
					  
					   
						if (!file.exists(snowball_path)) {
						  saveRDS(snowball, snowball_path, version = 2)
				} #End if snowball exists
     } #End if verified         

  #11 If not verified, delete snowball
      if (verified==FALSE) {
        if (file.exists(snowball_path)) file.remove(snowball_path)
      }
     
  } #End of groundhog.library.single() function

