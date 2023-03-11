  
  #1 Preliminaries
  #2 Early return if everything is already attached
  #3 Get snowballs for all requested packages
  #4 Create libpaths
  #5 Install snowball 
    #5.2 Background (with R Script if source conflicts with it)
    #5.3 Forground
  #6 localize the snowballs
  #7 Check conflict now that it is all installed 
  #8 library() all pkgs
  #9 Verify each snowball, saving snowball .rds if successful  
  

#----------------------------------------------------------------------------------------

#' @export
  new.groundhog.library <- function(pkg, date,        quiet.install = TRUE,  
                            include.suggests = FALSE, ignore.deps=c(), 
                            force.source = FALSE,     force.install = FALSE, 
                            tolerate.R.version = "" , cores = -1)
  {
    
#--------------------------------------------------------------
    
  #1 Preliminaries
        
    #1.1 Save default libpaths to change back to them after exiting
        if (!exists("orig_lib_paths",envir=.pkgenv)) {
              .pkgenv[["orig_lib_paths"]] <- .libPaths()
           }
    
    #Note: 1.2 & 1.3 could be done on loading, but, better here : (i) in case the user makes a change, and (ii) avoid writing files without authorization
    #1.2  Verify a mirror has been set (utils.R #36)    
        set.default.mirror() 
    
    #1.3 Verify a personal library to save non-groundhog packages has been assigned (Utils.R #37)
        verify.personal.library.exists() 
    
    #1.4 Validate arguments entered (Utils.R #46)
        validate.groundhog.library(pkg, date,  quiet.install,  include.suggests ,ignore.deps, force.source , force.install, tolerate.R.version  ,cores)  
       
  
#>>> PENDING< DO NOT ALLOW REMOTE IN A BUNDLE
       
    #1.5 Reload databases if needed
        update_cran.toc_if.needed(date) 
 
              
    #1.6 On Exit refresh libpath and cran.toc (cran toc can be modified temporarily by a remote)
          
            on.exit({
                    #Read cran toc again to undo any changes with remote
                       .pkgenv[['cran.toc']] <- readRDS(file.path(get.groundhog.folder(),"cran.toc.rds"))
                    
                    #Return libpath, if it has been set.
					            if  (exists("orig_lib_paths",envir=.pkgenv)) .libPaths(.pkgenv[["orig_lib_paths"]])
                    })
            
    #1.7 Drop pre-existing .libPaths to avoid finding pkg in path without version match
        .libPaths('')
    

    #1.8 put package name in quotes if it is not an object and was not put in quotes
        pkg.catch <- try(typeof(pkg),silent=TRUE)
        if (as.character(class(pkg.catch))=="try-error") {
          pkg <- as.character(substitute(pkg))
          } 
          
    #1.9 Sandwich possible library() commands  
        pkg <- sandwich.library(pkg)  #utils.R function 34
   
        
    #1.10 Add groundhog.day to hogdays to alert of possible different days used in a snowball.conflict
        if (!is.null(.pkgenv[['hogdays']])) {
            .pkgenv[['hogdays']] <- unique(c(date, .pkgenv[['hogdays']]))
            } else {
            .pkgenv[['hogdays']]<- date
        
          }
    #1.11 how many cores? (totall -2 unless specified away from default of -1)
        if (cores == -1) {
          cores <- max(parallel::detectCores()-2,1)
          }   
#------------------------------------------------------------------------ 
    
#2 Early return if everything is already attached

  #2.1 Attached pkgs  
    attached.pkg_vrs <- get.attached()$pkg_vrs
    
  #2.2 Packages needed
        
      #Get vrs
        vrs    <- c()
        for (pkgk in pkg) {
        vrs <- c(vrs, get.version(pkgk, date))
        }
        
      #Get pkg_vrs
        pkg_vrs <- paste0(pkg,"_",vrs)
        
      #See if they are git
        pkg_is_git <- basename(pkg)!=pkg
        
        
      #If all packages are attached, and all packages are not git, return early
        if (all(pkg_vrs %in% attached.pkg_vrs & !pkg_is_git))
        { 
        message1("All requested packages are already attached")
        return(invisible(TRUE))
        }
     
#------------------------------------------------------------------------ 
    
#3 Get snowballs for all requested packages
  #Save snowballs individually as a list and also as a single big snowball.all
        
        k <- 0 
        snowball.list <- list()

     
        for (pkgk in pkg)
          {
          k <- k+1
          snowball.k  <- get.snowball(pkgk , date , include.suggests , force.install)
    
        #options
            if (force.source==TRUE)  snowball.k$from      = 'source'
            if (force.install==TRUE) snowball.k$installed = FALSE
       
        #Add to snowball.all
            if (k==1) snowball.all <- snowball.k
            if (k> 1) snowball.all <- rbind(snowball.all, snowball.k)
            
        #Add to list
            snowball.list[[k]] <- snowball.k
          
         } #End loop over pkgs
        
#------------------------------------------------------------
        
#4 Create libpathcs

  #4.1 Create all paths if they don't exist (so that they can be added to libpath in 3.5)
        for (j in 1:nrow(snowball.all))
        {
          dir.create(snowball.all$installation.path[j],recursive = TRUE, showWarnings = FALSE)
          
        }
        
  #4.2 Set libpaths for big snowball
        .libPaths(unique(snowball.all$installation.path))
        
        
        
#------------------------------------------------------------------------ 
            
#5 Install snowball 
      

    #5.1 Do we need to install source from background?
    #-----------------------------------------------------------------------------
    #Note: R will not let you install a package that is in use. For binaries
    #      pkgs are not formally installed, just unzipped, so we bypass this check
    #      but for source pkgs we need to install in background
    #-----------------------------------------------------------------------------
        
        # Any source package that needs install is loaded and thus needs background install?
            snowball.install.source <- snowball.all[snowball.all$from=='source' & snowball.all$installed==FALSE,]
            n.source.conflict       <- sum(snowball.install.source$pkg %in% get.active()$pkg)
        
    #5.2 BACKGROUND Install
        
        if (n.source.conflict > 0) 
         {
          
          #Save the snowball as an rds file
              arguments_path <- file.path(get.groundhog.folder(), paste0("temp/background_install_arguments.rds"))
              dir.create(dirname(arguments_path),showWarnings = FALSE,recursive=TRUE)
              arguments <- list(snowball=snowball.all, date=date, cores=cores)
              saveRDS(arguments , arguments_path,version=2 , compress=FALSE)
              
          #Execute snowball.install in background with system
              script_path <- system.file("background_scripts/backgroung_install.snowball.R", package = "groundhog")
              system(paste0("Rscript ",script_path , " " , arguments_path) )
              
            #Delete temp path
              unlink(arguments_path)
              
        } #End n conflict>0

    #5.2 FOREGROUND INSTALL
        if (n.source.conflict == 0)  {
             install.snowball(snowball.all,date, cores)        

          } 
       
  
#------------------------------------------------------------------------ 
   
            
#6 localize the snowballs
            
    #Drop base pkgs from snowball.all
      snowball.all<-snowball.all [!snowball.all$pkg %in% base_pkg(),]
        
    #localize
      localize.snowball(snowball.all)   #Note: source pacakges are already localized, but not binaries   
              

#------------------------------------------------------------------------ 

      
#7 Check conflict now that it is all installed 
    check.snowball.conflict(snowball.all, pkg.requested=pkg , force.install=force.install, ignore.deps=ignore.deps, date=date)

      
#------------------------------------------------------------------------ 
      
      
#8 Library all pkgs
      for (pkgk in pkg)  base.library(pkgk, character.only=TRUE)

      
#------------------------------------------------------------------------ 


#9 Verify each snowball, saving snowball .rds if successful  
      
  for (k in 1:length(snowball.list))
       {
         
    #8.1 Take one snowball
       snowball<-snowball.list[[k]]
         
    #8.2 Verified: TRUE or FALSE?
       verified <- verify.snowball.loaded(snowball, ignore.deps)  
       
       #Includes 'successfully attached' msg, see #verify.snowball.loaded.R
          
      
    	#8.3 Path to snowball
						snowball_dir <- paste0(get.groundhog.folder() , '/snowballs_v2' )
						snowball_file <- paste0(pkg[k] , "_" ,  gsub( "-", "_" , date) , '.rds')  
						snowball_path <- file.path(snowball_dir, snowball_file)
	
    #If TRUE 
       
        if (verified==TRUE) { 
     
    #8.4 Update  column `installed` in  snowball
          ip <- data.frame(utils::installed.packages(snowball$installation.path), stringsAsFactors=FALSE)
          snowball$installed <- (snowball$pkg %in% ip$Package | snowball$pkg %in% .pkgenv[['base_pkg']]) #if in packages or in base.packages
          
				   
		#8.5 Save snowball RDS 
							if (!file.exists(snowball_path)) {
						  saveRDS(snowball, snowball_path, version = 2, compress=FALSE)
						  } 	
		
		#8.6 add snowball to loaded by groundhog
					.pkgenv[['groundhog_loaded_pkgs']] <- 	c(.pkgenv[['groundhog_loaded_pkgs']] , snowball$pkg)
			
		#8.7 Update groundhog session (dataframe with everything loaded with groundhog in this R session
					update.groundhog.session(snowball)  #utils.R -  function #41

			 
     } #End if (verified==TRUE)              
     
         
         
  #8.7 If FALSE, delete snowball
      if (verified==FALSE) {
        if (file.exists(snowball_path)) file.remove(snowball_path)
      }    
         
         
         
  }#End of loop over elements of snowball.list
                 
  }  #End new.groundhog.folder()
  
  