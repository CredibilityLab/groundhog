  groundhog.library.single.remote <-  function(pkg, date,  quiet.install ,  include.suggests , ignore.deps, force.source , force.install )
  { 
    #0 Note that Date and R validated in groundhog.library()
      #Do not accept force.source or force.install
          if (force.source==TRUE) {
            message("When installing remote packages, like '",pkg, "' the 'force.source' option may not be set to TRUE")
            exit()
          }
    
       if (force.install==TRUE) {
            message("When installing remote packages, like '",pkg, "' the 'force.install' option may not be set to TRUE")
            exit()
          }
  
    #1 Process pkg-->usr, remote_id
        pkg_list<-make.pkg_list(pkg)
        usr <- pkg_list$usr
        remote_id <- pkg_list$remote_id
        pkg <- pkg_list$pkg
        git <- pkg_list$remote_id
        
    #2 Check if conflict with already loaded remote package (unlike CRAN pkg_vrs here we must use the *date*, version number is not enough)
        
          #Full identifier of pkg called: remote, usr, pkg, date. ('github::crsh/papaja_2021_10-01')
                git_usr_pkg_date <- paste0(git  , "_", usr, "_", pkg ,"_", gsub("-","_",date))
    
                      
            #2.1 This same pkg_date loaded & attached     : skip 
              if (git_usr_pkg_date %in% .pkgenv[['remotes.attached']]) {
                    message1("groundhog says: the package '", pkg_list$usr_pkg, "', for '",date,"', is already attached.")
                    return(TRUE)
              }
             
            #2.3 This package is attached or loaded, and does not match one that was loaded remotely
              active <- get.active()


    #3 Get snowball
        snowball <- get.snowball.remote(pkg, date, remote_id, usr,include.suggests=include.suggests,force.install=force.install)
        
        
      #3.1 Check if more than 60 days since date when installing 
        if (snowball$installed[snowball$pkg==pkg]==FALSE)
        {
          today <- Sys.Date()
          days <- as.numeric(round(difftime(today,date,units='days'),0))
          if (days<60)
          {
            
          #Check cookie, when was person last warned
            mins_since_warned<- get.minutes.since.cookie('60_days_remote')
            
          #Show warning if more than 30 minutes
            if (mins_since_warned>30)
            {
            #Save cookie with time when warning is shown
                save.cookie('60_days_remote')
            
            #Draft message
              msg <- paste0(
                    "For enhanced reproducibility, it is recommended that for GitHub and Gitlab packages, ",
                    "a date at least 2 months in the past is used for loading packages (so, before '", today-60 ,"').",
                    "The reason for this recommendation is that changes ('commits') on Git can be retroactive, ",
                    "that is, be saved with a *past* timestamp. This time inconsitency is much less likely, ",
                    "but not impossible, beyond 60 days. \n ",
                    "   1) Type 'stop' to use a different date \n ",
                    "   2) Type 'ignore' to continue with '", date,"'.")
          
            #Show
              answer<-infinite.prompt(format.msg(msg),c('stop','ignore'))
            
            #Process answers
              if (answer=='stop') exit()
              if (answer=='ignore') message1("\nOK. Will not show this warning again during next 30 minutes.\n")
            
          
                } #End since 30 minutes since last warned
              } #End if 60 days
            } #End if not installed
        
        
        #3.2 Force source 
          if (force.source==TRUE) {
            snowball$from <- ifelse(snowball$sha!='', 'source', snowball$from)
          }      
          

    #4 Check snowball conflict
        check.snowball.conflict(snowball, force.install,ignore.deps,date)  

       for (pathk in snowball$installation.path) {
          if (!file.exists(pathk))  {
            dir.create(pathk,recursive=TRUE,showWarnings = FALSE)
          }
        }
    
    #4.1 libpath entire snowball
      .libPaths(c(snowball$installation.path, .libPaths()))
    

    #4.2 Create remotes_df
        if (is.null(.pkgenv[['remotes_df']])) {
           .pkgenv[['remotes_df']] <- data.frame(pkg=character(),date=character(),attached=c() , stringsAsFactors = FALSE)
            }
        
    #5 install snowball (install will only happen if needed, this function also adds to  .libPath())
        install.snowball(snowball, date,recycle.files=TRUE)

    
    #6 Attach pkg

        #Turn off warnings
          warn0 <- getOption("warn")
          options(warn = -1)
          
        #library()
          base.library(pkg, character.only=TRUE) #utils. function 26
          
        #Options back
          options(warn = warn0)


      
   #7 If verified, save snowball
      verified <- verify.snowball.loaded(snowball,ignore.deps)  

          
          
      if (verified==TRUE) 
      { 
        
        #Reminder that version is not enough
         message1( "(version current on '", remote_id , "' on '", date,"')"  )
         
        #Path to snowball
          snowball_dir <- paste0(get.groundhog.folder() , '/snowballs/' , remote_id )
          snowball_file <- paste0(usr ,"_", pkg , "_" ,  gsub( "-", "_" , date) , '.rds')  
          snowball_path <- file.path(snowball_dir, snowball_file)
          
        #Update  what is installed in the snowball
          ip<-data.frame(utils::installed.packages(snowball$installation.path), stringsAsFactors=FALSE)
          snowball$installed <- snowball$pkg %in% ip$Package
          snowball$installed <- TRUE
          saveRDS(snowball, snowball_path, version = 2)
         
             
   #8 Add package_date to .envpkg vector of loaded remotes to prevent conflicts not idnetified by vrs 
      
        #8.1 Snowball subset for remote rows
          snowball.remotes <- subset(snowball,snowball$sha!='')
        
        #8.2 Attached remotes
          attached.list <- utils::sessionInfo()$otherPkgs # pkgs in attached
          attached.pkg  <- names(attached.list)
     
       
        #8.4 active
            active <- get.active()                          

        
        #8.6 Classify remotes as attached or loaded
            loaded   <- snowball.remotes$pkg %in% active$pkg
            attached <- snowball.remotes$pkg %in% attached.pkg 
            loaded <- ifelse(attached==TRUE, FALSE, loaded)     #if attached, don't count as loaded
            
        #8.7 Combine pkg with remote, and date
            new.remotes <- paste0(snowball.remotes$from , "_" , snowball.remotes$usr, "_", snowball.remotes$pkg , "_" , gsub("-","_",date))
         
            
        #Store full pkg reference to quickly load if called again   
            #Save the attached
              if (is.null(.pkgenv[['remotes.attached']]))  .pkgenv[['remotes.attached']] <-   new.remotes[attached]
              if (!is.null(.pkgenv[['remotes.attached']])) .pkgenv[['remotes.attached']] <- c(new.remotes[attached] , .pkgenv[['remotes.attached']])
          
           #Save the loaded
              if (is.null(.pkgenv[['remotes.loaded']]))  .pkgenv[['remotes.loaded']]  <-   new.remotes[loaded]
              if (!is.null(.pkgenv[['remotes.loaded']])) .pkgenv[['remotes.loaded']] <- c(new.remotes[loaded] , .pkgenv[['remotes.loaded']])
  
        #8.8 Store remote packages that have been loaded, to identify potential conflicts with CRAN dependencies of other packages
              if (is.null(.pkgenv[['remote_packages']]))   .pkgenv[['remote_packages']]  <- snowball.remotes$pkg
              if (!is.null(.pkgenv[['remote_packages']]))  .pkgenv[['remote_packages']]  <- c(snowball.remotes$pkg,.pkgenv[['remote_packages']])

        #8.9 update remotes.df
            rdf <- .pkgenv[['remotes_df']]                                           #localize the .pkgenv[]
            rdf$attached <- ifelse(rdf$pkg %in% attached.pkg, TRUE, rdf$attached)    #turn attach to true if appropriate
            .pkgenv[['remotes_df']] <- rdf                                           #unlocalize
            
               
        
				#8.10 localize everything that's not base
					snowball.no_base <- snowball[!snowball$pkg %in% base_pkg(),]
					localize.snowball(snowball.no_base)
					
					
			  #8.11 Update groundhog session (dataframe with everything loaded with groundhog in this R session)
						update.groundhog.session(snowball)  #utils.R -  function #41

      }#End if verified is true
        
        
    #9 If something failed, delete all saved information that may cause problem to be unfixable (snowball, catalog, sha_time)
        if (verified==FALSE)
        {
        #9.1 Snowball
           #Path to snowball
            snowball_dir <- paste0(get.groundhog.folder() , '/snowballs/' , remote_id )
            snowball_file <- paste0(usr ,"_", pkg , "_" ,  gsub( "-", "_" , date) , '.rds')  
            snowball_path <- file.path(snowball_dir, snowball_file)
          #Delete
            if (file.exists(snowball_path)) unlink(snowball_path)

            
        #9.2 Catalog
            #path
              clone_catalog_path <- file.path(get.groundhog.folder() , 'clone_catalog.rds')
              
            #read it
              catalog <- readRDS(clone_catalog_path)
                      
            #Drop all rows with remote packages from this call
              catalog.use_pkg  <- paste0(catalog$usr , "_" , catalog$pkg)                   #[1] usr_pkg in the catalog
              snowball.usr_pkg <- paste0(snowball.remotes$usr, "_", snowball.remotes$pkg)   #[2] usr_pkg in this snowball (subset of remots)
              catalog <- catalog[!catalog.use_pkg %in% snowball.usr_pkg, ]                  #Only keep [1] which are not in [2]  
         
        #9.3 Sha time
             sha_path <- paste0(get.groundhog.folder(),"/sha_times/", remote_id,"_" ,usr,"_",pkg,".rds")
             if (file.exists(sha_path)) unlink(sha_path)
              
              
          
        } #END if validation did not work
        
        
} #End of function


  
  
  