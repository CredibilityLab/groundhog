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
                git_usr_pkg_date<-paste0(git  , "_", usr, "_", pkg ,"_", gsub("-","_",date))
    
                      
            #2.1 This same pkg_date loaded & attached     : skip 
              if (git_usr_pkg_date %in% .pkgenv[['remotes.attached']]) {
                    message1("groundhog says: the package '", pkg_list$usr_pkg, "', for '",date,"', is already attached.")
                    return(TRUE)
              }
             
            #2.3 This package is attached or loaded, and does not match one that was loaded remotely
              active <- get.active()
              if (pkg %in% active$pkg  && (!git_usr_pkg_date %in% .pkgenv[['remotes.loaded']])) {
              
                msg <- paste0("|IMPORTANT\n",
                              "|    A version of the package '",pkg, "' is already loaded and it may or may not match \n", 
                              "|    the version available on '",remote_id, "' on '",date,"'.\n",
                              "|    Restart the R session (CTRL/CMD-SHIFT F10 in R-Studio) and try again\n",
                              "|    If the problem persist visit https://groundhogR.com/conflict for further suggestions\n",
                              "|    Type 'OK' to confirm you have read this message")
                infinite.prompt(msg,'ok')
                exit()
              }

    #3 Get snowball
        snowball <- get.snowball.remote(pkg, date, remote_id, usr,include.suggests=include.suggests,force.install=force.install)

        #3.1 Force source 
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
    
      
    #4.2 Create/update remotes.df
        if (is.null(.pkgenv[['remotes_df']])) {
           .pkgenv[['remotes_df']] <- data.frame(pkg=character(),date=character(),attached=c() , stringsAsFactors = FALSE)
            }
        
    #5 install snowball (install will only happen if needed, this function also adds to  .libPath())
        install.snowball(snowball, date,recycle.files=TRUE)

        
        
    #6 Attach pkg
        base.library(pkg, character.only=TRUE) #utils. function 26

    #7 verify snowball loaded
        verified <- verify.snowball.loaded(snowball,ignore.deps)  
 
      
   #7 If verified, save snowball
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


  
  
  