  groundhog.library.single.remote <-  function(pkg, date,  quiet.install ,  include.suggests , ignore.deps, force.source , force.install )
  { 
    #0 Note that Date and R validated in groundhog.library()
  
    #1 Process pkg-->usr, remote_id
        pkg_list<-make.pkg_list(pkg)
        usr <- pkg_list$usr
        remote_id <- pkg_list$remote_id
        pkg <- pkg_list$pkg
        git <- pkg_list$remote_id
        
    #2 Check if conflict with already loaded remote package (unlike CRAN pkg_vrs here we must use the *date*)
        
          #Full identifier of pkg called: remote, usr, pkg, date. ('github::crsh/papaja_2021_10-01')
                git_usr_pkg_date<-paste0(git  , "_", usr, "_", pkg ,"_", gsub("-","_",date))
    
                      
            #2.1 This same pkg_date loaded & attached     : skip 
              if (git_usr_pkg_date %in% .pkgenv[['remotes.attached']]) {
                    message1("groundhog says: the package '", pkg_list$usr_pkg, "', for '",date,"', is already attached.")
                    return(TRUE)
              }
              
            
             #2.2 This same pkg_date loaded only     : attach it 
            
              if (git_usr_pkg_date %in% .pkgenv[['remotes.loaded']]) {
                    library(pkg)   
                   #Verify it was attacged
                    if (!pkg %in% .packages()) message("Failed to load '",pkg,"'")
                
                    message1("groundhog says: the package ('", pkg_list$usr_pkg, "'), for ;",date,"', was loaded, now it is also attached.")
                    return(TRUE)
              }
             
            #2.3 Different pkg_date loaded              : stop
              active <- get.active()
                
              if (pkg %in% active$pkg) {
              
                msg <- paste0('Groundhog says, warning!\n', 
                              'A version of the package "',pkg, '" is already loaded and it may or may not match \n', 
                              'the version as available on "',date,'". To ensure the version for that date is used \n', 
                              'you should restart the R session (CTRL/CMD-SHIFT F10 in R-Studio) and run this command,\n',
                              'again.')
                message(msg)
                exit()
              }
  
    #3 Get snowball
        snowball <- get.snowball.remote(pkg, date, remote_id, usr,include.suggests=include.suggests)

      #3.1 Force source 
        if (force.source==TRUE) {
          snowball$from <- ifelse(snowball$sha!='', 'source', snowball$from)
        }      
        
      #3.2 Force install
        if (force.install==TRUE) snowball$installed=FALSE


    #4 Check snowball conflict
        check.snowball.conflict(snowball, force.install,ignore.deps,date)  

    #4 install snowball (install will only happen if needed, this function also adds to  .libPath())
        install.snowball(snowball, date)

    #5 Attach pkg
        library(pkg, character.only = TRUE)
    
    #6 verify snowball loaded
        verified <- verify.snowball.loaded(snowball,ignore.deps)  
 
      
   #7 If verified, save snowball
      if (verified==TRUE) 
      { 
        
        #Reminder that verison is not enough
         message1( "(version current on '", remote_id , "' on '", date,"')"  )
         
        #Path to snowball
          snowball_dir <- paste0(get.groundhog.folder() , '/snowballs/' , remote_id )
          snowball_file <- paste0(usr ,"_", pkg , "_" ,  gsub( "-", "_" , date) , '.rds')  
          snowball_path <- file.path(snowball_dir, snowball_file)
          snowball$installed <- TRUE
          saveRDS(snowball, snowball_path, version = 2)
          
         
             
   #8 Add package_date to .envpkg vector of loaded remotes to prevent conflicts not idnetified by vrs 
      
        #8.1 Snowball subset for remote rows
          snowball.remotes <- subset(snowball,sha!='')
        
        #8.2 Attached remotes
          attached.list <- utils::sessionInfo()$otherPkgs # pkgs in attached
          attached.pkg  <-names(attached.list)
          
        #8.3 Available remotes
            #Get subset of paths in libPath which are from groundhog 
              groundhog_libpaths <- get.groundhog_libpaths()
               
            #See which packages are installed there 
              ip <- data.frame(utils::installed.packages(groundhog_libpaths))
            
            #Combine pkg_vrs
             libs.pkg <- ip$Package
       
        #8.4 active
            active <- get.active()                          
  
       #8.5 available is active+lib
           available.pkg <- c(libs.pkg, active$pkg)
        
        #8.6 Classify remotes as attached or available
            loaded   <- snowball.remotes$pkg %in% available.pkg
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

    
      }#End if verified
        
} #End of function


  
  
  