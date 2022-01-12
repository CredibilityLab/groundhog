
  groundhog.library.single.remote <-  function(pkg, date,  quiet.install ,  include.suggests , 
    ignore.deps, force.source , force.install )
      { 
      #0 Note that Date and R validated in groundhog.library()
    
      #1 Process pkg-->usr, remote_id
          pkg_list<-make.pkg_list(pkg)
          usr <- pkg_list$usr
          remote_id <- pkg_list$remote_id
          pkg <- pkg_list$pkg
          remote_pkg_date <- paste0(pkg_list$remote_id ,"_",pkg,"_", gsub("-","_",date))
      
          
      #2 Check if conflict with already loaded remote package
            #Full identifier of pkg called: remote, usr, pkg, date. ('github::crsh/papaja_2021_10-01')
                  git_usr_pkg_date<-paste0(pkg_list$git_usr_pkg,"_",gsub("-","_",date))
      
                        
              #2.1 This same pkg_date loaded & attached     : skip 
              
                if (git_usr_pkg_date %in% .pkgenv[['remotes.attached']]) {
                      message1("groundhog says: the package '", pkg_list$usr_pkg, "', for '",date,"', is already attached.")
                      return(invisible(TRUE))
                }
                
              
               #2.2 This same pkg_date loaded onlyd     : attach it 
              
                if (git_usr_pkg_date %in% .pkgenv[['remotes.loaded']]) {
                      message1("groundhog says: the package ('", pkg_list$usr_pkg, "'), for ;",date,"', was loaded, now it is alsoattached.")
                      return(invisible(TRUE))
                }
               
              #2.3 Different pkg_date loaded                : give warning
                active <- get.active()
                  
                if (pkg %in% active$pkg) {
                
                  msg <- paste0('Groundhog says, warning!\n', 
                                'A version of the package "',pkg, '" is already loaded and it may or may not match \n', 
                                'the version as available on "',date,'". To ensure the version for that date is used \n', 
                                'you should restart the R session (CTRL/CMD-SHIFT F10 in R-Studio) and run this command,\n',
                                'again.')
                  message(msg)
                  return(invisible(TRUE))
                }
    
      #3 Get snowball
          snowball <- get.snowball.remote(pkg, date, remote_id, usr)
  
      #4 install snowball (ensures all packages in the snowball are installed)
          install.snowball(snowball, date)

      #5 Load snowball
           snowball.remotes <- load.snowball(snowball,ignore.deps)  
           
           #snowball.remotes is a subset of the snowball, containing the remote package, and a new column attached:TRUE/FALSE
           #used below to record which remote packages are aleady loaded
           
           #If loding produces an error, what follows will not be execute (exit in load.snowbal())
           #so corrupted snowballs should not be saved
           
     #6 if snowball was not already saved, save it now 
          
          #Path to snowball
            snowball_dir <- paste0(get.groundhog.folder() , '/snowballs/' , remote_id )
            snowball_file <- paste0(usr ,"_", pkg , "_" ,  gsub( "-", "_" , date) , '.rds')  
            snowball_path <- file.path(snowball_dir, snowball_file)
      
        if (!file.exists(snowball_path)) {
          saveRDS(snowball, snowball_path, version = 2)
        } #End if snowball exists
          
     #11 Add to pacakge vectors  attached pkgs 
          new.remotes_date<-paste0(snowball.remotes$from, "::" , snowball.remotes$usr, "/" , snowball.remotes$pkg,"_", gsub("-","_",date))
          new.remotes.attached <- new.remotes_date[snowball.remotes$attached==TRUE]
          new.remotes.loaded   <- new.remotes_date[snowball.remotes$attached==FALSE]

        
          #Save the attached
            if (is.null(.pkgenv[['remotes.attached']])) {
              .pkgenv[['remotes.attached']] <- new.remotes.attached
            } else {
               .pkgenv[['remotes.attached']] <- unique(c(new.remotes.attached , .pkgenv[['remotes_date.attached']]))
            }
          #Save the loaded
            if (is.null(.pkgenv[['remotes.loaded']])) {
              .pkgenv[['remotes.loaded']] <- new.remotes.loaded
            } else {
               .pkgenv[['remotes.loaded']] <- unique(c(new.remotes.loaded , .pkgenv[['remotes_date.loaded']]))
            }
            
      
          
          
  } #End of function


  
  
  