

#pkg: vector

#Checks if all pacakges in the vector are already attached, used by groundhog.library() to early return



all.already.attached<-function(pkg,date)
{
  # Count remote
    n.remote=sum(basename(pkg)!=pkg)
  
   #2 Non-remote requested pkg  
      if (n.remote==0)
      {
    
      #Attached pkgs  
        attached.pkg_vrs <- get.attached()$pkg_vrs
      
      #Packages requests 
      #Get vrs
          vrs    <- c()
          for (pkgk in pkg) {
            vrs <- c(vrs, get.version(pkgk, date))
          }
        
        #Get pkg_vrs
          pkg_vrs <- paste0(pkg,"_",vrs)
        
        #See if they are git
        
      #If all packages are attached, and all packages are not git, return early
        if (all(pkg_vrs %in% attached.pkg_vrs)) {
          message1("All requested packages are already attached")
          return(TRUE)
          }
       
      } #ENd if not-remote
     
        
    #3 Remote requested pkg  
      if (n.remote==1)  #cannot have more than 1 remote in 1 request
      {
        # Process pkg-->usr, remote_id
            pkg_list<-make.pkg_list(pkg)
            usr <- pkg_list$usr
            remote_id <- pkg_list$remote_id
            pkg <- pkg_list$pkg
            git <- pkg_list$remote_id

        #Full identifier of pkg called: remote, usr, pkg, date. ('github::crsh/papaja_2021_10-01')
            git_usr_pkg_date <- paste0(remote_id  , "_", usr, "_", pkg ,"_", gsub("-","_",date))
  
                      
        #This same pkg_date loaded & attached     : skip 
          if (git_usr_pkg_date %in% .pkgenv[['remotes.attached']]) {
              #message1("The package '", pkg_list$usr_pkg, "', for '",date,"', is already attached.")
              return(TRUE)
          }
      
      }  #End if remote 
    
    #4 If we are here, return FALSE
            return(FALSE)
    
}
