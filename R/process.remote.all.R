
#This function loops over process.remote.single so that ifa  remote has remote dependencies, all those
#dependencies are processed as well

 process.remote.all <- function(git_usr_pkg, date)
    {
   
      #1 Standardize the name to be remot::usr_pkg
        git_usr_pkg <- standardize.git_usr_pkg(git_usr_pkg)
        
      #2 If the wanted baton exists locally, get it
        
        #2.1 Baton directory
          baton_dir <- paste0(get.groundhog.folder(), "/batons" )
          if (!file.exists(baton_dir)) dir.create(baton_dir, recursive=TRUE, showWarnings = FALSE)
   
        #2.2 This baton path
          baton_path <- paste0(baton_dir , "/",  gsub("/|::", "_", git_usr_pkg), "_" ,gsub("-", "_", date) , ".rds")
      
        #2.3 Load and return if it exists
          if (file.exists(baton_path)) {
            baton <- readRDS(baton_path)
            return(baton)
            }
          

          
    #Continue below if not yet saved locally
      
      #3 Generate files for the target  remote
        baton <- process.remote.single(git_usr_pkg , date)
      
      #4 Loop creating additional entries onto the same baton for remote dependencies
        k=1
        while (length(baton$remotes.pending) >0)
          {
          #3.1 add counter
            k=k+1
            
          #3.2 Next remote package to create snowball for
            git_usr_pkg_k <- as.character(baton$remotes.pending[1])
            #git_usr_pkg_k <- baton$remotes.pending[1]
            
          #3.3 Make the new snowball and pass on the baton
            baton <- process.remote.single(git_usr_pkg_k , date, baton=baton)
        
         #3.4 Fuse if something goes wrong and loop keeps going
            if (k>1000) break
        }
        
        return(baton)
        
        
    }  #End of function
        
 

#Alias for this function 
  get.baton <- process.remote.all 