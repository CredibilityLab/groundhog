
#This function loops over process.remote.single so that ifa  remote has remote dependencies, all those
#dependencies are processed as well

 process.remote.all <- function(git_usr_pkg, groundhog.day)
    {
      #1 local vars
        pkg <- basename(git_usr_pkg)
      
      #2 Generate files for the target  remote
        baton <- process.remote.single(git_usr_pkg , groundhog.day)
      
      #3 Loop creating additional rows in thos files for dependencies tha are remotes too
        k=1
        while (length(baton$remotes.pending) >0)
          {
          #3.1 add counter
            k=k+1
            
          #3.2 Next remote package to create snowball for
            git_usr_pkg_k <- as.character(baton$remotes.pending[1])
            
          #3.3 Make the new snowball and pass on the baton
            baton <- process.remote.single(git_usr_pkg_k , groundhog.day, baton=baton)
        
         #3.4 Fuse if something goes wrong and loop keeps going
            if (k>1000) break
        }
        
        return(baton)
    }
        