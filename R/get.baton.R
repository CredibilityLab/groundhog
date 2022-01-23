
#This script contains two functions,
#1) Get baton.single extracts information about a remote package from its description file
#2) Get baton loops chasing all remotes found as dependencies untli there are none left



#Function 1 - Process single remote, adding results to baton

     get.baton.single <- function (pkg, date, remote_id, usr, 
                                  baton=list(remotes.pending=c(), install.from=c(), sha=c(), installation_path =NULL, rows.toc=NULL, usr=c())
                                  )
      {
       
      #0 Add remote_id and usr to baton
       baton$usr <- c(baton$usr, usr)
       baton$install.from <- c(baton$install.from, remote_id) 
              
      #1. Get sha & append to baton
        sha <- get.sha(pkg,date,remote_id ,usr)
        baton$sha <- c(baton$sha , sha)    

        
      #2 Get installation path and add to baton
        installation_path.k <- get.installation_path_remote(pkg , date, remote_id, usr)  #see remote_functions.R
         baton$installation_path <- c(baton$installation_path,installation_path.k )
         
        
      #3 Read description in local clone
            #3.1 Create/Update clone if needed 
              #update.clone(pkg , groundhog.day , remote_id, usr)
              validate.clone_date(pkg, date, remote_id, usr)
              
            #3.2 Load git2r
                #  https://github.com/ropensci/git2r/issues/255   --  reading description with git2r
                load.pkg_utility('git2r' , groundhog.day)

            #3.3 read the raw description file
                dir_path <- paste0(get.groundhog.folder() , "/git_clones/" , remote_id)
                clone_path <- paste0(dir_path , "/", usr, "_" ,pkg)
        				gitr2_lookup <- .pkgenv[['git2r_lookup']]
        				gitr2_content<- .pkgenv[['git2r_content']]
        				gitr2_tree  <-  .pkgenv[['git2r_tree']]

                #description_raw <-git2r_content(git2r_tree(git2r_lookup(clone_path, sha))["DESCRIPTION"])
                description_raw <-content(tree(lookup(clone_path, sha))["DESCRIPTION"])
                
            #3.4  save contents in temporary file
                tmp<-file.path(get.groundhog.folder(), 'temp', paste0('description_',usr,'_',pkg,"_",sha))
                writeLines(description_raw, tmp)
            
            #3.5 read the description file into data.frame with read.dcf
                 description_df <- data.frame(read.dcf(tmp))
            
            #3.6   delete temporary file
                 unlink(tmp)
          
       #4) Find remotes dependencies in the DESCRIPTION of this remote package 
            new.remotes <- description_df$Remotes
            
       #5) Produce row of toc (for cran.toc)
            #Ensure all columns needed for toc are in the description_df
              #var names in each data.frame
                vars_in_toc <- c("Package","Version","Imports","Depends","Suggests","LinkingTo")
                vars_in_des <- names(description_df)
                
              #if a variable is not in description, make it ''
                description_df[setdiff(vars_in_toc,vars_in_des)] <- ''
                
              #Keep only vars we need
                row.toc <-description_df[, vars_in_toc]          #keep only variables we will use
      
                  
        #6) Add published in 1970 to ensure it is selected when building snowball 
            #(note, all other packages with same name will be dropped from when making the snowball)
              row.toc$Published <- as.Date('1970-01-01')
        
        #7 Remove symbols from the dependencies text          
                #7.1 Function that cleans
                  clean_text <- function(y) {
                  return(gsub("\\s*\\([^\\)]+\\)|\\n| ","",as.character(y))) #Kill the parenthense, \n, and space
                  }
                
                #7.2 Apply to dependencies columns
                  row.toc[,c('Depends','Imports','Suggests','LinkingTo')] <- clean_text( row.toc[,c('Depends','Imports','Suggests','LinkingTo')])
 
        
                      
        #8 append to rows if it exists, otherwise rows=row
              if (!is.null(baton$rows.toc))  {
                baton$rows.toc <- rbind(baton$rows.toc, row.toc)
              } else {
                if ( is.null(baton$rows.toc))  baton$rows.toc <- row.toc
              }
      
        #9 Update pending
        #New remotes
          if (!is.null(new.remotes)) {
            baton$remotes.pending <- c(baton$remotes.pending , new.remotes)
            }
          
        #10 Remove the remote we just installed
            usr_pkg <- paste0(usr,"/",pkg)  #it is entered as usr/pkg so create that value to look up
            baton$remotes.pending <-baton$remotes.pending [!baton$remotes.pending %in% usr_pkg] 
      
             
      return(baton)  
     }
     
#----------------------------------------------------------------------------------
     
  #Function 2 - Loop over the pending remotes   
     get.baton <- function(pkg, date, remote_id, usr)
    {
          
      #1 Start baton with  target  remote
        baton <- get.baton.single(pkg , date, remote_id, usr)
      
      #4 Loop creating additional entries onto the same baton for remote dependencies
        k <- 0
        while (length(baton$remotes.pending) >0)
          {
            
          #3.1 add counter
            k=k+1
            
          #3.2 Next remote package to create snowball for
            pkgk  <- basename(baton$remotes.pending[1])
            usrk <- dirname(baton$remotes.pending[1])
            
          #3.3 Make the new snowball and pass on the baton
            baton <- get.baton.single(pkgk , date, remote_id, usr=usrk, baton=baton)
        
         #3.4 Fuse if something goes wrong and loop keeps going
            if (k>200) break
        }
        
        return(baton)
        
        
    }  #End of function
        
    