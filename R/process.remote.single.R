
#Obtain information necessary to install a single remote package

    process.remote.single <-  function(git_usr_pkg , groundhog.day,   baton=list(remotes.pending=c(), install.from=c(), sha=c(), remote.pkg_lib=NULL, rows.toc=NULL, usr=c()))
    {
    #This function gets a row for toc file for a package, and passes on its obtained arguments to the next iteration
         
       #0 Local vars
          remote_id <- get.remote_id(git_usr_pkg)
          usr_pkg   <- get.usr_pkg(git_usr_pkg)
          pkg       <- basename(usr_pkg)
          usr       <- dirname(usr_pkg)
   
        #0.5 add usr to batton
          baton$usr <- c(baton$usr, usr)
          
       #1 get sha and add to baton
          sha <- get.sha(git_usr_pkg , groundhog.day)  #this sha
          baton$sha<-c(baton$sha , sha)                #all sha
            
          
       #2 Get path to lib
          lib_remote <- get.lib_remote(git_usr_pkg , sha)
      
       #3 Read the description file from the server
          #save contents in temporary file
            tmp<-file.path(get.groundhog.folder(), 'temp', paste0('description_',usr,'_',pkg,"_",sha))
            if (remote_id=='github') writeChar(remotes:::github_DESCRIPTION(usr, pkg, ref=sha),tmp)
            if (remote_id=='gitlab') writeChar(remotes:::gitlab_DESCRIPTION(usr, pkg, ref=sha),tmp)
          
          #read the description file into data.frame
            description_df <- data.frame(read.dcf(tmp))
          
          #delete temporary file
            unlink(tmp)
          
       #4) Find remotes dependencies in the DESCRIPTION of this remote package 
            new.remotes <- description_df$Remotes
              
       #5) Turn description file into toc.row (like the rows in cran.toc.rds)
            #Ensure all columns needed for toc are in teh description_df
              #var names in each dataframe
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
          
    #8 Same with location row, create dataframe or add to it
         remote.pkg_lib.k <- data.frame(remote.pkg=pkg, lib = lib_remote)
         if (is.null(baton$remote.pkg_lib)) {
                baton$remote.pkg_lib  <- remote.pkg_lib.k
                } else {
                baton$remote.pkg_lib  <- rbind(baton$remote.pkg_lib, remote.pkg_lib.k)
                }
         
    #9 Update pending
        #New remotes
          baton$remotes.pending <- c(baton$remotes.pending , new.remotes)
        
        #Remove the remote we just installed
          baton$remotes.pending <-baton$remotes.pending [!baton$remotes.pending %in% git_usr_pkg]
      
    #10 Add remote location of this package
          baton$install.from <- c(baton$install.from, remote_id)
          
    #11 Baton with list of the three files being updated
        return(baton)  
    
  }
  