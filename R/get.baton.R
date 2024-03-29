
#This script contains two functions,
#1) Get baton.single extracts information about a remote package from its description file
#2) Get baton loops chasing all remotes found as dependencies until there are none left


#0 Auxiliary function that cleans fields of text
                  clean_text <- function(y) {
                  return(gsub("\\s*\\([^\\)]+\\)|\\n| ","",as.character(y))) #Kill the parentheses, \n, and space
                  }


#Function 1 - Process single remote, adding results to baton

     get.baton.single <- function (pkg, date, remote_id, usr, include.suggests,
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
              validate.clone_date(pkg, date, remote_id, usr)
              
            #3.2 Load git2r
                load.pkg_utility('git2r' , date)

            #3.3 read the raw description file
                #  https://github.com/ropensci/git2r/issues/255   --  reading description with git2r
                
                dir_path <- paste0(get.groundhog.folder() , "/git_clones/" , remote_id)
                clone_path <- paste0(dir_path , "/", usr, "_" ,pkg)
                description_raw <- git2r::content(git2r::tree(git2r::lookup(clone_path, sha))["DESCRIPTION"])
                description_raw <- gsub("[\r\n]", "", description_raw) #drop /r and /n from the text file
                  #note:
                  #git2r is in the suggests in DESCRIPTION , but preferred usage is to load it using groundhog() 
                  #to avoid version conflicts for users who are using git2r elsewhere in their session
                
            #3.4  save contents in temporary file
                tmp<-file.path(get.groundhog.folder(), 'temp', paste0('description_',usr,'_',pkg,"_",sha))
                dir.create(dirname(tmp),showWarnings = FALSE,recursive = TRUE)
                writeLines(description_raw, tmp)
                
            #3.5 read the description file into data.frame with read.dcf
                 description_df <- data.frame(read.dcf(tmp), stringsAsFactors=FALSE)
            #3.6 If more than one row assume it is not a dcf file
                 if (nrow(description_df) > 1)
                 {
                   
                  description_df <- read.desc2(tmp)   #function 29 in utils.R
                   
                 }
                 
                 
            #3.7   delete temporary file
                 unlink(tmp)
          
         
           
            #3.8 Exit if invalid description file
                 if (nrow(description_df)!=1) {
                   msg = paste0("The DESCRIPTION file for package '", pkg , "' is invalidly formatted. Installation aborted.")
                   gstop(msg)
                  }
                 
       #4) Find remotes dependencies in the DESCRIPTION of this remote package 
                new.remotes <- as.character(description_df$Remotes)
                # new.remotes <- description_df$Remotes

          #4.1 Drop suggested remotes if include.suggests=FALSE
                if (include.suggests==FALSE && !is.null(new.remotes) && length(new.remotes) > 0 && regexpr(',',new.remotes)>1) 
                {
                #Turn string to vector with all remotes
                  new.remotes.vector <- unlist(strsplit(new.remotes,","))
                  

                #Split pkg_usr because suggest column only has pkg, not usr
                  new.remotes.vector_pkg <- sapply(strsplit(new.remotes.vector,"/") , "[" ,2)
                  new.remotes.vector_usr <- sapply(strsplit(new.remotes.vector,"/") , "[" ,1)
                  
                #Clean suggest as vector
                  suggest_temp <- description_df$Suggests
                  suggest_temp <- clean_text(suggest_temp)
                  suggest.vector <-unlist(strsplit(suggest_temp,","))
                  
                #Drop remotes that are suggested 
                  keep <- !new.remotes.vector_pkg %in% suggest.vector
                  
                  #If no remotes survive, make empty vector
                    if (sum(keep)==0) new.remotes=c()
                  
                  #If some survive, recreate the cell with the packages that are not suggested
                    if (sum(keep)>0) {
                      new.remotes <- paste0(new.remotes.vector_usr[keep] , "/" , new.remotes.vector_pkg[keep], collapse=',')
                      }#End if sum(keep)>0
                  
                }# End if include.suggests==FALSE
                
                
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
              row.toc$Depends<-clean_text(row.toc$Depends)
              row.toc$Imports<-clean_text(row.toc$Imports)
              row.toc$Suggests<-clean_text(row.toc$Suggests)
              row.toc$LinkingTo<-clean_text(row.toc$LinkingTo)

            
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
          
        #11 Some remote DESCRIPTION files mess up their own names:
            #baton$rows.toc$Package=pkg
             
      return(baton)  
     }
     
#----------------------------------------------------------------------------------
     
  #Function 2 - Loop over the pending remotes   
     get.baton <- function(pkg, date, remote_id, usr,include.suggests)
    {
          
      #1 Start baton with  target  remote
        baton <- get.baton.single(pkg , date, remote_id, usr,include.suggests)
      
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
            baton <- get.baton.single(pkgk , date, remote_id, usr=usrk, baton=baton,include.suggests = include.suggests )
        
         #3.4 Fuse if something goes wrong and loop keeps going
            if (k>200) break
        }
        
        return(baton)
        
        
    }  #End of function
        
    