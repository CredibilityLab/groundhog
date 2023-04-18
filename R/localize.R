#Takes a snowball, lends packages to local library, and takes packages from local library
#either back to groundhog library or to a backup library (for pkgs not installed with groundhog originally)
#-------------------------------------------------------------------------------------------------------------
#1 Early return
#2 Save restore point
#3


  localize.snowball <- function(snowball)
  {
    #0 Drop duplicates
    snowball<-snowball[!duplicated(snowball$pkg_vrs),]
    
    #1 Early return if empty snowball
      if (nrow(snowball)==0) return(TRUE)
    
    #2 Restore point: save dataframe with installed packages with today's date if not yet saved, for possible restore later

      restore_path <- paste0(get.groundhog.folder(),"/restore_points/", get.r.majmin(), "/",Sys.Date(),".rds")
      if (!file.exists(restore_path)) {
        
        #Create directory for IP  
          dir.create(dirname(restore_path),showWarnings = FALSE,recursive = TRUE)
        
        #Get IP
          ip.local <- get.ip('local')  #utils #59
          
        #Drop base pkgs
          ip.local <- ip.local[!ip.local$Package %in% base_pkg(), ]
      
        #Drop possible duplicates (if multiple paths exists with the same package, we will work with the first one, and replace that one)
           ip.local<- ip.local[! duplicated( ip.local$Package),]

        #Save it
          saveRDS(ip.local , restore_path)
          
        #Add to restore points in environment
          .available.restore.points <<- get.restore.points() #utils #55, loaded in zzz.R, now we add the new date

      } #End of #2
  
        
  #3  Ensure $sha exists in snowball (when we create a non-remote snowball the column sha is not present)
        if (!'sha' %in% names(snowball)) snowball$sha <- ''

      
  #4. Installed.packages: local, backup and groundhog
      
          #4.1 installed.packages #utils #59
            ip.local     <- get.ip('local')     # .libPaths[1]
            ip.groundhog <- get.ip('groundhog') # in all of groundog
            ip.backup    <- get.ip('backup')    # pks removed from local and not belonging to groundhog
            loans        <- get.loans()         #package lent already from groundhog to personal library[1] (#utils #60)

              
            #NOTE on MD5 vs 'pvs' to identify packages:
                #ip <-> snowballs, with pvs (pkg_vrs_sha)  
                #ip <-> loans, with MD5
                
                  #same pkg_vrs but different commits, and remotes pkgs from CRAN pkgs with the same pkg_vrs (rio from CRAN vs rio from github)
                  #(sha is "" for pkgs originally on CRAN) 
                
            #4.2 if sha is NA, make it "", plays more nicely with paste() functions
                    snowball$sha[is.na(snowball$sha)] <-''
                    
            #4.3 pvs for snowball and loans()
                    snowball.pvs <- ifelse(snowball$sha=="",  snowball$pkg_vrs,  paste0(snowball$pkg_vrs , "@" , snowball$sha))
                    loans.pvs    <- ifelse(loans$sha=="",     loans$pkg_vrs,     paste0(loans$pkg_vrs    , "@" , loans$sha)) 
                    
            #4.4 Subset of the snowball that we have borrowed
                    borrowed <- snowball.pvs %in% loans.pvs
                  
            #4.5 If all of them are, nothing left to do, done localizing
                if (all(borrowed)) return(invisible(TRUE))
    
            #4.6 How many will be lent 
              n.lend <- sum(!borrowed)

    #-------------------------------------------------------------- 
              
              
    #5 Purge: pkgs to remove from local
    
      #5.1 Pkgs in snowball, but the version of it in the local library  did not originate in a groundhog installation
      # we know their pvs does not match (see #4.4 above, because they are !borrowed)
                
          ip.purge <- ip.local[(ip.local$Package %in% snowball$pkg[!borrowed]),]
          
          
      #5.2 Process purge, deleting or returning to groundhog depending on origin of pkg in local library now
          
           purge.local(ip.purge , loans) #see Function 1 in interlibrary.functions.R
           
           #This is a separate function because it is used also with restore.library()
           #so we remove pkgs from local when installing new ones and when restoring library.
           
    #-------------------------------------------------------------- 
  
             
    #6 Borrow
    #6.1 Read loans again (it was probably update in #5 in function `purge.local`, see #2.5 in that script, save.loans() after making returns)
           
    #Find pkgs that need to come to local library: new loans
      
      if (n.lend>0)
      {
        snowball.lend <- snowball[!borrowed,]
        
      #From groundhog to local
        local.library <- .pkgenv[["orig_lib_paths"]][1]
        from.groundhog_to_local <- paste0(snowball.lend$installation.path, "/", snowball.lend$pkg)
        to.groundhog_to_local   <- paste0(local.library,"/",snowball.lend$pkg)
      
        
       #As precaution, delete any destination folder k=1
        for (fk in to.groundhog_to_local)
          {
          if (file.exists(fk)) unlink(fk,recursive=TRUE)
          }

        outcome.loans <- c()
        for (k in 1:length(from.groundhog_to_local))
        {
        outcome.loans[k] <- file.rename.robust(from=from.groundhog_to_local[k] , to=to.groundhog_to_local[k])
        }
        

      #add to loans
        #vector with path to all DESCRIPTION files to store md5 in loans[]
          description.path <- paste0(to.groundhog_to_local , "/DESCRIPTION")  
          
        #Get MD5 for all DESCRIPTION files
          groundhog.md5 <- tools::md5sum(description.path)
        
        #Vertically add to existing loans data.frame
          loans<-rbind(loans, data.frame(pkg_vrs            = snowball.lend$pkg_vrs, 
                                         groundhog_location = snowball.lend$installation.path,
                                         md5                = groundhog.md5, 
                                         sha                 = snowball.lend$sha,
                                         stringsAsFactors = FALSE))
        
        #Drop the row name, which is a long location path
          loans<-data.frame(loans,row.names = NULL)  
        
      #Delete parent folder name in groundhog folder  (e.g., rio_0.5.4/rio  we moved /rio so delete rio_0.5.4)
        for (fk in from.groundhog_to_local)
        {
          unlink(dirname(fk),recursive=TRUE)
          
        }
        
      #Update loans.rds
        save.loans(loans)   #utils #60
        
      }
      


} #End localize function

     
  