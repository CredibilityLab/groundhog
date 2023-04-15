#Takes a snowball and it lends all packages from the groundhog library to the default one

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
          ip.all <- get.ip('all_local')  #utils $59
          
        #Drop base pkgs
          ip.all <- ip.all[!ip.all$Package %in% base_pkg(), ]
      
        #Drop possible duplicates (if multiple paths exists with the same package, we will work with the first one, and replace that one)
          ip.all<-ip.all[! duplicated(ip.all$Package),]

        #Save it
          saveRDS(ip.all , restore_path)
          
        #Add to restore points in environment
          .available.restore.points <<- get.restore.points() #utils #55, loaded in zzz.R, now we add the new date

      } #End of #2
  
        
  #3  Ensure $sha exists in snowball (when we create a non-remote snowball the column sha is not present)
        if (!'sha' %in% names(snowball)) snowball$sha <- ''

      
  #4. Installed.packages: ip (local), ip.groundhog, and ip.backup
      
          #4.1 Local
              
            #Get installed.packages in the local library (the first of them if many)
            #for restore we use  ALL local libraries ip.all
            #usually there will be just one local and ip.all[] and ip[] will be the same
      
                ip <- get.ip('local')  #utils #59 
              
                
            #We will do early return if all pkgs in the snowball have been borrowed already
                  loans <- get.loans()      #package lent already from groundhog to personal library[1] (#utils #60)
                  
                
            #NOTE on MD5 vs 'pvs' to identify packages:
                #Nutshell
                #IP <-> snowballs, with pvs (pkg_vrs_sha)
                #IP <-> loans, with MD5
                
                  #same pkg_vrs but different commits, and remotes pkgs from CRAN pkgs with the same pkg_vrs (rio from CRAN vs rio from github)
                  #(sha is "" for pkgs originally on CRAN) 
                
                    
                #more details
                #We not have MD5 for pkgs in snowballs where we have not yet obtained downloaded the package, 
                #so to compare snowballs with IP (installed packages) we use pvs
                #Conversely, we have sha for snowball and for loans, but we do not have it for installed.packages because that is not 
                #obtainable from the IP
                
                  
                  #if sha is NA, make it "", plays more nicely with paste() functions
                    snowball$sha[is.na(snowball$sha)] <-''
                    
                  #pvs for snowball and loans()
                    snowball.pvs <- ifelse(snowball$sha=="",  snowball$pkg_vrs,  paste0(snowball$pkg_vrs , "@" , snowball$sha))
                    loans.pvs    <- ifelse(loans$sha=="",     loans$pkg_vrs,     paste0(loans$pkg_vrs    , "@" , loans$sha)) 
                    
                  #Subset of the snowball that we have borrowed
                    borrowed <- snowball.pvs %in% loans.pvs
                  
                #If all of them are, nothing left to do, done localizing
                if (all(borrowed)) return(invisible(TRUE))
    
            #Message on how many
              n.lend <- sum(!borrowed)

          #4.2 installed.packages() in groundhog  and backup
              ip.groundhog <- get.ip('groundhog') 
              ip.backup    <- get.ip('backup')
              
              
    #5 Purge
    
      #5.1 Get ip.purge:  pkgs that need to be purged  from the local library 
      #    They are a pkg we have in the snowball, but the version of it in the local library  did not originate in a groundhog installation
                
          ip.purge <- ip[(ip$Package %in% snowball$pkg[!borrowed]),]
          
          
      #5.2 Process purge, deleting or returning to groundhog depending on origin of pkg in local library now
          
           purge.local(ip.purge , loans) #see Function 1 in interlibrary.functions.R
  
             
    #6 Borrow
      
    #Find pkgs that need to come to local library: new loans
      snowball.lend <- snowball[!borrowed,]
      
      if (nrow(snowball.lend)>0)
      {
      #From groundhog to local
        local.library <- .pkgenv[["orig_lib_paths"]][1]
        from.groundhog_to_local <- paste0(snowball.lend$installation.path, "/", snowball.lend$pkg)
        to.groundhog_to_local   <- paste0(local.library,"/",snowball.lend$pkg)
      
        
       #As precaution, delete any destination folder
        for (fk in to.groundhog_to_local)
          {
          if (file.exists(fk)) unlink(fk,recursive=TRUE)
          }
    
        
      #Move to local
        outcome.loans <- file.rename(from.groundhog_to_local , to.groundhog_to_local)
        
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

     
  