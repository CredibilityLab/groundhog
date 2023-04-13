#Takes a snowball and it lends all packages from the groundhog library to the default one

  localize.snowball <- function(snowball)
  {
    #1 Early return if empty snowball
      if (nrow(snowball)==0) return(TRUE)
    

    #2 Restore point: save dataframe with installed packages with today's date if not yet saved, for possible restore later

      restore_path <- paste0(get.groundhog.folder(),"/restore_points/", get.r.majmin(), "/",Sys.Date(),".rds")
      if (!file.exists(restore_path)) {
        
        #Create directory for IP  
          dir.create(dirname(restore_path),showWarnings = FALSE,recursive = TRUE)
        
        #Get IP
          ip.all <- get.ip('all_local')
          
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
              
            #Get installed.packages in the local library (the first of them if many, for restore, ip.all above,  we have ALL local libraries)
             #usually there will be just one local and ip.all[] and ip[] will be the same
      
                ip <- get.ip('local')  #utils #59 
              
                
            #Early return if we have all we need   
             
                #package lent already from groundhoog to personal library[1] (#utils #60)
                  loans <- get.loans()    
                  
                #Combine pkg_vrs with sha (pvs) to uniquely identify remotes with same pkg_vrs but differnt commits, 
                 #(note: sha is "" for pkgs originally on CRAN) 
                  snowball$sha[is.na(snowball$sha)] <-''
                  snowball.pvs <- ifelse(snowball$sha=="",  snowball$pkg_vrs,  paste0(snowball$pkg_vrs , "@" , snowball$sha))
                  loans.pvs    <- ifelse(loans$sha=="",     loans$pkg_vrs,     paste0(loans$pkg_vrs    , "@" , loans$sha)) 
                  borrowed <- snowball.pvs %in% loans.pvs
                  
                #If all of them are, nothing left to do, done localizing
                if (all(borrowed)) return(invisible(TRUE))
    
            #Message on how many
              n.lend <- sum(!borrowed)

          #4.2 installed.packages() in groundhog  and backup
              ip.groundhog <- get.ip('groundhog') 
              ip.backup    <- get.ip('backup')
              
              
    #6 Find pkgs that need to be taken out from the  local library "purged"
      #They are a pkg we have in the snowball, but it did not originate from groundhog
              
        ip.purge <- ip[(ip$Package %in% snowball$pkg[!borrowed]),]
      
      #the third condition leads to dropping pkg_vrs that match what we want, but were not obtained from groundhog
      #this mostly protects against getting it from a remote server, so jsonlite 1.8.1 coming frmo an unconfirmed source is droped
     
      
    #7 Allocate to return to groundhog vs back-up 
      ip.return <- ip.purge[ip.purge$pkg_vrs %in% loans$pkg_vrs,]  #if in loans, return
      ip.backup <- ip.purge[!ip.purge$pkg_vrs %in% loans$pkg_vrs,] #otherwise, back it up
      
      
    #8 Carry out returns, if any
      if (nrow(ip.return)>0)
      {
      
      #Get DESCRIPTION MD5 for all packages in ip.return
        description.path <- paste0(ip.return$LibPath, "/" , ip.return$Package , "/DESCRIPTION")
        ip.return$md5 <- tools::md5sum(description.path)
        
      
        #FROM
          from.local_to_groundhog <- paste0(ip.return$LibPath, "/",ip.return$Package)
          
        #Get the location where those files were before they were borrowed
          ip.return<-merge(ip.return, loans, by='md5')
          to.local_to_groundhog <- paste0(ip.return$groundhog_location, "/", ip.return$Package)
        
        
    
      #As precaution, delete any destination folder, and create the parent
        for (fk in to.local_to_groundhog)
          {
          if (file.exists(fk)) unlink(fk,recursive=TRUE)
          dir.create(dirname(fk),recursive = TRUE,showWarnings = FALSE)
          }
        
      #Move back to groundhog 
          outcome.return <- file.rename(from.local_to_groundhog , to.local_to_groundhog)
          

      #remove returned packages from loans
        loans<-loans[!loans$md5 %in% ip.return$md5,]  #use md5 to merge because this way if the same pkg_vrs is 
                                                      #available from CRAN and GitHub, it knows which to return
      
      }
      
      
    
      
      
    #9 Carry out backups, if any
      
    #  We only back up packages that have not been borrowed, and for these packages we do not know if they are remote
    #  or originally from CRAN, we ignore that, not taking sha into account at all (since we do not have it)
    #  when restored, we will just copy whatever pkg it was to original local library position
      
      if (nrow(ip.backup)>0)
      {
      #Directory for backups  
        backup.dir <- paste0(get.groundhog.folder(),"/restore_library/",get.r.majmin(),"/")
        dir.create(backup.dir, showWarnings = FALSE, recursive = TRUE)
      
      #From and To for backups
        from.local_to_backup <- paste0(ip.backup$LibPath,"/",ip.backup$Package)
        to.local_to_backup   <- paste0(backup.dir,ip.backup$pkg_vrs)
      
      #Subset that are new
        new <- !file.exists(to.local_to_backup) #are they new to the backup? (do not replace existing files)
        
      #Move it
        outcome.backup <- file.rename(from.local_to_backup[new] , to.local_to_backup[new])
      
      
      }
    
      
    #10 Do the loans
      
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
                                         sha                 = snowball.lend$sha)  )
        
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

     
  