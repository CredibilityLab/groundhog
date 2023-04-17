


#Function 1 - Fed the subset of an IP that needs to be purged, and based on a loans[] dataframe it returns loans or backups the pkgs

purge.local <- function (ip.purge , loans)
  {
   

  #1 Allocate to be purged packages to return to groundhog vs to back up 
  #   If MD5 matches loan, return it, else goes to backup
      ip.return <- ip.purge[ip.purge$md5 %in% loans$md5,] 
      ip.backup <- ip.purge[!ip.purge$md5 %in% loans$md5,] 
      
      
  #2 Carry out returns, if any
      if (nrow(ip.return)>0)
      {
       
      #2.1 Merge IP with loans() to get  groundhog location for pkgs in IP
        ip.return <-merge(ip.return, loans, by='md5')
        
      #2.2 FROM/TO paths
        from.local_to_groundhog <- paste0(ip.return$LibPath, "/",ip.return$Package)
        to.local_to_groundhog   <- paste0(ip.return$groundhog_location, "/", ip.return$Package)
        
        
      #2.3 As precaution, delete any destination folder, and create the parent
        for (fk in to.local_to_groundhog)
          {
          if (file.exists(fk)) unlink(fk,recursive=TRUE)
          dir.create(dirname(fk),recursive = TRUE,showWarnings = FALSE)
          }
        
      #2.4 Execute #for debugging:  k=1
		  outcome.return <-c () 
          for (k in 1:length(from.local_to_groundhog))
          {
          outcome.return[k] <- file.rename.robust(from=from.local_to_groundhog[k] , to= to.local_to_groundhog[k])
          
          #See file.rename.robust.R() it tries renaming only when file is ready, and switches to copying upon failure
          
          }
         
          #Any failures?
          #if (mean(outcome.return)<1) {
           # message("groundhog says:\nwarning. The following folders failed to be created:\n",pasteQC(to.local_to_groundhog[!outcome.return]))
          #}
          
           
      #2.5 remove returned packages from loans
        loans<-loans[!loans$md5 %in% ip.return$md5,]  
        save.loans(loans)  #update .rds (Utils #60.2)
      } #End of 2
      
  #----------------------------------------------------------------------------------
      
  #3 Carry out backups, if any
      if (nrow(ip.backup)>0)
      {
       
      #3.1 Directory for backups  
        backup.dir <- paste0(get.groundhog.folder(),"/restore_library/",get.r.majmin(),"/")
        dir.create(backup.dir, showWarnings = FALSE, recursive = TRUE)
      
      #3.2 From and To for backups
        from.local_to_backup <- paste0(ip.backup$LibPath,"/",ip.backup$Package)
        to.local_to_backup   <- paste0(backup.dir,ip.backup$pkg_vrs,"/",ip.backup$Package)
      
        #make directories of type 'pkg_vrs', we then put pkg inside it 
          dirs=dirname(to.local_to_backup)
          for (k in 1:length(dirs)) dir.create(dirs[k], recursive=TRUE,showWarnings = FALSE)
        
      #3.3 Subset that are new
        new <- !file.exists(to.local_to_backup)    #are they new to the backup? (do not replace existing files)
        
      #3.4 Move new backup files
		outcome.backup <- c()
         for (k in 1:length(from.local_to_backup))
          {
           

          outcome.backup[k] <- file.rename.robust(from=from.local_to_backup[new][k] , to= to.local_to_backup[new][k])
          #See file.rename.robust.R() it tries renaming only when file is ready, and switches to copying upon failure
          
          }
        
          #Any failures?
          #if (mean(outcome.backup)<1) {
           # message("groundhog says:\nwarning. The following folders failed to be created:\n",pasteQC(to.local_to_backup[!outcome.backup]))
          #}
        
      #3.5 Delete backup files that somehow already are backed up
        unlink(from.local_to_backup[!new],recursive = TRUE)
      } #End 3
    
        
  }