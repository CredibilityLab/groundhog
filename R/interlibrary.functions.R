


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
        ip.return <- merge(ip.return, loans, by='md5')
        
      #2.2 FROM/TO paths
        from.local_to_groundhog <- paste0(ip.return$LibPath, "/",ip.return$Package)
        to.local_to_groundhog   <- paste0(ip.return$groundhog_location, "/", ip.return$Package)
        
        
    #2.4 Execute 
		      file.rename.robust2(from.local_to_groundhog, to.local_to_groundhog)  #file.rename.robust2.R

           
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
      
         
      #3.3 Subset that are new
        new <- !file.exists(to.local_to_backup)    #are they new to the backup? (do not replace existing files)
        
      #3.4 Move new backup files
		    file.rename.robust2(from=from.local_to_backup[new], to= to.local_to_backup[new])

          
      #3.5 Delete local files that somehow already are backed up
         if (sum(new)>0) {
           for (k in 1:sum(new))
           {
             purge.pkg_path(from.local_to_backup[!new][k]) #utils #65
           }
           }
         #Not clear a scenario exists that creates this, but just in case.
         #If a pkg is local, and we need to remove it, and we did not move it to backup, then
         #we just delete it from here via purge
         
    } #End 3
    
        
  }