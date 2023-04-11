#Takes a snowball and it copies all packages from the groundhog library to the default one
#This serves the following purposes

  localize.snowball <- function(snowball , localize.quietly = FALSE)
  {
    #1 Early return if empty snowball
      if (nrow(snowball)==0) return(TRUE)
    

    #2 Restore point: save dataframe with installed packages with today's date if not yet saved, for possible restore late

      restore_path <- paste0(get.groundhog.folder(),"/restore_points/", get.r.majmin(), "/",Sys.Date(),".rds")
      if (!file.exists(restore_path)) {
        
        #Create directory for IP  
          dir.create(dirname(restore_path),showWarnings = FALSE,recursive = TRUE)
        #Get IP
          ip <- data.frame(utils::installed.packages( .pkgenv[["orig_lib_paths"]][-length(.pkgenv[["orig_lib_paths"]])]),row.names = NULL,stringsAsFactors = FALSE)
        #Drop base pkgs
          ip <- ip[!ip$Package %in% base_pkg(), ]
      
        #Drop possible duplicates (if multiple paths exists with the same package, we will work with the first one, and replace that one)
          ip<-ip[! duplicated(ip$Package),]
          
        #Get pkg_vrs
          ip$pkg_vrs <- paste0(ip$Package,"_",ip$Version)
        #Keep only lib and pkg_vrs
          ip <- data.frame(LibPath=ip$LibPath, pkg_vrs=ip$pkg_vrs)
          
        #Save it
          saveRDS(ip , restore_path)
          
        #Add to restore points in environment
          .available.restore.points <<- get.restore.points() #utils #55, loaded in zzz.R, now we add the new date

      } #End of #2
  
        
  #3  Ensure $sha exists
        if (!'sha' %in% names(snowball)) snowball$sha=''
 
      
  #4. Installed.packages: ip (local), ip.groundhog, and ip.backup
      
          #4.1 Local
              
            #Get ip
             local.library<-.pkgenv[["orig_lib_paths"]][1]
             ip <- data.frame(utils::installed.packages(lib.loc =local.library ), stringsAsFactors=FALSE, row.names=NULL)
            
            #If none, create empty row to avoid errors when combining pkg_vrs
              if (nrow(ip)==0) ip[1,] <- rep('',ncol(ip)) 
            
            #Get pkg_vrs 
              ip$pkg_vrs<-paste0(ip$Package,"_",ip$Version)
              
            #Early return if we have all we need   
              loans<-get.loans()
              local.already.from_groundhog <- (snowball$pkg_vrs %in% ip$pkg_vrs ) & (snowball$pkg_vrs %in% loans)
              if (all(local.already.from_groundhog)) return(invisible(TRUE))
    
            #Message on how many
              n.lend <- sum(!local.already.from_groundhog | !snowball$sha %in% c('', NA))
              message2("\nGroundhog will lend ",n.lend," packages to the default personal library")
              message1("(you may undo changes at any time with `restore.library()`)")
          
              
                        
          #4.2 installed.packages() in groundhog  and backup
              ip.groundhog <- get.ip.groundhog() 
              ip.backup   <- get.ip.backup()
              
              #utils #58 (contains pkg_vrs and deals with emtpy df already)
                 
    
    
  
    #6 Find pkgs that need to leave local library
      #They are a pkg we need, and it did not come from groundhog originally
#PENDING: force remotes to be lent
      ip.dispose <- ip[(ip$Package %in% snowball$pkg) & !(ip$pkg_vrs %in% snowball$pkg_vrs[local.already.from_groundhog]) ,]
      
      #the third condition leads to dropping pkg_vrs that match what we want, but were not obtained from groundhog
      #this mostly protects against getting it from a remote server, so jsonlite 1.8.1 coming frmo an unconfirmed source is droped
     
      
    #7 Allocate to return to groundhog vs back-up 
      ip.return <- ip.dispose[ip.dispose$pkg_vrs %in% loans,]  #if in loans, return
      ip.backup <- ip.dispose[!ip.dispose$pkg_vrs %in% loans,] #otherwise, back it up
      
      
    #8 Carry out returns, if any
      if (nrow(ip.return)>0)
      {
      #From and To for returns
        from.local_to_groundhog <- paste0(ip.return$LibPath, "/",ip.return$Package)
        
        to.local_to_groundhog <- c()
          for (k in 1:nrow(ip.return))
          {
            to.local_to_groundhog[k]   <- paste0(get.pkg_search_paths(ip.return$Package[k], ip.return$Version[k]),"/",ip.return$Package[k])
          }
      #As precaution, delete any destination folder, and create the parent
        for (fk in to.local_to_groundhog)
          {
          if (file.exists(fk)) unlink(fk)
          dir.create(dirname(fk),recursive = TRUE,showWarnings = FALSE)
          }
        
      #Move back to groundhog 
          outcome.return <- file.rename(from.local_to_groundhog , to.local_to_groundhog)
          

      #remove returned packages from loans
        loans<-loans[!loans %in% ip.return$pkg_vrs]
      
      }
    
      
      
    #9 Carry out backups, if any
      if (nrow(ip.backup)>0)
      {
      #Directory for backups  
        backup.dir <- paste0(get.groundhog.folder(),"/restore_library/",get.r.majmin(),"/")
        dir.create(backup.dir, showWarnings = FALSE, recursive = TRUE)
      
      #From and To for backups
        from.local_to_backup <- paste0(ip.backup$LibPath,"/",ip.backup$Package)
        to.local_to_backup   <- paste0(backup.dir,ip.backup$pkg_vrs)
      
      #Move to backup
        outcome.backup <- file.rename(from.local_to_backup , to.local_to_backup)
      
      
      }
    
      
    #10 Do the loans
      
    #Find pkgs that need to come to local library: new loans
      snowball.lend <- snowball[!local.already.from_groundhog,]
      
      if (nrow(snowball.lend)>0)
      {
      #From groundhog to local
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
        loans<-c(loans,snowball.lend$pkg_vrs)
        
      #Delete parent folder name in groundhog folder  (e.g., rio_0.5.4/rio  we moved /rio so delete rio_0.5.4)
        for (fk in from.groundhog_to_local)
        {
          unlink(dirname(fk),recursive=TRUE)
          
        }
        
        
      #Update loans.rds
        save.loans(loans)   #utils #59.
        
      }
      


} #End localize function
  
     
  