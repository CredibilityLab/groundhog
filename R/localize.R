#Takes a snowball and it copies all packages from the groundhog library to the default one
#This serves the following purposes

  localize.snowball <- function(snowball , localize.quietly = FALSE)
  {
    #0 Early return if empty snowball
      if (nrow(snowball)==0) return(TRUE)
    

    #0.5 Restore point: save dataframe with installed packages with today's date if not yet saved, for possible restore late

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

      }
    


  #--------------------------------------------------------------------------               
        
        
    #1  Installed packages: local, groundhog and backup
        if (!'sha' %in% names(snowball)) snowball$sha=''
 
      
      #1.1 Locally
         ip <- data.frame(utils::installed.packages(lib.loc =.pkgenv[["orig_lib_paths"]][1] ), stringsAsFactors=FALSE, row.names=NULL)
        
        #If none, create empty row to avoid errors when combining pkg_vrs
          if (nrow(ip)==0) ip[1,] <- rep('',ncol(ip)) 
        
        #pkg_vrs 
          ip$pkg_vrs<-paste0(ip$Package,"_",ip$Version)
          
          
    ##############################################################################          
    #EARLY RETURN IF ALL LOCAL ALREADY
          if (all(snowball$pkg_vrs %in% ip$pkg_vrs)) return(invisible(TRUE))

    ##############################################################################
          
    
    #1.2 Message on how many
          n.localize <- sum(!snowball$pkg_vrs %in% ip$pkg_vrs  | !snowball$sha %in% c('', NA))
          message2("\nWill now copy ",n.localize," packages to default personal library")
          message1("(you may undo changes at any time with `restore.library()`)")
                
    #1.2 In Groundhog 
         #Master path   
             groundhog.master_path <- paste0(get.groundhog.folder() , "/R-" , get.r.majmin())
      
         #All pkgs in that path    
            ip.groundhog <- data.frame(utils::installed.packages(list.files((groundhog.master_path),full.names = TRUE)),
                                       row.names = NULL, stringsAsFactors = FALSE)
            
         #If none, create empty row to avoid errors when combining pkg_vrs
          if (nrow(ip.groundhog)==0) ip.groundhog[1,] <- rep('',ncol(ip.groundhog))
            
            
        #pkg_vrs
            ip.groundhog$pkg_vrs = paste0(ip.groundhog$Package,"_",ip.groundhog$Version)
        
         
            
    #1.3 Backup path
            backup.dir <- paste0(get.groundhog.folder(),"/restore_library/" , get.r.majmin() , "/")
            ip.backup <- data.frame(utils::installed.packages(list.files(backup.dir, full.names = TRUE)),
                                    row.names = NULL, stringsAsFactors = FALSE)
            if (nrow(ip.backup)==0) ip.backup[1,] <- rep('',ncol(ip.backup))
            ip.backup$pkg_vrs <- paste0(ip.backup$Package , "_" , ip.backup$vrs)
             
    
            
               
          #
  #--------------------------------------------------------------------------               
                 
                 
   #2 If entire snowball is not remote, assign sha='' to snowball
    

# Start the loop over the snowball  #k=6
      k.copied = 1  #how many have we copied
      
      #Sort snowball
      snowball<-snowball[order(snowball$pkg),]
  
      
      
for (k in 1:nrow(snowball))
      {
  

      #3 Short varnames
        pkg     <- snowball$pkg[k]
        pkg_vrs <- snowball$pkg_vrs[k]
        installation.path <- snowball$installation.path[k]
        sha <- snowball$sha[k]

                 
    #4 SKIP if already local 
      if (pkg_vrs  %in% ip$pkg_vrs & sha %in% c('', NA)) next
        
    #5 If package does not exist in groundhog folder, error    
        if (nrow(data.frame(utils::installed.packages(lib=installation.path), stringsAsFactors=FALSE,row.names=NULL))==0) {
          msg = paste0("groundhog says: failed to install '",pkg_vrs,"', localization failed (Error: localize.R #8 - try http://groundhogr.com/troubleshoot)")
          gstop(msg) #util #51
        }

        
    #6 Show feedback 
      if (localize.quietly==FALSE) message1("     Copying ",k.copied," of " , n.localize,": ",pkg_vrs)
       k.copied <- k.copied+1
      
    #6 PURGE: With conflict
    #  If different version of this pkg is already local, purge and ensure backup exists
         
         #Same pkg, different vrs
          if (pkg %in% ip$Package &  !(pkg_vrs %in% ip$pkg_vrs)) 
            {
          
          #6.1 Get pkg_version in local folder now  
             pkg_vrs.existing <- ip$pkg_vrs[ip$Package==pkg]
            
          #6.2 If this pkg_vrs is neither in groundhog nor backup, save it to backup (if it was installed with groundhog it would be in groundhog) 
                 if (!(pkg_vrs.existing %in% ip.groundhog$pkg_vrs) &  #It is not in groundhog
                     !(pkg_vrs.existing %in% ip.backup$pkg_vrs))       #it is not in backup) 
                {
            
                #Copy to groundhog.folder
                 local.pkg_path <- file.path(ip$LibPath[ip$Package==pkg] , pkg) 
                 backup.pkg_path <- paste0(backup.dir, pkg_vrs.existing)
                 dir.create(backup.pkg_path, recursive = TRUE,showWarnings = FALSE)
                  copy.outcome <- file.copy(local.pkg_path , #copy contents from personal folder
                                            backup.pkg_path,                       
                                            recursive = TRUE)   #include all files
                
                  
                  
                  

                } #End 6.2
               
       #6.3  Rename to "_#####_PURGE" 
            old<- file.path(ip$LibPath[ip$Package==pkg] , pkg) 
            random <- paste0(sample(letters,size=6),collapse = '')
            new <- paste0(old , "_",random,"_PURGE")  #add 6 random letters and _PURGE
            purged   <- file.rename(old , new)
              
    } #End #6 - if conflict 
         
     
#-------------------------------------------------------------
      
    #7  Copy the folder from groundhog folder
         #path to copy pkg from and to
		    local_folder <- .pkgenv[["orig_lib_paths"]][-length(.pkgenv[["orig_lib_paths"]])]
            from_path <-paste0(installation.path,'/',pkg)  #groundhog_folder
            to_path <-  paste0(local_folder[1])            #local_folder
                               
        
        #Make to path if it does not exist (libpath with pkg specific folder)
        if (!file.exists(to_path)) {
          dir.create(to_path,recursive=TRUE)
          }
        
        copy.outcome <- file.copy(from_path ,    #copy contents of the "pkg_vrs/pkg" folder
                        to_path,                 #to the local library listed first
                        recursive = TRUE)        #include all files
        
        if (copy.outcome==FALSE) {
          message("groundhog says: failed to copy '", pkg_vrs,"' to default personal library")
        }
        
 
   
    } #End snowball loop
        
} #End localize function
  
     
  