#Takes a snowball and it copies all packages from the groundhog library to the default one
#This serves the following purposes

  localize.snowball <- function(snowball , localize.quietly = TRUE)
  {
    #0 Early return if empty snowball
      if (nrow(snowball)==0) return(TRUE)
    
    #0.5 Vector with purged 
        purged.pkg_vrs = c()
        
    #1 Load installed packages
        ip <- data.frame(utils::installed.packages(lib.loc =.pkgenv[["orig_lib_paths"]][1] ), stringsAsFactors=FALSE)
        
      #If none, create empty row to avoid errors when combining pkg_vrs
        if (nrow(ip)==0) {
        ip[1,] <- rep('',ncol(ip))
        }
        
    #2 Make pkg_vrs
        ip$pkg_vrs<-paste0(ip$Package,"_",ip$Version)
      
    
    #3 If entire snowball is not remote, assign sha='' to snowball
      if (!'sha' %in% names(snowball)) snowball$sha=''
    

 #4 Start the loop over the snowball
  for (k in 1:nrow(snowball))
    {
    #5 Short varnames
      pkg     <- snowball$pkg[k]
      pkg_vrs <- snowball$pkg_vrs[k]
      installation.path <- snowball$installation.path[k]
      sha <- snowball$sha[k]
         
    #6 If localized already, skip  
      if (pkg_vrs %in% .pkgenv[['localized']]) next

    #7 If local folder already has this version and the pkg in snowball is not remote
      if (pkg_vrs  %in% ip$pkg_vrs & sha %in% c('', NA)) next
      
        
    #8 If package does not exist in groundhog folder, error    
        if (nrow(data.frame(utils::installed.packages(lib=installation.path), stringsAsFactors=FALSE,row.names=NULL))==0) {
          msg = paste0("groundhog says: failed to install '",pkg_vrs,"', localization failed (Error: localize.R #8 - try http://groundhogr.com/troubleshoot)")
          gstop(msg) #util #51
        }

    #9 all paths except the last one
         local_folder <- .pkgenv[["orig_lib_paths"]][-length(.pkgenv[["orig_lib_paths"]])]
      
    #10 If this one is installed, set it ready to be purged next time we load groundhog 
        if (pkg %in% ip$Package) {
          
            #10.1 Copy to groundhog folder if it does not exist 
          
             #File paths
               non.groundhog.path <- file.path(ip$LibPath[ip$Package==pkg] , pkg) 
               groundhog.path     <- paste0 (dirname(installation.path), "/", ip$pkg_vrs[ip$Package==pkg] , "/")
               exists <- file.exists(groundhog.path) && nrow(utils::installed.packages(groundhog.path))>0
              
                  
            #10.2 If it does not exist and it is not remote, create and copy
               if (exists==FALSE  & sha %in% c('', NA)) {

                #Create
                  dir.create(groundhog.path,recursive = TRUE, showWarnings = FALSE)

                #Copy non-remote to groundhog.folder
                  copy.outcome <- file.copy(non.groundhog.path ,        #copy contents of the "pkg_vrs/pkg" folder
                              groundhog.path,           #to the local library listed first
                              recursive = TRUE)          #include all files

              } #End 10.2
               
               
            #10.3 to the purge list 
              old<- file.path(ip$LibPath[ip$Package==pkg] , pkg) 
              random <- paste0(sample(letters,size=6),collapse = '')
              new <- paste0(old , "_",random,"_PURGE")  #add 6 random letters and _PURGE
              purged   <- file.rename(old , new)
              
            #10.4 Keep track of purged
              if (purged==TRUE) {
                purged.pkg_vrs <- c(purged.pkg_vrs , ip$pkg_vrs[ip$Package==pkg])
                
              } #End if successful purge 
            
        } #End if this pkg we are installing is already installed in ip so we need to purge
         
         
    #11  Copy the folder from groundhog folder
         #path to copy pkg from and to
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
        
    #12 Show feedback 
      if (localize.quietly==FALSE & copy.outcome==TRUE) message1("  copying: ",pkg_vrs)
     
        
    #13 Add to localized  vector
        .pkgenv[['localized']]<-c(.pkgenv[['localized']], pkg_vrs)
         
} #End snowball loop
        
    
     #14 add to purged.rds
          if (length(purged.pkg_vrs)>0)
          {
          new.df <- data.frame(pkg_vrs = purged.pkg_vrs, time=as.numeric(Sys.time()))
        
        #Read 
          old.df <- read.local.rds('purged.rds') #utils #52
          
        #Update
          if (nrow(old.df)==0)  updated.df <- new.df
          if (nrow(old.df)> 0)  updated.df <- rbind(old.df, new.df)
          
        #Keep only the most recent change to the df, for a given pkg, not pkg_vrs
          updated.df$pkg <- get.pkg(updated.df$pkg_vrs)                 #get pk
          updated.df <- updated.df[order(-updated.df$time),]            #sort by time
          updated.df <- updated.df[duplicated(updated.df$pkg)==0,]      #keep the last of each pkg_vrs
          updated.df <- updated.df[,!names(updated.df) %in% c('pkg')]   #drop 'pkg' column added for dupplicate detection
          
          save.local.rds(updated.df , 'purged.rds')  #utils #52         #save
		  

          }#End if new files
        
        
  } #End localize function
  
     
  