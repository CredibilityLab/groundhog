#After a snowball is installed successfully, so we now know all pkgs are installed, we 
# we save the snowball to avoid having to compute it again in the future/



save.snowball<-function(snowball, include.suggests) {
      
  
  #1 Set path  
  		#1.1 CRAN
          if (!'sha' %in% names(snowball))
            {
               #dir
              snowball_dir <- paste0(get.groundhog.folder() , '/snowballs_v2' )
              
              #file
                  if (include.suggests==FALSE) snowball_file <- paste0(pkg , "_" ,  gsub( "-", "_" , date) , '.rds')  
                  if (include.suggests==TRUE)  snowball_file <- paste0(pkg , "_" ,  gsub( "-", "_" , date) , '_with_suggests.rds')  
            
              #path
                  snowball_path <- file.path(snowball_dir, snowball_file)
            } 
        
      #1.2 Remote (github or gitlab)
          if ('sha' %in% names(snowball))
            {
            #remote_id inferred from installation.path
              remote_id = "" 
              if (regexpr('gitlab',snowball$installation.path[nrow(snowball)])[[1]] > -1) remote_id='gitlab'
              if (regexpr('github',snowball$installation.path[nrow(snowball)])[[1]] > -1) remote_id='github'
            
            #dir
              snowball_dir <- paste0(get.groundhog.folder() , '/snowballs_v2/' , remote_id )
            
            #FILE
               if (include.suggests==FALSE) snowball_file <- paste0(usr ,"_", pkg , "_" ,  gsub( "-", "_" , date) , '.rds')  
               if (include.suggests==TRUE)  snowball_file <- paste0(usr ,"_", pkg , "_" ,  gsub( "-", "_" , date) , '_with_suggests.rds')  
            #FULL PATH
               snowball_path <- file.path(snowball_dir, snowball_file)
            
          } 
       
  #--------------------------     
     
  #2 Update  column `installed` in  snowball
      ip <- data.frame(utils::installed.packages(snowball$installation.path), stringsAsFactors=FALSE)
      snowball$installed <- (snowball$pkg %in% ip$Package | snowball$pkg %in% .pkgenv[['base_pkg']]) #if in packages or in base.packages
          
				   
    
	#3 Save snowball RDS 
			saveRDS(snowball, snowball_path, version = 2, compress=FALSE)
  }