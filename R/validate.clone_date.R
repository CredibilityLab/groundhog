
    validate.clone_date <- function(pkgk, datek, remote_idk, usrk)
    {
    
    #0 Read local clone catalog 
      
      #Path
        clone_catalog_path <- file.path(get.groundhog.folder() , 'clone_catalog.rds')
      
      #Read or initialize empty
        if (file.exists(clone_catalog_path)) {
          clone_catalog <- readRDS(clone_catalog_path)
          } else {
          clone_catalog <- data.frame(remote_id=character(0), usr=character(0), pkg = character(0), last.saved =character(0))            
          }
      
          
      #1 If a clone exists and it is saved after validate date, return TRUE, as in, already updated
           #Row of catalog for this pkg, if any
                clone_catalog_row <- subset(clone_catalog, clone_catalog$pkg==pkgk & clone_catalog$usr==usrk & clone_catalog$remote_id==remote_idk)
                
            #If there is at least one row, and it it later than date, then return TRUE
                if (nrow(clone_catalog_row)>0 && max(clone_catalog_row$last.saved) > datek) 
                    {
                    return(TRUE)
                    }
          

      #2 Paths to local clone
          clone_path <- get.clone_path(pkgk, usrk, remote_idk)   #see remote_functions.R
           
                
      #3 Check if clone exist by looking for DESCRIPTION file in that path
          description_path <- file.path(clone_path,"DESCRIPTION")
          local_git_exists <- file.exists(description_path)  
            
            
      #3 if local clone exists, 'pull' to update (we are pulling because if we were up to date we would have had an early return)
            if (local_git_exists==TRUE) {
              
            #Ensure we got 'git2r' package loaded to interact with git clone
                load.pkg_utility('git2r',datek)  
                
            #Use the git2r_pull function with its new name
                pull_result <- git2r::pull(clone_path) 

            #Update catalog
                save.clone_catalog_update(pkgk,remote_idk,usrk) #Function 2 below
                
                
                return(TRUE)
            }
            
          
          
      #4 if local git does not exist, create it by cloning 
          
          if (local_git_exists==FALSE) {
           
          
            #4.1 Ensure  'git2r' is loaded
                load.pkg_utility('git2r',datek)  
				
					#remote_functions.R - Function 2
          
			#4.2 Clone github repository
            
              #Create folder if it does not exist
                  if (!file.exists(clone_path)) dir.create(clone_path,recursive=TRUE)
            
              #Path to local clone
                  git_path <- paste0('https://' , remote_idk , ".com/" , usrk , "/" , pkgk)
                
              #Clone it
                  try(git2r::clone(git_path, clone_path),silent=FALSE)
            
          } #End if local_git_exists
          
          
          
      #5 check again, update directory if it worked, error message otherwise
          local_git_exists <- file.exists(description_path)  #set in #2
          
          if (local_git_exists==TRUE)
              {
              save.clone_catalog_update(pkgk , remote_idk , usrk) #Function 2 below

              } else { 

              #Delete folder
                unlink(clone_path,recursive=TRUE,force=TRUE)
                
              msg <- paste0 ("groundhog says: Unable to obtain R package '",usrk,"/",pkgk,"' from '",remote_idk, "'. ",
							 "Check spelling of package, make sure it is an R package, and that it is available ",
							 "on '",remote_idk, "',  check your internet connection, and/or visit ",
							 "http://groundhogR.com/troubleshoot.")
							 
			gstop(msg) #utils #51
			
          }  #End of if local exists

          
          
    } #End function 1
           

#------------------------------------------------------------------------
  
    
  #Function 2 - save to clone_catalog 
      save.clone_catalog_update <- function(pkg, remote_id, usr)
      {
        
        #New row for the local git catalog
          new_row  <- data.frame(remote_id=remote_id, usr=usr, pkg = pkg, last.saved =Sys.Date(), stringsAsFactors=FALSE)            
        
        #Catalog path
          clone_catalog_path <- file.path(get.groundhog.folder() , 'clone_catalog.rds')
      
        #Read and append if it exists
          if (file.exists(clone_catalog_path)) {
            #Read it
              clone_catalog <- readRDS(clone_catalog_path)
              
            #Drop this pkg from it
              clone_catalog <- subset(clone_catalog, pkg!=pkg)
              
            #Add new row existing
              clone_catalog <- rbind(clone_catalog, new_row)
        
          } else {
            
        #If it does not exist, this line becomes the file
            clone_catalog <- new_row
          }
          
          
        #Save clone_catalog
          saveRDS(clone_catalog, clone_catalog_path, version=2,compress=FALSE) #version 2 for backwards compatible
        
      } 
      
#----------------------------------------------------

      
      
      

  
  