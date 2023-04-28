

#This function tries to rename all paths in the `from` field to `to`
#It first tries to do file.rename() for all,and then chekcs that all
#pkgs are installed successfully in the to, if they are not, it flags an erro
#and switches to copy-pasting
#
#file.rename.robust() (without the '2') did the first step in a loop, but felt slower
#in case we need to switch back, staying here as a backup script till at least next revision




file.rename.robust2<-function(from,to)
  {

  #0 Decide which method to use 
    #Assume we rename
      method='renaming'
      
    #if told otherwise by cookie, copy
      if (cookie.exists("copy_instead_of_renaming")) {
        method<-'copying'
        }  

    
  #1) Rename all files at once 
    if (method=='renaming')
    {

    #1.1) Rename them
      #Ensure parent paths exist
        for (dk in dirname(to)) dir.create(dk, showWarnings = FALSE,recursive = TRUE)

      #Rename
        file.rename(from , to)
      
    #1.2) Verify 
      outcome.rename <- basename(to) %in% data.frame(utils::installed.packages(dirname(to)),
                                                     row.names = NULL, stringsAsFactors = FALSE)$Package
      
    #1.3) If failed, stop
      if (!all(outcome.rename)) {
              
        #Draft message
            msg_copy_instead_of_renaming <-
             paste0("Groundhog was not able to move packages to/from groundog folder ",
                    "with the faster 'rename' method, will switch to (slightly) slower ",
                    "(copying-and-deleting). To over-rule this decision, ",
                    "and have groundhog try the faster method again in future calls, ",
                    "run `try.renaming.method.again()`. Note that if it fails again, it will simply again change the ",
                    "default to copying-and-deleting.")
               
          
        #If not copied, similar message, stop trying the renaming method
              save.cookie("copy_instead_of_renaming")
        
        #failure message
            gstop(format_msg(msg_copy_instead_of_renaming))  
            
          }  
  

    }#End if renaming
    
    
 #-------------------------------------   
    
    #2 METHOD=COPYING
    
      if (method=="copying")
      {

      #2.1 Console ...k feedback if more than 5
        n_to <- length(to)
        if (n_to > 5) {
          cat('\n')  
          message1("Will now copy ",n_to," packages to ",dirname(dirname(to[1])))
          }
      #2.2 Loop over files to copy
        for (k in 1:n_to)
        {
           #Show "...k"
                if (k%%10==1) cat('\n')         #print up to -10 per row
                if (length(to)>5) cat('...',k)  #show how far along we are
                
          
          #Assume we will copy
                skip.copy <- FALSE
              
        #2.3 Decide to skip, if destination already has same MD5 version
            if (dir.exists(to[k])) {
              
              
              #Get MD5s for the DESCRIPTION files #Utils.R #64
                md5.from <- get.md5(from[k])
                md5.to   <- get.md5(to[k])
                
              #If match skip
                if (md5.from==md5.to) skip.copy <- TRUE
          
            
                
                
            } 
         
         
          
        #2.4 Copy if destination does not have it
              if (skip.copy==FALSE)
              {
              #Ensure `to` directory exist
                dir.create(to[k] , recursive = TRUE,showWarnings = FALSE)
        
            
              #Copy 
                outcome=file.copy(from[k],dirname(to[k]),recursive = TRUE) #CHECK @@@
              
              #Verify copy
                  if (outcome!=TRUE) {
                    msg<-paste0("Failed copying '" , from[k] , "' to '" , to[k] , "'")
                    gstop(msg)
                  } #End verification
              } #End 2.4
        
        
         #2.5 Delete when copying form local, via _PURGE if FROM is 
              
              #Local path as recorded on .pkg load
                 local_path <- .pkgenv[['default_libpath']][1]
                  
              #Check if local path contains the name of .libpaths()[1] 
                 from_is_local <- (regexpr(local_path, from[k])[[1]]>=1)
                 

              #If local, delete via purge 
                  if (from_is_local==TRUE) purge.pkg_path(from[k]) #utils #65
        } #end loop
        
    #Skip line in console at the end if we showed ...k
      if (n_to > 5) cat('\n')          
        
    } #End of copying method

  
} #End function