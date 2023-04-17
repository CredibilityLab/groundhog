 #Robust file rename
   
  #This more robust method of file.rename() has two elements
  #1) Before attempting to rename from->to, it tries renaming from->from, only when that succeeds it tries from->to
  #   This was added because renaming packages from / to dropbox, in older R versions was generating errors as
  #   the files were not immediately ready. The issue is likely to arise in other situations as well

  #2) If a cookie exists that tells it otherwise, file.rename() is not attempted and instead
  #   file.copy() and unlink() is



   file.rename.robust <- function(from,to) {
     
     #1. Decide wheter to try renaming
     
        #Assume renaming  
            method <- 'renaming'
      
        #Switch to copy if cookie exists      
             if (cookie.exists("copy_instead_of_renaming")) {
              method<-'copy_and_delete'
            }

     #2 COPY BY RENAMING: 
          if (method=='renaming')
          {
          #2.0 Loop making sure number of files 
            

          #2.1 Loop  attempting to rename to its already existing name from->from
            jtot <- 30 #how many attempts
            
            #message we will show if we fail to rename
                msg_copy_instead_of_renaming <- paste0("After ",jtot," attempts, groundhog was not able to copy '",from,"' by renaming it. From now on, ",
                     "groundhog will rely on the slower method of *moving* (copying-and-deleting) rather renaming package locations, ",
                     "for this and also future groundhog.library() calls. ",
                      "To over-rule this decision, and have groundhog try the faster method again in future calls, ",
                     "run `try.renaming.method.again()`. Note that if it fails again, it will simply again change the ",
                     "default to copying-and-deleting.")
              
            
            
            for (j in 1:jtot) 
            {
              
            #2.2 Pilot renaming to same name
              outcome.j <- file.rename(from,from)  
              if (outcome.j==TRUE) break
              
            #2.3 If Pilot Failed keep trying for some times\
              # (cannot rename to own name-->file is not yet ready for copying)
                if (outcome.j==FALSE) {
              
                  #Message if it is taking longer than expected
                        if (j==2) {
                            message("...waiting for '",from,"' to be ready to be moved...")
                            if  (regexpr('dropbox', tolower(get.groundhog.folder()))>0) {
                              message("This delay is likely caused because the groundhog folder is in Dropbox\n",
                                      "You can change its location with `set.groundhog.folder()`")
                                } #End if dropbox
                      
                            } #End if j==2
                    
                      if (j>2)  cat("...",j)
                  
                      #Wait 5 seconds before retrying
                          Sys.sleep(10)
              
                     } #End if FALSE
            
              } #End loop #2.1

            

        #2.5 If loop ends and we have not attempted file-rename or if we tr and we are still here
          if (j==jtot & outcome.j==FALSE) {
            
            #Cookie
              save.cookie("copy_instead_of_renaming")
          
            #msg
              
              gstop(format.msg(msg_copy_instead_of_renaming))
                   
              }#End j=jtot
          
          
        #2.7 If renaming is attempted (outcome.j) 
            if (outcome.j==TRUE) {
            
              #Rename file
                file.rename(from, to)
                
              #Verify it is copied
                outcome.n <- basename(to) %in% data.frame(utils::installed.packages(dirname(to)),row.names = NULL, stringsAsFactors = FALSE)$Package
                
              #If failed
                if (outcome.n==FALSE) {
                    
                  #If not copied, similar message, stop trying the renaming method
                        save.cookie("copy_instead_of_renaming")
                  
                  #failure message
                      gstop(format.msg(msg_copy_instead_of_renaming))  
                      
                    }
              #if succeed, return TREU
                return(outcome.n)
                
                }  #End if attemped renaming``
                
          } #End if method = 'renaming
          
        
      #-----------------------------------
            
            
      #3 METHOD: Copy and delete
          if (method=='copy_and_delete')
          {
          if (groundhog.in.dropbox()==TRUE) {
            message("Reminder: having groundhog folder in Dropbox makes things slower.\n",
                    "You can use `set.groundhog.folder(<path>)` to change its location.\n",
                    "You can also run `try.renaming.method.again()` to give the faster appraoch\n",
                    "another chance.")
          }
            
          dir.create(to,recursive = TRUE,showWarnings = FALSE)
          message1("Copying ",basename(from)," from groundhog's to R's default library.")
          outcome=file.copy(from,dirname(to),recursive = TRUE)
          if (outcome==TRUE) unlink(from)
          if (outcome!=TRUE) {
            msg<-paste0("Failed copying '" , from , "' to '" , to , "'")
            gstop(msg)
          return(outcome)
          }
        }
   }