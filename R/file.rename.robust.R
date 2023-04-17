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

     #2 RENAME 
        if (method=='renaming')
        {
        #2.0 Loop making sure number of files 
          

        #2.1 Loop  attempting to rename to its already existing name from->from
          jtot <- 30 #how many attempts
          for (j in 1:jtot) 
          {
            
          #Pilot renaming to same name
            outcome.j <- file.rename(from,from)  
          
          #When it succeeds, rename to the 'to' name (which actually moves the file)
            if (outcome.j==TRUE) {
            
              #Rename file, early return
                outcome.n <- file.rename(from, to)
                return(outcome.n)
                }  #ENd if TRUE
          
          #If FALSE
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
          } #End loop
          
          #Skip a line so that the ... counter is not 
              cat("\n")
          
        #2.2 If loop ends and we are still here
          if (j==jtot) {
            
            #Cookie
              #save.cookie("copy_instead_of_renaming")
          
            #msg
              # msg <- paste0("After ",jtot," attempts, groundhog was not able to copy '",from,"' by renaming it. From now on, ",
              #        "groundhog will rely on the slower method of *moving* (copying-and-deleting) rather renaming package locations, ",
              #        "for this and also future groundhog.library() calls. ",
              #         "To over-rule this determination, and have groundhog try the faster method again in future calls, ",
              #        "run `try.renaming.method.again()`. Note that if it fails again, it will simply again change the ",
              #        "default to copying-and-deleting.")
              
              #message(format.msg(msg))
             
          }#End j=jtot
        } #End if method = 'renaming
          
      #3 Copy and delete
          if (method=='copy_and_delete')
          {
          dir.create(to,recursive = TRUE,showWarnings = FALSE)
          message1("Attempting to copy ",basename(dirname(from))," from groundhog's library to the default library", dirname(from)," to ",to)
          outcome=file.copy(from,dirname(to),recursive = TRUE)
          if (outcome==TRUE) unlink(from)
          if (outcome!=TRUE) gstop("Failed copying '",from,"' to '", to , "'")
          return(outcome)
          }
        }
   