

#This function tries to rename all paths in the `from` field to `to`
#It first tries to do file.rename() for all,and then chekcs that all
#pkgs are installed succesfully in the to, if they are not, it flags an erro
#and switches to copy-pasting
#
#file.rename.robust() (without the '2') did the first step in a loop, but felt slower
#in case we need to switch back, staying here as a backup script till at least next revision




file.rename.robust2<-function(from,to)
  {
    
    #Check if cookie indicates renameing by copying
      method='renaming'
      if (cookie.exists("copy_instead_of_renaming")) {
        method<-'copying'
        }  
    
    
  #1) Rename all files at once 
    if (method=='renaming')
    {
          
        #1.1) Rename them 
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
                gstop(format.msg(msg_copy_instead_of_renaming))  
                
              }  
  

    }#End if renaming
    
    
 #-------------------------------------   
    
    #2 Copy and delete
    if (method=="copying")
      {
      message1('Will copy ',length(to),' packages')
      cat('\n')
      for (k in 1:length(to))
        {
        cat('...',k)
        dir.create(to[k] , recursive = TRUE,showWarnings = FALSE)
        message1("Copying ",basename(from[k])," from groundhog's to R's default library.")
        outcome=file.copy(from,dirname(to[k]),recursive = TRUE)
        if (outcome==TRUE) unlink(from[k])
        if (outcome!=TRUE) {
        msg<-paste0("Failed copying '" , from[k] , "' to '" , to[k] , "'")
        gstop(msg)
        } #End failure

      } #end loop
    } #End of copying
          
  
} #End function