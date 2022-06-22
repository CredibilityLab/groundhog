#If a package is being loaded with groundhog and we want to show a specific warning with an OK
#this function shows it with a prompt, if OK goes on, otherwise stops and in any case does not show
#warning again for some number of days


pkg_specific.warnings <- function(pkg)
  {
  
  #1 Set of packages with some warning
      all_pkgs_with_warnings <- c('foreach')
      
  #2 if pkg does not belong to set of packages with warnings, stop with early return
      if (!pkg %in% all_pkgs_with_warnings) return(invisible(TRUE))
  
  
  #--------------------------------------------------------
  #3. Messages
      
      #EXAMPLE: Decided not to use, but keeping in case future revisions do include pkg warnigns
      #3.1 Foreach
      
#         if (pkg=='foreach')
#         {
#         #
#           
#         
#         #Message
#           msg <- paste0("|IMPORTANT\n",
# 								      "|     groundhog says: the package `foreach` can jeopardize \n",
# 								      "|     reproducibility by loading packages from the default\n",
#                       "|     R library in background.  This is easy to solve adding\n",
#                       "|     just three lines of code to the `foreach` loops.\n",
#                       "|     More information at: https://groundhogr.com/foreach")
#         #Number of days
#           days_till_shown_again <- 90
#           
#         }
      
 #--------------------------------------------------------     
 #4 Show prompt if more than `days` have past
        #4.1 Path to cookie    
          msg.cookie.path <- file.path(get.groundhog.folder(), paste0("warnings_shown/" , pkg , ".txt"))
          
        #4.2 How many days since it was last shown
          if (file.exists(msg.cookie.path)) {  
      
          #How many days since it was created
            create.time <- file.info(msg.cookie.path)$ctime
            days <- difftime(Sys.time() , create.time, units='days')
            } else { 
            days=5000
            }
          
            
        #4.3 If less than `days` early return
            if (days < days_till_shown_again)  return(invisible(TRUE))
            
        #4.4 if more than so many days show the msg and save the cookie 
            if (days>=days_till_shown_again) {
              
          #cookie
            if (!file.exists(dirname(msg.cookie.path))) dir.create(dirname(msg.cookie.path))
            utils::write.csv(Sys.time() , msg.cookie.path)
            
          #show message
            msg<-paste0(msg,"\n",
                        "|     This warning will not be shown again within ", days_till_shown_again, " days.\n",
                        "|     Type 'OK' to confirm you have read this message.\n")
            answer<-infinite.prompt(msg,"ok")
            return(invisible(TRUE))
          } #End if showing the message
          
}
