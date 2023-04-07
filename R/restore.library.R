#' Restore version of packages in personal library replaced by groundhog.
#'
#' When groundhog installs a package, it saves two copies of it.
#' One goes on the stable groundhog library (to find its location: `get.groundhog.folder()`)
#' the other goes to the default personal library (to find its location: `.libPaths()[1]`).
#' Because the personal library can only hold one version of a package, groundhog replaces 
#' existing versions of packages that already exist in the personal library (if any), just like 
#' `install.packages()` does. But, it makes a backup copy of those replaced packages. 
#' You can restore your personal non-groundhog library to how it was prior to groundhog modifying it 
#' (e.g., you can test groundhog for the first time, and then undo any modifications to your
#' personal library). Run `restore.library()` and the backup
#' copy of the original package version will be restored to the personal library. 
#' Thanks to keeping the backup copies, restoring an entire library takes but a few seconds.
#' Groundhog makes a single restore point for every day that `groundhog.library()` 
#' leads to replacing even a single package into the personal library.
#' View available dates with `.available.restore.points`. 
#' 
#' 
#'@param days an optional numeric argument used to choose among alternative restore points.
#' When `days` is set, groundhog restores the personal library to the  most recent restore point, that 
#' is at least `days` days ago. `restore.library()` by default restores
#' the most current restore point (e.g., if some packages were installed with groundhog today, to 
#' how the library was before today's installations). `days = -1` will restore back to the first
#'restore point available.


#' @examples
#' \dontrun{
#' restore.library()
#' restore.library(7)
#' restore.library(-1)

#' }
#'
#' @details For more information about groundhog check out [groundhogr.com](http://groundhogr.com)

#' @export
#'

restore.library<-function(days)
  {
  
  #1 Current IP
    ip <- data.frame(utils::installed.packages(.libPaths()[-length(.libPaths())]),row.names=NULL)
    ip$pkg_vrs <- paste0(ip$Package,"_",ip$Version)
           
    #drop already set to be purged by previous actions (e.g., installs) as they are not really 'installed' anymore
     ip <- ip[regexpr('_PURGE', ip$Package)<0,] 


  #2 Chose restore point to use 
    #2.1 Set the path
      restore_dir <- paste0(get.groundhog.folder(),"/restore_points/", get.r.majmin())
      dir.create(restore_dir, recursive = TRUE,showWarnings = FALSE)    

      
    #2.3 Files available
      ip_files <- list.files(restore_dir)
        
        #if none, end
        if (length(ip_files)==0) {  
            message1("No restore points are available.")
            exit()
          }
        
    #2.4 Turn filenames to dates
      ip_dates <- as.Date(substr(ip_files, 0,10))  
      
    #2.5 If missing `days` argument, get the latest
      if (missing(days)) {
        datek <- max(ip_dates)
      }
      
    #2.6 else, get the new date prior to `days`
      if (!missing(days)) {
          days.since <- Sys.Date() - ip_dates
          ip_dates <- ip_dates[days.since > days]
          if (length(ip_dates)==0) {
            message("The oldest restore point is from ",max(days.since)," days ago.")
            exit()
          }
          datek <- max(ip_dates)
      }
      
    #2.7 Read pkg_vrs to be restored
      ip.restore <- readRDS(paste0(restore_dir, "/", datek, ".rds"))
    
  #3 Compare
      ip.add   <-  ip.restore[!ip.restore$pkg_vrs %in% ip$pkg_vrs,]
      ip.purge <-  ip        [!ip$pkg_vrs %in% ip.restore$pkg_vrs,] 

      n.add   <- nrow(ip.add)
      n.purge <- nrow(ip.purge)
      
      if (n.add+n.purge==0) {
        message1("The library already matches the restore point, no need to install/uninstall any packages.")
        return(invisible(TRUE))
        }
      

  #4 Inform what would be restored
      #4.0 Restore back to ONLY date
       if (length(ip_dates)==1) 
        {
        msg <- paste0(
                "Will restore the (non-groundhog) library of packages as it was prior\n",
                "to any changes made on '", datek ,"'")
        }
      
      #4.1 Restore back to 1st date
      if (missing(days) & length(ip_dates)>1) 
        {
        msg <- paste0(
                "Will restore the (non-groundhog) library of packages as it was prior to any changes\n",
                "made on '", datek ,"', the first date available as a restore point. To choose a \n",
                "different restore point use the 'days' argument (indicating # of days to go back).")
        }

      #4.2 Restore back to # days ago
      if (!missing(days))
        {
            msg <- paste0(
                "Will restore the (non-groundhog) library of packages as it was prior to any changes\n",
                "made on '", datek , "' (the most recent restore point available from more than the \n",
                "requested '",days,"' days ago).")
      }
      
      #4.3 Package counts
          msg <- paste0(msg, "\n", 
               "\nThis entails restoring ", n.add, " and uninstalling ", n.purge ," packages.\n",
               "This process usually takes a few seconds to execute.\n")

    
          
      #4.5. Print message out
        msg<-paste0(msg, "\nTo proceed type 'restore', to stop type anything else.")

        message2()
        message1(msg)
        answer <-readline(prompt="  >")
        if (tolower(answer) != 'restore') {
          
          message("You typed '",answer,"', the library was NOT restored.")
          return(invisible(FALSE))
        }
        
 #5 Execute if typed restore
   if (tolower(answer) == 'restore') {
      
       
  #5.1 Purge  k=1
      if (n.purge>0)
      {
              
          #Subset of IP that need to be purged
             sub <-ip$pkg_vrs %in% ip.purge$pkg_vrs 
             
          #Their old name
             old <- file.path(ip$LibPath[sub],ip$Package[sub])
             
          #New name
             #Random letters for file names
               random <-c()
               for (k in 1:length(old)) { 
                 random[k] = paste0(sample(letters,size=6),collapse = '')
               }
               
             #New
              new <- paste0(old , "_",random,"_PURGE")  #add 6 random letters and _PURGE
              purged   <- file.rename(old , new)
              
              message1('Will now prepare ',n.purge,' packages for uninstallation:\n',
                       paste0(1:n.purge, ") ", sort(ip$pkg_vrs[sub]),"\n"))
       
      } #ENd if somethign to purge.
      

  #6 Restore deleted packages #k=1
        
          if (n.add > 0)
          {
          outcome <- c()
          message1("\nWill now restore ", n.add , " packages.")

          for (k in 1:n.add)
          {
          #8.1 Setup paths 
            #pkg info
              pkg_vrs <- ip.add$pkg_vrs[k]
              pkg <-get.pkg(pkg_vrs)
              vrs <-get.vrs(pkg_vrs)
              
            #8.2 location in the backup folder
              backup.dir <- paste0(get.groundhog.folder(),"/restore_library/" , get.r.majmin() , "/")
               from       <- paste0(backup.dir, pkg_vrs,"/",pkg)
               to         <- ip.add$LibPath[k]  #this is the folder where it was deleted from 
              
               
             #8.3 If package not found in backup, skip
               ip.k <- data.frame(utils::installed.packages(dirname(from)),row.names=NULL, stringsAsFactors = FALSE)
               if (nrow(ip.k)==0)
               {
                 message("Did not find backup for ",pkg_vrs," will skip and not restore it.")
                 next 
                 
               }
              
            #8.4 if the destination local folder has been deleted or no longer available, go to default
              if (!file.exists(to)) {
                message("Note: package ",pkg_vrs," used to be installed in '",to[k], "' but that folder\n",
                        "no longer exists. Will re-install in '",.libPaths()[1] ,"' instead.")
                to <- .libPaths()[1]
              }
              pkg.local_path  <- paste0(to,"/",pkg)
              
          #8.5 Purge destination package if it exists 
              if (file.exists(pkg.local_path)) {
                random <- paste0(sample(letters,size=6),collapse = '')
                new <- paste0(pkg.local_path , "_",random,"_PURGE")  #add 6 random letters and _PURGE
                purged   <- file.rename(pkg.local_path , new)
              } #ENd purge
                
          #8.6 Copy from groundhog to local
              #Message
                message1("Restoring " , k , " of " , n.add, ":  '",basename(from),"'")
               
              #Copy
                outcome[k] <- file.copy(from , to, recursive = TRUE)    
                
          } #ENd for loop over re-installing pkgs
          
          } #End if anything to reinstall
         
          #9 Save restore cookie so when groundhog restarts it says erstore complete
            restore_cookie <- file.path(restore_dir , "restore_pending_restart_cookie.rds")
            saveRDS(as.numeric(Sys.time()) , restore_cookie)
            
          #10 Message
            msg = paste0("done.\n\n\n",
                         "===============================================================\n",
                         "                  ***  VERY IMPORTANT  ***  \n\n",
                         "   To finalize the library restore you need to:\n",
                         "       1) Restart the R session (in R Studio CMD/SHFT-CTRL-F10)\n",
                         "       2) Run `library('groundhog')` again.\n\n",
                         "  Only when you do (2) will temporary files that can create conflicts\n",
                         "  be deleted. If you do not do (2), packages may behave erratically.\n\n",
                         "                 ***  RESTART THE R SESSION   ***\n",
                         "===============================================================")
                         infinite.prompt(msg,valid_answers='uncle',must.restart = TRUE)
            
      } #IF they answered 'restore
      

  
  } #End of `restore` function