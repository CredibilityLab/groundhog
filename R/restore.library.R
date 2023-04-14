#' Restore version of packages in personal library replaced by groundhog.
#'
#' When groundhog installs a package, it installs it into  groundhog's library. 
#' To find the location of this file direction: `get.groundhog.folder()`. Groundhog then immediately 
#' moves the installed package (and its dependencies) to the default personal library 
#' (to find its location: `.libPaths()[1]`). Because the personal library can only 
#' hold one version of a given package, before moving  new packages in, groundhog moves any existing 
#' other versions of those packages out to another directory (a local archive).
#' 
#' `restore.library()` allows instantaneously reversing these changes, removing packages 
#' installed by groundhog, and restoring  packages removed by it. This allows, for instance, 
#' testing out groundhog, and reversing any changes to the library 
#' made while doing so.
#' 
#' Groundhog makes a single restore point for every day that `groundhog.library()` 
#' leads to changes in the personal library. By default `restore.library` restores the
#' most recent among them (e.g., earlier that same day),
#' 
#' Restore points are saved permanently and can be restored at any point. 
#' To view saved restore dates run `.available.restore.points`. To choose among them use 
#' the `days` argument in `restore.library` (see below). 
#' 
#' 
#'@param days an optional numeric argument used to choose among alternative restore points.
#' When `days` is set, groundhog restores the personal library to the  most recent restore point, that 
#' is at least `days` days ago. For example, if there are two restore points: one from today, and one from 7 days ago,
#' setting `days=3` would restore to the latter, and setting `days=8` would result in an error. 
#' `days = -1` will restore back to the oldest restore point available.


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
    ip.local <- get.ip('local')
           
 
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
      
      
  #3 Read restore IP for datek, all packages in install.package for restore point
      ip.restore <- readRDS(paste0(restore_dir, "/", datek, ".rds"))
    
  #4 Set of backedup pkgs being brought back
      ip.add   <-  ip.restore[!ip.restore$md5 %in% ip.local$md5,]  #We used to have these packages, but no longer do, so we add them
      n.add   <- nrow(ip.add)
      
  #5 Set of pkgs beind eliminated
      ip.purge <- ip.local[!ip.local$md5 %in% ip.restore$md5,]    #We have these packages but did not use to, so we purge them
      n.purge <- nrow(ip.purge)
      
      

      if (n.add + n.purge == 0) {
        message1("The library already matches the restore point, no need to install/uninstall any packages.")
        return(invisible(TRUE))
        }
      

  #6 Inform user what would happend if they go forward
      
      #6.0 Restore back to ONLY date
       if (missing(days)) 
        {
        msg <- paste0(
                "You have requested restoring the (non-groundhog) library of packages \n",
                "as it was prior to any groundhog induced changes made since '", datek ,"'")
        }
      
      
      
      #6.3 Package counts
          msg <- paste0(msg, "\n", 
               "\nThis entails restoring ", n.add, " and uninstalling ", n.purge ," packages.\n",
               "This process should be nearly instantaneous regardless of the number of packages.\n")

    
          
      #6.4. Print message out
        msg<-paste0(msg, "\nTo proceed type 'restore', to stop type anything else.")

        message1(msg)
        answer <-readline(prompt="  >")
        if (tolower(answer) != 'restore') {
          
          message("You typed '",answer,"', the library was NOT restored.")
          return(invisible(FALSE))
        }
        
        
        
 #7 If user says "RESTORE"
   if (tolower(answer) == 'restore') {
      
    
  #7.1 Purge packages (back to groundhog or put to backup) 
      loans <- get.loans()
      purge.local (ip.purge , loans)  #function 1 in interlibrary.functions.R
     
      #This sends back to groundhog pkgs that came from there (based on MD5)
      #and sends to backup those that do no match md5
     
 
  #------------------------------------------------
  #7.2 Restore deleted packages #k=1
        
          if (n.add > 0)
          {
           
          #From/To based on ip.add (recall: ip.add is a subset of ip.restore)
            from <-paste0(backup.dir, ip.add$pkg_vrs)
            to   <- ip.add$LibPath
            
          #Skip if from does not exist skip
            skip.no_backup <- !file.exists(from)
            skip.other_pkg <- file.exists(to)
            skip <- skip.no_backup | skip.other_pkg
          
          #Move 
            outcome <- file.rename(from[!skip] , to[!skip]) 
            
          #Message
            #All success
              if (sum(outcome)==n.add) message1("Restored ",n.add, " packages.")
          
            #Skipped based on FROM
              if (sum(skip.no_backup)>0) {
                message("An additional ",sum(skip.no_backup), " other packages packages could not be restored due to missing backup:")
                message(pasteQC(from[skip.no_backup]))
              } 
          
          #Skipped based on TO
              if (sum(skip.other_pkg)>0) {
                message("An additional ",sum(skip.other_pkg), " other packages packages could not be restored because they \n",
                        "were already available in the destination library (and were not installed with groundhog):")
                message(pasteQC(from[skip.other_pkg]))
              } 
          } 
  
    #------------------------------------------------
            
  #10 Message
        msg = paste0("Library restoration completed.\n",
                     "Restart the R session to proceed.")
        infinite.prompt(msg,valid_answers='uncle',must.restart = TRUE)
            
      } #IF they answered 'restore
      

  
  } #End of `restore` function
