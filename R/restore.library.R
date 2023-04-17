#' Restore default library of packages, undoing all changes made by groundhog
#'
#' Instantaneously reverse changes made by groundhog to the default personal 
#' library where R packages are usually installed into.  
#' If you are just trying groundhog out for the first time,
#' or you generally rely on base R's `library()` and want to use `groundhog.library()` for a specific
#' one time purpose then you may want to run `restore.library()` 
#' when you are done with your one-time use of groundhog; you will undo any and all 
#' changes made by `groundhog`. Restoring a library takes less than a second even if the library 
#' has 100s of packages.
#'  
#'@param days an optional numeric argument used to choose among alternative restore points.
#' When `days` is set, groundhog restores the personal library to the  most recent restore point, that 
#' is at least `days` days ago. If it is not set it restores to the most recent restore point. 
#' For example, if there are two restore points: one from today, and one from 7 days ago,
#' running `restore.library()` would restore to the former, setting `days=3` would restore to the latter, 
#' and setting `days=8` would result in an error. `days = -1` restores to the oldest restore point available.


#' @examples
#' \dontrun{
#' restore.library()
#' restore.library(7)
#' restore.library(-1)

#' }
#'
#' @details 
#'When groundhog installs a package, it installs it into groundhog's library 
#' (location of that library is obtainable with [get.groundhog.folder()]).
#'Groundhog then immediately moves the installed package(s) (and their dependencies) 
#'to the default personal library (location of that library obtainable with: `.libPaths()[1]`). 
#'Altering the packages in the local folder is necessary for groundhog to work properly for two main reasons. First,
#'R Studio often loads packages from that library before users run the code in a script, creating
#'version conflicts that cannot be avoided when attempting to load other versions of those
#'packages with `groundhog`. Second, R scripts often run processes in independent R sessions, for 
#'example when doing parallel processing. Those background processes will also look for 
#'packages in the default personal folder. Because the personal library can only hold 
#'one version of a given package, before moving new packages in, groundhog moves 
#'any existing other versions of those packages out, to another directory (a local archive).
#'Those files are not deleted, just moved, making it easy and fast to recover.
#'When the first change in the personal folder is made on a given calendar date, 
#'groundhog makes a list of all packages available in the personal folder before such change (saving
#'a copy of the results from `installed.packages(.libPaths()[1])`), this saved file is referred to as a 
#'a 'restore point'. With `restore.library()` groundhog looks up a restore point, obtain the set of packages
#'that used to be installed, and removes any packages *installed by groundhog* which are in the personal library
#'but were not in that restore point; similarly, it moves back to the local library any packages *removed by groundhog*
#'that were in the restore point but are not currently there. This process is essentially instantaneous even for 100+ packages.
#'Note that there is only one restore point per calendar date, so one effectively restores
#'the personal library to how it was before *any* changes were made to it that day with groundhog. 
#'Restore points are saved permanently and can be restored at any point. 
#'The set of restore points available is stored in the hidden dataframe `.available.restore.points`. 
#'To choose among them use the `days` argument in `restore.library`. The default is to restore 
#'based on the most recent restore point, so if a user installs groundhog, tests it, and wants to 
#'undo all changes made by groundhog, the default behavior will achieve this goal.

#' @export
#'

restore.library<-function(days)
  {
  
  #1 Current IP
    ip.local     <- get.ip('local')
    ip.backup    <- get.ip('backup')
    ip.groundhog <-get.ip('groundhog')
    loans        <- get.loans()
 

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

      
#---------------------------------------------------------------------------
  #ADD PACAKGES THAT WERE REMOVED BY GROUNDHOG    
      
          
  #4 Subset of pacakges in to-be-restored IP, but not in local, so they need to be added
        ip.add0   <-  ip.restore[! ip.restore$md5 %in% ip.local$md5,]  #We used to have these packages, but no longer do, so we add them
      
    #4.1 Keep subset that we know were originally removed by groundhog
        ip.add   <-  ip.add0[ip.add0$md5 %in% ip.backup$md5,]  #only keep those that were removed by groundhog
      
    #4.2 If some were removed not by groundhog make a note 
        non.groundhog_removed <- ip.add0$pkg_vrs[!ip.add0$pkg_vrs %in% ip.add$pkg_vrs]
        
    #4.3 Count how many are being added
         n.add  <- nrow(ip.add)
      
      
#----------------------------------------------------
      
      
  #5 Set of pkgs being eliminated
      ip.purge0 <- ip.local[!ip.local$md5 %in% ip.restore$md5,]    #We have these packages but did not use to, so we purge them
      
    #5.1 Keep subset taht we know were originally added by groundhog
      ip.purge <- ip.purge0[ip.purge0$md5 %in% loans$md5,]    #We have these packages but did not use to, so we purge them
      
    #5.2  i some were added not by groundhog make a note 
        non.groundhog_installed <- ip.purge0$pkg_vrs[!ip.purge0$pkg_vrs %in% ip.purge$pkg_vrs]
        
    #5.3 count number being removed
         n.purge <- nrow(ip.purge)
      

  #6 Early return if no changes 
      if (n.add + n.purge == 0) {
        message1("No packages added or removed by groundhog since restore point.")
        return(invisible(TRUE))
        }
      

  #7 Inform user what would happened if they go forward
      
      #7.1 Package counts
          message1("Proceeding entails restoring ", n.add, " packages and uninstalling ", n.purge ," packages.\n",
                    "This process should be nearly instantaneous regardless of the number\nof packages involved.\n")

      #7.2 Warnings for non-groundhog changes
          n1=length(non.groundhog_installed)
          n2=length(non.groundhog_removed)
          if (n1+n2>0) {
              msg <- paste0("-------------------------------------------------------------\n",
                            "Warning: Since the creation of the restore point,\n")
              if (n1>0) {
                      msg<-paste0(msg, "an additional ", n1 , " packages have been installed but not\n", 
                                  "by groundhog, these packages will *not* be removed:\n",
                                  pasteQC(non.groundhog_installed),"\n")
                }
              if (n2>0) {
                        msg<-paste0(msg, "an additional ", n2 , " packages have been removed but not\n",
                                  "by groundhog, these will *not* be restored:\n",
                                   pasteQC(non.groundhog_removed))

                }
              msg<-paste0(msg, ".\nProceeding with restoring only the groundhog produced\n",
                                  "changes to the local library may lead packages to not\n",
                                  "work properly.\n",
                                   "See 'https://groundhogR.com/restore' for more information.\n",
                                   "-------------------------------------------------------------")
              message(msg)
          }
          
      #7.3. Prompt for 'restore'
        message1("To proceed type 'restore', to stop type anything else.")
        answer <-readline(prompt="  >")
        if (tolower(answer) != 'restore') {
          
          message("You typed '",answer,"', the library was NOT restored.")
          return(invisible(FALSE))
        }
       
#----------------------------- 
        
        
 #8 If user says "RESTORE"
   if (tolower(answer) == 'restore') {
      
    
  #8.1 Purge packages (back to groundhog or put to backup) 
      purge.local (ip.purge , loans)  #function 1 in interlibrary.functions.R
     
      #This sends back to groundhog pkgs that came from there (based on MD5)
      #and sends to backup those that do no match md5
     
 
  #8.2 Restore deleted packages #k=1
          skip.no_backup=skip.other_pkg=c()
          if (n.add > 0)
          {        
            backup.dir <- paste0(get.groundhog.folder(),"/restore_library/",get.r.majmin(),"/")
            dir.create(backup.dir, showWarnings = FALSE, recursive = TRUE)
           
          #From/To based on ip.add (recall: ip.add is a subset of ip.restore)
            from <-paste0(backup.dir, ip.add$pkg_vrs,"/", ip.add$Package)
            to   <- paste0(ip.add$LibPath, "/", get.pkg(ip.add$pkg))
            
          #Skip if from does not exist skip
            skip.no_backup <- !file.exists(from)
            skip.other_pkg <- file.exists(paste0(to,"/", ip.add$pkg))
            skip <- skip.no_backup | skip.other_pkg
          
          #Move 
            outcome <- file.rename(from[!skip] , to[!skip])
            
			for (k in 1:length(from[!skip]))
			  {
			  outcome[k] <- file.rename.robust(from[!skip][k] , to[!skip][k])
			  #See file.rename.robust.R() it tries renaming only when file is ready, and switches to copying upon failure
			  
			  }
            
          #Delete parent directory 
          #(so, from = 'pkg_vrs'/'pkg'
          #   With 'rename' we take 'pkg' out  of the backup, 
          #   With unlink  we take 'pkg_vrs' out of the backup
            unlink(dirname(from),recursive=TRUE)
            
          
            } 
  
    #------------------------------------------------
      
  #Skipped based on FROM
        if (sum(skip.no_backup)>0) {
        message(sum(skip.no_backup), "  packages originally removed by groundhog could not be restored because the backup was missing:")
        message(pasteQC(from[skip.no_backup]))
              } 
          
  #Skipped based on TO
        if (sum(skip.other_pkg)>0) {
                message(sum(skip.other_pkg), " packages originally removed by groundhog could not be restored because another version of the same \n",
                        "package was installed, not with groundhog, after the restore point was created:")
                message(pasteQC(from[skip.other_pkg]))
              } 
      
      
      
  #9 Compare obtained with expected
        ip.now<-get.ip('local')
      
        
        n.purge.success <- sum(!ip.purge$md5 %in% ip.now$md5)
        n.add.success  <- sum(ip.add$md5 %in% ip.now$md5)

  
        
        
  #10 Message
        
        msg = paste0("Restore library results:\n",
                      "  - ", n.purge.success, " out of ", n.purge, " intended packages were successfully removed\n",
                      "  - ", n.add.success," out of ", n.add, " intended packages were successfully restored")
        prompt.msg = "\nRestart the R session to finalize the restore process."
      
      #Verify
        message1(msg)
        infinite.prompt(prompt.msg , valid_answers='uncle' , must.restart = TRUE)
            
      } #IF they answered 'restore
      

  
  } #End of `restore` function