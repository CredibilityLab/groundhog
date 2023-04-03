#' Restore version of packages in personal library replaced by groundhog.
#'
#' When groundhog installs a package, it saves two copies of it.
#' One goes on the stable groundhog library (to find its location: `get.groundhog.folder()`)
#' the other goes to the default personal library (to find its location: `.libPaths()[1]`.
#' Because the personal library can only hold one version of a package, groundhog replaces the
#' version in the personal library, but makes a copy of it in groundhog's stable library. 
#' To restore to your default personal library the version 
#' of packages it had prior to using groundhog, run `restore.library()` and the backup
#' copy of the original package version will be copied to the personal library. 


#' @examples
#' \dontrun{
#' restore.library()
#' }
#'
#' @export
#'

restore.library<-function(days)
  {
  
  #1 Current IP
    ip <- data.frame(installed.packages(.libPaths()[-length(.libPaths())]),row.names=NULL)
    ip$pkg_vrs <- paste0(ip$Package,"_",ip$Version)
           
  #2 Chose restore point to use 
    #2.1 Set the path
      ip_dir <- paste0(get.groundhog.folder(),"/installed_packages/", get.r.majmin())
      dir.create(ip_dir, recursive = TRUE,showWarnings = FALSE)    

      
    #2.3 Files available
      ip_files <- list.files(ip_dir)
        
        #if none, end
        if (length(ip_files)==0) 
            {   message1("No restore points are available.")
            exit()
          }
        
    #2.4 Turn filenames to dates
      ip_dates <- as.Date(substr(ip_files, 0,10))  
      
    #2.5 If missing `days` argument, get the first  
      if (missing(days)) {
        datek <- ip_dates[order(ip_dates)[1]]
      }
      
    #2.6 else, get the latest date prior to `days`
      if (!missing(days)) {
          days.since <- Sys.Date() - ip_dates
          ip_dates <- ip.dates[days.since > days]
          datek <- max(ip_dates)
      }
      
    #2.7 Read pkg_vrs to be restored
      pkg_vrs.restore <- readRDS(paste0(ip_dir, "/", datek, ".rds"))

    
  #3 Compare
      pkg_vrs.add   <-  pkg_vrs.restore[!pkg_vrs.restore %in% ip$pkg_vrs]
      pkg_vrs.purge <-  ip$pkg_vrs[!ip$pkg_vrs %in% pkg_vrs.restore] 

      if (length(pkg_vrs.add)+length(pkg_vrs.purge)==0) {
        message1("The library already matches the restore point, no need to install/uninstall any packages.")
        return(invisible(TRUE))
        }
      

  #4 Inform what would be restored
      #4.0 Back to ONLY date
       if (length(ip_dates)==1) 
        {
        msg <- paste0(
                "Will restore the (non-groundhog) library of packages as it was prior\n",
                "to any changes made on '", datek ,"'")
        }
      
      #4.1 Back to 1st date
      if (missing(days) & length(ip_dates)>1) 
        {
        msg <- paste0(
                "Will restore the (non-groundhog) library of packages as it was prior to any changes\n",
                "made on '", datek ,"', the first date available as a restore point. To choose a \n",
                "different restore point use the 'days' argument (indicating # of days to go back).")
        }

      #4.2 Back to # days ago
      if (!missing(days))
        {
            msg <- paste0(
                "Will restore the (non-groundhog) library of packages as it was prior to any changes\n",
                "made on '", datek , "' (the latest restore point available > ",days," days ago.")
      }
      
      #4.3 Package counts
          msg <- paste0(msg, "\n", 
               "\nThis entails re-installing ", length(pkg_vrs.add), " and uninstalling ",length(pkg_vrs.purge) ," packages.\n",
               "This process takes only seconds to execute.\n")

      #4.4 Where they are being reinstalled to
          if (length(pkg_vrs.add)>0) {
            
            msg<-paste0(msg, "Packages will be installed in: '",.libPaths()[1],"'")
          }
          
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
      if (length(pkg_vrs.purge)>0)
      {
              
          #Subset of IP that need to be purged
             sub <-ip$pkg_vrs %in% pkg_vrs.purge
             
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
              
              message1('The following packages have been prepared for uninstallation:\n   ',pasteQC(ip$pkg_vrs[sub]))
       
      } #ENd if somethign to purge.
      

  #6 Restore deleted packages
        
          if (length(pkg_vrs.add)>0)
          {
          outcome <- c()
          message1("\nWill now restore ", length(pkg_vrs.add), " packages.")

          for (k in 1:length(pkg_vrs.add))
          {
          #8.1 Setup paths 
              pkg_vrs <- pkg_vrs.add[k]
              pkg <-get.pkg(pkg_vrs)
              vrs <-get.vrs(pkg_vrs)
              installation.path <- get.pkg_search_paths(pkg,vrs)
              from <- paste0(installation.path , "/" ,pkg)
              to   <- .libPaths()[1] 
              old  <- paste0(to,"/",pkg)
              
          #8.2 Purge destination package if it exists 
              if (file.exists(old)) {
                random <- paste0(sample(letters,size=6),collapse = '')
                new <- paste0(old , "_",random,"_PURGE")  #add 6 random letters and _PURGE
                purged   <- file.rename(old , new)
              } #ENd purge
                
          #8.3 Copy from groundhog to local
              #Message
                message1("Re-installing " , k , " of " , length(pkg_vrs.add), ":  '",basename(installation.path),"'")
               
              #Copy
                outcome[k] <- file.copy(from , to, recursive = TRUE)    
                
          } #ENd for loop over re-installing pkgs
          
          } #End if anything to reinstall
         
             
            
          #10 Message
            msg = paste0("===============================================================\n",
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