  
#This script is executed from within new.groundhog.library() when a source file that needs to be installed is already loaded
#it is run in background so that R Studio does not stop it from running, it install the pacakge to groundhog folder, then localize
#copies it


  #Grab package and date from system
   args          <- commandArgs(TRUE)
   path          <- as.character(args[1])  #this is passed on after system() if it is not passed on, the code will not run

#--------------------------------------------
#1 Validate
 
  #1.1 Early return if path is missing
    
    if (is.na(path)) {
      message('groundhog says: the script `background_install.snowball.list.R` must be executed in background with system() and providing a valid path.')
      return(FALSE)
    }

 
  #1.2 Early return if file with snowball list does not exist
     if (!file.exists(path))
     {
       message('groundhog says: the script `background_scripts/background_install.snowball.list.R` received a non-existing path for the snowball.list.')
       return(FALSE)
      }

 
  #1.3 Read the arguments for install.snowball.list() calls
    arguments<-readRDS(path)
    snowball.list <- arguments$snowball.list
    pkg           <- arguments$pkg
    date          <- arguments$date
    cores         <- arguments$cores  
    
 
  #1.4  If empty early return
    if (length(snowball.list)==0)
    {
    message('groundhog says: the script `broundhog_scripts/background_install.snowball.list.R` received an existing path, but the snowball.list is empty')
    return(FALSE)
    }

 
#--------------------------------------------
 
#2 Execute installation 
    
    
    #suppressPackageStartupMessages(library('groundhog'))
    
    
    #Bring these variables to the environment, so that when install.snowball.lists() passes it to teh core processors, it will find them in the environment
      get.vrs <- groundhog:::get.vrs
      get.pkg <- groundhog:::get.pkg
      get.pkg_search_paths <- groundhog:::get.pkg_search_paths 
      .pkgenv <- groundhog:::.pkgenv
      base_pkg <- groundhog:::base_pkg
      get.groundhog.folder <- groundhog:::get.groundhog.folder

    groundhog:::install.snowball.list(pkg, snowball.list , date,cores)

