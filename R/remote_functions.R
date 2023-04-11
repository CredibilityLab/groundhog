#Function 1 get.clone_path()      - Get path to clone
#Function 2 load.pkg_utility()    - Load a package needed for remote installation (e.g., git2r)
#Function 3 get.sha_time()        - Get sha_time data.frame (reading clone's commits with git2r)
#Function 4 get sha for a particular package date
#Function 5 Get lib for remote install of the package
#Function 6 Identify remote ('github' vs 'gitlab';)
#Function 7 make_package take a value entered by user, pkg or usr/pkg, or git::usr/pkg and turns it into a list with all the parts
#Function 8 try_install_git()   - attempt to install a package from a local git repository

#----------------------------------------------------------------


#Function 1 Get path to clone
  get.clone_path <- function (pkg, usr, remote_id) 
   {
   clone_path <- paste0(get.groundhog.folder() , "/git_clones/" , remote_id, "/", usr, "_" ,pkg)
   return(clone_path)
  }
  

#----------------------------------------------------------------
  
#Function 2 Load a package needed for remote installation (e.g., git2r)
  load.pkg_utility <- function(pkg_utility , date)
  {
    
      if (pkg_utility %in% .packages()) return(TRUE)
    
      

      #Check date is after when git2r when from S4 to S3 classes
        
        date2<-date
        
        if (date<'2018-10-15') {
            msg <- paste0("groundhog relies on `gitr` and `remotes` to install packages from GitHub ",
                          "and Gitlab. To address a  backwards incompatible change in `git2r` ",
                          "introduced in 2018, when groundhog loads those packages in order ",
                          "to install packages from Github or Gitlab, and the requested date is ",
                          "prior to '2018-10-15', that date is used instead, ",
                          "loading `git2r_0.23.0` and `remotes_2.0.0`. You *can* load earlier ",
                          "versions of those packages directly with groundhog, but those earlier ",
                          "versions will not allow loading github and gitlab packages with groundhog. \n \n ")
            
            if (pkg_utility=='git2r') message1(format.msg(msg,header="NOTE"))
            date2<-'2018-10-15'
            
          }
        
    
      ip <- utils::installed.packages()
          
      
          if (! pkg_utility %in% c(.packages(),ip)) {
          
          #Attempt to load via groundhog  
            #Save existing path
              libPaths.before<-.libPaths()
              
            #groundhog call using date 2
              groundhog.library(pkg_utility, date2 ,tolerate.R.version = get.rversion()) #Always accept different versions here
              
            #return libpath
              .libPaths(libPaths.before)
              
              
         #Final check giving error if it failed
           if (! pkg_utility %in% .packages()) {
             exit("groundhog says: Error. Could not load '" , pkg_utility, "'.")
            } #End of final check that git2r is now available
             
          
      } #End if pkg_utility not loaded
  } #End of function 2
  

    
  #Function 3 - get sha_time data frame from clone commits
      get.sha_time <- function(pkg, date, remote_id, usr)
      {
        
      #Check if sha_time file exists, and sandwiches the date, in which case we just output that without reading clone itself
        #Path to file
          sha_path <- paste0(get.groundhog.folder(),"/sha_times/", remote_id,"_" ,usr,"_",pkg,".rds")
         
        #Load it and return the data.file if it was saved at a date that sandwiches the date being requested
          if (file.exists(sha_path))
              {
              #Read the sha_time
                sha_time <- readRDS(sha_path)
                
              #Force sha to be string rather than factor (extra safety, data.frame(string as factor should be set to FALSE, but in case it was already saved)
                sha_time$sha=as.character(sha_time$sha)
                
              #If highest commit date is higher than requested date, we can use existing sha_time
                 if (max(sha_time$time)>date.to.time(date)) {
                    return(sha_time)
                  } #End if date is sandwiched
                
             
                
              } #End if file exists
              
          
          
       #Ensure clone exists
         validate.clone_date (pkg, date, remote_id, usr)
        
       #Ensure we have git2r
          load.pkg_utility('git2r',date)
        
       #Path for the clone
          clone_path <- get.clone_path (pkg, usr, remote_id) 

       #Read all commits 
          all_commits <- git2r::commits(repo=clone_path , time=TRUE)

      #Loop extracting time and sha
        commit.time = commit.sha = c()
        for (k in 1:length(all_commits)){
           commit.time[k] <- all_commits[[k]]$author$when$time
           commit.sha[k]  <- all_commits[[k]]$sha
        }
        
      
      #Create data.frame sha_time  
        sha_time <- data.frame(sha=commit.sha, time=commit.time,stringsAsFactors = FALSE)
        
      #Save sha time
        if (!file.exists(dirname(sha_path))) {
          dir.create(dirname(sha_path),recursive = TRUE,showWarnings = FALSE)
        }
        
        #Save the sha_time
          saveRDS(sha_time , sha_path,version=2,compress=FALSE)
        
        
        return(sha_time)
      }  
    
  
#----------------------------------------------------
  
    #Function 4 - get sha for a particular package 

    get.sha <- function(pkg, date, remote_id, usr) {
       

        #Read the sha_time
            sha_time <- get.sha_time(pkg, date,  remote_id, usr)

        #Get time of first second of next day
            time <-  as.numeric(as.POSIXct(as.Date(date)+1, format="%Y-%m-%d"))
    
        #IF time before 1st, STOP with error
            if (time<min(sha_time$time)) {
                date0 <- as.Date(min(sha_time$time)/86400,origin='1970-01-01')
                msg<-paste0("Groundhog says: the package '" , usr , "/" , pkg ,"' was not yet available on ",
                            "'" , remote_id ,"' on '", date, "'. The first commit was on '",date0,"'. ")
                gstop(msg) #util #51
              }
                    
        #Find last commit before the 1st second of the day after the request
            time.k <- max(sha_time$time[sha_time$time < time])
      
        #Find the sha associated with it, if there were ties, choose the first
            sha <- sha_time[which(sha_time$time==time.k),]$sha[1]
          
        #Return
            return(sha)
      
    }
    
#----------------------------------------------------

#Function 5 - Get lib for remote install   
    get.installation_path_remote <- function(pkg, date , remote_id, usr)
    {
    #1 sha
      sha <- get.sha(pkg, date, remote_id,usr)
      short_sha = substr(sha, 0 , 7)
    
    #2 Folder for Saving the package 
     rv <- as.character(getRversion())
     rv <- gsub("\\.\\d+(-w)?$", "", rv)
    
    #3 Combine
      path <- paste0(get.groundhog.folder() , "/R-" , rv, "/_" , remote_id , "/" , usr, "_", pkg, "_" , short_sha)
    
    return(path)
    }
    
#----------------------------------------------------  
  
    
 #Function 6 Identify remote ('github' vs 'gitlab';)
       get.remote_id <- function(pkg)
       {
          #0 cran
              if (basename(pkg)==pkg) {
                return('cran')
              }
         
          #1 github:  If none specified or if github specified, 
                remote_id = ''
                if (strpos1('::',pkg)==-1 | strpos1('github::',pkg)>-1 | strpos1('github.com',pkg)>-1) {
                  return('github')
                }
                
          #2 gitlab: if gitlab is specified
                if (strpos1('gitlab::',pkg)> -1 | strpos1('gitlab.com',pkg)>-1) {
                  return('gitlab')
                }
      
          #3 End if remote is unknown
               if (remote_id=="") {
                    msg <- paste0('groundhog can only install non-CRAN packages from  GitHub and Gitlab.',
                                    'The package "',pkg,'" is not recognized as either.')
                    gstop(msg) #util #51)
               }
            return(remote_id)
        } #End get.remote_id
       
 

#Function 7 -  make_package take a value entered by user, pkg or usr/pkg, or git::usr/pkg and turns it into a list with all the parts
  make.pkg_list <-function(pkg) {
    
    #0 Remote id
        remote_id <- get.remote_id(pkg)
  
    #1. Determine pkg.type ('pkg', 'user_pkg', 'git_user_pkg')
      pkg.type='unknown'
      if (pkg==basename(pkg))                        pkg.type='pkg'
      if (strpos1("::",pkg)<0 & strpos1("/",pkg)>0)  pkg.type='usr_pkg'
      if (strpos1("::",pkg)>0 & strpos1("/",pkg)>0)  pkg.type='git_usr_pkg'
      if (pkg.type=='unknown') exit('groundhog says: "' , pkg , '"is not a valid name for a package')
      
    
      
    #2 Fill in alternative sytnaxes for pkg
      #type=pkg
      
      if (pkg.type=='pkg')    {
          pkg <- pkg 
          usr_pkg <- '' 
          git_usr_pkg <- ''
      }
      
      
      #type==usr_pkg
      
        if (pkg.type=='usr_pkg') {
          usr_pkg <- pkg
          git_usr_pkg <- paste0('github::',usr_pkg)
          pkg     <- basename(pkg)
          git    <- 'github'
        }
      
      
      #type==git_usr_pkg
      
      if (pkg.type=='git_usr_pkg')  {
          usr_pkg     <- strsplit(pkg,"::")[[1]][2]
          git_usr_pkg <- pkg
          pkg         <- basename(pkg)
          }
     
    #3 usr
      usr <-dirname(usr_pkg)
      
    #4 Produce package as a namedList (see function in utils.R)
        pkg_list <- namedList(pkg , usr_pkg , git_usr_pkg, pkg.type, remote_id, usr)
      
    #5 output it    
      return(pkg_list)
      
  }
  

  
  
#----------------------------------------------
  
#Function 8 - try install_git
  
  #    -----------  Workaround given ambiguity  2022-07-10    -----
  #There is ambiguity whether install_git() needs to have the path as "file://" or just the path/
  #This functions tries by default without the 'file://' and if it fails, it makes a note of it to 
  #always add 'file://' first, but then it will update the moment trying with file:// fails but without it succeeds
  
  
  try_install_git<-function(path,  dependencies, lib, ref, INSTALL_opts)      {

      
      #1 Should we start without adding 'file://
    
        #Assume we don't add it
          add_file_first <- FALSE
        #Cookie path
          cookies_dir <- paste0(get.groundhog.folder(),"/cookies")
          dir.create(cookies_dir,recursive = TRUE, showWarnings = FALSE)
          cookie_path <- file.path(cookies_dir, "add_file_first.csv")
        
        #if it exists, we do add it
          if (file.exists(cookie_path)) add_file_first==TRUE
        
        
      #2 File 1 & file 2
        #Add 'file://' to a path
        file_path <- paste0('file://',path)
        
        #If that should be the 1st one, make it
          if (add_file_first==TRUE) 
            {
            path1<-file_path
            path2<-path
          } else {
        
        #But if itshould be the 2nd, do that
          path1<-path
          path2<-file_path
        } #End if we should try 'file://' first
        
        
      #3 Try path 1
          try1 <- try(remotes::install_git(url=path1,  dependencies=dependencies  , lib=lib, ref=ref, INSTALL_opts=INSTALL_opts ))
  

      #4 If try 1 success, done
            if (!methods::is(try1,'try-error')) return(invisible(TRUE))
            
            
      #5 If try 1 failure, try path 2 
            
             if (methods::is(try1, 'try-error')) {
              
              message1("Will now try using the modified path: '" , path2, "'")
              try2 <- try(remotes::install_git(url=path2,  dependencies=dependencies  , lib=lib, ref=ref, INSTALL_opts=INSTALL_opts ))
             }
              
      #6 If try 2 works, adjust cookie
              if (!methods::is(try2, 'try-error'))
              {
              #if the cookie exists, delete it, so we try without file first
                if (file.exists(cookie_path)) {
                  unlink(cookie_path)
                }
              time <- as.numeric(Sys.time())
              
              #If the cookie doesn't exist, create it, so we try it first
                if (!file.exists(cookie_path)) {
                  utils::write.csv (time , file=cookie_path)
                }
              
              return(invisible(TRUE))  
            
            } #End if second try was a problem
       
            
      #7 If try 2 fails, give up
            if (methods::is(try2,'try-error')) {
              message("groundhog says:\n",
                      "The 2nd path also did not work, could not install package.")
            }
  } #End function
  
  
 
  
  
  
  
