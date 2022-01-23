#Function 1 Get path to clone
#Function 2 Load a package needed for remote installation (e.g., git2r)
#Function 3 Get sha_time data.frame (reading clone's commits with git2r)
#Function 4 get sha for a particular package date
#Function 5 Get lib for remote install of the package

#----------------------------------------------------------------


#Function 1 Get path to clone
  get.clone_path <- function (pkg, usr, remote_id) 
   {
   clone_path <- paste0(get.groundhog.folder() , "/git_clones/" , remote_id, "/", usr, "_" ,pkg)
   return(clone_path)
  }
  

#----------------------------------------------------------------
  
#Function 2 Load a package needed for remote installation (e.g., git2r)
  load.pkg_utility <- function(pkg_utility , groundhog.day)
  {
       if (! pkg_utility %in% .packages()) {
          
              #Explain to user which version of 'git2r' is being used
                message2()
                message1("To proceed we need to load the package '", pkg_utility, "'.\n",
                  "It will now be loaded by 'groundhog', loading its version available on CRAN on\n",
                  "on the date '", groundhog.day , "'. If you want to use a different version\n",
                  "of '", pkg_utility, "' you may load it via library() or via groundhog.library(),\n",
                  "using a different date, before running the command you just run.\n")
          
          #Attempt to load via groundhog  
            groundhog.library(pkg_utility, groundhog.day)
          
         #Final check giving error if it failed
           if (! pkg_utility %in% .packages()) {
             exit("groundhog says: Error. Could not load '" , pkg_utility, "'.")
            } #End of final check that git2r is now available
            
      } #End if pkg_utility not loaded
  } #End of function 2
  

  
  
  
  #----------------------------------------------------

    
  #Function 3 - get sha_time data frame from clone commits
      get.sha_time <- function(pkg, date, remote_id, usr)
      {
       #Ensure clone exists
         validate.clone_date (pkg, date, remote_id, usr)
        
       #Ensure we have git2r
          load.pkg_utility('git2r' , date)  
        
       #Path for the clone
          clone_path <- get.clone_path (pkg, usr, remote_id) 

       #Use 'git2r' command 'commits' to read all commits      
          all_commits <- commits(repo=clone_path , time=TRUE)

      #Loop extracting time and sha
        commit.time = commit.sha = c()
        for (k in 1:length(all_commits)){
           commit.time[k] <- all_commits[[k]]$author$when$time
           commit.sha[k]  <- all_commits[[k]]$sha
        }
        
      #Create data.frame sha_time  
        sha_time <- data.frame(sha=commit.sha, time=commit.time)
        return(sha_time)
      }  
    
  
#----------------------------------------------------
  
    #Function 4 - get sha for a particular package 

    get.sha <- function(pkg, date, remote_id, usr) {
       
        #Read the sha_time
            sha_time <- get.sha_time(pkg, date,  remote_id, usr)

        #Get time of first second of next day
            time <-  as.numeric(as.POSIXct(as.Date(date)+1, format="%Y-%m-%d"))
    
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
                    message('groundhog can only install non-CRAN packages from  GitHub and Gitlab.\n',
                                    'The package "',pkg,'" is not recognized as either.')
                    exit()
               }
            return(remote_id)
        } #End get.remote_id
       
 
    
      
#Function 9 -  make_package take a value entered by user, pkg or usr/pkg, or git::usr/pkg and turns it into a list with all the parts
  make.pkg_list <-function(pkg) {
    
    #0 REmote id
        remote_id <- get.remote_id(pkg)
  
    #1. Determine pkg.type ('pkg', 'user_pkg', 'git_user_pkg')
      pkg.type='unknown'
      if (pkg==basename(pkg))                        pkg.type='pkg'
      if (strpos1("::",pkg)<0 & strpos1("/",pkg)>0)  pkg.type='usr_pkg'
      if (strpos1("::",pkg)>0 & strpos1("/",pkg)>0)  pkg.type='git_usr_pkg'
      if (pkg.type=='unknown') exit('groundhog says: "' , pkg , '"is not a valid name for a package')
      
    
      
    #2 Fill in alterantive sytnaxes for pkg
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
  

  
  