
        
 #Function 1 - get path to where sha_time[,] is saved .rds file (this file saves commits for a github,gitlab package)
  get.sha_time.rds_path<- function(git_usr_pkg)
    {
      remote_id <- get.remote_id(git_usr_pkg)
      usr_pkg <- get.usr_pkg(git_usr_pkg)
      rds_path <- file.path( get.groundhog.folder() , remote_id , paste0(usr_pkg ,".sha_time.rds"))
      return(rds_path)
    }
     

  
   
#-------------------------------------
  
#Function 2 - Get path to library for remote package (e.g.  _github/usr/pkg_1234abc
  
    get.lib_remote <- function(git_usr_pkg, sha)
    {
    #0 Local vars
      remote_id <- get.remote_id(git_usr_pkg)
      usr_pkg <- get.usr_pkg(git_usr_pkg)
      
    #1 Short sha
      short_sha = substr(sha, 0 , 7)
    
    #2 Folder for Saving the package 
     rv <- as.character(getRversion())
     rv <- gsub("\\.\\d+(-w)?$", "", rv)
    
    #3 Combine
      lib_remote <- paste0(get.groundhog.folder() , "/R-" , rv, "/_" , remote_id , "/" , usr_pkg, "_" , short_sha)
    
    return(lib_remote)
    }
      
    
#-------------------------------------
    
 #Function 3 Identify remote ('github' vs 'gitlab';)
       get.remote_id <- function(git_usr_pkg)
       {
          #0 cran
              if (basename(git_usr_pkg)==git_usr_pkg)
              {
                return('cran')
              }
         
          #1 github:  If none specified or if github specified, 
                remote_id = ''
                if (strpos1('::',git_usr_pkg)==-1 | strpos1('github::',git_usr_pkg)>-1 | strpos1('github.com',git_usr_pkg)>-1) {
                  return('github')
                }
                
          #2 gitlab: if gitlab is specified
                if (strpos1('gitlab::',git_usr_pkg)> -1 | strpos1('gitlab.com',git_usr_pkg)>-1) {
                  return('gitlab')
                }
      
          #3 End if remote is unknown
               if (remote_id=="") {
                    message('groundhog can only install non-CRAN packages from  GitHub and Gitlab.\n',
                                    'The package "',git_usr_pkg,'" is not recognized as either.')
                    exit()
               }
            return(remote_id)
        } #End get.remote_id
       
       #Examples:
       #get.remote_id('github::CredibilityLab/groundhog')
       #get.remote_id('gitlab::pkg/er')
       #get.remote_id('https://github.com/CredibilityLab/groundhog')
       #get.remote_id('belgium::pkg/er')
       #get.remote_id('pkg/er')  
       #get.remote_id('rio')    
       
       
#-------------------------------------

       
  #Function 5 Clean git_usr_pkg (drop gitlab:: github:: or http:// ...)
       get.usr_pkg <- function(git_usr_pkg)
        {
        x <- strsplit(git_usr_pkg,"[:/]")[[1]] #Split at any non-alphanumeric
        n <- length(x)                         #Number of pieces
        usr_pkg <- paste0(x[n-1], "/" , x[n])  #Last two
        return(usr_pkg)
        }
    
        #Examples:     
        #get.usr_pkg('https://github.com/CredibilityLab/groundhog')
        #get.usr_pkg('CredibilityLab/groundhog')
        #get.usr_pkg('gitlab::CredibilityLab/groundhog')

       
#-------------------------------------
  
#Function 6 - delete file with commits list
  refresh_commits <- function(git_usr_pkg)
        {
        #Parameters
          remote_id <- get.remote_id(git_usr_pkg)
          usr_pkg <- get.usr_pkg(git_usr_pkg)
        
        #File path
          #rds_path <- file.path( get.groundhog.folder() , remote_id , paste0(usr_pkg ,".sha_time.rds"))
          rds_path <- get.sha_time.rds_path(git_usr_pkg)

        if (!file.exists(rds_path)) {
          message1("You attempted to refresh the local file that lists all commits for '",git_usr_pkg, "'\n",
                   "That involves deleting the file: '",rds_path,"',\n",
                   "but that file does not exist. You may have already run this command, ",
                  "or there may be a typo in '" , git_usr_pkg,"'.")
                  exit()
                  }
        unlink.outcome <- unlink(rds_path)
        
        }
  
  
#-------------------------------------
     
#Function 7 - Parse the full github::usr/pkg@sha string into a list with all the parts
  parse_git_usr_pkg_sha <- function(git_usr_pkg_sha)
  {
   pkg.split  <- strsplit(git_usr_pkg_sha,"@")[[1]]
   git_usr_pkg <- pkg.split[1]
   sha <- pkg.split[2]
   remote_id <- get.remote_id(git_usr_pkg)
   usr_pkg <- get.usr_pkg(git_usr_pkg)
   pkg <- basename(usr_pkg)
   usr <- dirname(usr_pkg)
   pkg.parsed <- namedList(git_usr_pkg_sha, git_usr_pkg, sha, remote_id, usr_pkg, pkg, usr)
   return(pkg.parsed)
    
  }
  
#Function 8 - Standardize remote pkg name
  standardize.git_usr_pkg <- function(pkg)
    {
     #if it is cran, output the same pkg
       if (basename(pkg)==pkg) return(pkg)
    
    #if not cran, process it
     usr_pkg <- get.usr_pkg(pkg)
     remote_id <- get.remote_id(pkg)
     git_usr_pkg <- paste0(remote_id , "::" , usr_pkg)
     return(git_usr_pkg)
  }
  
  pkg1='rio'
  pkg2='usr/rio'
  pkg3='gitlab::usr/rio'
  
#Function 9 -  make_package take a value entered by user, pkg or usr/pkg, or git::usr/pkg and turns it into a list with all the parts
  make.pkg_list <-function(pkg) {
    
    #1. Determine pkg.type ('pkg', 'user_pkg', 'git_user_pkg')
      pkg.type='unknown'
      if (pkg==basename(pkg))                        pkg.type='pkg'
      if (strpos1("::",pkg)<0 & strpos1("/",pkg)>0)  pkg.type='usr_pkg'
      if (strpos1("::",pkg)>0 & strpos1("/",pkg)>0)  pkg.type='git_usr_pkg'
      if (pkg.type=='unknown') exit('groundhog says: "' , pkg , '"is not a valid name for a package')
      
      
    #2 Create list elmeents  for package 
      #type=pkg
      
      if (pkg.type=='pkg')    {
          pkg <- pkg 
          usr_pkg <- '' 
          git_usr_pkg <- ''
      }
      
      
      #type==usr_pkg
      
        if (pkg.type=='usr_pkg') {
          usr_pkg <- pkg
          git_usr_pkg <- paste0('github::',user_pkg)
          pkg     <- basename(pkg)
        }
      
      
      #type==git_usr_pkg
      
      if (pkg.type=='git_usr_pkg')  {
          usr_pkg     <- strsplit(pkg,"::")[[1]][2]
          git_usr_pkg <- paste0('github::',usr_pkg)
          pkg         <- basename(pkg)
          }
      
      
    #3 Produce package as a namedList (see function in utils.R)
        pkg_list <- namedList(pkg , usr_pkg , git_usr_pkg, pkg.type)
          
      return(pkg_list)
      
    }
  
  
  
  