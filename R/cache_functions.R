#Various functions associated with checking cache of recently installed packages
#The cache is used to speedup loading of already installed version-controlled
#packages. When a pkg is called for we check if the cache contains it for the
#requested date, and if it does, we go straight to base.library() it.
#
#---------------------------------------------------------------------------------

#0 Get cache path

#1 Read the cache
#2 Add pkg to cache
#3 Initial check that it is up to date (more recent that most recent installed pkg)

#0 Get cache path

get.cache_path=function()
{
  rv <- as.character(getRversion())
  rv <- gsub("\\.\\d+(-w)?$", "", rv)
  cache_path=paste0(get.groundhog.folder(),"/cache_",rv,".rds")
  return(cache_path)
}


  #1 Read existing cache  

    read.cache=function()
       {
        cache_path <- get.cache_path()
       if (file.exists(cache_path)) {
          cache=readRDS(cache_path)
       } else {
            
         cache=list(date='1970-01-01',pkg='')
       }
       return(cache)
    }
    
    
    
    
  #2 Add pkgs to cache
    
    add.cache=function(pkgs,date)
    {
      
      
      #Read it
        cache=read.cache()
    
      #If date is different, clear it
        if (cache$date!=date) cache=list(date=date, pkg='')
      
      #Add all the packages
        cache$pkg=c(cache$pkg, pkgs)
      
      #Delete duplicates
        cache$pkg = unique(cache$pkg)
      #Save it
         cache_path = get.cache_path()
         saveRDS(cache, cache_path,version=2)
    }
     
    
    
  #3 Check if cache is current (more recent than most recent installed.package)
    is.cache.current = function()
      {
      # Get the first library path
        lib_path <- .libPaths()[1]
    
      # List all directories in this path
        dir_list <- list.dirs(lib_path, full.names = TRUE, recursive = FALSE)
    
      # Get modification times for these directories
        it <- sapply(dir_list, function(x) file.info(x)$mtime)
    
      # File Path  
        cache_path <- get.cache_path()
        
      #If file does not exist, return FALSE
        if (!file.exists(cache_path)) return(FALSE)
        
      #if it does exist, compute time
        cache_time = as.numeric(file.info(cache_path)$mtime) 
        
      #Difference in time
        dif = cache_time - max(it)
     
      #Return
        current=FALSE
        if (dif > 0) current=TRUE 
        return (current)
    }
     