# Function that return a table of contents (toc) dataframe, with package, dates, and dependencies
# The default is the  current cran.toc.rds file but if pkg in the call is remote (non-cran)
# then the default is modified to include the remote packages as extra rows in the toc
# it also updated automatically if the date requested merits. The update based on date happens first
# because the cran.toc.rds on the server only includes cran packages

  get.full_toc <- function(pkg ='', date='', refresh = FALSE)
    {
    
    #1 If a full_toc is set in the environment already, return that toc unless refresh=TRUE
        if (!is.null(.pkgenv[["full_toc"]]) & refresh==FALSE) {
          return(.pkgenv[["full_toc"]])
            } 
    
    #2 Update rds file if the date merits
        if (date!='') update.databases.if.needed(date)
    
    #3 Load cran toc from rds, call it full_toc, instead of cran.toc, for it can later include not just cran, but also remote packages
        full_toc <- readRDS(file.path(get.groundhog.folder(), 'cran.toc.rds'))
        
    #4 Early return if pkg not specified (function just trying to load the full_toc,not update it)
        if (pkg=='')  {
            .pkgenv[["full_toc"]] <- full_toc
            return(full_toc)
            }
        
    #5 Check if pkg is remote  (this is left as a modular step, separate from #3, to facilitate adding other remotes in the future or different structure)
        remote <- FALSE
        if (get.remote_id(pkg)!='cran') remote <- TRUE
          
    
    #6-#9 if it is remote, adjust toc
      if (remote==TRUE) 
      {
        
      #6 Rename pkg to include only the package name
          pkg.list <- make.pkg_list(pkg)
          
      #7 Process that remote pkg, and any remote dependencies, creating a baton with information that is passed on
          baton <- process.remote.all(pkg.list$git_usr_pkg ,  date)
          
      #8 If remote packages exist also in cran, drop
          full_toc <- subset(full_toc, !Package %in% baton$rows.toc$Package)
              
      #9 Add remote to full_toc 
            full_toc <- rbind(baton$rows.toc, full_toc)
      }
        
  #10 Assign environmental variable
      .pkgenv[["full_toc"]] <- full_toc
    

  #11 Output
      return(full_toc)
      
  } #End of function
  
  