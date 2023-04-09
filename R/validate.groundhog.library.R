# 

 validate.groundhog.library <- function(pkg, date,  quiet.install,  include.suggests ,  
                            ignore.deps, force.source , force.install, force.source.main, force.install.main, 
                            tolerate.R.version  ,cores )
  {
      #1. pkg & date included
             if (missing(pkg) || missing(date)) {
              msg=paste0("You must include both a package name and a date in 'groundhog.library()' ")
              gstop(msg)
             }
     

           
      #2 Valid date
           validate.date(date) #Function defined in utils.R
       
      #2.5 Text pkg
            if (!is.character(pkg)) {
              msg=paste0("The pkg argument in groundhog.library() '",pkg,"', is not valid. It must be a character.")
              gstop(msg)
            }
            
       #3 T/F options (utils.R - function 27)
          validate.TF(include.suggests)
          validate.TF(force.source)
          validate.TF(force.install)
          validate.TF(force.source.main)
          validate.TF(force.install.main)
        
            
      #4 ignore.deps
            if (length(ignore.deps)>0) {
              active<-get.active()
              ss <- .pkgenv[['session.snowballs']]

             if (!all(ignore.deps %in% active$pkg | ignore.deps %in% ss$pkg)) {
               
               msg = paste0("All packages included in the ignore.deps() option must be already loaded or be\n",
                       "be dependencies for packages loaded in this session, but the following is/are not: ",
                        paste0(dQuote(ignore.deps [!ignore.deps %in% active$pkg]), collapse=" ,"))
                gstop(msg) #util #51)
              } #End if some are not loaded
              } #End if ignore.deps() are requested
  
     #5 Validate R vs tolerate.R.version)
        validate_R(date , tolerate.R.version)

    #6 Validate Core
        tot.cores<-parallel::detectCores()
        if (!is.numeric(cores) || cores %% 1!=0  || cores< -1 || cores==0 ||  cores> tot.cores)
        {
        msg = paste0('`cores` must be an integer values between 1 & ',tot.cores,' but you entered ',cores)
        gstop(msg)
        }
        
   
    #7 60 days for remotes
        n.remote <- sum(basename(pkg)!=pkg)
        if (n.remote>0)
        {
          today <- Sys.Date()
          days <- as.numeric(round(difftime(today,date,units='days'),0))
          min_since_warned <- get.minutes.since.cookie('60_days_remote')
          
          #less thatn 60 days since date, and more than 30 minutes since last warning
          if (min_since_warned>30 & days<60)
          {
            
          save.cookie('60_days_remote')
          msg <- paste0(
              "For packages from GitHub and Gitlab, it is better to use dates at least 60 days in the past ",
              "(so, before '", today-60 ,"'). The reason is that Git packages can be changed retroactively (with a *past* ",
              "timestamp, so the package you get with '",date,"' today may be different from the one you get with '",date,"' in the future). ",
              "This problem is less likkely, but not impossible, using a date before '",today-60,"'. You can re-run the groundhog.library() ",
              "call as is, or modify the date as suggested. This message will not be shown again during the next 30 minutes.")
          gstop(msg,format=TRUE)
          }
          

        }
        
        
        
  }
