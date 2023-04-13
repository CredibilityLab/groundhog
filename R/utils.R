#This script has functions used throughout the package
#1  get.pkg(), get.vsr()     : Extract package and version information from pkg_vrs
#2  is.pkg_vrs.installed()   : Is pkg_vrs installed (within same R-minor version)? 
#3  as.DateYMD               : Format Y-M-D as date
#4  get.rdate()              : Date for R being used
#5  get.rversion()           : Get R Version major:minor:patch
#6  message1()               : regular font
#7  message2()               : Bold
#8  namedList()              : automatically name object in list 
#9  quit.menu()              : prompt to quit call upon mismatch oR0f dates
#10 exit()                   : Stop message which does not say error
#11 Dropped (mran related)
#12 base_pkg()                : Output vector with all base packages
#13 ignore.deps_default()     :  Default packages to ignore conflicts with (but gives warning)
#14 Is this on R studio
#15 get.r.majmin              : Get major minor (but no patch) version of R
#16 get.r.majmin.release()    : Get major minor AND patch
#17 validate.date()           : Validate date  
#18 strip.prompt)_            : Clean up prompt answer, lowercase and no quotes
#19 exit.if.download.failed() : Verify file was download
#20 Check if lib is for GitHub (possibly a legacy function)
#21 Robust reading of text online : try_readlines ()
#22 Turn dates to unix time
#23 strpos1()                - string position - simpler gregexpr for strpos  
#24 get.groundhog_libpaths() - get subset of paths in libPath() that belong to groundhog
#25 get.mismatched_versions_report() : Compare two sets of pkg_vrs vectors, obtaining text report of any mismatches  (not used anymore)
#26 base.libary()           : Copy library() to use when finalizing loading a pkg
#27 Validate.tf()
#28 DROPPED
#29 read.desc2()
#30 DROPPED
#31 pasteQC()                  :  paste a vector separating elements by quots  c('a','b','c')-->  string: '"a","b","c"'
#32 infinite.prompt()          :  ask the same question until a valid answer is provided
#33 get.packages_df()          :  data.frame with installed packages in local library
#34 sandwich.library()         :  turn a string containing library calls into a vector of pkg names
#35 format.msg()               :  format output to have fixed width and starting symbol (e.g., "|    ")
#36 set.default.mirror()       :  set a CRAN mirror if none is already set
#37 verify.personal.library.exists(): if no folder for saving personal packages exists, prompted to create it
#38 restart.text()             :  tells user to either quit() R or use CMD-SHFT-F10 based on whether they use R Studio
#39 get.minutes.since.cookie() :  reads csv file with Sys.time() of last time this cookie was saved
#40 get.repos_from.snowball()  :  infer repository (cran, GitHub, GitLab) from other information already in the snowball
#41 add.session.snowballs() :     Update session dataframe with info on all groundhog loaded snowballs
#42 check.groundhog.version()  :  if more than `min.days' days since last check, check if groundhog needs to be updated
#43 Get operating system       :  windows or mac or mac_arm?
#44 Check consent              :  Does .../R_groundhog exists?
#45 Turn path from '\' to '/'  :  c:\users --> c:/users
#46 MOVED
#47 DROPPED
#48 message.batch.installation.feedback() : feedback while installing a batch in parallel
#49 - get.parallel.time()  :  estimate parallel time of installation
#50 get pkg_list from path
#51 gstop()                    :  show message in green then "** groundhog stopped**" in red and exit())
#52 read/save loadl rds 
#53 Download in batches        : breaks a list of pkgs into batches downloaded sequentially
#54 filesize_format            : turn bytes file size to human readable
#55 get.restore.points         : vector with dates available 
#57 View conflicts             : view conflicting pkgs in recent call
#58 Get ip.groundhog():        : installed.packages data.frame for all packages in groundhog library and loans
#59 get ip.backup()            : same for all packages that have been saved to the backup folder
#60 get.loans() & /save.loans(): load and save database with all lent packages
####################################################################################
    


#1. get.pkg get.vrs -  Extract package and version information from pkg_vrs
  get.pkg <- function(x) substr(x, 1, regexpr("_", basename(as.character(x)))- 1)
  get.vrs <- function(x) substr(x, regexpr("_", basename(as.character(x))) + 1, nchar(x))

#2.  Is pkg_vrs installed (within same R-minor version)?
      is.pkg_vrs.installed <- function(pkg, vrs) {
        #Assume base package is installed
        if (pkg %in% base_pkg()) {
          return(TRUE)
          } else {
          (get.installed_path(pkg, vrs) %in% get.pkg_search_paths(pkg, vrs))
          } #End else
      }

  

#3.  Format Y-M-D as date
  as.DateYMD <- function(x) as.Date(x, format = "%Y-%m-%d",origin='1970-01-01')

#4. R being used
  get.rdate <- function() {
    date <- paste0(R.version$year, "-", R.version$month, "-", R.version$day)
    return(as.DateYMD(date))
  }

#5 Get R Version
  get.rversion <- function() {
    r.version <- paste0(R.version$major, ".", R.version$minor)
    return(r.version)
  }

#6 Message1()
  #message1() are messages that are coloured if the terminal supports it and
  # that have a special "groundhog-msg" class that makes it possible to disable
  # them selectively using suppressMessages(     , class = "groundhog-msg")
  message1 <- function(..., domain = NULL, appendLF = TRUE, quiet = getOption("quiet.groundhog", default = FALSE)) {
    if (quiet) {
      return(invisible())
    }
    if (.pkgenv[["supportsANSI"]]) {
      msg <- .makeMessage("\033[36m", ..., "\033[0m", domain = domain, appendLF = appendLF)
    } else {
      msg <- .makeMessage(..., domain = domain, appendLF = appendLF)
    }
    msg <- simpleMessage(msg)
    msg <- structure(msg, class = c("groundhog-msg", class(msg)))
    message(msg)
  }

#7 Message2()
  message2 <- function(..., domain = NULL, appendLF = TRUE, quiet = getOption("quiet.groundhog", default = FALSE)) {
    if (quiet) {
      return(invisible())
    }
    msg <- list(...)
    if (length(msg) == 0) {
      msg <- c("groundhog says:")
    }
  
    if (.pkgenv[["supportsANSI"]]) {
      msg <- .makeMessage("\033[1;36m", msg, "\033[0m", domain = domain, appendLF = appendLF)
    } else {
      msg <- .makeMessage(msg, domain = domain, appendLF = appendLF)
    }
    msg <- simpleMessage(msg)
    msg <- structure(msg, class = c("groundhog-msg", class(msg)))
    message(msg)
  }

#8 Named list
#Automatically name elements in list with name of the objects in the list
# https://stackoverflow.com/questions/16951080/can-lists-be-created-that-name-themselves-based-on-input-object-names
  namedList <- function(...) {
      L <- list(...)
      snm <- sapply(substitute(list(...)), deparse)[-1]
      if (is.null(nm <- names(L))) {
        nm <- snm
      }
      if (any(nonames <- nm == "")) {
        nm[nonames] <- snm[nonames]
      }
      stats::setNames(L, nm)
    }

#9 Quit menu
  quit.menu <- function(date, quiet = getOption("quiet.groundhog", default = FALSE)) {
    if (quiet) {
      return(invisible())
    }
    message1(
      "Type 'Q', 'quit' or 'stop' to stop the script.\nAnything else to continue"
    )
    x <- readline("")
    if (tolower(x) %in% c("q", "quit", "stop")) {
      message2()
      message1("You typed ", x, " so script stops...")
      msg.R.switch(date)
      exit("---")
    } # end if quit
  
    message1("You typed '", x, "' the script continues...")
  } # end quit.menu

#10 exit() Stop message which does not say error
  exit <- function(...) {
    message1(...)
    
     #Read cran toc again to undo any changes with remote
        #Only run if main folder exists, otherwise we have loop, 
        #exiting when it does not exist, and asking that it exist when exiting
          main_folder <-  paste0(path.expand("~"), "/R_groundhog")
          if (file.exists(main_folder)) {
            .pkgenv[['cran.toc']] <- readRDS(file.path(get.groundhog.folder(),"cran.toc.rds"))
          }     
          
    #Return libpath
       .libPaths(.pkgenv[["orig_lib_paths"]])
        invokeRestart("abort")
  }



#12 Base packages
    base_pkg <- function() {
     .pkgenv[['base_pkg']]    #assigned on zzz, base packages based on installed.packages(priority='base')
    }

    
        
#13 Default packages to ignore conflicts with (but gives warning)
  ignore.deps_default <- function() return(c())

  ignore.deps_default____OLD <- function() {
  
  #Packages r-studio tends to load automatically
      Rstudio.deps <- c(
        "knitr",     
        "rmarkdown", 
        "xfun"       
        )
  
  #Recommended and thus hard to uninstall packages
      ip <- data.frame(utils::installed.packages(),stringsAsFactors = FALSE,row.names=NULL)
      recommended.pkgs <- unique(subset(ip, ip$Priority=="recommended")$Package) #unique because there may be two versions of the same package in different libraries
      
  #Combine
      ignore.deps <- c(Rstudio.deps, recommended.pkgs)
    
  #Return
      ignore.deps<-c()
      return(ignore.deps)
           
    }



#14 Is this on R studio
  is_rstudio <- function() {
    # More reliable than the env variable because it works as expected even when
    # code is called from the Terminal tab in RStudio (NOT the Console).
    identical(.Platform$GUI, "RStudio")
  }

#15 Get major minor (but no patch) version of R
  get.r.majmin <- function() {
     major <- as.numeric(R.version$major)
     minor <- as.numeric(strsplit(R.version$minor, "\\.")[[1]][1])
     majmin <- paste0(major, ".", minor)
     return(majmin)
     }
   
#16 Get major minor patch
 get.r.majmin.release <- function()
 {
   r.majmin <- get.r.majmin()
   R.toc <- toc("R") # Get R toc
   R_same.majmin <- grep(paste0("^", r.majmin), R.toc$Version, value = TRUE)
   R1 <- R_same.majmin[1]
   release.date <- R.toc[R.toc$Version==R1,]$Published
   return(release.date)
    }

#17 validate date  
  validate.date <- function(entered_date)
      {
       msg=paste0("The date you entered '", entered_date,"', is not valid.\n",
                "Please use the 'yyyy-mm-dd' format"
          )
    
       
       # correct format
        d <- try(as.Date(entered_date, format="%Y-%m-%d"),silent = TRUE)
          if ("try-error" %in% class(d) || is.na(d)) {
             gstop(msg) #util #51)
          }

       
              
      #The format check does not verify that the day is at most 2 characters long,  (e.g., it accepts 2022-01-109)
       if (is.character(entered_date)) { 
       d.parts <- strsplit(entered_date,"-")[[1]]     #split date by '-
        if (nchar(d.parts[3])>2) {
           gstop(msg) #util #51)
        }
       }
   
       
      # numeric
         if (is.numeric(entered_date)) {
           gstop(msg) #util #51)
        }
 
           
  }#End is valid date
  
  
#18 Clean up prompt answer, lowercase and no quotes
  strip.prompt <- function(x)
  {
     x <- gsub('`' ,"", x)
     x <- gsub('"' ,"", x)
     x <- gsub("'" ,"", x)
     x <- tolower(x)
     x
  }
  
  
#19 Verify file was downloaded
    exit.if.download.failed <- function(file.name,file.path)
    {
    if (!file.exists(file.path)) {
      message2("Groundhog says: Error!")
      message1("Attempt to download '", file.name, "' failed.")
      message1("We recommend trying again. If the problem persists, ")
      message1("check your internet connection. If you are connected and")
      message1("nevertheless cannot download please check:")
      message1("https://groundhogr.com/troubleshoot")
      message('\ngroundhog.library() request aborted.')
      exit()
    }#End if path exists
  } #End function

#20 Check if lib is for GitHub
    is.lib.GitHub=function(lib.path) {
      pos = regexpr('_GitHub', lib.path)[[1]] 
      return(pos>0) #TRUE if pos>0
      
      }
  
    
#21 Robust reading of text online : try_readlines ()

      try_readLines <- function(url,...) {
    out <- tryCatch(
          {
          readLines(con=url, warn=FALSE, ...) 
            
          },
        error=function(cond) {
            message1(paste("groundhog says: Unable to connect to :", url))
            message1("The error generated by R:")
            message(cond)
            return(FALSE)
            exit()
        },
        warning=function(cond) {
            message1(paste("groundhog says: Unable to connect to :", url))
            message1("The warning generated by R:")
            message(cond)
            return(FALSE)
            exit()
        }
      )    
      return(out)
      }
      
      
  
#Function 22 - Turn dates to unix time
      timestamp.to.time <- function(timestamp)  as.numeric(as.POSIXct(timestamp, format="%Y-%m-%dT%H:%M:%OS"))
      date.to.time      <- function(date)       as.numeric(as.POSIXct(date,      format="%Y-%m-%d"))
      

#Function 23 - string position - simpler gregexpr for strpos
      strpos1 <- function(needle, haystack) as.numeric(gregexpr(needle, haystack)[[1]])
      

      
#Function 25 Compare versions, showing versions that actively mismatch 
      get.mismatched_versions_report <- function (found.pkg_vrs, need.pkg_vrs)
      {
      #Rename
        pkg_vrs1<-unique(found.pkg_vrs)
        pkg_vrs2<-unique(need.pkg_vrs)
        
      #sort
        pkg_vrs1 <- sort(pkg_vrs1)
        pkg_vrs2 <- sort(pkg_vrs2)
        
      #Extract pkg
        pkg1   <-  as.character(sapply(pkg_vrs1, function(x) { strsplit(x,"_")}[[1]][1]))
        pkg2   <-  as.character(sapply(pkg_vrs2, function(x) { strsplit(x,"_")}[[1]][1]))
  
      #Pkg in 1 list that are in the other 
        pkg1.in2 <- pkg1 %in% pkg2
        pkg2.in1 <- pkg2 %in% pkg1
        
      #Subset only if the pkg matches
        pkg1 <- pkg1[pkg1.in2]  
        pkg2 <- pkg2[pkg2.in1]  
        pkg_vrs1 <- pkg_vrs1[pkg1.in2]  
        pkg_vrs2 <- pkg_vrs2[pkg2.in1]  
        
      #Mismatches
        mismatch <- pkg_vrs1 != pkg_vrs2
        
      #Report mismatches
        if (sum(mismatch)==0) return('')
        if (sum(mismatch)>0) {
          msg <- paste0(
                  "Found:\n",paste0(pkg_vrs1,collapse = ' , '),"\n\n",
                  "Needed:\n",paste0(pkg_vrs2,collapse = ' , ')
                  )
          
          return(msg)
        }
        
      }
      

#Function 26 - replace base library
      base.library <- base::library
      
#Function 27
    #Validate arguments
    
      #27.1 TRUE/FALSE
        validate.TF <- function(x)
          {
            if (x==TRUE | x==FALSE) {
              
               return() } else {
                msg=paste0('groundhog says: "' , substitute(x) , '" can only be TRUE or FALSE')
                 gstop(msg) #util #51)
               }
        }
      
        
#Function 28 - abandoned
   
    
#29 Read non dcf DESCRIPTION file saves to data.frame
          read.desc2 = function(filepath) {
            
            #Row to be generated, starts as empty list
              row=list()
            
            #Start the connection
              con = file(filepath, "r")
            #While not empty, read the line
              while ( TRUE ) {
              line = readLines(con, n = 1)
                  if ( length(line) == 0 ) {
                  break
                  }
            #Split based on ":"
                  line_split <- strsplit(line,":")
            #Grab field and value
                  field<-trimws(line_split[[1]][1])
                  value<-trimws(line_split[[1]][2])
            #Save as list element
                  row[[field]]<-value
              }
            #Close connection
                  close(con)
                  
            #Turn list to data.frame
                  return(data.frame(row))
                }
    
          


#31 pasteQC
    pasteQC<-function(x)
      {
      paste0("'", paste0(x ,collapse="', '"),"'")
    }
    
    
#32 infinite.prompt
    
    infinite.prompt <- function(text_msg, valid_answers,must.restart=FALSE)
      {
      
        
      
      #Initialize values
        answer <- ''
        k <- 1
        
      #Loop
        while (!tolower(answer) %in% valid_answers)
        {
           
        message1 (text_msg)
        
         #Truncate answer to first text.max characters when displaying it back to the user
          text.max<-20
          answer2 <- ifelse(nchar(answer)>text.max, paste0(substr(answer,1, text.max),"..."), answer)
      
        #Draft text showing answer
          msg.answer <- paste0('You typed --> "' , answer2 , '".  That is sadly not an accepted response.')
        
        #Add counter if k>1
          if (k>1) {
              msg.answer<-paste0(k,") ",msg.answer)
              }
        #Show feedback on wrong answer
          if (answer!='') {
            message("        ", msg.answer)
            }
        #Show text they MUST restart session
          if (answer!='' && must.restart==TRUE){
            message("        You need to restart the R session to continue")
          }
          
        #Ask for answer
          answer <-readline(prompt = "|   >")
        
        #Add counter
          k=k+1
        } #End while
        return(answer)
    } #End prompt    

    
    
  

    
#33 get.packages_df  -  data.frame with installed packages in local library
    get.packages_df <- function()
      {
      local_library <-   .pkgenv[['default_libpath']][1:(length(.pkgenv[['default_libpath']])-1)]
      path  <- list.files(local_library,full.names=TRUE)
      pkg_current   <- basename(path)
      pkg <- gsub("_DISABLED", "", pkg_current)
      pkg <- gsub("_PURGE", "", pkg)
      disabled <- regexpr('_DISABLED', pkg_current) >0
      purged <- regexpr('_PURGE', pkg_current) >0
      all_df <-data.frame(pkg, pkg_current, path,disabled,purged, stringsAsFactors=FALSE)
      packages_df <- all_df[all_df$pkg!="groundhog",]
      return(packages_df)
    }
      
    
#34 sandwich.library() - turn a string containing library calls into a vector of pkg names
    sandwich.library <- function(x) {
      #1. Early return if 'library' not found
      
        #If x is a vector
          if (length(x)>1) return(x)
      
        #If string does not contain library or require
          if (as.numeric((regexpr('library\\(', x))==-1 & regexpr('require\\(', x))==-1) return(x)
      
      #2. Process assuming it does contain library or require otherwise 
          x <- gsub("library\\(","",x)
          x <- gsub("require\\(","",x)
          x <- gsub("\\)","",x)
          x <- gsub("'","",x)
          x <- gsub("\n"," ",x)
          x <- strsplit(x," ")[[1]]
          x <- x[x!=""]
          return(x)
        } #End of function
    
    
#35 Format msg: format output to have fixed width and starting symbol (e.g., "|    ")
    
    
 format.msg <- function(msg,width=70, header='IMPORTANT.', pre="|")
{
  #Line counter
    j<-0
  #Lines with formatted message starts empty
    msg.lines=c()
  #Turn message into vector of words
    msg.left <- strsplit(msg,' ')[[1]]

  #Loop over lines
    while (length(msg.left)>0)
    {
     j=j+1
     msg.lines[j]=''

    #loop over words
      while (nchar(msg.lines[j]) + nchar(msg.left[1]) <width)
      {
      new.word <- msg.left[1]
      msg.left <- msg.left[-1]
      if (regexpr('\n', new.word)>0) break   #skip line if \n is found
      msg.lines[j] <- paste0(msg.lines[j],new.word," ")   #add the next word
      
      if (length(msg.left)==0) break
    }
      msg.lines[j]<- paste0(pre,"    ", msg.lines[j] ) 
      if (length(msg.left)==0) break
    }
      
  #formatted 
    #Add |  
      msg.lines <- gsub("\n", "\n|", msg.lines)
      
      
    #Join al
      msg.formatted <- paste0(msg.lines,collapse="\n")
      
    #Add header
      msg.formatted <- paste0(pre,header,"\n",msg.formatted)
      
    #Add ------------- on top
      sep.line <- c(paste0(rep('-',width+5)) , "\n" )
      msg.formatted<-c(sep.line, msg.formatted)
    
    return(msg.formatted)
}

    
   
#36 Set default mirror
 set.default.mirror<-function()
 {
  r <- getOption("repos")
      if (regexpr('http', r)[[1]] == -1)
      {
      r["CRAN"] <- "http://cran.r-project.org/" 
      options(repos=r)
      }
  
    #If an instance of R does not have a CRAN mirror, use the cran.r-project.org/ URL by default
  
 }
 
#37 Does personal folder to install R packages (not groundhog, but R's default) exist
    verify.personal.library.exists<-function()
      {
      #Create personal library if it does not exist
         default_library <- Sys.getenv("R_LIBS_USER")
      
         
      #Interactive Session
        if (length(.libPaths()) <= 1 & !file.exists(default_library))
          {
          
          #Start msg
              msg <- paste0("R does not have a personal library to save packages to. ",
                            "The default location for it is: '", default_library,"'. \n ")
          
          
          #Interactive menu
            if (interactive()==TRUE) {
                  msg <-paste0(msg,
                       "   1) Type 'create' to create that directory \n ",
                       "   2) Otherwise type 'stop'")
                
                answer<-infinite.prompt(format.msg(msg),c('create','stop'))
                if (answer=='create') {
                      dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE)
                      }
                
                if (answer=='stop') {
                      exit()
                }} 
          #If script
            if (interactive()==FALSE) {
                  msg <- paste0("To work with groundhog you need to create that library.",
                                 "One way to do that is to run `dir.create('", default_library , "',recursive=TRUE)`")

                  message(msg)
                  exit()
                }
                
                
      }#End if library does not exist
         
       
    
    }#End function
        
#38 restart.text()  - give instructions for restarting in R
    
    restart.text <- function()
    {
      
      text1 <- "(In R Studio press CMD/CTRL-SHIFT-F10) \n "
      text2 <- "(Not using R Studio: type 'quit()' and restart R) \n "
      
      if (Sys.getenv("RSTUDIO")==1) return (text1)
      if (Sys.getenv("RSTUDIO")!=1) return (text2)
      
      
    }

    
#39 save.cookie() and read.cookie()
    
    #39.1 SAVE
        save.cookie <- function(cookie_name)
        {
        #Create cookies directory
          cookies_dir <- paste0(get.groundhog.folder(),"/cookies")
          if (!file.exists(cookies_dir)) dir.create(cookies_dir,recursive=TRUE)
          
        #Cookie path
          cookie_path <- file.path(cookies_dir, paste0(cookie_name,".csv"))
          
        #Save time
          utils::write.csv(as.numeric(Sys.time()),cookie_path,row.names = FALSE)
        }
        
   #39.2 READ
      get.minutes.since.cookie <- function(cookie_name)
      {
      #Cookie path
        cookies_dir <- paste0(get.groundhog.folder(),"/cookies")
        cookie_path <- file.path(cookies_dir, paste0(cookie_name,".csv"))
        
      #Exists? Return 999999 if it does not, contents if it does
        if (!file.exists(cookie_path)) return (999999)
        if (file.exists(cookie_path)) {
          time0 <- utils::read.csv(cookie_path)$x  
          seconds <- as.numeric(Sys.time()-time0)
          minutes <- seconds/60
          return(minutes)
        }
     }#End of read cookie
   
   
#40 get.repos_from.snowball()
      
      get.repos_from.snowball <- function(snowball)
      {
      #If missing, or explicitly MRAN or CRAN, then it is a CRAN package.
        repos <- ifelse(snowball$from %in% c(NA,'', 'CRAN','GRAN','MRAN'),'CRAN',snowball$from)
      return(repos)
      }

    
#41 Add snowball so session data.frame
    
      add.session.snowballs <- function(snowball)
      {
      #The function has as argument snowball_with_repos, to make sure the snowball submitted includes $repos
      # this variable is set in groundhog.library.single() and groundhog.library.single.remote() just before
      # calling on the function update.groundhog.session()
        
      #Add sha if CRAN (this allows later knowing if a package has been loaded from remote)
          if (!'sha'  %in% names(snowball)) snowball$sha=NA
        
      #Add repos
            snowball$repos <- get.repos_from.snowball(snowball)  #function 40,  just above
            
      #Subset of columns from snowball
				    session.snowballs.k      <-snowball[,c('pkg','vrs','pkg_vrs','repos')]
					  
			#Add time
					  session.snowballs.k$time <- as.numeric(Sys.time())
					  
			#Requested
					  session.snowballs.k$requested <- FALSE                  #No package was requested
					  session.snowballs.k$requested[nrow(snowball)] <- TRUE   #Except the last one
					
			#Add to session
					  .pkgenv[['session.snowballs']] <- 	rbind(.pkgenv[['session.snowballs']] , session.snowballs.k )
      }
      
 
      
#42  check.groundhog.version()  - if more than `min.days' days since last check, check if groundhog needs to be updated
      
      check.groundhog.version <- function(min.days=7)
      {
      #Early return if the folder with groundhog has not been set yet 
          main_folder <-  fw(paste0(path.expand("~"), "/R_groundhog")) #fw: function #50
          if (!file.exists(main_folder)) return(invisible(""))
        
      #Start with high numbers  
        last.check.days=9999  
        
      #Skip cookie time if min.days=0
          if (min.days>0)
          {
        
      #How many days has it been since we checked
          last.check.minutes <- get.minutes.since.cookie('check_groundhog_version')
          last.check.days    <- (last.check.minutes/60)/24
          save.cookie('check_groundhog_version')
        
      #If less than min.days, early return
          if (last.check.days<min.days) return(invisible(""))
      
          } #End if min.days>0
        
      #If more than `min.days` days, check version on server
          if (last.check.days>=min.days)
              {
              #Try to read from groundhogr.com   
                  groundhog.version_cran <- tryCatch(
                  as.character(readLines("https://groundhogr.com/groundhog_version.txt")),
                  warning = function(w) NULL,
                  error = function(e) NULL
                )
              
              #If NULL then early return
                  if (is.null(groundhog.version_cran)) return(invisible(""))
              
              #Get majmin
                groundhog.version_using <- as.character(packageVersion("groundhog"))
                gv.using <- as.numeric(strsplit(groundhog.version_using, "\\.")[[1]])
                gv.cran  <- as.numeric(strsplit(groundhog.version_cran, "\\.")[[1]])
                gv.using.majmin <-  10000*gv.using[1] + gv.using[2]
                gv.cran.majmin  <-  10000*gv.cran[1]  + gv.cran[2]

              #If server's is bigger, prompt to update
                if (isTRUE(gv.cran.majmin > gv.using.majmin)) {
                    msg<-paste0(
                    "########################################################\n",
                    "OUTDATED GROUNDHOG\n",
                    "You are using version  '" , groundhog.version_using, "'\n",
                    "The current version is '" , groundhog.version_cran, "'\n\n",
                    "You can read about the changes here:\n",
                    "https://groundhogr.com/changelog\n\n",
                    "Update by running: `install.packages('groundhog')`\n",
                    "########################################################\n"
                    )
                    return(msg)
                    }  #End mismatch in version
                
                  
            } #End last check more than `min.days`  ago
      }#End of function 42
      
#43 Get operating system
  get.os <- function()
  {
    
  #Default
    os <- 'other'

  #If it has been set by hand, read it
    path <-  paste0(path.expand("~"), "/R_groundhog/options/os.txt") #see groundhog.options.R
    if (file.exists(path)) {
      os = scan(path,what='character',quiet=TRUE)
      if (os!='other') return(os)
    }
    
    #There was a plan to have a groundhog.option() function but was abandoned.
    #This is a placeholder, if there were to be an issue with some OS not being recognized
    #users could by hand add this file, write their os and groundhog would work
    #but if this ends up affecting more users groundhog will be changed to have a more robust os detection.
    
  # use contrib.url() to rely on R's processing of sys.info() alternatives
    repos <- as.character(getOption("repos"))
    bin.url <- utils::contrib.url(repos,type='binary')
    if (regexpr('windows', bin.url)[[1]]>0) os<-'windows'
    if (regexpr('macosx', bin.url)[[1]]>0)  os<-'mac'
    if (regexpr('arm64', bin.url)[[1]]>0 &  os=='mac') os<-'mac_arm'
  
    
  #If other show warning
    return(os)
  }
    

    
#44 Check consent has been given to save files locally
      check.consent <- function(ask=TRUE) {
        
      #Folder with cookie with location of groundhog folder, its existence means consent
         main_folder <-  fw(paste0(path.expand("~"), "/R_groundhog")) #fw: function #50
        
      #See if consent has been given by seeing if the folder exists
        consent <- (file.exists(main_folder))
        if (consent==TRUE) return(TRUE)
        if (consent==FALSE & ask==FALSE) return(FALSE)

        
       #If no consent, ask 
         if (consent == FALSE & ask==TRUE) {
            msg       = paste0("groundhog needs authorization to save files to '",main_folder,
                        "'\n", "Enter 'OK' to provide authorization, and 'NO' not to.")
            
            batch_msg =  paste0("\n\n*********IMPORTANT MESSAGE FROM GROUNDHOG*****************************************\n",
                         "groundhog needs to save packages in a local directory in order to work.\n",
                         "Per CRAN policy, you need to actively authorize groundhog to do this.\n",
                         "Please run `set.groundhog.folder(<path>)` for the path you wish to use.\n",
                         "If you are unsure which path to use, the following path is a reasonable default:\n",
                         "   '", main_folder , "'\n",
                         "(This only needs to be done once on a given computer)\n",
                         "**********************************************************************************\n\n")
          #For batch files
            if (interactive()==FALSE)
            {
              message(batch_msg)
              return(FALSE)
              
            }
            
            
          #Run the prompt
           answer = infinite.prompt(msg, c('ok','no') , must.restart=FALSE)
            
          #Interactive message, if using R interactively
            if (tolower(answer)=="ok")  {
              dir.create(main_folder, recursive = TRUE, showWarnings = FALSE)
              return(TRUE)
              }
            if (tolower(answer)=="no") {
              message("You did not provide permission to save files locally, groundhog will not work until you do.")
              return(FALSE)
              }
         } #End  if no consent and we are asking for it
        
        
      }#End of function
          
#45 Turn \ into /
      fw <- function(x) gsub("\\\\", "/", x)
      
      

#46 Moved to its own function
#47 DROPPED
#48 Moved to its own function
 
  
#49 - get.parallel.time()  :  estimate parallel time of installation
get.parallel.time<-function(times,cores)
{
  #Sort times
    times <- sort(times,decreasing=TRUE)
    
  #initiates times with small but sortable values
    y = seq(0,.01,length.out=cores) 

  #In loop, assign the next one, to the lowest total so far
    for (k in 1:length(times))
    {
    y[which.min(y)]=y[which.min(y)] + times[k]  
    }
  #The longest link is the estimated time
    return(max(y))
}


#50 get.pkg_list
  get.remote_df.from.path<-function(snowball_path)
  {
    #Split path
      parts <- strsplit(snowball_path,"/")[[1]]
      n <- length(parts)
      remote_id <- parts[n-1]
      remote_id <- gsub("_", "", remote_id)
    
    #Split usr/pkg
      parts2 <- strsplit(parts[n],"_")[[1]]
      usr <- parts2[1]
      pkg <- parts2[2]
      sha <- parts2[3]
      
    return(data.frame(remote_id = remote_id , usr=usr, pkg=pkg, sha=sha))  
  }
      
      
#51 stop
  gstop <- function(msg,format=FALSE) {
    #Format the message with line breaks and border if requested
    if (format==TRUE) msg=format.msg(msg) #util #35
    message1(msg)
    message("**Groundhog stopped**")
    exit()
    }
  
#52 read/save purge and grounhog.installed. rds
  
  read.local.rds <- function(filename)
  {
    #Using R 
    r.version    <- get.r.majmin()
    
    #Path to files
      dir <- paste0(get.groundhog.folder() , "/R-" , r.version ,"/rds_files/")
      path <- file.path(dir,filename)
      
      
    #Read the file
      df <- data.frame()
      if (file.exists(path)) df<-readRDS(path)

    #Return 
      return(df)
  }
  
  save.local.rds <- function(df, filename)
  {
    #Using R 
    r.version    <- get.r.majmin()
    
    
  
    #Path to files
      dir<- paste0(get.groundhog.folder() , "/R-" , r.version ,"/rds_files/")
      path <- file.path(dir,filename)
  
     #Ensure dir exists  
      dir.create(dir,showWarnings = FALSE,recursive = TRUE)
  
    #Save
      saveRDS(df,path,version=2)
  }
  

#53 Download in batches
  download.files.in_batches <- function(url.files , zip.files , batch.size = 20)
    {
    #Sort alphabetically by package name
      j  <- order(basename(url.files))
      url.files <- url.files[j]
      zip.files <- zip.files[j]
    
    #Split the vectors
      #https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks
      url.split <- split(url.files, ceiling(seq_along(url.files)/batch.size))
      zip.split <- split(zip.files, ceiling(seq_along(zip.files)/batch.size))
      btot <- length(url.split)  
      
  

    #Download in loop
      for (bk in 1:btot)
      {
      if (btot>1) message2("Batch ",bk, " of ", btot,". Downloading the following files:")
      message1("     ",paste(url.split[[bk]],collapse='\n     '))
      
      #In R-3.6.3 a warning is show about using a vector, but it works with vectors, so turning it off here
        w0 <- getOption('warn')
        options(warn=-1)      
        
        try (utils::download.file(url.split[[bk]], zip.split[[bk]],quiet=TRUE,method='libcurl'))
        
        #Turn warning back on
        options(warn=w0) 
          
      
      }
      
  }#End of #53
  
  
#54 filesize_format
  #(written by chatGPT, to avoid relying on utils:::)
  filesize_format <- function(size_in_bytes) {
  if(size_in_bytes < 1024) {
    return(paste0(size_in_bytes, " B"))
  }
  units <- c("KB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB")
  u <- -1
  while(size_in_bytes >= 1024 & u < length(units)) {
    size_in_bytes <- size_in_bytes / 1024
    u <- u + 1
  }
  return(paste0(round(size_in_bytes, 1), " ", units[u+1]))
  }
  
  
#55 get.restore.points  
  
  get.restore.points <- function()
  {
    #0 Check consent to avoid zzz..R triggering loop
        if (check.consent(ask=FALSE)==FALSE) {  
          return(data.frame(date='',days='')[0,])
          }
  
    #1 Path
      restore_dir <- paste0(get.groundhog.folder(),"/restore_points/", get.r.majmin())
      dir.create(restore_dir, recursive = TRUE,showWarnings = FALSE)    

      
    #2 Files available
      restore_files <- list.files(restore_dir)
        
    #3 If none, end
        if (length(restore_files)==0) {  
          return(data.frame(date='',days='')[0,])
          }
        
    #4 Turn filenames to dates
      restore_dates <- as.Date(substr(restore_files, 0,10))  
    
    #5 Make data.frame
      df <- data.frame(dates=restore_dates, days = as.numeric(Sys.Date()- restore_dates))
      
    #6 Return it
      return(df)
    
  }
  
#57 view conflicts
  view.conflicts.function <- function() {
    return(.pkgenv[['conflicts']])
    }

  
#58 Get ip.groundhog()
  location='backup'
  get.ip <- function(location)
  {
    #1 Get all subfolders for backup and groundhog
    
        #1.1 For groundhog. backup, all_local there is a folder with subfolders for each pkg, get all pkgs
            if (location %in% c('backup','groundhog','all_local'))
                {
                 #Path containing all subfolders with pkg_vrs   
                  if (location=='groundhog') {
                        cran_path   <- paste0(get.groundhog.folder() , "/R-" , get.r.majmin())
                        github_path <- paste0(cran_path,"/_github")
                        gitlab_path <- paste0(cran_path,"/_gitlab")
                        master_path=c(cran_path, github_path, gitlab_path)
                      }
                    
                    
                  if (location=='backup')    master_path <- paste0(get.groundhog.folder(),"/restore_library/" , get.r.majmin() , "/")
                  if (location=='all_local') master_path <- .pkgenv[["orig_lib_paths"]]

                #All pkgs in that path    
                  all.paths<- list.files(master_path,full.names=TRUE)
            }

        #1.2 For local there is just one 
          if (location=='local') all.paths <- .pkgenv[["orig_lib_paths"]][1]
      
          
  
     #2 Get the installed.packages
        ip <- data.frame(utils::installed.packages(all.paths), row.names = NULL, stringsAsFactors = FALSE)
         
     #3 Create pkg_vrs
        if (nrow(ip)>0)  ip$pkg_vrs <- paste0(ip$Package,"_",ip$Version)
        if (nrow(ip)==0) ip$pkg_vrs <- character()
        
      
     #4 Select columns
        ip <- ip[,c(names(ip) %in% c("LibPath", "Package","Version","pkg_vrs"))]
        
     #5 Add MD5 for DESCRIPTION file to merge & compare with `loans`
        if (nrow(ip)>0)  {
            description.path <- paste0(ip$LibPath, "/" , ip$Package , "/DESCRIPTION")
            ip$md5 <- tools::md5sum(description.path)
            }
          
        if (nrow(ip)==0) ip$md5 <- character()
        
    #6 End
        return(ip)
    }
        
  
#60 Loans
  #60.1 get 
  #pkg_vrs, location, 
  get.loans<-function(verfiy.package.exists=TRUE) {
    
    #Start empty
    loans <- data.frame(pkg_vrs=character() , 
                        groundhog_location=character(),    #to move it back here
                        sha=c(),                           #sha to match a pkg in snowball with what's already loaded
                        md5=c())                           #MD5 of DESCRIPTION file to ensure right pkg is selected
    
    #Read it if it exists
      loans_path<-paste0(get.groundhog.folder(),"/loans/",get.r.majmin(),".rds")  
      dir.create(dirname(loans_path), showWarnings = FALSE,recursive = TRUE)
      if (file.exists(loans_path)) loans<-readRDS(loans_path)
      
    #Sort it
      loans<-loans[order(loans$pkg_vrs),]
      
    #Verify package exists in local folder
      if (verfiy.package.exists==TRUE)
      {
        ip.local <- get.ip('local') #utils #58
        loans   <- loans[loans$md5 %in% ip.local$md5,]
      }
       
    #output
      return(loans)
  }
  
  #60.2 Save loans
   save.loans<-function(loans) {
     loans_path<-paste0(get.groundhog.folder(),"/loans/",get.r.majmin(),".rds")  
     dir.create(dirname(loans_path), showWarnings = FALSE,recursive = TRUE)
     saveRDS(loans,loans_path,version=2,compress=FALSE)
      }