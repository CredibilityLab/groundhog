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
#11 get.available.mran.date: Available mran dates
#12 base_pkg()                : Output vector with all base packages
#13 ignore.deps_default()     :  Default packages to ignore conflicts with (but gives warning)
#14 Is this on R studio
#15 get.r.majmin              : Get major minor (but no patch) version of R
#16 get.r.majmin.release()    : Get major minor AND patch
#17 validate.date()           : Validate date  
#18 strip.prompt)_            : Clean up prompt answer, lowercase and no quotes
#19 exit.if.download.failed() : Verify file was download
#20 Check if lib is for github (possibly a legacy function)
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
#38 restart.text()             :  tells user to either quit() R or use CMD-SHIF-F10 based on whether they use R Studio
#39 get.minutes.since.cookie() :  reads csv file with Sys.time() of last time this cookie was saved
#40 get.repos_from.snowball()  :  infer repository (cran, github, gitlab) from other information already in the snowball
#41 update.groundhog.session() :  Update groundhog session with new packages loaded and requested with groundhog
#42 check.groundhog.version()  :  if more than `min.days' days since last check, check if groundhog needs to be updated
########################################################################
    


#1. get.pkg get.vrs -  Exract package and version information from pkg_vrs
  get.pkg <- function(x) substr(x, 1, regexpr("_", basename(x)) - 1)
  get.vrs <- function(x) substr(x, regexpr("_", basename(x)) + 1, nchar(x))

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

#4. R beind used
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
    } # End if quit
  
    message1("You typed '", x, "' the script continues...")
  } # End quit.menu

#10 exit() Stop message which does not say error
  exit <- function(...) {
    message1(...)
    
     #Read cran toc again to undo any changes with remote
        #Only run if main folder exists, otherwise we have loop, 
        #exiting when it does not exist, and asking that it exist when exitin
          main_folder <-  paste0(path.expand("~"), "/R_groundhog")
          if (file.exists(main_folder)) {
            .pkgenv[['cran.toc']] <- readRDS(file.path(get.groundhog.folder(),"cran.toc.rds"))
          }     
          
    #Return libpath
       .libPaths(.pkgenv[["orig_lib_paths"]])
        invokeRestart("abort")
  }

#11 Available mran dates
get.available.mran.date <- function(date0, date1) {
  missing.mran.dates <- .pkgenv[["missing.mran.dates"]]

  all.dates <- date0:date1 # All dates in range
  available.dates <- all.dates[!all.dates %in% missing.mran.dates] # Those that are not missing
  if (length(available.dates) == 0) {
    return(as.Date("1970-01-01"))
  } # If none remain, end

  # Report mid value
  n.dates <- length(available.dates)

  if (n.dates == 0) {
    message1(
      "We looked for the version of the package you need in MRAN ",
      "but it was not found there"
    )
    exit()
  }

  # ceiling() rather than floor() or round() to work when n.dates <- 1
  mid.date.k <- ceiling(n.dates / 2)
  mid.date <- available.dates[mid.date.k]
  return(as.Date(mid.date, origin = "1970-01-01"))
} # End of function

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
      ip <- data.frame(utils::installed.packages(),stringsAsFactors = FALSE)
      recommended.pkgs <- unique(subset(ip, ip$Priority=="recommended")$Package) #unique becuase there may be two versions of the same package in different libraries
      
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
   
#16 Get major minor pach
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
       msg=paste0("\ngroundhog says: The date you entered '", entered_date,"', is not valid.\n",
                "Please use the 'yyyy-mm-dd' format"
          )
    
       
       # correct format
        d <- try(as.Date(entered_date, format="%Y-%m-%d"),silent = TRUE)
          if ("try-error" %in% class(d) || is.na(d)) {
              message1(msg)
               exit()
          }

       
              
      #The format check does not verify that the day is at most 2 charcters long,  (e.g., it accepts 2022-01-109)
       if (is.character(entered_date)) { 
       d.parts <- strsplit(entered_date,"-")[[1]]     #split date by '-
        if (nchar(d.parts[3])>2) {
            message1(msg)
            exit()
        }
       }
   
       
      # numeric
         if (is.numeric(entered_date)) {
          message1(msg)
          exit()
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
  
  
#19 Verify file was download
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

#20 Check if lib is for github
    is.lib.github=function(lib.path) {
      pos = regexpr('_github', lib.path)[[1]] 
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
      

#Function 23 - string positiong - simpler gregexpr for strpos
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
                message('groundhog says: "' , substitute(x) , '" can only be TRUE or FALSE')
                 exit()
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
      
        #Draft text showign answer
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
        } #end while
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
        } #ENd of function
    
    
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
 
#37 Does personal folder exist
    verify.personal.library.exists<-function()
      {
      #Create personal library if it does not exist
      default_library <- Sys.getenv("R_LIBS_USER")
      
      if (length(.libPaths()) <= 1 & !file.exists(default_library)) {
        msg <- paste0("R does not have a personal library to save packages to. ",
               "The default location for it is: '", default_library,"'. \n ",
               "   1) Type 'create' to create that directory \n ",
               "   2) Otherwise type 'stop'")
        
        answer<-infinite.prompt(format.msg(msg),c('create','stop'))
        if (answer=='create') {
              dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE)
              }
        
        if (answer=='stop') {
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

    cookie_name='warng1'
    
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
        repos <- ifelse(snowball$from %in% c(NA< 'CRAN','MRAN'),'CRAN',snowball$from)
      return(repos)
      }

    
#41 Update groundhog session with new packages loaded and requested with groundhog
      update.groundhog.session <- function(snowball)
      {
      #The fucntion has as argument snowball_with_repos, to make sure the snowball submitted includes $repos
      # this variable is set in groundhog.library.single() and groundhog.library.single.remote() just before
      # calling on the function update.groundhog.session()
        
      #Add repos
            snowball$repos <- get.repos_from.snowball(snowball)  #function 40,  just above
            
      #Get requested pkg from snowball
            pkg <- snowball$pkg[nrow(snowball)]
          
      #Subset from snowball
				    groundhog.session_df.k <-snowball[,c('pkg','vrs','pkg_vrs','repos')]
					  
			#Add time
					  groundhog.session_df.k$time <- as.numeric(Sys.time())
					  
			#TRUE FALSE for whether package was explicitly requested
					  groundhog.session_df.k $requested <- groundhog.session_df.k$pkg == pkg     
					  
			#ADD NEW ROWS
					  .pkgenv[['groundhog.session_df']] <- 	rbind(.pkgenv[['groundhog.session_df']] , groundhog.session_df.k )
      }
      
        
#42  check.groundhog.version()  - if more than `min.days' days since last check, check if goundhog needs to be updated
      
      check.groundhog.version <- function(min.days=7)
      {
        
      #How many days has it been
          last.check.minutes <- get.minutes.since.cookie('check_groundhog_version')
          last.check.days    <- (last.check.minutes/60)/24
        
      #If less than min.days, early return
          if (last.check.days<min.days) return(invisible(TRUE))
          
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
                  if (is.null(groundhog.version_cran)) return(invisible(FALSE))
              
              #Get majmin
                groundhog.version_using <- as.character(packageVersion("groundhog"))
                gv.using <- as.numeric(strsplit(groundhog.version_using, "\\.")[[1]])
                gv.cran  <- as.numeric(strsplit(groundhog.version_cran, "\\.")[[1]])
                gv.using.majmin <-  10000*gv.using[1] + gv.using[2]
                gv.cran.majmin  <-  10000*gv.cran[1]  + gv.cran[2]
    
    
              #If server's is bigger, prompt to update
                if (isTRUE(gv.cran.majmin > gv.using.majmin)) {
                    message2()
                    message1(
                    "\n\n\n",
                    "          OUTDATED GROUNDHOG\n",
                    "            You are using version  '" , groundhog.version_using, "\n",
                    "            The current version is '" , groundhog.version_cran, "'\n\n",
                    "            You can read about the changes here: https://groundhogr.com/changelog\n\n",
                    "Update by running: \ninstall.packages('groundhog')"
                    )
                    }  #End mismatch in version
                  
            } #ENd last check more than `min.days`  ago
      }#End of function 42
      
#43 Get operating system
  get.os <- function()
  {
  # use contrib.url() to rely on R's processing of sys.info() alternatives
    repos <- as.character(getOption("repos"))
    bin.url <- contrib.url(repos,type='binary')
    os <- 'unknown'
    if (regexpr('windows', bin.url)[[1]]>0) os<-'windows'
    if (regexpr('macosx', bin.url)[[1]]>0) os<-'mac'
    if (regexpr('arm64', bin.url)[[1]]>0 & os=='mac') os<-'mac_arm'
    return(os)
  }
    
    