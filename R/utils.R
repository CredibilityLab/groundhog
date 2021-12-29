#This script has functions used throughout the package

#1  Exract package and version information from pkg_vrs
#2  Is pkg_vrs installed (within same R-minor version)?
#3  Format Y-M-D as date
#4  R beind used
#5  Get R Version
#6  Message1()
#7  Message2()
#8  Named list
#9  Quit menu
#10 exit() Stop message which does not say error
#11 Available mran dates
#12 Base packages
#13 Default packages to ignore conflicts with (but gives warning)
#14 Is this on R studio
#15 Get major minor (but no patch) version of R
#16 Get major minor AND patch
#17 validate date  
#18 Clean up prompt answer, lowercase and no quotes
#19 Verify file was download
  
  
########################################################################
    


#1. Exract package and version information from pkg_vrs
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
      setNames(L, nm)
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
      c(
        "base",
        "compiler",
        "datasets",
        "graphics",
        "grDevices",
        "grid",
        "methods",
        "parallel",
        "splines",
        "stats",
        "stats4",
        "tcltk",
        "tools",
        "utils"
      )
    }

#13 Default packages to ignore conflicts with (but gives warning)
ignore.deps_default <- function() {
  
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
  validate.date <- function(date)
      {
      # numeric
        bad.date <- 0
         if (is.numeric(date)) {
          bad.date <- 1
        }
 
      # correct format
        d <- try(as.Date(date, format="%Y-%m-%d"))
          if("try-error" %in% class(d) || is.na(d)) {
            bad.date <- 1
          }
            
      #If bad date die 
          if (bad.date==1) {
            message(
                "\ngroundhog says: error!\n",
                "The date you entered '", date,"', is not valid.\n",
                "Please use the 'yyyy-mm-dd' format"
                  )
            exit()
          }
  }#End is valid date
  
  
#18 Clean up prompt asnwer, lowercase and no quotes
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

  
  
    
    