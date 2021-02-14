# 2.0.5 Parse pkg_vrs into pkg and vrs
get.pkg <- function(x) substr(x, 1, regexpr("_", basename(x)) - 1)
get.vrs <- function(x) substr(x, regexpr("_", basename(x)) + 1, nchar(x))

# Is pkg_vrs installed (within same R-minor version)?
#
is.pkg_vrs.installed <- function(pkg, vrs) {
  (get.installed_path(pkg, vrs) %in% get.pkg_search_paths(pkg, vrs))
}

# Format Y-M-D as date
#
# @param x character string containing the date in the format "%Y-%m-%d"
#
as.DateYMD <- function(x) as.Date(x, format = "%Y-%m-%d",origin='1970-01-01')

# 2.2  R Being used
# 2.2.1 R Date
get.rdate <- function() {
  date <- paste0(R.version$year, "-", R.version$month, "-", R.version$day)
  return(as.DateYMD(date))
}

# 2.2.2 R Version
get.rversion <- function() {
  r.version <- paste0(R.version$major, ".", R.version$minor)
  return(r.version)
}

# message1() are messages that are coloured if the terminal supports it and
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
# 2.8 Automatically name elements in list with name of the objects in the list
# https://stackoverflow.com/questions/16951080/can-lists-be-created-that-name-themselves-based-on-input-object-names
#' @importFrom stats setNames
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

# 2.10 Quit menu
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

# Stop message which does not say error
exit <- function(...) {
  message1(...)
  invokeRestart("abort")
}

# Function added on 2020 05 18
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

#Packages that R Studio loads automatically and other packages likely to be loaded without explicit calls
ignore.deps_default <- function() {
  c("testthat", 
    "rstudioapi",
    "knitr",      #Loaded by R STudio for .rmd files
    "rmarkdown",  #Loaded by R STudio for .rmd files
    "xfun"        #Loaded by R STudio for .rmd files
    )
}




is_rstudio <- function() {
  # More reliable than the env variable because it works as expected even when
  # code is called from the Terminal tab in RStudio (NOT the Console).
  identical(.Platform$GUI, "RStudio")
}





get.r.majmin <- function() {
   major <- as.numeric(R.version$major)
   minor <- as.numeric(strsplit(R.version$minor, "\\.")[[1]][1])
   majmin <- paste0(major, ".", minor)
   return(majmin)
   }
   
 get.r.majmin.release <- function()
 {
   r.majmin <- get.r.majmin()
   R.toc <- toc("R") # Get R toc
   R_same.majmin <- grep(paste0("^", r.majmin), R.toc$Version, value = TRUE)
   R1 <- R_same.majmin[1]
   release.date <- R.toc[R.toc$Version==R1,]$Published
   return(release.date)
    }


  
 