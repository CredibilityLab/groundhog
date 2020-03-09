#1. Preliminaries
#1.1 Set folder for Groundhog R  files
#[this will be changed]
chooseCRANmirror(graphics=FALSE, ind=1)


groundhogR.folder=paste0(path.expand('~'),"/groundhogR")
groundhogR.url="http://groundhogR.com"

#1.2 Get available packages to see if each attempted to install is new
current.packages=data.frame(available.packages())[,c(1,2)]
current.packages$pkg_vrs=paste0(current.packages$Package,"_",current.packages$Version)

#1.3 base packages
ip=data.frame(installed.packages())
dep.base=as.character(subset(ip,Priority=='base')$Package)

#2.0.5 Parse pkg_vrs into pkg and vrs
get.pkg=function(x) substr(x,1,regexpr('_', basename(x))-1)
get.vrs=function(x) substr(x,regexpr('_', basename(x))+1,nchar(x))

#2.16 Is pkg_vrs installed (within same R-minor version)
is.pkg_vrs.installed=function(pkg,vrs) (get.installed_path(pkg,vrs) %in% get.pkg_search_paths(pkg,vrs))

#' Formamt Y-M-D as date
as.DateYMD = function(x) as.Date(x, format="%Y-%m-%d")

#2.2  R Being used
#2.2.1 R Date
get.rdate=function()     {
  r.current=R.version$version.string
  date=paste0(R.version$year,"-",R.version$month,"-",R.version$day)
  date=(date)
  return(date)
}

#2.2.2 R Version
get.rversion=function()     {
  r.version=paste0(R.version$major,".",R.version$minor)
  return(r.version)
}

#2.5 Colored cat()
#source("https://raw.githubusercontent.com/r-lib/testthat/717b02164def5c1f027d3a20b889dae35428b6d7/R/colour-text.r")
colourise <- function(text, fg = "black", bg = NULL) {
  term <- Sys.getenv()["TERM"]
  colour_terms <- c("xterm-color","xterm-256color", "screen", "screen-256color")

  if(rcmd_running() || !any(term %in% colour_terms, na.rm = TRUE))  return(text)

  col_escape <- function(col) paste0("\033[", col, "m")

  col <- .fg_colours[tolower(fg)]
  if (!is.null(bg)) col <- paste0(col, .bg_colours[tolower(bg)], sep = ";")

  init <- col_escape(col)
  reset <- col_escape("0")
  paste0(init, text, reset)
}

.fg_colours<-c("black"="0;30","blue"="0;34","green"="0;32","cyan"="0;36","red"="0;31","purple"="0;35","brown"="0;33","lightgray"="0;37","darkgray"="1;30","lightblue"="1;34","lightgreen"="1;32","lightcyan"="1;36","lightred"="1;31","lightpurple"="1;35","yellow"="1;33","white"="1;37")
.bg_colours<-c("black"="40","red"="41","green"="42","brown"="43","blue"="44","purple"="45","cyan"="46","light gray" = "47")

rcmd_running <- function() {  nchar(Sys.getenv('R_TESTS')) != 0
}#End colourise

#2.5.1 Simplified cat functions for just one pre-specified color used throughout groundhogR
cat1 = function(msg)   cat(colourise(msg, "cyan"), "\n")         #normal
cat2 = function(msg="") {
  if (msg=="")  cat(colourise(paste0("\ngroundhog.library() says [using R-",get.rversion(),"]:"), "lightcyan"), "\n")       #BOLD
  if (msg!="")  cat(colourise(msg, "lightcyan"), "\n")       #BOLD
}

#2.8 Automatically name elements in list with name of the objects in the list
#https://stackoverflow.com/questions/16951080/can-lists-be-created-that-name-themselves-based-on-input-object-names
namedList <- function(...) {
  L <- list(...)
  snm <- sapply(substitute(list(...)),deparse)[-1]
  if (is.null(nm <- names(L))) nm <- snm
  if (any(nonames <- nm=="")) nm[nonames] <- snm[nonames]
  setNames(L,nm)
}

#2.10 Quit menu
quit.menu= function(date) {
  cat1("Type 'Q' to stop the script\nAnything else to continue")
  x = readline("")
  if (tolower(x)=="q" | tolower(x)=="quit" | tolower(x)=="stop") {
    cat2()
    cat1(paste0("You typed ",x," so script stops..."))
    cat1(msg.R.switch(date))
    stop('---')
  } #End if quit

  if (tolower(x)!="q") cat1(paste0("You typed '",x,"' the script continues..."))

}#End quit.menu

#2.18 Plot console
cat1.plot=function(x)
{
  #Get existing margins to return to them after console use
  old.par <-   par(mar=c(.25,.25,.25,.25))

  #Catch user's attention
  #    plot(c(.25,.5,.75),c(.5,.5,.5),cex=10,pch=16,col='red',xlim=c(0,1))
  # Sys.sleep(.75)
  #Set no margins
  plot(1,1,col='white',xaxt ='n',yaxt='n',xlab='',ylab='',xlim=c(0,1),ylim=c(0,1))
  text(.5,1,"groundhogR's Console",font=2,col='cyan4')
  text(0,.85,adj=c(0,1),x,font=1,cex=.9,col='cyan4')
  segments(x0=0,x1=.4,y1=.15,y0=.15,col='cyan4')
  text(0,.1,font=1,pos=4,cex=.75,col='cyan4',"You can avoid this console by running:\ngroundhog.library(..., plot.console=FALSE)")

  #Return margins to defaults
  par(old.par)
}
