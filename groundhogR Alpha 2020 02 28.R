

     rm(list = ls())

#1. Preliminaries
  #1.1 Set folder for Groundhog R  files
      #[this will be changed]
     groundhogR.folder=paste0(path.expand('~'),"/groundhogR")
     groundhogR.url="http://groundhogR.com"

  #1.2 Get available packages to see if each attempted to install is new
      current.packages=data.frame(available.packages())[,c(1,2)]
      current.packages$pkg_vrs=paste0(current.packages$Package,"_",current.packages$Version)

  #1.3 base packages
    ip=data.frame(installed.packages())
    dep.base=as.character(subset(ip,Priority=='base')$Package)

  #1.4 Messages
    msg.R.switch=function(date) {
          rv=r.version.check(date)
          paste0("####################################################################################################\n",
          "INSTRUCTIONS TO RUN OLDER R VERSIONS IN R STUDIO:\n(copy-paste somewhere to have available when you close this window)\n\n",
          "WINDOWS:\n",
          "  1) Download R-",rv$r.need.full," from https://cran.r-project.org/bin/windows/base/old/\n",
          "  2) Install it (e.g., double click on downloaded file)\n",
          "  3) Either: Restart R Studio pressing down the CTRL key\n",
          "     OR:     Within R Studio: Tools->Global Options->(first textbox)\n\n\n",
          "MAC:\n",
          "  1) Download R-",rv$r.need.full," from\n",
          "     https://cran.r-project.org/bin/macosx/old/               [up to R-3.3.3]\n",
          "     https://cran.r-project.org/bin/macosx/el-capitan/base/   [since R-3.4.0]\n\n",

          "  2) Switch which R you use with a utility called 'R Switch'\navailable from: https://mac.r-project.org/RSwitch-1.2.dmg).\n\n\n",
          "LINUX and more details for Windows and Mac: http://tiny.cc/SwitchR\n",
          " ####################################################################################################\n")
    }

  hr="\n-------------------------------"


#2 Auxiliary functions

#2.0 Formamt Y-M-D as date
      as.DateYMD=function(x) as.Date(x, format="%Y-%m-%d")

#2.0.5 Parse pkg_vrs into pkg and vrs
     get.pkg=function(x) substr(x,1,regexpr('_', basename(x))-1)
     get.vrs=function(x) substr(x,regexpr('_', basename(x))+1,nchar(x))

#2.1 Show table of contents [toc] (package versions and publication dates)  for a pkg, sorted chronologically
    toc=function(pkg,dependencies=FALSE) {
     if (!exists("cran.toc")) load.cran.toc(update.toc=FALSE)
     if ( dependencies)  output=subset(cran.toc,Package==pkg)[,c("Version","Published","Imports","Depends","Suggests")]
     if (!dependencies)  output=subset(cran.toc,Package==pkg)[,c("Version","Published")]

     if (nrow(output)==0) {
       cat2()
       cat1(paste0("There is no package '",pkg, "' in our database of all CRAN packages ever posted.\n",
                   "   Keep in mind that:\n",
                   "    1. package names are cAsE seNsiTive\n",
                   "    2. The package name needs to be in quotes: e.g., toc('groundhogR') \n",
                   "    3. Only CRAN packages can be loaded via groundhogR"))
       stop()
      }

     output=output[order(output$Published),]
     return(output)
    }



#2.1.5 Crosstoc: show toc table with multiple packages
    cross.toc=function(pkgs,date1="1970-1-1",date2=Sys.Date())
    {

      if (!exists("cran.toc")) load.cran.toc(update.toc=FALSE)
      n=length(pkgs)
      toc.all=toc(pkgs[1])
      toc.all$Package=pkgs[1]
      for (k in 2:n)
        {
        tock=toc(pkgs[k])
        tock$Package=pkgs[k]
        toc.all=rbind(toc.all,tock )
      }

     #Sort
        toc.all=toc.all[order(toc.all$Published),]
      #date subset
        return(subset(toc.all,Published>date1 & Published<date2))

    }



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

#2.3 Get version from date
     get.version=function(pkg,date, current.deps=c("Rcpp"))  {
        #1. Get toc
          dfk=toc(pkg)
      #2 Validate
        #2.1 Check if Package exists in our records
           if (nrow(dfk)==0) {
               cat2()
               cat1 (paste0("groundhog.library() does not have the package ",pkg," indexed so it cannot install or use it. Note: It only has CRAN packages"))
              stop("")
              }
        #2.2 Check if date request comes after first date for that package
              if (dfk$Published[1]>date) {
               cat2()
               cat1 (paste0("According to our records, the package: '",pkg,"' was not yet available on CRAN on '",date,"'"))
              stop("")
              }
        #2.3 Check if date requested comes before most up to date date
              last.toc.date=max(cran.toc$Published,na.rm=T)
              if (date>last.toc.date) stop ("groundhog.library() index of packages ends on  ",last.toc.date,
                                          " which is before the date you entered:",date,".\nThe index updates automatically ",
                                           "every time you call groundhog.library()")
     #3 If pkg is in current.deps, deliver version for current version of R
      if (pkg %in% current.deps) {
          using.r.rdate=get.rdate()                          #date of R being used
          version.k=max(which(dfk$Published<using.r.rdate))  #which kth date of pkg matches it
          vrs=dfk[version.k,]$Version                        #get kth version
          return(vrs)
       }


     #4 Get version
          version.k=max(which(dfk$Published<date))  #Position of last package available before the date
          return(dfk$Version[version.k])            #Return that kth element
     }



  #2.3.6 Get installation time
      get.installation.time=function(pkg,vrs)         {
        dfk=subset(cran.times,pkg_vrs == paste0(pkg,"_",vrs))       #subset of package
        if (nrow(dfk)==1) return(dfk$installation.time)             #lookup installation times
        if (nrow(dfk)!=1) return(180)                               #if not found, assume 3 minutes
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


    #2.6 R Paths to search for an already installed package (versions of same minor, in reverse chronological order)
          get.pkg_search_paths=function(pkg,vrs)
            {
              #R versions
                R.toc=toc("R")
                R.toc$major=sapply(  strsplit(R.toc$Version,"\\."), `[`, 1)
                R.toc$minor=sapply(  strsplit(R.toc$Version,"\\."), `[`, 2)
                R.toc$path=sapply(  strsplit(R.toc$Version,"\\."), `[`, 3)
                R.toc$Published=as.DateYMD(R.toc$Published)

              #Subset of R.toc for same minor as r.using
                rv=r.version.check("2019-01-01")  #use arbitrary date for we don't use 'r.need', just r.using
                subset.R.toc=subset(R.toc,minor==rv$r.using.minor & major==rv$r.using.major & Published<=get.rdate())

            #Sort versions from most recent
              subset.R.toc=subset.R.toc[order(subset.R.toc$Published,decreasing=T),]

            #paths
              pkg_search_paths=paste0(groundhogR.folder,"/R-",subset.R.toc$Version,"/",pkg,"_",vrs)

            #Ensure directories exist
              return(pkg_search_paths)
              }


    #2.7 Check if package version is installed for current or past R versions,

          get.installed_path=function(pkg,vrs)
          {
          #Get full paths
            pkg_search_paths=get.pkg_search_paths(pkg,vrs)

          #Search for package till found
           installed_path=""
            for (pathk in pkg_search_paths)
                {
              #If directory and file exists, test if package is installed, if file does not exist; it is not installed anyway
                if (file.exists(pathk)) {
                df.pkg=data.frame(installed.packages(lib=pathk))  #Note: in {groundhogR} each package version gets a 'library' within the R version
                if (nrow(df.pkg) >0) {
                  installed_path=pathk
                  break
                } #If file exists identified by possible search path
                } #End if found
                } #End:   for (pathk in
            return(installed_path)
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

#2.9 R Verson comparison
    r.version.check=function(date)
      {
        #Using
            r.using.major=  as.numeric(R.version$major)
            r.using.minor=  as.numeric(strsplit(R.version$minor,"\\.")[[1]][1])
            r.using.patch=  as.numeric(strsplit(R.version$minor,"\\.")[[1]][2])
            r.using.full=paste0(r.using.major,".",r.using.minor,".",r.using.patch)
        #need
            r.need.full=get.version("R",date)
            r.need.split=strsplit(r.need.full,"\\.")[[1]]
            r.need.major=as.numeric(r.need.split[1])
            r.need.minor=as.numeric(r.need.split[2])
            r.need.patch=as.numeric(r.need.split[3])
        return(namedList(r.using.full, r.using.major, r.using.minor, r.using.patch,
                         r.need.full, r.need.major, r.need.minor, r.need.patch))
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


#2.11 FIND BINARY DATE FOR MRAN, given R version being used
   get.R.pkg.date=function(pkg_vrs,R_vrs)
    {
      #1. Get pkg from pkg_vrs
          pkg=get.pkg(pkg_vrs)
          vrs=get.vrs(pkg_vrs)

      #2. cross.toc with R - all available pkg versions and R
          cross1=cross.toc(c(pkg,"R"))

      #3, Which row have pkg_vrs vs R_vrs
          k.pkg=match(pkg_vrs,paste0(cross1$Package,"_",cross1$Version))
          k.R=match(paste0("R_",R_vrs),paste0(cross1$Package,"_",cross1$Version))

      #4. Both ks in vector
          ks=c(k.pkg, k.R)

      #5. From one to the other (cross2: subset from cross1 where  pkg_vrs to R_vrs or vice versa)
           cross2=cross1[min(ks):max(ks),]

      #6. If the package came first:
           if (k.pkg<k.R)
           {
            #6.1. If there is another version of the package in the subset, it means it changed before arriving at the desired R, so return ""
                if (sum(cross2$Package==pkg)>1) return("1970-01-01")
            #6.2 If there is only one package in the set, then starting with last row, the desired package is available for that R, take midpoint till next
                if (sum(cross2$Package==pkg)==1) {
                  start.date=cross1[k.R,  ]$Published  #start.date: start of period when pkg_vrs binary was avilable for this R-version

            #6.3 If not using the most recent R, the midpoint is towardsthe next one, if most recent, halfway to today
                  if (k.R<nrow(cross1))  end.date=as.DateYMD(cross1[k.R+1,]$Published)-2  #If already replaced, when it was replaced, minus 2 days for caution
                  if (k.R==nrow(cross1)) end.date=as.DateYMD(Sys.Date())-2    #If not yet replaced, still current with today's MRAN, but use minus two days for caution

            #6.4  If end.date not yet in toc, update toc
                  if (max(cran.toc$Published)<end.date) load.cran.toc(T)
                  }#ENd 6.2 --  if date will be found
          }#End if package came first

      #7. If  R came first:
           if (k.pkg>k.R)
           {
            #7.1. If there is another version of R, different minor, it changed
                if (sum(cross2$Package=="R")>1) return("1970-01-01")
            #7.2 If there is only one version of R in the set,
                if (sum(cross2$Package=="R")==1) {
                #Start date when pkg was available, is when it is released
                  start.date=cross1[k.pkg,  ]$Published  #start.date: start of period when pkg_vrs binary was avilable for this R-version
                #End date is either when then ext package version is released or the present.
                   if (k.pkg<nrow(cross1)) end.date=as.DateYMD(cross1[k.pkg+1,]$Published)-2  #If already replaced, when it was replaced, minus 2 days for caution
                   if (k.pkg==nrow(cross1))  end.date=as.DateYMD(Sys.Date())-2    #If not yet replaced, still current with today's MRAN, but use minus two days for caution


                #If end.date not yet in toc, update toc
                  if (max(cran.toc$Published)<end.date) load.cran.toc(T)


                } #There is only 1 version of R within set
           }#R with after

              start.date=as.DateYMD(start.date)
              end.date=as.DateYMD(end.date)
              as.numeric(end.date-start.date)
              mid.date=as.numeric((end.date-start.date))/2+start.date


          return(mid.date)
   }#End.function 2.11





    get.date.for.install.binary=function(pkg_vrs)
      {
        binary.date=as.DateYMD("1970-01-01")
      #R being used
          r.using.full =get.rversion()  #Get current
          r.using.major=R.version$major
          r.using.minor=strsplit(R.version$minor,"\\.")[[1]][1]

      #Get R Toc
         R.toc=toc("R")          #Get R toc

       #Extract  major,minor,patch to  R versions in toc
         R.toc$major=sapply(  strsplit(R.toc$Version,"\\."), `[`, 1)
         R.toc$minor=sapply(  strsplit(R.toc$Version,"\\."), `[`, 2)
         R.toc$path=sapply(  strsplit(R.toc$Version,"\\."), `[`, 3)

      #Find first and last current major.minor
        R.same.minor=subset(R.toc,major==r.using.major & minor==r.using.minor )
        k0=match(R.same.minor[1,]$Version,R.toc$Version)
        k1=match(R.same.minor[nrow(R.same.minor),]$Version,R.toc$Version)

      #Find exact match, and split from rest, to start with perfect match
        k.same=match(get.rversion(),R.toc$Version)
        k.others=k1:k0
        k.others=k.others[k.others!=k.same]

       #Loop over them
            for (k in c(k.same,k.others))
              {
              R_vrs=R.toc[k,1]
              binary.date=get.R.pkg.date(pkg_vrs=pkg_vrs,R_vrs=R_vrs)
              if (binary.date>"1970-01-01") break
            }
      #If date if from before MRAN's first day, 2014-09-17, go to 1970
            if (binary.date<"2014-09-17") binary.date=as.DateYMD("1970-01-01")
            return(as.DateYMD(binary.date))
      }



#2.12 update cran.toc() if needed
    update_cran.toc_if.needed=function(date)
      {
       #1 Format entered date by user
          date=as.DateYMD(date)

       #2 Load cran.toc if not yet loaded
          if (!exists("cran.toc")) load.cran.toc(update.toc=FALSE)

       #3 If user wants  a newer date than available, update it.
           cran.toc$Published=as.DateYMD(cran.toc$Published)      #Convert cran.toc $Published, to a date variable
           max.date=max(cran.toc$Published)                       #Most recent date in cron

      #4 Compare most recent to entered date
          if (max.date<date)
            {
            cat2()
            cat1(paste0("The date you entered, '",date,"', requires updating your database with the list of CRAN package versions, \n",
                       " for it goes only until ",max.date, ".The udate is happening as you read this. "))

          #Stop if date is in the future
              msg.future.date=paste0("GroundhogR's database is updated multiple times a day, but, to ensure reproducibility of your script,",
                                     "given time zone differences and delays in updating different CRAN mirrors, don't use a date more recent than ",
                                     "two days ago, (i.e., the most recent date you may use today with groundhogR is:'",Sys.Date()-2,"').")
              if (date>Sys.Date()-2) {
                cat2()
                cat1(msg.future.date)
                stop()
              }
          #Update the database
               load.cran.toc(TRUE)
          } #End if desired date is after most recent date
      } #End 2.12



#2.13 r.version mismatch check
      check.mismatch.R=function(date)
        {
         #Get versions of R being used a needed
            rv=r.version.check(date)

         #If major or minor do not match, give warning
             if (rv$r.using.major!=rv$r.need.major | rv$r.using.minor!=rv$r.need.minor) {

          #Check if have not warned for the last hour
             now=Sys.time()

          #If there is not previous time, assign it a full year to trigger the warning
             if (!exists('r.mismatched.last.time')) since.warning=60*365

          #Time since last warning.
             if (exists('r.mismatched.last.time')) since.warning=as.numeric(round(difftime(now, r.mismatched.last.time, units='mins'),0))

          #If more than a 60 minute gap give full warning
            if (since.warning>60)
            {
            cat2()
             cat1("---------------- R Version check ----------------")
             cat1(paste0("You are using  R : '",rv$r.using.full,", but for the date you entered (",date,"),\n",
                         "the current R was: '",rv$r.need.full,"'"))
             cat1(paste0("This version mismatch has two consequences:\n",
                  "1) Some of the packages you install, and their dependencies, probably will take longer to install, possibly *minutes* longer.\n",
                  "2) There is some chance the original code won't run in the newer R version"))

             cat2("\nBottom line")
             cat1(paste0("It's probably worth continuing as-is, and if you get tired of waiting for packages to install,\nor get errors trying to run ",
                    "it, then you try it with the appropriate version of R. \nRunning older R is actually quite easy. To get instructions, execute:\nmsg.R.switch('",date,"') "))
                   r.mismatched.last.time<<-Sys.time()

             cat1("\n\nRegardless of your choice, this warning will not appear again within the next hour")
             quit.menu(date=date)


             }#End if more than 60 minutes since last warning

            #If less than 60 minute, short version
                if (since.warning<=60)
                {
                cat2()
                cat1(paste0("R Version mismatch. Using: R-",rv$r.using.full," but the on '",date,"' it was : R-",rv$r.need.full,
                         ".\n Full warning shown earlier (scroll up), won't be shown again for the next ",60-since.warning," minutes."))
                }#End if recently warned
             }#END warning check

      }#End of function 2.13



  #2.14 Get Active packages as data.frame() OUTPUT $pkg has the pkg name, $pkg_vrs the pkg_vrs
      get.active=function()
          {
          loaded.list  =utils::sessionInfo()$loadedOnly                   #pkgs in name space
          attached.list=utils::sessionInfo()$otherPkgs                    #pkgs in attached
          active.pkg=c(names(loaded.list), names(attached.list))          #Get names of active packages
          active.vrs=c(lapply(loaded.list, function(x) x$Version), lapply(attached.list, function(x) x$Version))
          active.pkg_vrs=paste0(active.pkg,"_",active.vrs)                 #merge to pkg_vrs

        #Drop those in base R
          active.in.base=active.pkg %in% dep.base
          active.pkg=active.pkg[!active.in.base]
          active.vrs=active.vrs[!active.in.base]
          active.pkg_vrs=active.pkg_vrs[!active.in.base]

         df=data.frame(active.pkg,active.pkg_vrs)
         names(df)=c("pkg","pkg_vrs")
         df
      }



  #2.15 Check snowball conflict
      check.snowball.conflict=function(snowball)
      {

       #Load active packages (function 2.14)
          active=get.active()

       #How many match pkg vs pkg_vrs
          count.same.pkg    =sum(snowball$pkg %in% active$pkg)
          count.same.pkg_vrs=sum(snowball$pkg_vrs %in% active$pkg_vrs)

       #If counts are different, give warning
        #8.8.3 If different # of packages match pkg vs pkg_vrs, we have same packages  different vrs: stop
                 if (count.same.pkg_vrs != count.same.pkg)
                 {
                 cat2()
                 cat1(paste0("A different version of the package you want to load, or one of its dependencies, is already have loaded.\n",
                              "This can create reproducibility problems, as it will be ambiguous which of those version you are using\n",
                             " with your script.  To solve this you have two options.\n\n",
                            "Option 1.*RECOMMENDED* Restart R session (in R Studio press: CTRL/CMD-SHIFT-F10) to unload all packages\n",
                            "Option 2. Run groundhog.library() with option: 'ignore.package.conflicts=TRUE'\n",
                            "This option can cause errors. Avoid using it unless you are trying to run old scripts which were not written\n",
                            "using groundhog.library() and thus you don't know the groundhog day which will render them reproducible.\n\n"))
                  stop.msg=paste0("----------------- package not loaded - see above -- PRESS: CTRL/CMD-SHIFT-F10  ----------------")
                  stop(stop.msg)
                 }#End if different count
      }#End fucntion 2.15


#2.16 Is pkg_vrs installed (within same R-minor version)
      is.pkg_vrs.installed=function(pkg,vrs) (get.installed_path(pkg,vrs) %in% get.pkg_search_paths(pkg,vrs))




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



########################################################################################
#Main functions


#3 Load the dataframe listing all CRAN packages, their dependencies and Publication date
  load.cran.toc=function(update.toc=FALSE)
      {
      #3.0 Ensure directory for groundhogR exists
        dir.create(groundhogR.folder, showWarnings = FALSE)    #Create if inexistent

      #3.1 Paths two databases (toc and times:
         #LOCAL
            toc.path=paste0(groundhogR.folder,"/cran.toc.csv.gz")
            times.path=paste0(groundhogR.folder,"/cran.times.csv.gz")

          #URL
            toc.url  =paste0(groundhogR.url,"/cran.toc.csv.gz")
            times.url=paste0(groundhogR.url,"/cran.times.csv.gz")

      #3.2 JUST LOAD
             if (file.exists(toc.path) & file.exists(times.path) & update.toc==FALSE) {

               #TOC
                  cran.toc=read.csv(toc.path,stringsAsFactors = F)[,-1]
                  cran.toc$Published=as.DateYMD(cran.toc$Published)
                  cran.toc<<-cran.toc #Move the cran.toc outside the function space, to global environemnt, later will be package environemnt.

              #Times
                  cran.times=read.csv(times.path,stringsAsFactors = F)[,-1]
                  cran.times$update.date=as.DateYMD(cran.times$update.date)
                  cran.times<<-cran.times
                }  #End 3.2 - no update

      #3.3 UPDATE
            if (file.exists(toc.path) & file.exists(times.path) & update.toc==TRUE)  {

            #3.3.1 load databases
              existing.toc=  read.csv(toc.path,   stringsAsFactors = F)[,-1]
              existing.times=read.csv(times.path, stringsAsFactors = F)[,-1]

            #3.3.2 create pkg_vrs for unique identifyier of packages
              existing.toc.pkg_vrs  =paste0(existing.toc$Package,"_",existing.toc$Version)
              existing.times.pkg_vrs=paste0(existing.times$Package,"_",existing.times$Version)

            #3.3.3 highest date
               max.existing.toc.date   = max(as.DateYMD(existing.toc$Published)) - 2 #lookup two days prior to handle timezone and cran delays
               max.existing.times.date = max(as.DateYMD(existing.times$update.date)) - 2 #lookup two days prior to handle timezone and cran delays

            #UPDATE TOC
                #3.3.4 Try updating toc by downloading additional rows from groundhogR server
                   add.toc=try(read.csv(paste0(groundhogR.url,"/differential.toc.php?current_date=",max.existing.toc.date))[-1])

                #3.3.5 If sucess loading URL
                  if (class(add.toc)=='data.frame')
                  {
                  #Get pkg_vrs for packages to add
                      add.toc.pkg_vrs=paste0(add.toc$Package,"_",add.toc$Version)

                  #Drop repeated rows in URL and existing (there is the last day of partial to full overlap)
                      add.toc.net=add.toc[!add.toc.pkg_vrs %in% existing.toc.pkg_vrs,]

                   #Add net
                     cran.toc=rbind(existing.toc, add.toc.net)
                     cran.toc<<-cran.toc  #Save cran.to to environemnt


                  #save to local drive
                     write.csv(cran.toc,file=gzfile(toc.path))
                  } #End 3.3.5 - if succeeded at downloading file from website


             #UPDATE TIMES
                #3.3.6 Try updating times by downloading additional rows from groundhogR server
                   add.times=try(read.csv(paste0(groundhogR.url,"/differential.times.php?current_date=",max.existing.times.date))[-1])


                #3.3.7 If sucess loading URL
                  if (class(add.times)=='data.frame')
                  {
                  #Get pkg_vrs for packages to add
                      add.times.pkg_vrs=paste0(add.times$Package,"_",add.times$Version)

                  #Drop repeated rows in URL and existing (there is the last day of partial to full overlap)
                      add.times.net=add.times[!add.times.pkg_vrs %in% existing.times.pkg_vrs,]

                   #Add net
                     cran.times=rbind(existing.times, add.times.net)
                     cran.times<<-cran.times  #Save cran.to to environemnt


                  #save to local drive
                     write.csv(cran.times,file=gzfile(times.path))
                  } #End 3.3.5 - if succeeded at downloading file from website



            #Feedback to user on existing cran.to
              cat2()
              cat1(paste0("This computer had a database with a list of all versions available for each CRAN package up to ",
                           max.existing.toc.date+2, " for a total of N=",nrow(existing.toc)," package versions."))         #Add back the two days we took out
              if (class(add.toc)=="data.frame") cat1 (paste0("We checked for additions to CRAN since then, and added ",
                           nrow(add.toc.net)," additional entries to the list.\n"))
              if (class(add.toc)!="data.frame") cat1 (paste0("We tried to update till today but it did not work"))

               cat1(paste0("The file with the list is stored here: ",toc.path,hr))
            } #End if local file exist


      #3.4 if either cran.toc does not exist, download the entire up to date copy
          if (!file.exists(toc.path) | !file.exists(times.path)) {
            #3.4.1 Attempt download
              d1=try(download.file(toc.url, toc.path))
              d2=try(download.file(times.url, times.path))

            #3.4.2 if it fails (d1), give bad news
              if (class(d1)=="try-error") {
                    cat2()
                    cat1(paste0("Could not load the database of packages available in CRAN neither locally from this\n",
                        "computer nor from our server. Unfortunately this means you cannot use groundhog.libray()"))
                    cat1("This could happen if this is your first time using {groundhogR} and you are off-line.")
                    stop("!")
                                } #End if download did not work
            #3.4.3 Load the toc
              cran.toc=read.csv(file=gzfile(toc.path),stringsAsFactors = F)[,-1]
              cran.toc$Published=as.DateYMD(cran.toc$Published)
              cran.toc<<-cran.toc

              cran.times=read.csv(file=gzfile(times.path),stringsAsFactors = F)[,-1]
              cran.times$update.date=as.DateYMD(cran.times$update.date)
              cran.times<<-cran.times



            #3.4.4 tell users
              cat2()
              cat1(paste0("GroundhogR requires a database listing all versions of every package ever in CRAN.\n",
                    "The database was not found in this computer so it was just downloaded from http://groundhogR.com\n",
                   "The database contains N = ",nrow(cran.toc), " entries. The most recent entry is from ",max(cran.toc$Published),".\n",
                   "The database will be automatically updated when the 'date' in groundhog.library('date') so requires.\n",
                    "You may also update it by running: load.cran.toc(update.toc=TRUE)."))
              cat1(paste0("The file is stored locally here:'",toc.path,"'",hr))
            } #End if file does not exist
  } #End of load cran.toc



#4 Get dependencies for ONE package
     # FROM:pkg_vrs, TO: data.frame(Imports, Depends, Packages)
     get.dependencies=function(pkg,date,include.suggests=FALSE)
        {
        #Get version from date
           vrs=get.version(pkg,date)
        #Get dependencies if version exists
            row=subset(cran.toc,Package==pkg & Version==vrs)[,c("Imports","Depends","Suggests")]   #row in mastertoc
            dep=c(row$Imports,row$Depends)                      #merge
            if (include.suggests) dep=paste0(dep,row$Suggests)  #add 'Suggests' dependencies if requested
            dep=unlist(strsplit(dep,","))                      #turn to array
            dep=dep[dep!=""]                                   #drop empty values
            dep=dep[dep!="R"]                                  #drop R as a dependency
            dep=dep[!dep %in% dep.base]                        #base dependencies from R, see #1.7
            return(dep)
     } #End get.dependencies()



#5 Get dependencies for dependencies, infinite levels downs
     get.all.dependencies=function(pkg,date,include.suggests=FALSE)
      {
      #5.1. Populate the starting point with this package and its dependencies
          #[a] Vector with pending deps, for looping install
               pending=get.dependencies(pkg,date,include.suggests=include.suggests) #include.suggests=TRUE means that suggested dependencies and their depdencies are installed.

          #[b] dep12: data.frame with two columns, pkg-left, dependency-right, for snowball loading
               dep12=data.frame(pkg=as.character(), dep2=as.character())
               if (length(pending>0)) dep12=data.frame(pkg=pkg, dep2=pending)

      #5.2 Loop over pending, adding to dep12, and both adding and subtracting from pending till it's empty
            k=1
            while (length(pending) != 0) {
              #5.3 Grab the first among the pending dependencies
                  depk=pending[1]

              #5.4 Get its dependencies
                  pendingk=get.dependencies(pkg=depk,date=date,include.suggests=FALSE)   #NEVER include suggested deps for deps

              #5.5 Drop depk from pending
                  pending=pending[-match(depk,pending)]

              #5.6 if pendingk not empty, update pending and dep12
                    if (paste0(pendingk,collapse="")!="")
                    {
                    #[a] Process pendingk prior to adding to pending()
                      #drop empty
                         pendingk=pendingk[pendingk!=""]
                    #Already processed dropped
                          already.processed=pendingk %in% dep12[,1]  #identify in pending those already processed
                          pendingk.net=pendingk[!already.processed]  #drop them
                          pending=unique(c(pending, pendingk.net))   #Unique so that if we add somethign already pending we don't add it
                          pending=pending[pending!=""]               #drop empty

                    #Add pendingk.net to dep12 if any
                          if (length(pendingk.net)>0) {
                              dep12k=data.frame(pkg=depk,dep2=pendingk.net)
                              dep12=rbind(dep12,dep12k)
                          }
                      }#End 5.5 if some new dependencies to add
                    pending

                   k=k+1
                  if (k>50000) break  #In case the loop does not converge to a stable dataframe
            }#End while
            return(dep12=dep12)
          }#End function get.all.dependencies



#6 Install from source

     install.source=function(pkg_vrs,lib,date, force.download=FALSE)
     {
      #6.1 Preliminaries
        pkg=get.pkg(pkg_vrs)
        vrs=get.vrs(pkg_vrs)

      #6.2 Paths
          tarball.name=paste0(pkg_vrs,".tar.gz")                 # Name of tarball
          tarball.dir=paste0(groundhogR.folder,"/_tarballs")     # Folder to save it
          tarball.path=paste0(tarball.dir,"/",tarball.name)      # Path to tarball itself

      #6.3 Ensure folder exists for tarball saving exists
           dir.create(tarball.dir, showWarnings = FALSE,recursive=TRUE)

      #6.4 Download tarball if needed
          if (!file.exists(tarball.path) | force.download==TRUE)
          {
      #6.4.1 Set URL for download
              toc.pkg=toc(pkg) #get toc() to see if we have the most current version

      #6.4.2 Find tarball in CRAN, based on whether it is current or not current
            if (pkg_vrs %in% current.packages$pkg_vrs)  file.url=paste0("https://cran.r-project.org/src/contrib/",pkg_vrs,".tar.gz")
            if (!pkg_vrs %in% current.packages$pkg_vrs) file.url=paste0("https://cran.r-project.org/src/contrib/Archive/",pkg,"/",pkg_vrs,".tar.gz")

            down.tarball.attempt=try(download.file(file.url,destfile=tarball.path))


      #6.4.3 If downloading fails, try from MRAN on date requested as current, then that date as archive, then first day on MRAN as current, then archive
            if (class(down.tarball.attempt)=="try-error")  down.tarball.attempt=try(download.file(paste0("https://cran.microsoft.com/snapshot/",date,"/src/contrib/",pkg_vrs,".tar.gz"),destfile=tarball.path))
            if (class(down.tarball.attempt)=="try-error")  down.tarball.attempt=try(download.file(paste0("https://cran.microsoft.com/snapshot/",date-2,"/src/contrib/Archive/",pkg,"/",pkg_vrs,".tar.gz"),destfile=tarball.path))
            if (class(down.tarball.attempt)=="try-error")  down.tarball.attempt=try(download.file(paste0("https://cran.microsoft.com/snapshot/",date-2,"/src/contrib/",pkg_vrs,".tar.gz"),destfile=tarball.path))
            if (class(down.tarball.attempt)=="try-error")  down.tarball.attempt=try(download.file(paste0("https://cran.microsoft.com/snapshot/",date+1,"/src/contrib/Archive/",pkg,"/",pkg_vrs,".tar.gz"),destfile=tarball.path))
            if (class(down.tarball.attempt)=="try-error")  down.tarball.attempt=try(download.file(paste0("https://cran.microsoft.com/snapshot/",date+1,"/src/contrib/",pkg_vrs,".tar.gz"),destfile=tarball.path))
            if (class(down.tarball.attempt)=="try-error")  down.tarball.attempt=try(download.file(paste0("https://cran.microsoft.com/snapshot/",date,"/src/contrib/Archive/",pkg,"/",pkg_vrs,".tar.gz"),destfile=tarball.path))
            if (class(down.tarball.attempt)=="try-error")  down.tarball.attempt=try(download.file(paste0("https://cran.microsoft.com/snapshot/2014-09-18/src/contrib/",pkg_vrs,".tar.gz"),destfile=tarball.path))
            if (class(down.tarball.attempt)=="try-error")  down.tarball.attempt=try(download.file(paste0("https://cran.microsoft.com/snapshot/2014-09-18/src/contrib/Archive/",pkg,"/",pkg_vrs,".tar.gz"),destfile=tarball.path))
          }

       #6.5 Success downloading?
            #yes--> install
              if (file.exists(tarball.path))  {
                #Create the folder
                  dir.create(lib,showWarnings = F,recursive = T)
                #Install the package
                  install.packages(tarball.path, type="source",lib=lib,  dependencies=FALSE,repos=NULL)
                } #End if success


        #6.6 no--> convey bad news
                      if (!file.exists(tarball.path))
                        {
                        cat1(paste0('could not find the tarball file for package ',
                           pkg,' version:',vrs,' in CRAN (',file.url,'). \nMaybe you are offline?.'))
                          stop("Installation failed.")
                          }
    } #End of install.source



#Function 7 - Get snowball
      get.snowball=function(pkg,date,include.suggests=F, force.source=F, current.deps="Rcpp")
      {

        #1) Get dependencies
          dep12=get.all.dependencies(pkg,date)

        #2) process dep12  (topographical sort) to get order of installation
           k=0       #counter of iterations attempting to sort
           indep=c()
           #In each loop we take all dependencies without dependencies and add them to the sequence of installation
           #Do until all dependencies have been assigned an order (so dep12 is empty)
            while (nrow(dep12)>0) {
              k=k+1
              indep.rows= !(dep12$dep2 %in% dep12$pkg) ##Find dependecies without dependencies  TRUE/FALSE vector
            #Add those dependencies to the list of independencies
              indepk=unique(as.character(dep12$dep2[indep.rows]))
              indep=c(indep, indepk)
            #Drop those rows from both
              dep12=dep12[!indep.rows,]
            #Safety valve in case loop impossible to end
              if (k==50000) break
             }

       #3) Add pkg at the end
            indep=c(indep,pkg)

       #4) Get the version of each package
            snowball.pkg=as.character(indep)
            snowball.vrs=as.character(mapply(get.version,indep,date,current.deps))  #current.deps replaces the version of those dep with the version that's current when installing
            snowball.pkg_vrs=paste0(indep,"_",snowball.vrs)

       #5 Snowball table, with installed | CRAN | MRAN | TARBALL | INSTALLATION TIME
            snowball.installed=mapply(is.pkg_vrs.installed, snowball.pkg,  snowball.vrs)                                 #5.1 Installed?  TRUE/FALSE
            snowball.CRAN=(snowball.pkg_vrs %in% current.packages$pkg_vrs) & (tail(toc('R'),1)$Version==get.rversion())  #5.2 Binary in CRAN and using most recent R TRUE/FALSE
            snowball.MRAN.date=as.Date(sapply(snowball.pkg_vrs,get.date.for.install.binary),origin="1970-01-01")  #5.3 Binary date in MRAN?
            snowball.MRAN.date=as.DateYMD(snowball.MRAN.date)
            snowball.MRAN=!snowball.MRAN.date=="1970-01-01"
            snowball.from=ifelse(snowball.MRAN,"MRAN","source")      #MRAN if available, if not source
            snowball.from=ifelse(snowball.CRAN,"CRAN",snowball.from) #Replace MRAN if CRAN is available and using most recent version of R

          #Installation time from source
            snowball.time=round(mapply(get.installation.time,snowball.pkg,snowball.vrs),0)

          #Adjust installation time
            adjustment=2.5 #install times in CRAN's page are systematically too long, this is an initial adjustment factor
            snowball.time=pmax(round(snowball.time/adjustment,0),1)

          #Installation path
            #Only r.path portion differs across where the package is obtained
              r.path=c()
              for (k in 1:length(snowball.pkg))
                {
                if (snowball.from[k]=="MRAN")   r.path[k]=get.version("R",snowball.MRAN.date[k])
                if (snowball.from[k]=="CRAN")   r.path[k]=max(toc("R")$Version)
                if (snowball.from[k]=="source") r.path[k]=get.rversion()
              }
           #Vector with paths
              snowball.installation.path=paste0(groundhogR.folder,"/R-", r.path , "/" ,snowball.pkg_vrs)


            #data.frame()
              snowball=data.frame(snowball.pkg, snowball.vrs, snowball.pkg_vrs,  #Identify pkg
                  snowball.installed,                            #Installed?
                  snowball.from,                                 #Where to install from
                  snowball.MRAN.date,                            #MRAN date, in case MRAN is tried
                  snowball.time,                                 #time to install
                  snowball.installation.path, stringsAsFactors = F)                    #directory to save the binary package to
                  #how long will it take

            names(snowball)=c("pkg","vrs","pkg_vrs","installed","from","MRAN.date", "installation.time","installation.path")


            return(snowball)
            }






 #Function 7 Estimate seconds left in install
    estimate.seconds.left=function(k,start.time,snowball)
        {
        #Time assigned to binary installs
          time.per.binary=5 #assumed 5 seconds per binary
        #Time so far
          time.so.far=as.numeric(difftime(Sys.time(),start.time,units="secs"))

        #Total time estimate
          estimated.total.source=round(sum(snowball[snowball$from=='source' & snowball$installed==F,]$installation.time),0) #For source, use estimated install time
          estimated.total.binary=time.per.binary*sum(snowball$from!='source' & snowball$installed==F) #For binary, assume 5 seconds per package that is not installed
          estimated.total=estimated.total.source+estimated.total.binary

        #subset of snowball to pacakges left
          N=nrow(snowball)
          snowball.left=snowball[(k+1):N,]

          estimated.left.source=round(sum(snowball.left[snowball.left$from=='source' & snowball.left$installed==F,]$installation.time),0)
          estimated.left.binary=time.per.binary*sum(snowball.left$from!='source' & snowball.left$installed==F)       #For binary, assume 5 seconds per package that is not installed
          estimated.left=estimated.left.source+estimated.left.binary

          if (k==1 | time.so.far < 30) return(estimated.total)
        #If not the first package, adjust estimate
          if (k>1) {
              actual.past=round(as.numeric(Sys.time()-start.time,units='secs'),0)
              estimated.past.source=sum(snowball$installation.time[1:(k-1)][snowball$from[1:(k-1)]=="source" & snowball$installed[1:(k-1)]==FALSE])
              estimated.past.binary=time.per.binary*sum(snowball.left$from!='source' & snowball.left$installed==F)            #For binary, assume 5 seconds per package that is not installed
              estimated.past=estimated.past.source+estimated.past.binary

              progress=estimated.past/estimated.total

              ae.ratio=(actual.past/estimated.past)*progress+1*(1-progress)   #The ratio of actual/adjustment is a weighted average of just 1 (use estimate as is), and the empirical reatio so far in this intallation progress so fart
              return(ae.ratio*estimated.left)
          }
    }


 #Function 8 INstallation feedback
    installation.feedback=function(k,date, snowball, start.time, plot.console=TRUE)
      {

      #Get R being used and needed
        rv=r.version.check(as.DateYMD(date))

      #How much time so far in this installation process
        time.so.far=as.numeric(difftime(Sys.time(),start.time,units="secs"))

      #Shorter variables
        N=nrow(snowball)
        pkg_vrs=snowball[N,"pkg_vrs"]

     #Estimate of time
        seconds.left=estimate.seconds.left(k,start.time,snowball)
        finish.time.estimate=format(Sys.time()+seconds.left,"%H:%M")
        finish.time.max=format(Sys.time()+seconds.left*3,"%H:%M")

      #If first one being installed, show dots
          if (k==1 | time.so.far<5 & plot.console==TRUE)
            {
              plot(c(.25,.5,.75),c(.5,.5,.5),cex=10,pch=16,col='red',xlim=c(0,1),xaxt='n',xlab='**LOADING**',ylab='',yaxt='n')
              Sys.sleep(.75)
          }
      #Show plot.console feedback
          if (plot.console==TRUE)
              {
              msg.plot=paste0("> groundhog.library() is in the process of installing '",pkg_vrs,"'.\n\n",
                               "> '",pkg_vrs,"' depends on ",N-1," other packages which will be installed if needed as well.\n",
                              "> We are now installing **",snowball[k,"pkg_vrs"],"**, #",k," out of the ",N, " packages total:\n\n",
                              "> The time now is ",format(Sys.time(),"%H:%M"),", and we quite roughly estimate the ",
                              "process to end around ",finish.time.estimate,"\n",
                              "> It is unlikely to finish after ", finish.time.max,"\n",
                              "> These estimates will be revised after each package installs, but they will remain noisy estimates.\n\n\n")


              #Add msg if R mismatch
                if (rv$r.using.major!=rv$r.need.major | rv$r.using.minor!=rv$r.need.minor) {
                  msg.plot.=paste0("> Installation is slow because you are using R-",get.rversion(),"\n",
                                   "> If you run this script with the R version available on '",date,"', i.e., on R-",get.version("R",date),",\n",
                            "   the entire installation would take about a minute or two.\n",
                              "> Instructions for running older version of R:  http://tiny.cc/SwitchR")
                  }

              cat1.plot(msg.plot)


              }#ENd if plot.console==T
      #Show cat 1 feedback
       cat2()
       cat1(paste0("Installing package #",k," out of ",nrow(snowball), " needed.\npacakge:",snowball[k,"pkg_vrs"],"'.\n\n"))
    }




#8 Function 8 - Install snowball
      install.snowball=function(pkg,date, include.suggests, force.install=FALSE,force.source=FALSE,plot.console=TRUE,quiet.install=TRUE)
        {
        #0 Get the snowball
          snowball=get.snowball(pkg,date,include.suggests,force.install,current.deps="Rcpp")

        #0.1 pkg_vrs for target package
          pkg_vrs=as.character(snowball[nrow(snowball),'pkg_vrs'])
       #0.2 determine if we want the plot to show
        if (sum(snowball$from=="source" & snowball$installed==FALSE)==0) plot.console=FALSE

         #1. FORCE INSTALL
                if (force.install==TRUE & sum(snowball$installed)>0) {
            #Subset of packages that are installed
                snowball.installed=subset(snowball,installed==T)
            #Get their path
                snowball.installed$paths=mapply(get.installed_path,snowball.installed$pkg,snowball.installed$vrs)
             #Rename the paths so they are not found and installation takes place
                snowball.installed$paths_rename=paste0(snowball.installed$paths,"_forcing.reinstall")    #new names
                file.rename(snowball.installed$paths,   snowball.installed$paths_rename)                 #assign them
            } #End #1 force

        #2. FORCE SOURCE
            if (force.source==TRUE) snowball$from="source" #Change values from optimized, to 'source'

        #3 INSTALLATION LOOP

              start.time=Sys.time()    #When loops starts, to compute actual completion time
              for (k in 1:nrow(snowball))
              {
              #3.0 Assign path
                lib.k=as.character(snowball[k,'installation.path'])

              #3.1 Install if needed
                if (!snowball[k,'installed'])
                {

                #3.2 Feedback to user on time and pogresss
                  installation.feedback(k, date,snowball, start.time, plot.console=TRUE)

                #3.3 Shorter variable names
                  pkg.k=snowball[k,'pkg']
                  vrs.k=snowball[k,'vrs']
                  pkg_vrs.k=snowball[k,'pkg_vrs']
                  from.k=snowball[k,'from']
                  mran.date.k=snowball[k,'MRAN.date']

                #3.4 Create directory
                  dir.create(lib.k,recursive = T,showWarnings = F)

                #3.5 INSTALL K FROM CRAN
                   if (from.k=="CRAN") install.packages(pkg.k,  dependencies=FALSE,lib=lib.k,type="both",quiet=quiet.install)

                #3.6 INSTALL K FROM MRAN
                   if (from.k=="MRAN") {
                      #Try MRAN 1
                        install.packages(pkg.k,lib=lib.k, type='binary', repos=paste0('http://mran.microsoft.com/snapshot/',mran.date.k,'/'),dependencies = FALSE,quiet=quiet.install)
                      #Try MRAN 2: If still not installed, try day after (some MRAN days are missing)
                        if (!is.pkg_vrs.installed(pkg.k,vrs.k)) install.packages(pkg.k,lib=lib.k, type='binary', repos=paste0('http://mran.microsoft.com/snapshot/',mran.date.k+1,'/'),dependencies = FALSE,quiet=quiet.install)
                      #Try MRAN 3: If still not installed, try 2 days earlier
                       if (!is.pkg_vrs.installed(pkg.k,vrs.k))  install.packages(pkg.k,lib=lib.k, type='binary', repos=paste0('http://mran.microsoft.com/snapshot/',mran.date.k-2,'/'),dependencies = FALSE,quiet=quiet.install)
                    } #end MRAN


                #3.7 INSTALL K FROM SOURCE IF SUPPOSED TO, OR I STILL NOT INSTALLED
                      if (from.k=="source" | !is.pkg_vrs.installed(pkg.k,vrs.k)) install.source(pkg_vrs.k, lib.k,date)

                #3.8 VERIFY INSTALL
                    now.installed=is.pkg_vrs.installed(pkg.k,vrs.k)

                    #3.8.5 If not success, try source again, forcing download of file
                        if (now.installed==FALSE) install.source(pkg_vrs.k, lib.k,date,force.download = TRUE,quiet=quiet.install)

                    #3.8.6 Verify install again
                        now.installed=is.pkg_vrs.installed(pkg.k,vrs.k)

                #3.9 Installation failed
                    if (!now.installed)  {

                    #If using different R version, attribute to that:
                        rv=r.version.check(date)
                        if ( (rv$r.using.minor != rv$r.need.minor) |  (rv$r.using.major != rv$r.need.major)) {
                        cat2()
                        cat1(paste0("As mentioned before, there is a discrepancy between the R Version you are using R-",rv$r.using.full,
                          "\nand the one that matches the date (",date,") you entered: R-",rv$r.need.full,". ",
                          "It was worth a try \nbut the install did not work. Follow the instructions below to",
                          " try this script in the correct R version."))
                        cat1.plot(paste0("> The installation of ",pkg_vrs," failed. Please see console for details."))

                        #Long instructions written above
                            cat1(msg.R.switch(date))
                        }

                  #PENDINGrename back the flders taht were renamed
                  #

                   #Stop the script
                       cat2(paste0("\n\n\n--------------------   The package ",pkg_vrs, " did NOT install.-----------------"))
                          opt = options(show.error.messages=FALSE)
                          on.exit(options(opt))
                           stop()
                          }

          #Installation succeded
                if (now.installed) {
                  cat2()
                  cat1(paste0(pkg_vrs.k," installed succesfully. Saved to: ",lib.k))
                  #delete temporary renamed foler if they were created
                }

            }#End of check for whetehr already installed

          #Add to libpath, unless it is the one to be installed
              .libPaths(c(lib.k, .libPaths()))


          }#End loop install
          if (plot.console==TRUE) cat1.plot(paste0("> Installation of ",pkg_vrs," has completed.\n\n\n",
                    "You may clear this window executing 'dev.off()'"))
      }#End install.snowball()




  # 8  Final function:   groundhog.library()
     groundhog.library=function(pkg, date,
                                  quiet.install=TRUE,            #Run install.packages() with quiet=TRUE?
                                  plot.console=TRUE,             #Should information on installation time left be printed in Plots dialogue whe installing from source>
                                  include.suggests=FALSE,        #Should suggested packages be installed?
                                  current.deps=c("Rcpp"),        #Dependencies that will install current version regardless of gdate
                                  ignore.package.conflicts=FALSE,#With TRUE, if an already attached package will be installed,it is detached, but all depednecies are left installed, with default, FALSE, script stops and asks for session restart
                                  force.source=FALSE,            #TRUE will skip CRAN and MRAN attempts, download tar.gz, and install from it
                                  force.install=FALSE)           #Even if package is found for this R-build, it will reinstall it.
    {
    #8.1 Load cran.toc if not yet loaded
          if (!exists("cran.toc")) load.cran.toc(update.toc=FALSE)

    #8.2 Update cran.toc() if needed for entered date (#2.12)
          update_cran.toc_if.needed(date)

    #8.3 Check for r.mismatch (#2.13)
          check.mismatch.R(date)

    #8.4 Get vrs
          vrs=get.version(pkg,date)
          pkg_vrs=paste0(pkg,"_",vrs)

    #8.5 GET SNOWBALL (#6)
          snowball=get.snowball(pkg,date,include.suggests)

    #8.6 CHECK FOR CONFLICT SNOWBALL <->ACTIVE PACKAGES
         if (ignore.package.conflicts==FALSE) check.snowball.conflict(snowball)

    #8.7 Install pacakges if needed
         install.snowball(pkg,date, include.suggests, force.install=force.install,force.source=force.source,plot.console=plot.console,quiet.install=quiet.install)

    #8.8 Do library() call for pkg_vrs
        library(pkg,character.only=TRUE)

    #8.9  verify success
             #pkg_vrs we wanted to load
                 pkg_vrs=paste0(pkg,"_",vrs)
             #load active packagse
                 active=paste0(capture.output(utils::sessionInfo()),collapse=",")
             #Found there?
                 pos=regexpr(pkg_vrs,paste0(active))
                 cat2()
                 if (pos>0)   cat1(paste0("Succesfully loaded ",pkg_vrs," and its ",nrow(snowball)-1," dependencies."))
                 if (pos==-1) cat1(paste0("FAILED to load ",pkg_vrs))

        } #End of groundhog.library()





