
  
######################################################################################
#INTRODUCTION
# Groundhog testing is  not automatized because an R session needs to be restarted 
# after each test,  and the output carefully (humanly) evaluated. Also
# the testing requires conflicts with local libraries and the installation of
# 100s of packages; so testing is done 'by hand', using the scripts below
#
######################################################################################


#OUTLINE
  #Set 0 - Various forms of calling packages to be loaded
  #Set 1 - Enumerated conflicts '
  #Set 2 - Previous Bugs
  #Set 3 - Install many packages
  #Set 4 - Remote packages
  #Set 5 - others
  #Set 6 - Restore  Library


#Version being tested (only locally available)

install.packages('https://groundhogr.com/groundhog_3.0.0.9002.tar.gz',repos=NULL)
#install.packages('c://dropbox/groundhogr/groundhog_3.0.0.tar.gz',repos=NULL)

library('groundhog')
#gd=get.groundhog.folder()
#set.groundhog.folder(paste0(gd,"/temp1"))
set.groundhog.folder('c:/temp/101')


test.day <- groundhog:::get.r.majmin.release()+105 #release of this R version + 45 days

#Set 0 - various forms of calling packages to be loaded
  
  #Missing/wrong arguments
      #Missing pkg
        groundhog.library(date=test.day)
    
      #Missing date
        groundhog.library('rio')
    
      #Date is a vector
        groundhog.library('rio',c('2023-01-01','2023-02-01'))
        
      #wrong format for date
        groundhog.library(2,2)
        
      blu#Nonexistent pkg on cran
        groundhog.library('tio',test.day)

      #inexistent pkg on github
        groundhog.library('github::crsh/powerful_999',test.day)

       
      #Remote with multiple
        pkgs<-c('pwr','crsh/papaja')
        groundhog.library(pkgs,test.day)
        
         
      #Remote with multiple
        pkgs<-c('pwr','metafor')
        groundhog.library(pkgs,test.day , force.source.main = TRUE)
        groundhog.library(pkgs,test.day , force.install.main = TRUE)
        
        
        
  #Single package, with and without quotes
    library('groundhog')
    groundhog.library('pwr',test.day)  #quotes
    groundhog.library(pwr,test.day)    #no quotes

  #Object containing a single package
    pkg1='pwr'
    groundhog.library(pkg1,test.day)    
    
  #Vector containing many packages
    pkg=c('pwr','metafor')
    groundhog.library(pkg,test.day)    #single package, no quotes, show warning to use ""

  #Direct cal to many packages
    groundhog.library(c('pwr','metafor'),test.day)
    
    
  #Sandwiching library and combine already attached with successfully atched
    library('groundhog')
    groundhog.library("
          library('pwr')
          library(metafor)
          library(Hmisc)
          library(jsonlite)", 
      test.day)

    
    #Outdate R
      library('groundhog')
      groundhog.library('haven','2022-04-01')  


#######################################    
#Set 1 - Enumerated conflicts '

      
    # 2.1) A *requested.pkg* was previously loaded with groundhog but different version or repository
      
        #Groundhog CRAN date 1 & CRAN date 2
          library('groundhog')
          groundhog.library('pwr',test.day)
          groundhog.library('pwr',test.day-900,tolerate.R.version = groundhog:::get.rversion())

        #Groundhog CRAN date 1 & GIT 2
          library('groundhog')
          groundhog.library('pwr',test.day)
          groundhog.library('heliosdrm/pwr',test.day)
        
         #Groundhog GIT date 1 & GIT 2
          library('groundhog')
          groundhog.library('heliosdrm/pwr',test.day)
          groundhog.library('heliosdrm/pwr',test.day+1)

    # 2.2) A *dependency* was loaded for a different repos or date 
         #Groundhog CRAN date 1 & CRAN date 2
          
          
          #1 Single conflicting pkg, show it
            library('groundhog')
            groundhog.library('zip',test.day-800,tolerate.R.version = groundhog:::get.rversion())
            groundhog.library('rio',test.day)
            groundhog.library('rio',test.day,ignore.deps = 'zip')  #this does not download the new zip, uses the old one instead
          
            
          #2 Five conflicting pkg, mention them only, not listed
            library('groundhog')
            groundhog.library('haven',test.day-10,tolerate.R.version = groundhog:::get.rversion())
            groundhog.library('rio',test.day+150)
            .view.conflicts
          
          
    # 2.3) TEST BY HAND IN BAT FILE WITH NON-INTERACTIVE SESSION
            #*Any* conflict on a non-interactive session (because we cannot request after installation they restart in  a forceful way)
            #Run this from R script non-interactively
            
            library('groundhog')
            test.day <- groundhog:::get.r.majmin.release()+105 #release of this R version + 45 days
            groundhog.library('haven',test.day-20,tolerate.R.version = groundhog:::get.rversion())         
            groundhog.library('rio',test.day+80)

            
#3 AFTER (so a conflict with a non-groundhog loaded pkg)
        library('groundhog')
        groundhog.library('pwr',test.day-1200,tolerate.R.version = groundhog:::get.rversion()) 
     
        #restart session
        library('groundhog')
        library('pwr')  #loading the older version
        groundhog.library('pwr',test.day) 

    
  #Date is too recent for CRAN
    library('groundhog')
    groundhog.library('pwr',Sys.Date()-1)
    
  #Date is too recent for github (will not show it if package is already installed)
    #temporarily change groundhog folder so warning is shown
      d0=get.groundhog.folder()
      temp_path<-paste0(d0,'/test1')
      set.groundhog.folder(temp_path)
      
      #>> restart session
        library(groundhog)
        groundhog.library('heliosdrm/pwr',Sys.Date()-50) #this is shown once, and then a cookie will not show it for 30 minutes
    
    #return to baseline\
        library(groundhog)
        set.groundhog.folder(d0)



########################################################################
#Set 2 - Previous Bugs
        test.day <- groundhog:::get.r.majmin.release()+45 #release of this R version + 45 days

#1) Loading but not attaching dependencies (verify one can run lmer() without lme4::)
    library('groundhog')
    
    pkgs=c("robustlmm","R2ucare")
    groundhog.library(pkgs,test.day)
    lmer(NA)  #should read error in formula rather than cannot find function
    
#2) Folder with space names
    library('groundhog')
    current.folder<-get.groundhog.folder()
    set.groundhog.folder(paste0(current.folder,'/temp groundhog folder'))

    groundhog.library('jsonlite',test.day)
    
    #Reset the default folder
    set.groundhog.folder(current.folder)
    

#3) Source a script which uses groundhog with a vector of packages, used to stop executing
    source("http://groundhogr.com/source_file_for_test_groundhog.r")
    
    
#4) tolerate
  #DONE above for other tasks
    
#5) Source
  library('groundhog')
   groundhog.library('jsonlite',test.day,force.install=TRUE,force.source = TRUE)
    
#6) Main source and main install.
  library('groundhog')
   groundhog.library('metafor',test.day,force.install.main=TRUE , force.source.main= TRUE)
   groundhog.library('rio',test.day,    force.install.main=TRUE , force.source.main= FALSE)


#7) Include suggests
   
    library('groundhog')
    pks='rio'
    groundhog.library(pks, test.day ,include.suggests = TRUE)

    
#8) Update cran to
    groundhog:::load.cran.toc(TRUE)
    

#8 #DO THIS BY HAND:::delete R_groundhog ('main folder') verifying what happens if consent is given denied
  library('groundhog') 
  groundhog.library('pwr',test.day)
  
  set.groundhog.folder(paste0(gd,"/temp1"))
  
  
  #Then run a .bat file that executes R code doing library('groundhog') and trying to run groundhog.library(). 
  #It should prompt running set.groundhog.folder()
  #use files 'not_interactive.bat', modifying the location of the R script and R binary used to execute it"
  

  
#9 ) Base packages 
  groundhog.library('stats',test.day)     #already attached always
  groundhog.library('parallel',test.day)  #gets attached, same version always
  groundhog.library('mgcv',test.day)      #gets installed from local
  
  
########################################################################
#Set 3 -  Install many packages
  #Function 
    
#                             TESTING GROUNDHOG 
#
#This script enables testing {groundhog} under various scenarios
#
#Because groundhog installs packages, and because the goal is 0 dependencies for groundhog
#these tests are not implemented using {testthat} nor are they run automatically
#This script is meant merely for sharing how groundhog is tested by the maintainer of the package
#i.e., for transparency of quality control. It is also useful for people who want to contribute to
#the package
#############################################################################################

#Function that Tests sets of packages loaded/installed from groundhog
  test.groundhog <- function(set.of.packages=c(1:100),seed=111,groundhog.day=NULL,day.offset=0)
    {
    #Arguments
          #set.of.packages: the number indicates their ranking of 'most downloaded' in a relatively recent day 
          #                 of CRAN logs (recent respect to groundhog.day
          #                 a negative number implies installing a random subset of available packages
          #seed:            packages are installed in random order, for a random date, 
          #                 seed gives reproducibility of possibel issues
          #groundhog.day    as in groundhog.library()
          #start.with       in case an issue is found which is idyosincratic for a pakcage, testing can start from
          #                 the next package in the list with this argument
    #set the seed  
          set.seed(seed)
    
    #If groundhoglibrary does not exist, create it
        if (!file.exists(get.groundhog.folder())) dir.create(get.groundhog.folder(),showWarnings = FALSE)
      
    #0 Which version of R is being used?
            r.majmin=groundhog:::get.r.majmin()              #Which R version is being used
         
      
    #1. Date: Random number of days after release of majmin of R
          if (is.null(groundhog.day)) groundhog.day=groundhog:::get.r.majmin.release()+sample(size=1,1:150)+day.offset
          
    #2 Load most downloaded packages if a positive number is entered
        if (sum(set.of.packages)>0)
        {
          
        #2.2 most.downloaded.path  
          
          file_name <- paste0("rank_" , r.majmin , ".csv")
          local.path <- paste0(get.groundhog.folder(), "/" , file_name)
          most.downloaded.path <- paste0("http://groundhogr.com/data/",file_name)
        #2.3 if not available locally, download from http://groundhogr.com
          if (!file.exists(local.path)) download.file(most.downloaded.path, local.path) 
          
        #2.4  read locally    
          packages <- read.csv(local.path)
        
       #2.5 Sort
          packages <- packages[order(-packages$downloads),]
  
        #2.5.1 never install gmailr, it creates error because it replaces the base function message() (!!!!)
          packages <- packages[packages$pkg!='gmailr',]
  
                  
        #2.6 Keep those selected
          packages <- packages[set.of.packages,]$pkg
          
        #2.7 Shuffle the order
          packages <- sample(packages)
          
        }
    
    #3 Get available packages otherwise
         if (sum(set.of.packages)<0) 
         {
          packages <- data.frame(available.packages())$Package  #all available packages 
          packages <- sample(packages, size= -set.of.packages)  #in random order, choose the negative number entered
                                                                #for example, select 50 at rnadom if number entered in -50
         }
    #4 Loop installing
          j <- 1
          
      #4.1 Base and recommended packages
          ip <- data.frame(installed.packages(),stringsAsFactors = FALSE,row.names = NULL)
          base <- subset(ip, Priority=="base")$Package
          recommended <- subset(ip, Priority=="recommended")$Package
          
         for (pkgk in packages)
          {
           
          #Counter
            cat(
                "\n--------- ",j," out of ",length(packages),"-- package:",pkgk," using R-",r.majmin," on ",
                as.character(as.Date(groundhog.day,"yyyy-m-d")),"\n"
                )
          
          #Execute groundhog.library()
            exec_line=paste0("groundhog.library('", pkgk, "'," , "'", groundhog.day , "')") #line to execute 
            eval(parse(text=exec_line))                                              

          #Verify every package in snowball is availablerunning
              snowball.k=groundhog:::get.snowball(pkgk,groundhog.day)
              ip = data.frame(installed.packages(),row.names = NULL,stringsAsFactors = FALSE)
              ip.pkg_vrs = paste0(ip$Package , "_" , ip$Version)
          #ignore deps
              
              ignore.deps <- groundhog:::ignore.deps_default()
              if (!all(snowball.k$pkg_vrs %in% ip.pkg_vrs | snowball.k$pkg %in% ignore.deps)) {
                  message("----------groundhog tester says:  INSTALLATION ERROR---------------")
                  message("active packages:\n")
                  cat(sort(active$pkg_vrs))
                  message("\n\nsnowball:\n")
                  cat(sort(snowball.k$pkg_vrs))
                  return(list(pkg=pkgk, active=sort(active$pkg_vrs),snowball=snowball.k$pkg_vrs))

              break
                  } else {
                  message("Groundhog tester says:\nSUCCESS\nAll pkg_vrs in snowball are available for j=",j," (",pkgk,")")
                  message("############################")
                } #End of check for installation/loading success
              j<-j+1
       
             }#End of install loop
      } #end of function
      
  
    
# Installation can be tested with most downloaded packages as of the release of R's version matching that being used
# or based on available packages when testing. 
   
  
    library('groundhog')
    test.groundhog(1:10)                  
    
    
    
  #Switch to copying
    groundhog:::save.cookie('copy_instead_of_renaming')
    groundhog:::cookie.exists("copy_instead_of_renaming")
    
  #Same pkgs again with an ofset of time and the copy method
    test.groundhog(1:10,day.offset=140)
    try.renaming.method.again()
    #back to top 10, should not install anything
    test.groundhog(1:10)                   
    
    restore.library()
    test.groundhog(120:129)                
    test.groundhog(120:129,day.offset=91)  
    #back to top 10, should not install anything
    test.groundhog(120:129)                   
    
    
    test.groundhog(560:569)               
    test.groundhog(980:989)                
    test.groundhog(2501:2510)              
    
########################################################################
#Set 4 -  Remote packages


#4.1 - Run in 4.0.3
    #---------------------------------------------------
    #Test conflicts when a remote depends on a remote
    # Key is whether the second package, a dependency of the 1st, is loaded without an error 

   library('groundhog') 
    groundhog.library('crsh/papaja',test.day+10)
    groundhog.library('tidymodels/broom',test.day)
    
    
  # Same, but now with non matching dates
    library('groundhog') 
    groundhog.library('crsh/papaja','2016-10-01')
    groundhog.library('tidymodels/broom','2016-10-02')  #SHOULD NOT WORK
    
    
    
  # Load remote, then try to load CRAN version
      library('groundhog') 
      groundhog.library('crsh/papaja','2016-10-01')   #remote broom dependency
      groundhog.library('broom','2016-10-01')    
    
      #Should say script needs to be revised

      
  #remote dependency already loaded
    library('groundhog')
    groundhog.library('tidymodels/broom','2020-04-01')  
    groundhog.library('crsh/papaja','2020-04-01')
  
        #Should work
    
   
    
  # remote dependency already loaded, but wrong date
    library('groundhog')
    test.day <- groundhog:::get.r.majmin.release()+105 #release of this R version + 45 days
  
  #---------------------------------------------------------------------------------  

#4.2 - Test some popular github packages
  
    
    
    
   library('groundhog')
   
    
    groundhog.library( "yihui/knitr",test.day)
    groundhog.library( "crsh/papaja",test.day)
    groundhog.library( "weirichs/eatModel",test.day) #hthis would not load in groundhog v2.2.1 but loads with v3.0.0
    groundhog.library(  "mlr-org/mlr",test.day)
    groundhog.library( "joey711/phyloseq",test.day)    #fails, no bioconductor
    groundhog.library( "kassambara/survminer",test.day)

  #Test now with copying
    groundhog:::save.cookie('copy_instead_of_renaming')
    groundhog:::cookie.exists("copy_instead_of_renaming")
     groundhog.library( "yihui/knitr",test.day  +  90)
    groundhog.library( "crsh/papaja",test.day)
    groundhog.library( "weirichs/eatModel",test.day) #hthis would not load in groundhog v2.2.1 but loads with v3.0.0
    groundhog.library(  "mlr-org/mlr",test.day)
    
    groundhog.library( "kassambara/survminer",test.day)

    
    
    
########################################################################
#Set 5 -  Others
    
#5.1 Conflict in source install
    
    library('groundhog')
    groundhog.library('metafor','2022-05-01')

    library(metafor)
    library('groundhog')
    groundhog.library('metafor','2023-03-01',force.install.main=TRUE,force.source.main=TRUE)
    
    
#5.2 Set dropbox path
      library('groundhog')
    gd=get.groundhog.folder()
      set.groundhog.folder(gd)
      groundhog.library('jsonlite','2023-03-01')
  
      #PENDING: add to see what happens when dropbox was set by previous version of groundhog
      
#5.3 two different volumes
      
      
 
########################################################################
#Set 6 - Restore  Library
    
       
#6.1 Undo all changes in tests above (strong tes)
    restore.library()
    
    
#--------------------------------------------------
  #function that get dataframe with installed packages, and pkg_vrs included
       get.ip.local <-function(){
      path <- .libPaths()[1]
      ip <- data.frame(utils::installed.packages(path), row.names = NULL, stringsAsFactors = FALSE)
        if (nrow(ip)>0)  ip$pkg_vrs <- paste0(ip$Package,"_",ip$Version)
        if (nrow(ip)==0) ip$pkg_vrs <- character()
        
       ip <- ip[,c(names(ip) %in% c("LibPath", "Package","Version","pkg_vrs"))]
        if (nrow(ip)>0)  {
            description.path <- paste0(ip$LibPath, "/" , ip$Package , "/DESCRIPTION")
            ip$md5 <- tools::md5sum(description.path)
            }
          
        if (nrow(ip)==0) ip$md5 <- character()
       return(ip)
        }  

    
      groundhog::set.groundhog.folder('c:/temp/202')
    
#6.2 Testing three packages with multiple date changes
  #1 Install current versions of some packages
    install.packages('metafor')
    
  #2 Take an inventory of packages
    library('groundhog')
    ip0=get.ip.local()

  #3 start new groundhog folder to ensure restore day
    library('groundhog')
    gd1=get.groundhog.folder()

    set.groundhog.folder(paste0(gd1,"/test_restore2"))
    get.groundhog.folder()
  #4 Do first change, which should create a restore point before it 
      #restart
      library('groundhog')
      groundhog.library(c('metafor'),'2022-05-15')
      ip1=get.ip.local()

  #5 Do another change
      #restart session
      library('groundhog')
      groundhog.library(c('metafor'),'2023-01-15')
      ip2=get.ip.local()
    
      
      library('groundhog')
      groundhog.library(c('rio'),'2023-01-15')
      ip3=get.ip.local()
  
  
  #6 Use copy method
      #restart session
      library('groundhog')
      groundhog:::save.cookie('copy_instead_of_renaming')
      groundhog:::cookie.exists("copy_instead_of_renaming")
      
      groundhog.library('jsonlite','2022-05-15')
      ip4=get.ip.local()

  #7 Add another package  copying
      library('groundhog')
      groundhog.library(c('jsonlite','rio'),'2023-01-15')
      ip5=get.ip.local()

  #8 Install without groundhog
      ip6=get.ip.local()
  
  #9 One more as renaming    
      library('groundhog')
      groundhog.library('metafor','2023-03-15')
      ip7=get.ip.local()
  
      
  #8 Restore
      restore.library()
      ip8=get.ip.local()
      
      
      sum(!ip0$pkg_vrs %in% ip1$pkg_vrs)
      sum(!ip0$pkg_vrs %in% ip2$pkg_vrs)
      sum(!ip0$pkg_vrs %in% ip3$pkg_vrs)
      sum(!ip0$pkg_vrs %in% ip4$pkg_vrs)
      sum(!ip0$pkg_vrs %in% ip5$pkg_vrs)
      sum(!ip0$pkg_vrs %in% ip6$pkg_vrs)
      sum(!ip0$pkg_vrs %in% ip8$pkg_vrs)
      
      

      
      
      
      