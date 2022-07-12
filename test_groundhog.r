
  
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
  #Set 1 - Error and warnings with conflicts
  #Set 2 - Previously documented bugs
  #Set 3 - Automatized installation of packages in random order, via function
  #Set 4 - Remotes (github and gitlab)

#Version being tested (only locally available)
install.packages('http://groundhogr.com/groundhog_1.9.9.2022.07.12-1539.tar.gz',repos=NULL)
groundhog:::load.cran.toc(TRUE)
library('groundhog')



#Date to use for testing individual packages
    test.day <- groundhog:::get.r.majmin.release()+45 #release of this R version + 45 days
    set.groundhog.folder('c:/temp/full_test3')
    

#Set 0 - various forms of calling packages to be loaded
  
  #Missing/wrong arguments
      #Missing pkg
        groundhog.library(date=test.day)
    
      #Missing date
        groundhog.library('rio')
    
      #wrong format for date
        groundhog.library(2,2)
        
      #Nonexistent pkg on cran
        groundhog.library(2,test.day)
    
      #inexistent pkg on github
        groundhog.library('github::crsh/powerful_999',test.day)
    
        
  #Single package, with and without quotes
    library('groundhog')
    groundhog.library('pwr',test.day)  #quotes
    groundhog.library(pwr,test.day)    #no quotes

  #Object containing a single package
    pkg1='pwr'
    groundhog.library(pkg1,test.day)    
    
  #Vector containing many packages
    pkg2=c('pwr','metafor')
    groundhog.library(pkg2,test.day)    #single package, no quotes, show warning to use ""

  #Direct cal to many packages
    groundhog.library(c('pwr','metafor'),test.day)    
    
  #Sandwiching library
    library('groundhog')
    groundhog.library("
          library('pwr')
          library(metafor)
          library(tidyr)
          ", 
      test.day)
    
  
    
    
    
    
#######################################    
#Set 1 - Enumerated conflicts in 'check.snowball.conflict.R'

    #Conflict 1: force Install (is any package that needs to be *installed*  loaded)
    
        #Conflict caused by R Studio loading the pkg
          library('groundhog')
            pwr::cohen.ES()
            groundhog.library('pwr',test.day,force.install=TRUE)
            ok

        #Conflict caused by groundhog
            groundhog.library('pwr',test.day)
            groundhog.library('pwr',test.day,force.install=TRUE)
            ok
    
        
    #Conflict 2: Same remote, different date
        library('groundhog')
        groundhog.library('heliosdrm/pwr',test.day)
        groundhog.library('heliosdrm/pwr',test.day+1)
        ok
        

    #Conflict 3: Requested package was previously loaded with groundhog but different version or repository
        library('groundhog')
        groundhog.library('pwr',test.day)
        groundhog.library('heliosdrm/pwr',test.day)
  
        
    #Conflict 4: Dependency conflicts with previously loaded groundhog package
        
        #RUN THIS IN 4.1.x
          library('groundhog')
          groundhog.library('haven','2021-08-01')  
          groundhog.library('rio','2021-08-05')
          groundhog.library('rio','2021-08-05',ignore.deps='haven') 
        
        #RUN THIS IN 4.1.x
          library('groundhog')
          groundhog.library('crsh/papaja',test)  
          groundhog.library('rio','2021-08-05') 
        
          

        #Other R     
          library('groundhog')
          groundhog.library('rio',test.day)                              #load rio
          groundhog.library('haven',test.day-80,tolerate=groundhog:::get.rversion()) #try to load a depenency, haven, with diff date
  
        
        
    #Conflict 5: Any pkg in snowball was already loaded, different version, not with groundhog
        library('groundhog')

        #1 Install one version of pwr (v1.2-2)
            groundhog.library('pwr','2020-01-01',tolerate.R.version=groundhog:::get.rversion()) # Date 1
          #--->  Restart R session (that version will now be the default pkg)
        
        #2 load it
          library('groundhog')
          pwr::pwr.2p.test()
          
        #3 Now try to groundhog another date (v1.3.0)
            groundhog.library('pwr','2022-01-01',tolerate.R.version=groundhog:::get.rversion()) # Date 2
    
            
            
    #Conflict 5 with recommended pkg:
        library('groundhog')

        #1 Install one version v7.3-51.5
            groundhog.library('MASS','2020-01-01',tolerate.R.version=groundhog:::get.rversion()) # Date 1
  
          #--->  Restart R session (that version will now be the default pkg)
        
        #2 load it
          library('groundhog')
          x=MASS::abbey      #different version

          
        #3 Now try to groundhog another date (v7.3-54)
            groundhog.library('MASS','2022-01-01',tolerate.R.version=groundhog:::get.rversion()) # Date 2
    
        #4 restrat again (now it should load without a problem
          library('groundhog')
          x=MASS::abbey      
          groundhog.library('MASS','2022-01-01',tolerate.R.version=groundhog:::get.rversion()) # Date 2

            
    
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
#SET OF TESTS 2 -  CHECKING PREVIOUSLY FOUND BUGS
    
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
  library('groundhog')

  groundhog.library('pwr',test.day-400)
  ok
  groundhog.library('pwr',test.day-400,tolerate.R.version=groundhog:::get.rversion())

    
#5) Source
  library('groundhog')
   groundhog.library('jsonlite',test.day-400,force.install=TRUE,force.source = TRUE,tolerate.R.version=groundhog:::get.rversion())
    

#6) Include suggests
    library('groundhog')
    pks=c('jsonlite','metafor')
    groundhog.library(pks, test.day ,include.suggests = TRUE)

#7) Update cran to
    groundhog:::load.cran.toc(TRUE)
    
    
#8 #DO THIS BY HAND:::elete R_groundhog ('main folder') verifying what happens if consent is given denied
  
    
  library('groundhog')  #should be asked to give OK
  
  groundhog.library('pwr',test.day)
    
  
    
########################################################################
#SET OF TESTS 3 -  INSTALLATION OF PACKAGES IN RANDOM ORDER
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
  test.groundhog <- function(set.of.packages=c(1:100),seed=111,groundhog.day=NULL)
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
          if (is.null(groundhog.day)) groundhog.day=groundhog:::get.r.majmin.release()+sample(size=1,1:150)
          
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
          ip <- data.frame(installed.packages(),stringsAsFactors = FALSE)
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

          #Verify every package in snowball is active after running
              snowball.k=groundhog:::get.snowball(pkgk,groundhog.day)
              active=groundhog:::get.active()
              
          #ignore deps
              ignore.deps <- groundhog:::ignore.deps_default()
              if (!all(snowball.k$pkg_vrs %in% active$pkg_vrs | snowball.k$pkg %in% ignore.deps)) {
                  message("----------groundhog tester says:  INSTALLATION ERROR---------------")
                  message("active packages:\n")
                  cat(sort(active$pkg_vrs))
                  message("\n\nsnowball:\n")
                  cat(sort(snowball.k$pkg_vrs))
                  return(list(pkg=pkgk, active=sort(active$pkg_vrs),snowball=snowball.k$pkg_vrs))

              break
                  } else {
                  message("Groundhog tester says:\nSUCCESS\nAll pkg_vrs in snowball are active for j=",j," (",pkgk,")")
                  message("############################")
                } #End of check for installation/loading success
              j<-j+1
       
             }#End of install loop
      } #end of function
      
  
    
# Installation can be tested with most downloaded packages as of the release of R's version matching that being used
# or based on available packages when testing. 
    library('groundhog')
    test.groundhog(1:10)         #Install the 10 most downloaded packages 
    test.groundhog(510:525)      #install the 501-510 most downloaded packages
    test.groundhog(-10, seed=9)  #install 10 random packages available right now for this version of R

    
#---------------------------------------------  
#4.  Remotes

  

      
#4.1 - Run in 4.0.3
    #Test conflicts when a remote depends on a remote
    # Key is whether the second package, a dependency of the 1st, is loaded without an error 

   library('groundhog') 
    groundhog.library('crsh/papaja','2021-03-01')
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
    groundhog.library('tidymodels/broom','2020-04-02')  
    groundhog.library('crsh/papaja','2020-04-01')
    
    

#4.2 - Popular github packages
    popular_github_packages<-c(
            "tidyverse/dplyr",
            "satijalab/Seurat",
            "insightsengineering/teal.reporter",
            "RamiKrispin/TSstudio",
            "rmaia/pavo",
            "Rdatatable/data.table",
            "rstudio/connectwidgets",
            "sparklyr/sparklyr",
            "rstudio/reticulate",
            "tidyverse/ggplot2",
            "benjjneb/dada2",
            "insightsengineering/tern.rbmi",
            "kassambara/survminer",
            "plotly/plotly",
            "timoast/Signac",
            "Netflix/metaflow",
            "yihui/knitr",
            "mlr-org/mlr",
            "ropensci/drake",
            "rstudio/rticles",
            "r-spatial/sf",
            "ropensci/skimr")
    
    
    
    groundhog.library('mlr-org/mlr',test.day)
    
    library('groundhog')    
      test.day <- groundhog:::get.r.majmin.release()+95 #release of this R version + 45 days

    k=2
    for (pk in popular_github_packages[k:length(popular_github_packages)])
    {
    message('-------------------------------------------------------')
    message('---   groundhog testser: [',k,']  ',pk,'       ---')
    
    groundhog.library(pk,test.day)    
    k=k+1
      
    }
    test.day='2022-06-09'
    Sys.setenv(PATH = paste("C:/Rtools/bin", Sys.getenv("PATH"), sep=";"))

    groundhog.library('jeroen/jsonlite',test.day)

