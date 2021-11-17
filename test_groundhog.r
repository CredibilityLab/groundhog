# {groundhog}  tester
#
#This version : 2021 09 04 
#install.packages("http://groundhogr.com/groundhog_1.4.0.9009.tar.gz",repos=NULL,type='source', method='libcurl') #change this line for the current development version
#(note, this testing version, 1.4.0.9001  has not yet been created, it's the next to be used)
#To play around with code below, use the version on CRAN v1.4.0: install.packages('groundhog')

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



#Date to use for testing individual packages
    test.day <- groundhog:::get.r.majmin.release()+45 #release of this R version + 45 days

    test.day <-'2021-09-04'

    
    set.groundhog.folder('c:/dropbox/groundhog_folder/temp8')
#Set 0 - various forms of calling packages to be loaded

  #Single package, with and without quotes
    library('groundhog')
    groundhog.library('pwr',test.day)  #quotes
    groundhog.library(pwr,test.day)    #no quotes

  #Object containing a single package
    pkg1='pwr'
    groundhog.library(pkg1,test.day)    
    
  #Object containing many packages
    pkg2=c('pwr','metafor')
    groundhog.library(pkg2,test.day)    #single package, no quotes, show warning to use ""

  #Direct cal to many packages
    groundhog.library(c('pwr','metafor'),test.day)    
    
    
#######################################    
#Set 1 - Error and warnings with conflicts


  #Install in local library an older version of pwr to create conflicts later on
    install.packages("https://cran.r-project.org/src/contrib/Archive/pwr/pwr_1.2-2.tar.gz",repos=NULL,type='source')


  #Test conflict 1 - another version  already attached
   library(groundhog)
   library('pwr')
   groundhog.library('pwr',test.day)  #error, not attached, even after repeated, same message

   
  #Test conflict 2 - other versions of package loaded, cannot attach, later offered to uninstall 
                       #(since it is the package being called with conflict no offer to load early or ignore conflict, only to uninstall)
    library(groundhog)
    pwr::pwr.t.test(n=50,d=.4)  #load pkg
    groundhog.library('pwr',  test.day) #Default error ctrl-shift-f10
    groundhog.library('pwr',  test.day) #2nd time, offer to uninstall, accept it and see if it works
    
    see.unistalled.conflicts()
    reinstall.conflicts()
    
    
  #Test conflict 3 - same as 2, but conflict triggered by a loaded dependency rather than target package
    library(groundhog)
    library('rio')
    groundhog.library('tibble',test.day)

  #Test conflict 4 - another version already attached, for an ignore.deps case, same behavior as test conflict 1
    library(groundhog)
    library('knitr')
    groundhog.library('knitr',test.day)
  
  #Test conflict 5 - another version is loaded, and it is an ignored deps, so it is attached with a warning
    knitr::all_labels()  #to trigger loading but not attaching knitr
    library(groundhog)
    groundhog.library('knitr',test.day)
  
  #Test conflict 6 - another version already loaded, for involving dependencies in ignore.deps
    #install.packages('rio')
    library('groundhog')
    library('rio')
    groundhog.library('dplyr',test.day)   #Fails, then gives all 3 options
    groundhog.library('foreign',test.day) #Gives warning because it is a recommended packageoptions
    groundhog.library('AER',test.day)     #Fails, then gives all 3 options
    
    
########################################################################
#SET OF TESTS 2 -  CHECKING PREVIOUSLY FOUND BUGS
    
#1) Loading but not attaching dependencies (verify one can run lmer() without lme4::)
    library('groundhog')
    
    pkgs=c("robustlmm","R2ucare")
    groundhog.library(pkgs,test.day)
    lmer(NA)  #should read error in formula rather than cannot find functin
    
#2) Folder with space names
    library('groundhog')
    current.folder<-get.groundhog.folder()
    #PC:  set.groundhog.folder("c:/temp/another folder with spaces")
    #MAC: set.groundhog.folder(paste0(current.folder,'/temp groundhog folder')

    groundhog.library('jsonlite',test.day)
    
    #Reset the default folder
    set.groundhog.folder(current.folder)
    

#3) Source a script which uses groundhog with a vector of packages, used to stop executing
    source("http://groundhogr.com/source_file_for_test_groundhog.r")
    
    
#4) Run with option tolerate.R.Version (run this in R-3.2.5)
    groundhog.library('pwr','2021-08-01',tolerate.R.version = '3.2.5')
    
    
#ADDITIONAL OPTIONS

#5) Test source version
    library('groundhog')
    groundhog.library('pwr','2018-08-01',tolerate.R.version = '3.2.5',force.source = TRUE)
    
#6) Test for re-install
    library('groundhog')
    groundhog.library('pwr','2018-08-01',tolerate.R.version = '3.2.5',force.install = TRUE)
    
#7) Test for re-install with current date
    library('groundhog')
    groundhog.library('pwr',test.day,force.install = TRUE)
    
#8) Test for re-install with current date and source
    library('groundhog')
    pks=c('pwr','metafor')
    groundhog.library(pks,test.day,force.install = TRUE, force.source = TRUE)
    
#9) Include suggests
    library('groundhog')
    pks=c('pwr','rio')
    groundhog.library(pks, "2021-08-15",include.suggests = TRUE)
    
#
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
    set.groundhog.folder("c:/dropbox/groundhog_folder/temp7")
    groundhog.library('pwr','2021-08-15')
    library('groundhog')
    test.groundhog(1:10)         #Install the 100 most downloaded packages 
    test.groundhog(500:525)        #install the 500-525 most downloaded packages
    test.groundhog(-10, groundhog.day='2021-09-04',seed=9)  #install 10 random packages available right now for this version of R
