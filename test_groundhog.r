#This version : 2021 03 11


#install.packages("http://groundhogr.com/xxxxx.tar.gz",repos=NULL,type='source') #change this line for the current development version


# Groundhog testing is  not automatized because an r session needs to be restarted each time, 
# and the output carefully (humanly) evaluated.


#Sets of tests #
  #Set 1 - Error and warnings with conflicts
  #Set 2 - Previously documented bugs
  #Set 3 - Automatized installation via function


#######################################    
#Set 1 - Error and warnings with conflicts

  #date to use for testing individual packages
    test.day <- groundhog:::get.r.majmin.release()+45 #release of this R version + 45 days

  
    #Install older pwr package
    install.packages('https://cran.r-project.org/src/contrib/Archive/pwr/pwr_1.2-2.tar.gz',repos=NULL,type='source')

   
 #Test taht not answring prompt leads to loop taht prevents other code from being executed
      library(groundhog)
      groundhog.library('pwr', "2017-01-01")
      #run this to test the prompt loop
    
     
  #Test conflict 1 - another version  already attached
   library(groundhog)
   library('pwr')
   groundhog.library('pwr',test.day)  #error, not attached, even after repeated, same message

   reinstall.conflicts("2021-04-01")
   
   
  #Test conflict 2 - other versions of package loaded, cannot attach, later offered to uninstall 
                       #(since it is the package being called with conflict no offer to load early or ignore conflict, only to uninstall)
    library(groundhog)
    pwr::pwr.t.test(n=50,d=.4)  #load pkg
    groundhog.library('pwr',  test.day) #Default error ctrl-shift-f10
    groundhog.library('pwr',  test.day) #2nd time, offer to uninstall, accept it and see if it works
      rnorm(100)
      rbeta(100,4,3)
      plot(rnorm(100),rnorm(100))
    see.unistalled.conflicts()
    reinstall.conflicts('2021-04-01')
    
    
  #Test conflict 3 - same as 2, but conflict triggered by a loaded dependency rather than target package
    #install.packages("https://cran.r-project.org/src/contrib/Archive/hms/hms_0.1.tar.gz",repos=NULL,type='source')  #hms is a dependency of haven, install old version
    library(groundhog)
    library('haven')
    groundhog.library('hms',test.day)
    
  #Test conflict 4 - another version already attached, for an ignore.deps case, same behavior as test conflict 1
    library(groundhog)
    library('knitr')
    groundhog.library('knitr',test.day)
  
  #Test conflict 5 - another version is loaded, and it is an ignored deps, so it is attached with a warning
    knitr::all_labels()  #to trigger loading but not attaching knitr
    library(groundhog)
    groundhog.library('knitr',test.day)
    
 
  #Test conflict 6 - another version already loaded, for dependencies in ignore.deps
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
    
#2) Folder with space names
   library('groundhog')
    current.folder <- get.groundhog.folder()
    set.groundhog.folder(paste0(current.folder," a  a  a  a "))
    groundhog.library('pwr',test.day)
    set.groundhog.folder("c:/dropbox/groundhog_folder")
    
#3) Source a script which uses groundhog with a vector of packages
   library('groundhog')
   source("http://groundhogr.com/source_file_for_test_groundhog.r")
    
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
          
        #2.5.1 drop gmailr, it creates error with message()
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
            exec_line=paste0("groundhog.library(", pkgk, "," , "'", groundhog.day , "')") #line to execute 
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
          
    test.groundhog(1:100)         #Install the 100 most downloaded packages   -- note, gmailr is a package that creates a conflict with message(), generating errors unrelated to groundhog
    test.groundhog(500:525)       #install the 500-525 most downloaded packages
    test.groundhog(-10, seed=29)  #install 10 random packages available right now for this version of R

    
    
#SET 4 ABILITY TO USE DIFFERNT R VERSION (older than desired date)
    #Run this in R-3.6.3
    library(groundhog)
    groundhog.library('pwr' , '2021-04-01'), tolerate.R.version = '3.6.3')
    
    
    
    
    