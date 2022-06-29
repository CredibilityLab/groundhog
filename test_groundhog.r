
  
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
#install.packages('C:/git/groundhog_1.9.9.2022.06.27.tar.gz',repos=NULL)

library('groundhog')



#Date to use for testing individual packages
    test.day <- groundhog:::get.r.majmin.release()+45 #release of this R version + 45 days

    set.groundhog.folder('c:/temp/full_test')
    
    
    
    
    
#Set 0 - various forms of calling packages to be loaded
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
#Set 1 - Error and warnings with conflicts


  #Install in local library an older version of pwr to create conflicts later on
    install.packages("https://cran.r-project.org/src/contrib/Archive/pwr/pwr_1.2-2.tar.gz",repos=NULL,type='source')


  #Test conflict 1 - another version  already attached
   library(groundhog)
   library('pwr')
   groundhog.library('pwr',test.day)  #solve conflict, ask for restart

   
  #Test conflict 2 - other versions of package loaded, cannot attach, later offered to uninstall 
                       #(since it is the package being called with conflict no offer to load early or ignore conflict, only to uninstall)
    library(groundhog)
    pwr::pwr.t.test(n=50,d=.4)  #load pkg
    groundhog.library('pwr',  test.day) #Unload wrong version, load correct version

    
    
  #Test conflict 3 - same as 2, but conflict triggered by a loaded dependency rather than target package
    library(groundhog)
    library('vctrs')
    
    sessionInfo()
    groundhog.library('tibble','2022-04-01')

  #Test conflict with recommended package
    library(groundhog)
    x=MASS::abbey      #different version
    groundhog.library('MASS','2022-03-01')
    
    
    
########################################################################
#SET OF TESTS 2 -  CHECKING PREVIOUSLY FOUND BUGS
    
#1) Loading but not attaching dependencies (verify one can run lmer() without lme4::)
    library('groundhog')
    
    pkgs=c("robustlmm","R2ucare")
    groundhog.library(pkgs,'2022-05-01')
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

  groundhog.library('pwr','2018-08-01')
  groundhog.library('pwr','2018-08-01',tolerate.R.version='4.1.3')

    
#5) Source
    library('groundhog')
   groundhog.library('jsonlite','2020-08-01',tolerate.R.version = '4.2.0', force.install=TRUE,force.source = TRUE)
    
#6) Test for re-install
    library('groundhog')
    groundhog.library('jsonlite','2020-08-01',tolerate.R.version = '4.2.0',force.install = TRUE)
    
    
#7) Test for re-install with current date and source
    library('groundhog')
    pks=c('jsonlite','metafor')
    groundhog.library(pks,test.day,force.install = TRUE, force.source = TRUE)
    
#9) Include suggests
    library('groundhog')
    pks=c('jsonlite','metafor')
    groundhog.library(pks, "2022-06-10",include.suggests = TRUE)

#10) Load cran toc leads to errors
    groundhog:::load.cran.toc(TRUE)
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
    set.groundhog.folder("c:/temp/test_groundhog20220626")
    groundhog.library('pwr','2021-08-15')
    library('groundhog')
    test.groundhog(1:10)         #Install the 100 most downloaded packages 
    test.groundhog(310:335)        #install the 500-525 most downloaded packages
    test.groundhog(-10, seed=9)  #install 10 random packages available right now for this version of R

    
    groundhog.library('pbdMPI','2017-04-01')
    
    
#---------------------------------------------  
#4.  Remotes
  
#4.1 Remote A dependson Remote B, verify Remote B can be attache 
    
    #Run in 3.6.3 - Key is whether the second package, a dependency of the 1st, is laoded without an error 
   
    

   library('groundhog') 
    groundhog.library('crsh/papaja','2016-10-01')
    

    groundhog.library('tidymodels/broom','2016-10-01')  #SHOUDL WORK
    
    


#4.2 Same, but now with non matching dates
    library('groundhog') 
    groundhog.library('crsh/papaja','2020-04-01')
    groundhog.library('tidymodels/broom','2020-04-02')  #SHOULD NOT WORK
    
    
    toc('git2r')
#4.3 Load remote, then try to load CRAN version
      library('groundhog') 
      groundhog.library('crsh/papaja','2020-04-01')   #remote broom dependency
      groundhog.library('broom','2020-04-01')    
    
      #Should say script needs to be revised

      
#4.4 remote dependency already loaded
    library('groundhog')
    groundhog.library('tidymodels/broom','2020-04-01')  
    groundhog.library('crsh/papaja','2020-04-01')
  
        #Should work
    
   
    
#4.5 remote dependency already loaded, but wrong date
    library('groundhog')
    groundhog.library('tidymodels/broom','2020-04-02')  
    groundhog.library('crsh/papaja','2020-04-01')
    
    
#4.6 CRAN pkg with CRAN dependency and the dependency is already loaded as remote
    library('groundhog')
    groundhog.library('tidymodels/broom','2020-04-02')  
    groundhog.library('dragon','2020-04-02', ignore.deps = 'broom')  

   
    
    install.packages('C:/git/groundhog_1.9.9.2022.06.27.tar.gz',repos=NULL)
    library('groundhog')
    
#5 - Popular github packages
    popular_github_packages<-c(
            "tidyverse/dplyr",
            #"YuLab-SMU/clusterProfiler",  - requires bioconductor
            "satijalab/Seurat",
            "insightsengineering/teal.reporter",
            "RamiKrispin/TSstudio",
            "rmaia/pavo",
            # "joey711/phyloseq",              - requires bioconductor
            "rmcelreath/rethinking",
            #"jokergoo/ComplexHeatmap",
            "Rdatatable/data.table",
            "rstudio/connectwidgets",
            "sparklyr/sparklyr",
            "rstudio/reticulate",
            "tidyverse/ggplot2",
            "benjjneb/dada2",
            "insightsengineering/tern.rbmi",
            "kassambara/survminer",
            "plotly/plotly.R",
            "timoast/Signac",
            "Netflix/metaflow",
            "yihui/knitr",
            "mlr-org/mlr",
            "ropensci/drake",
            "rstudio/rticles",
            "r-spatial/sf",
            "ropensci/skimr",
            "mitchelloharawild/vitae",
            "stan-dev/rstan",
            "business-science/tidyquant")
    
    
    
    library('groundhog')    
    k=14
    for (pk in popular_github_packages[k:length(popular_github_packages)])
    {
    message('-------------------------------------------------------')
    message('---   groundhog testser: [',k,']  ',pk,'       ---')
    
      groundhog.library(pk,'2017-02-10')    
    k=k+1
      
    }
    
    


    groundhog.library('jeroen/jsonlite','2001-01-01',tolerate.R.version = '3.3.3')

