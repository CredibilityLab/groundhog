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
          
        #2.3 if not available locally, download from http://groundhogr.com
          if (!file.exists(local.path)) download.file(most.downloaded.path, local.path) 
          
        #2.4  read locally    
          packages <- read.csv(local.path)
        
        #2.5 Sort
          packages <- packages[order(-packages$downloads),]
          
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
      
  