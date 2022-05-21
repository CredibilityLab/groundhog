#This function installs all needed packages in a snowball but does not load them
#Designed for installing locally packages with a conflict so that they can be localized.



  groundhog.install<-  function(snowball)
  { 
  #1 Get vrs
     # vrs     <- get.version(pkg, date)

  #2 Update cran.toc() if needed for entered date 
      #update_cran.toc_if.needed(date)

  #3 Get snowball
      #snowball <- get.snowball(pkg=pkg , date=date , include.suggests=FALSE, force.install=FALSE)

  #4 message if installation will be necessary
    need.to.install.total <- sum(!snowball$installed)
    if (need.to.install.total > 0) {
      message2()
      message1("Will now install ",need.to.install.total, " packages")
      } #End 7
    
    
  #5 Install package
      install.snowball(snowball,date=date)
      
 
  } #End of groundhog.install() function

