# Installation feedback
#
# Print progress about the package installation process (number of packages
# already installed or remaining, time left, etc.)
#

installation.feedback <- function(k, date, snowball, start.time) {

  # Get R being used and needed
  
	rv <- r.version.check(as.DateYMD(date))

  # How much time so far in this installation process
	time.so.far <- as.numeric(difftime(Sys.time(), start.time, units = "secs"))

  # Shorter variables
	  N <- nrow(snowball)
	  pkg_vrs <- snowball[N, "pkg_vrs"]
	  

  # Estimate of time
	  seconds.left <- estimate.seconds.left(k, start.time, snowball)
	  finish.time.estimate <- format(Sys.time() + seconds.left, "%H:%M")
	  finish.time.max <- format(Sys.time() + seconds.left * 3, "%H:%M")


  # Msg of how many are being installed
	  msg.header <- paste0("\n\n\ngroundhog says: Installing '", snowball[k, "pkg_vrs"], "', package #", k, 
	                       " (from source) out of ", N, " needed")
	  
	#Give estimate time
	  msg <- c(
    		"> As of ", format(Sys.time(), "%H:%M"), ", the best guess is that all ", N," packages will install ",
    		  "around ",   finish.time.estimate)
  
	 #Giver upper end if more than 90 seconds		  
    if (seconds.left>90) {
      msg <- c(msg, ", most likely before ", finish.time.max, ".\n")
    }	
	  
  # Add msg if R mismatch and recent enough for groundhog and using Windows or Mac
	  if (rv$r.using.majmin != rv$r.need.majmin && date > "2014-09-18" && .Platform$pkgType != "source" && seconds.left>90) {
		  msg <- c(
		  msg,
		  "> Installation would be faster in R-", rv$r.need.full, ", which was current on: '",date,"'.\n",
		  "> Instructions for running other versions of R:  https://groundhogR.com/many"
		)
	  } # end if R mismatch


	  
	  



  message2(msg.header)
  message1(msg)
}
