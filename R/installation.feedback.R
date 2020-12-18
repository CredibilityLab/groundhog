#' Installation feedback
#'
#' Print progress about the package installation process (number of packages
#' already installed or remaining, time left, etc.)
#'
#' @inheritParams r.version.check
#' @inheritParams estimate.seconds.left
#'
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


  # Show feedback
	  msg.header <- paste0("\n\n\ngroundhog says: Installing '", snowball[k, "pkg_vrs"], "', package #", k, 
	                       " (from source) out of ", N, " needed")
	  msg <- c(
    		"> As of ", format(Sys.time(), "%H:%M"), ", the best guess is that all ", N," packages will be installed ",
    		  "around ",   finish.time.estimate,".\n",
    		"  It is somewhat unlikely (but not impossible) for the process to last past ", finish.time.max, "\n",
    		"> Estimates are revised after each package installs, but will remain noisy throughout")
	
	  

  # Add msg if R mismatch and recent enough for groundhog and using Windows or Mac
	  if (rv$r.using.majmin != rv$r.need.majmin & date > "2014-09-18" & .Platform$pkgType != "source") {
		msg <- c(
		  msg,
		  "\n> Installation is slow because you are using R-", rv$r.using.full,", a major update on the version\n",
		  " available on the requested date: '",date,"'.\n",
		  "> If you run this script with R-", rv$r.need.full, ", the installation would be faster.\n",
		  "> Moreover, note that some scripts will give different results in different versions of R.\n",
		  "> Instructions for running previous versions of R:  https://groundhogR.com/many"
		)
	  } # End if R mismatch

  # Add message if installing from source that output has been suppressed
	  if (snowball[k, "from"] == "source") {
		msg <- c(
		  msg,
		  "\n\n> When installing a package from source, abundant and fast-speed output is generated \n",
		  " flooding the console where these messages are printed. Thus, groundhog.library() supresses\n",
		  " such output. You may run groundhog.library() with the option 'quiet.install=FALSE' to display all output."
		) # End of message
		
	   
	  } # End of if source file
	  
	  



  message2(msg.header)
  message1(msg)
}
