 message.batch.installation.feedback <- function(snowball, snowflakes,  k , cores)
  {
  #0 Log file path
        log_path <- file.path(get.groundhog.folder(),"batch_installation_log.txt")
    
  #1. Get installation time for this batch 
    sec.k <- get.parallel.time(snowball$installation.time[snowball$pkg %in% snowflakes[[k]] ] , cores) #utils.R #44
  
  #2  Get total remaining time
          if (k < length(snowflakes))
            {
            sec.tot=0
              for (j in k:length(snowflakes)) 
              {
              sec.tot <- sec.tot + get.parallel.time(snowball$installation.time[snowball$pkg %in% snowflakes[[j]] ] , cores) #utils.R #44
              } 
            } else {
              sec.tot =  sec.k
            }
    
  #3 Translate to H:M
     now             <- format(Sys.time(), "%H:%M")
     estimate.batch  <- format(Sys.time() + sec.k, "%H:%M")
     estimate.tot    <- format(Sys.time() + sec.tot, "%H:%M")
          
  #4 Print message on console
      if (k==1) {
          message2("\n\nWill now install ", nrow(snowball)," packages from source files, in " , length(snowflakes), " batches.")
          message1("   Packages within a batch are installed in parallel.")
          message1("   Batches are installed sequentially.\n")
          message1("   As of ",now, " all ", nrow(snowball), " packages are expected to finish installing around ",estimate.tot, ".")
          message1("   (but this is a noisy estimate).")
        }
            
          
    #4.1 Batch counter
            n.batch = length(snowflakes[[k]])
            message1("\n\n------------------------------------------------------")
            message2("Batch " , k , " of ",length(snowflakes)," has " , n.batch , " packages.")
                  
    #4.2 set of packages
            #Message 
                #list with multiple files
                  if (n.batch> 1 ) {
                      message1("  They are: ", pasteQC(snowball$pkg_vrs[snowball$pkg %in% snowflakes[[k]] ]))
                               }
                
                #Single file
                  if (n.batch==1) {
                     message1("  It is: ", snowball$pkg_vrs[snowball$pkg %in% snowflakes[[k]] ])
                  }

    #4.3 how much time  left 
           
            #Always say how long this batch      
               msg <- paste0("  As of ",now, " this batch is expected to finish installing by ",estimate.batch)
                                      
            #Say also total, unless this is the last batch
               if (k<length(snowflakes)) {
                msg <-paste0(msg, " and all batches by ",estimate.tot,".")
                }
            
            #Now show it
              message1(msg)
              message1("  (but these are noisy estimates).")
            
    
    #5 View(installation feedback)
              
        #This value is set in groundhog.library()
          if (.pkgenv[['view.feedback']]  == TRUE)
          {
            
              if (k==1) reminderk <-"- Times estimates can be quite off"
              if (k==2) reminderk <-"- Opt-out of this window with `view.feedback=FALSE`"
              if (k==3) reminderk <-"- Install pkgs sequentially with `cores=1`"
              if (k==4) reminderk <-"- Minimize installs using same groundhog.day across scripts"
              if (k==5) reminderk <-"- See when pkg versions were published with toc(<pkg>)"
              if (k==6) reminderk <-"- Web:   https://groundhogR.com"
              if (k==7) reminderk <-"- GitHub https://github.com/CredibilityLab/groundhog/"
              if (k==8) reminderk <-"                </tips>"
              if (k>8 ) reminderk <-""
              installation_feedback.k <- data.frame(
                          as_of          = now,
                          batch          = paste0(k," of ",length(snowflakes)),
                          time_batch_installs = estimate.batch,
                          time_all_installs   = estimate.tot,
                          package_count_in_batch  = length(snowflakes[[k]]),
                          tips       = reminderk
                        )
              
           #Update .pkgenv[[]]   
              if (!is.null(.pkgenv[['df.feedback']])) .pkgenv[['df.feedback']]<-rbind(.pkgenv[['df.feedback']] , installation_feedback.k)
              if (is.null(.pkgenv[['df.feedback']]))  .pkgenv[['df.feedback']]<-installation_feedback.k 
              groundhog_installer <- .pkgenv[['df.feedback']]
              groundhog_installer=data.frame(a=1,b=1,c=1,e=1,f=1,g=1)
              names(groundhog_installer)=c("As of this time:",
                                           "Processing Batch #",
                                           "Estimated completion this batch", 
                                           "Estimated completion all batches",
                                           "# pkgs this batch",
                                           "Tips while you wait")
              View(groundhog_installer)
              try(View(groundhog_installer),silent=TRUE)
          }  
            
    }