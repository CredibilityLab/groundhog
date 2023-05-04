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
              tips<-c("- Opt-out of this window with `view.feedback=FALSE`" , 
                      "- Use same groundhog.day across scripts to minimize installs" , 
                      "- See when pkg versions were published with toc(<pkg>)" ,
                      "- Use single groundhog.library() call for many pkgs to save time",
                      "- See when pkg versions were published with toc(<pkg>)" , 
                      "- Read a blogpost while you wait: https://datacolada.org",
                      "- Bugs & suggestions -> https://github.com/CredibilityLab/groundhog/",
                      "- Visit groundhog's webapge -> https://groundhogr.com"
                      )

            # "Shuffle" them 
              tips<-tips[order(snowball$pkg[1:length(tips)])]
              
              #Cannot just do 'sample()' to shuffle because this is called multiple times across loops so 
              #it repeats the tips. Could solve with seed, but Do not want to set.seed() 
              #because that's global and seems undesirable to change seed globally
              #So, sort by the alphabetical order of 'pkg' in the snowball, which is arbitrary enough
              #Tips appear in the order() of the alphabetical order of the K first pkgs, where K is the # of tips
              
              
            
            #Add 1st time, and trail 1000 empty tips
              tips<-c("- Time estimates are noisy (merely orientative)                           |",
                      tips,
                      rep('',1000))
              
            #Generate the 'dataframe'
              installation_feedback.k <- data.frame(
                          as_of          = now,
                          batch          = paste0(k," of ",length(snowflakes)),
                          time_batch_installs = estimate.batch,
                          time_all_installs   = estimate.tot,
                          package_count_in_batch  = length(snowflakes[[k]]),
                          tips       = tips[k],
                          stringsAsFactors = FALSE
                        )
              
           #Update .pkgenv[[]]   
              if (!is.null(.pkgenv[['df.feedback']])) .pkgenv[['df.feedback']]<-rbind(.pkgenv[['df.feedback']] , installation_feedback.k)
              if (is.null(.pkgenv[['df.feedback']]))  .pkgenv[['df.feedback']]<-installation_feedback.k 
              groundhog_installer <- .pkgenv[['df.feedback']]
                            names(groundhog_installer)=c("As of this time:",
                                           "Processing Batch #",
                                           "Estimated completion this batch", 
                                           "Estimated completion all batches",
                                           "# pkgs this batch",
                                           "Tips while you wait")
              try(utils::View(groundhog_installer),silent=TRUE)
          }  
            
    }