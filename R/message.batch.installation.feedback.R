 message.batch.installation.feedback <- function(snowball, snowflakes,  k , cores)
  {
  #0 Log file path
        log_path <- paste0(get.groundhog.folder(),"/batch_installation_log.txt")
    
    
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
          
  #4 Print messageShow 
        if (k==1) {
              
              message2("\n\nWill now install ", nrow(snowball)," packages from source files, in " , length(snowflakes), " batches.")
              message1("   Packages within a batch are installed in parallel.")
              message1("   Batches are installed sequentially.\n")
              message1("   As of ",now, " all ", nrow(snowball), " packages are expected to finish installing around ",estimate.tot, ".")
              message1("   (but this is a noisy estimate).")
            }
            
            
        #5.1 Batch counter
            n.batch = length(snowflakes[[k]])
            message1("\n\n------------------------------------------------------")
            message2("Batch " , k , " of ",length(snowflakes)," has " , n.batch , " packages.")
             
                  
        #5.2 set of packages
            #Message 
                #list with multiple files
                  if (n.batch> 1 ) {
                      message1("  They are: ", pasteQC(snowball$pkg_vrs[snowball$pkg %in% snowflakes[[k]] ]))
                               }
                
                #Single file
                  if (n.batch==1) {
                    message1("  It is: ", snowball$pkg_vrs[snowball$pkg %in% snowflakes[[k]] ])
                  }

       #5.3 Log file
            message1("\n  No messages will be shown on the console  while installing, unless an error occurs.")
            message1("  You may consult the log file to monitor progress in real time:\n   '",log_path,"'\n")
        
            
            
      #5.4 how much time  left 
           
            #Always say how long this batch      
               msg <- paste0("  As of ",now, " this batch is expected to finish installing by ",estimate.batch)
              
            #Say also total, unless this is the last batch
               if (k<length(snowflakes)) {
                msg <-paste0(msg, " and all batches by ",estimate.tot,".")
                }
            
            #Now show it
              message1(msg)
              message1("  (but these are noisy estimates).")
            
    
  }