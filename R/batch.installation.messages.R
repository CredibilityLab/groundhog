
#Auxiliary function

  #get.parallel.time()  :  estimate parallel time of installation
  get.parallel.time<-function(times,cores)
  {
    #Sort times
      times <- sort(times,decreasing=TRUE)
      
    #initiates times with small but sortable values
      y = seq(0,.01,length.out=cores) 
  
    #In loop, assign the next one, to the lowest total so far
      for (k in 1:length(times))
      {
      y[which.min(y)]=y[which.min(y)] + times[k]  
      }
    #The longest link is the estimated time
      return(max(y))
  }




batch.installation.messages <- function(snowflakes,  k , cores)
  
  {
     #0 Log file paths 
        log_path = paste0(get.groundhog.folder(),"/installation_logs")
        installation_log_path      <-  file.path(log_path,  "full_log.txt")
        installation_progress_path <-  file.path(log_path, "summary_log.txt")      
    
    
    #1. Get installation time for this batch 
          sec.k <- get.parallel.time(storm.source$installation.time[storm.source$pkg %in% snowflakes[[k]] ] , cores) #utils.R #44
                    #get.parallel.time(): auxiliary function below

    #2. Get installation time for full set 
          if (k<length(snowflakes))
            {
            sec.tot=0
              for (j in k:length(snowflakes)) 
              {
              sec.tot <- sec.tot + get.parallel.time(storm.source$installation.time[storm.source$pkg %in% snowflakes[[j]] ] , cores) #utils.R #44
              } 
            } else {
              sec.tot =  sec.k
            }
              
                    
    #3 Compute time to show              
            now             <- format(Sys.time(), "%H:%M")
            estimate.batch  <- format(Sys.time() + sec.k, "%H:%M")
            estimate.tot    <- format(Sys.time() + sec.tot, "%H:%M")

    #4 count total batches
            n.batch <- length(snowflakes[[k]])
            
    #5 Show 
        #5.0 Header
            if (k==1) {
              
              message2("\n\nWill now install ", nrow(storm.source)," packages from source files, in " , length(snowflakes), " batches.")
              message1("   Packages within a batch are installed in parallel.")
              message1("   Batches are installed sequentially.\n")
              message1("   As of ",now, " all ", nrow(storm.source), " packages are expected to finish installing around ",estimate.tot, ".")
              message1("   (but this is a noisy estimate).")
            }
            
            
        #5.1 How many batches
            message1("\n\n------------------------------------------------------")
            message2("Batch " , k , " of ",length(snowflakes)," has " , n.batch , " packages.")
             
                  
        #5.2 set of packages
            #Message 
                #list with multiple files
                  if (n.batch>1) {
                      message1("  -> ", pasteQC(sort(storm.source$pkg_vrs[storm.source$url %in% source.url])))
                  }
                
                #Single file
                  if (n.batch==1) {
                    message1("  -> ", storm.source$pkg_vrs[storm.source$url %in% source.url])
                  }

       #5.3 Log file
            message1("\n  Recall: no feedback is provided while installing unless an error occurs.")
            message1("  You may consult the log file to monitor progress in real time:\n   '",installation_progress_path,"'\n")
        
            
            
      #5.4 how much time  left 
           
            #Always say hoow long this batch      
               msg <- paste0("\n  As of ",now, " this batch is expected to finish installing by ",estimate.batch)
              
            #Say also total, unless this is the last batch
               if (k<length(snowflakes)) {
                msg <-paste0(msg, " and all batches by ",estimate.tot,".")
                }
            
            #Now show it
              message1(msg)
              message1("  (but these are noisy estimates).")
            
        }
  
  

