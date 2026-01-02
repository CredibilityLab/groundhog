#Cache functions for get.ip() to avoid repeated expensive operations
#(list.files(), installed.packages(), md5sum())
#-------------------------------------------------------------------------------

#0 Get cache path
  get.ip.cache_path <- function() {
    cache_path <- paste0(get.groundhog.folder(), "/cache_ip/", get.r.majmin(), ".rds")
    return(cache_path)
  }

#1 Read cached IP for a location
  read.ip.cache <- function(location) {
    cache_path <- get.ip.cache_path()
    
    #If cache file doesn't exist, return NULL
    if (!file.exists(cache_path)) {
      return(NULL)
    }
    
    #Read cache
    cache <- tryCatch({
      readRDS(cache_path)
    }, error = function(e) {
      return(NULL)
    })
    
    #If location not in cache, return NULL
    if (is.null(cache) || !location %in% names(cache)) {
      return(NULL)
    }
    
    #Get cached entry
    cached_entry <- cache[[location]]
    
    #Check if cache entry is valid (has required fields)
    if (is.null(cached_entry) || !"ip" %in% names(cached_entry)) {
      return(NULL)
    }
    
    #Check validity by comparing source mtime
    source_paths <- get.ip.source_paths(location)
    if (!is.ip.cache.valid(location, source_paths, cached_entry)) {
      return(NULL)
    }
    
    #Return cached IP
    return(cached_entry$ip)
  }

#2 Get source paths for a location (for mtime tracking)
  get.ip.source_paths <- function(location) {
    if (location == 'groundhog') {
      cran_path   <- paste0(get.groundhog.folder(), "/R-", get.r.majmin())
      github_path <- paste0(cran_path, "/_github")
      gitlab_path <- paste0(cran_path, "/_gitlab")
      return(c(cran_path, github_path, gitlab_path))
    } else if (location == 'backup') {
      master_path <- paste0(get.groundhog.folder(), "/restore_library/", get.r.majmin(), "/")
      return(master_path)
    } else if (location == 'local') {
      if (is.null(.pkgenv[["orig_lib_paths"]])) {
        return(character())
      }
      return(.pkgenv[["orig_lib_paths"]][1])
    } else if (location == 'all_local') {
      if (is.null(.pkgenv[["orig_lib_paths"]])) {
        return(character())
      }
      return(.pkgenv[["orig_lib_paths"]][-length(.pkgenv[["orig_lib_paths"]])])
    }
    return(character())
  }

#3 Get max modification time of source paths
  get.source_paths_mtime <- function(source_paths) {
    if (length(source_paths) == 0) {
      return(0)
    }
    
    #Filter to existing paths only
    existing_paths <- source_paths[file.exists(source_paths)]
    if (length(existing_paths) == 0) {
      return(0)
    }
    
    #Get mtimes
    mtimes <- sapply(existing_paths, function(path) {
      if (file.exists(path)) {
        info <- file.info(path)
        if (is.na(info$mtime)) {
          return(0)
        }
        return(as.numeric(info$mtime))
      }
      return(0)
    })
    
    #Return max mtime
    if (length(mtimes) > 0) {
      return(max(mtimes, na.rm = TRUE))
    }
    return(0)
  }

#4 Check if cache is valid by comparing mtimes
  is.ip.cache.valid <- function(location, source_paths, cached_entry) {
    if (is.null(cached_entry) || !"source_mtime" %in% names(cached_entry)) {
      return(FALSE)
    }
    
    #Get current source mtime
    current_mtime <- get.source_paths_mtime(source_paths)
    
    #Compare: cache is valid if source mtime <= cached source_mtime
    #(if source hasn't been modified since cache was created)
    if (current_mtime <= cached_entry$source_mtime) {
      return(TRUE)
    }
    
    return(FALSE)
  }

#5 Save IP to cache
  save.ip.cache <- function(location, ip, source_paths) {
    cache_path <- get.ip.cache_path()
    
    #Create directory if needed
    dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)
    
    #Read existing cache or create new
    cache <- tryCatch({
      if (file.exists(cache_path)) {
        readRDS(cache_path)
      } else {
        list()
      }
    }, error = function(e) {
      return(list())
    })
    
    #Get source mtime
    source_mtime <- get.source_paths_mtime(source_paths)
    
    #Create cache entry
    cache[[location]] <- list(
      ip = ip,
      source_mtime = source_mtime,
      cache_mtime = as.numeric(Sys.time())
    )
    
    #Save cache
    tryCatch({
      saveRDS(cache, cache_path, version = 2, compress = FALSE)
    }, error = function(e) {
      #Silently fail if can't save cache
    })
  }

#6 Get cached IP dataframe (internal helper)
  get.cached.ip.dataframe <- function(location) {
    cache_path <- get.ip.cache_path()
    if (!file.exists(cache_path)) {
      return(NULL)
    }
    
    cache <- tryCatch({
      readRDS(cache_path)
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(cache) || !location %in% names(cache) || is.null(cache[[location]]$ip)) {
      return(NULL)
    }
    
    return(cache[[location]]$ip)
  }

#7 Add packages to cache (incremental update)
  add.packages.to.ip.cache <- function(location, new_packages_df) {
    #new_packages_df should be a dataframe with columns: LibPath, Package, Version, pkg_vrs, md5
    if (is.null(new_packages_df) || nrow(new_packages_df) == 0) {
      return(invisible(NULL))
    }
    
    #Debug: message and save each row being added
    for (i in 1:nrow(new_packages_df)) {
      pkg_row <- new_packages_df[i, ]
      message("Adding to cache [", location, "]: ", pkg_row$pkg_vrs)
      
      #Save individual row to temp folder for debugging
      temp_dir <- "c:/temp"
      dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S_%OS3")
      temp_filename <- paste0(pkg_row$pkg_vrs, "_", timestamp, ".rds")
      temp_path <- file.path(temp_dir, temp_filename)
      tryCatch({
        saveRDS(pkg_row, temp_path, version = 2, compress = FALSE)
        message("  Saved debug file: ", temp_path)
      }, error = function(e) {
        #Silently fail if can't save debug file
      })
    }
    
    cache_path <- get.ip.cache_path()
    source_paths <- get.ip.source_paths(location)
    
    #Read existing cache
    cache <- tryCatch({
      if (file.exists(cache_path)) {
        readRDS(cache_path)
      } else {
        list()
      }
    }, error = function(e) {
      return(list())
    })
    
    #Get existing IP or create empty
    if (location %in% names(cache) && !is.null(cache[[location]]$ip)) {
      existing_ip <- cache[[location]]$ip
    } else {
      #Create empty dataframe with correct structure
      existing_ip <- data.frame(
        LibPath = character(),
        Package = character(),
        Version = character(),
        pkg_vrs = character(),
        md5 = character(),
        stringsAsFactors = FALSE
      )
    }
    
    #Remove any existing entries for these packages (by pkg_vrs) and add new ones
    if (nrow(existing_ip) > 0) {
      existing_ip <- existing_ip[!existing_ip$pkg_vrs %in% new_packages_df$pkg_vrs, ]
    }
    
    #Combine: existing (minus duplicates) + new
    updated_ip <- rbind(existing_ip, new_packages_df)
    
    #Update source mtime to CURRENT folder mtime
    #Add a small buffer (2 seconds) to prevent immediate invalidation due to file system timing
    #This ensures that if get.ip() is called right after we update, the cache won't be invalidated
    source_mtime <- get.source_paths_mtime(source_paths)
    if (source_mtime > 0) {
      source_mtime <- source_mtime + 2  # Add 2 second buffer to prevent race condition
    }
    
    #Save back to cache
    cache[[location]] <- list(
      ip = updated_ip,
      source_mtime = source_mtime,  # This is the CURRENT mtime + buffer, so cache will remain valid
      cache_mtime = as.numeric(Sys.time())
    )
    
    tryCatch({
      saveRDS(cache, cache_path, version = 2, compress = FALSE)
    }, error = function(e) {
      #Silently fail if can't save cache
    })
  }

#8 Remove packages from cache (incremental update)
  remove.packages.from.ip.cache <- function(location, packages_to_remove) {
    #packages_to_remove can be: vector of pkg_vrs, or vector of Package names, or dataframe with pkg_vrs column
    if (is.null(packages_to_remove) || length(packages_to_remove) == 0) {
      return(invisible(NULL))
    }
    
    cache_path <- get.ip.cache_path()
    source_paths <- get.ip.source_paths(location)
    
    #Read existing cache
    cache <- tryCatch({
      if (file.exists(cache_path)) {
        readRDS(cache_path)
      } else {
        return(invisible(NULL))
      }
    }, error = function(e) {
      return(invisible(NULL))
    })
    
    #Get existing IP
    if (!location %in% names(cache) || is.null(cache[[location]]$ip)) {
      return(invisible(NULL))
    }
    
    existing_ip <- cache[[location]]$ip
    
    #Determine what to remove
    if (is.data.frame(packages_to_remove)) {
      if ("pkg_vrs" %in% names(packages_to_remove)) {
        remove_pkg_vrs <- packages_to_remove$pkg_vrs
      } else if ("Package" %in% names(packages_to_remove)) {
        remove_pkg_vrs <- existing_ip[existing_ip$Package %in% packages_to_remove$Package, ]$pkg_vrs
      } else {
        return(invisible(NULL))
      }
    } else {
      #Assume it's pkg_vrs if they match existing, otherwise try as Package names
      if (any(packages_to_remove %in% existing_ip$pkg_vrs)) {
        remove_pkg_vrs <- packages_to_remove
      } else {
        remove_pkg_vrs <- existing_ip[existing_ip$Package %in% packages_to_remove, ]$pkg_vrs
      }
    }
    
    #Remove packages
    if (length(remove_pkg_vrs) > 0) {
      updated_ip <- existing_ip[!existing_ip$pkg_vrs %in% remove_pkg_vrs, ]
    } else {
      updated_ip <- existing_ip
    }
    
    #Update source mtime
    source_mtime <- get.source_paths_mtime(source_paths)
    
    #Save back to cache
    cache[[location]] <- list(
      ip = updated_ip,
      source_mtime = source_mtime,
      cache_mtime = as.numeric(Sys.time())
    )
    
    tryCatch({
      saveRDS(cache, cache_path, version = 2, compress = FALSE)
    }, error = function(e) {
      #Silently fail
    })
  }

#9 Compute IP for specific installation paths (helper for incremental updates)
  compute.ip.for.paths <- function(installation_paths) {
    #installation_paths: vector of paths where packages are installed
    if (length(installation_paths) == 0) {
      return(data.frame(
        LibPath = character(),
        Package = character(),
        Version = character(),
        pkg_vrs = character(),
        md5 = character(),
        stringsAsFactors = FALSE
      ))
    }
    
    #Don't filter by file.exists() - installed.packages() will handle missing paths gracefully
    #The path might not exist immediately after installation due to timing, but we should still try
    #Get installed.packages for these specific paths
    ip <- tryCatch({
      data.frame(utils::installed.packages(installation_paths), row.names = NULL, stringsAsFactors = FALSE)
    }, error = function(e) {
      return(data.frame(
        LibPath = character(),
        Package = character(),
        Version = character(),
        pkg_vrs = character(),
        md5 = character(),
        stringsAsFactors = FALSE
      ))
    })
    
    if (nrow(ip) == 0) {
      ip$pkg_vrs <- character()
      ip$md5 <- character()
      return(ip)
    }
    
    #Create pkg_vrs
    ip$pkg_vrs <- paste0(ip$Package, "_", ip$Version)
    
    #Select columns
    ip <- ip[, c(names(ip) %in% c("LibPath", "Package", "Version", "pkg_vrs"))]
    
    #Normalize LibPath to match format used in get.ip() (use normalizePath to handle path separators)
    #This ensures consistency with cached data
    if (nrow(ip) > 0) {
      ip$LibPath <- normalizePath(ip$LibPath, winslash = "/", mustWork = FALSE)
    }
    
    #Add MD5
    description.path <- paste0(ip$LibPath, "/", ip$Package, "/DESCRIPTION")
    ip$md5 <- tools::md5sum(description.path)
    
    return(ip)
  }

#10 Update cache for a location (recompute and save - fallback when incremental not possible)
  #' @noRd
  update.ip.cache <- function(location) {
    source_paths <- get.ip.source_paths(location)
    
    #Compute IP directly (same logic as get.ip() but without cache check)
    #1 Get all subfolders for backup and groundhog
    if (location %in% c('backup','groundhog')) {
      #Path containing all subfolders with pkg_vrs   
      if (location=='groundhog') {
        cran_path   <- paste0(get.groundhog.folder() , "/R-" , get.r.majmin())
        github_path <- paste0(cran_path,"/_github")
        gitlab_path <- paste0(cran_path,"/_gitlab")
        master_path=c(cran_path, github_path, gitlab_path)
      }
      
      if (location=='backup') {
        master_path <- paste0(get.groundhog.folder(),"/restore_library/" , get.r.majmin() , "/")
      }
      
      #All pkgs in that path    
      all.paths<- list.files(master_path,full.names=TRUE)
    }
    
    #1.2 Local  
    if (location=='local') {
      all.paths <- .pkgenv[["orig_lib_paths"]][1]
    }
    if (location=='all_local') {
      all.paths <- .pkgenv[["orig_lib_paths"]][-length(.pkgenv[["orig_lib_paths"]])]
    }
    
    #2 Get the installed.packages
    ip <- data.frame(utils::installed.packages(all.paths), row.names = NULL, stringsAsFactors = FALSE)
    
    #3 Create pkg_vrs
    if (nrow(ip)>0) {
      ip$pkg_vrs <- paste0(ip$Package,"_",ip$Version)
    }
    if (nrow(ip)==0) {
      ip$pkg_vrs <- character()
    }
    
    #4 Select columns
    ip <- ip[,c(names(ip) %in% c("LibPath", "Package","Version","pkg_vrs"))]
    
    #5 Add MD5 for DESCRIPTION file to merge & compare with `loans`
    if (nrow(ip)>0) {
      description.path <- paste0(ip$LibPath, "/" , ip$Package , "/DESCRIPTION")
      ip$md5 <- tools::md5sum(description.path)
    }
    
    if (nrow(ip)==0) {
      ip$md5 <- character()
    }
    
    #Save to cache
    save.ip.cache(location, ip, source_paths)
  }

#7 Invalidate cache for a location (remove entry)
  invalidate.ip.cache <- function(location) {
    cache_path <- get.ip.cache_path()
    
    if (!file.exists(cache_path)) {
      return(invisible(NULL))
    }
    
    #Read cache
    cache <- tryCatch({
      readRDS(cache_path)
    }, error = function(e) {
      return(list())
    })
    
    #Remove location entry
    if (location %in% names(cache)) {
      cache[[location]] <- NULL
      
      #Save cache (even if empty)
      tryCatch({
        saveRDS(cache, cache_path, version = 2, compress = FALSE)
      }, error = function(e) {
        #Silently fail
      })
    }
  }

#8 Invalidate all cache
  invalidate.ip.cache.all <- function() {
    cache_path <- get.ip.cache_path()
    if (file.exists(cache_path)) {
      tryCatch({
        unlink(cache_path)
      }, error = function(e) {
        #Silently fail
      })
    }
  }

