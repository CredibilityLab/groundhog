#' Load the dataframe listing all CRAN packages, their dependencies and Publication date
#' @importFrom utils read.csv write.csv
load.cran.toc <- function(update.toc = FALSE) {
  groundhogR.url <- "http://groundhogR.com"

  # 3.0 Ensure directory for groundhogR exists
  dir.create(groundhogR.folder, showWarnings = FALSE) # Create if inexistent

  # 3.1 Paths two databases (toc and times:
  # LOCAL
  toc.path <- paste0(groundhogR.folder, "/cran.toc.csv.gz")
  times.path <- paste0(groundhogR.folder, "/cran.times.csv.gz")

  # URL
  toc.url <- paste0(groundhogR.url, "/cran.toc.csv.gz")
  times.url <- paste0(groundhogR.url, "/cran.times.csv.gz")

  # 3.2 JUST LOAD
  if (file.exists(toc.path) & file.exists(times.path) & !update.toc) {

    # TOC
    cran.toc <- read.csv(toc.path, stringsAsFactors = FALSE)[, -1]
    cran.toc$Published <- as.DateYMD(cran.toc$Published)
    cran.toc <<- cran.toc # Move the cran.toc outside the function space, to global environemnt, later will be package environemnt.

    # Times
    cran.times <- read.csv(times.path, stringsAsFactors = FALSE)[, -1]
    cran.times$update.date <- as.DateYMD(cran.times$update.date)
    cran.times <<- cran.times
  } # End 3.2 - no update

  # 3.3 UPDATE
  if (file.exists(toc.path) & file.exists(times.path) & update.toc) {

    # 3.3.1 load databases
    existing.toc <- read.csv(toc.path, stringsAsFactors = FALSE)[, -1]
    existing.times <- read.csv(times.path, stringsAsFactors = FALSE)[, -1]

    # 3.3.2 create pkg_vrs for unique identifyier of packages
    existing.toc.pkg_vrs <- paste0(existing.toc$Package, "_", existing.toc$Version)
    existing.times.pkg_vrs <- paste0(existing.times$Package, "_", existing.times$Version)

    # 3.3.3 highest date
    max.existing.toc.date <- max(as.DateYMD(existing.toc$Published)) - 2 # lookup two days prior to handle timezone and cran delays
    max.existing.times.date <- max(as.DateYMD(existing.times$update.date)) - 2 # lookup two days prior to handle timezone and cran delays

    # UPDATE TOC
    # 3.3.4 Try updating toc by downloading additional rows from groundhogR server
    add.toc <- try(read.csv(paste0(groundhogR.url, "/differential.toc.php?current_date=", max.existing.toc.date))[-1])

    # 3.3.5 If sucess loading URL
    if (is.data.frame(add.toc)) {
      # Get pkg_vrs for packages to add
      add.toc.pkg_vrs <- paste0(add.toc$Package, "_", add.toc$Version)

      # Drop repeated rows in URL and existing (there is the last day of partial to full overlap)
      add.toc.net <- add.toc[!add.toc.pkg_vrs %in% existing.toc.pkg_vrs, ]

      # Add net
      cran.toc <- rbind(existing.toc, add.toc.net)
      cran.toc <<- cran.toc # Save cran.to to environemnt


      # save to local drive
      write.csv(cran.toc, file = gzfile(toc.path))
    } # End 3.3.5 - if succeeded at downloading file from website


    # UPDATE TIMES
    # 3.3.6 Try updating times by downloading additional rows from groundhogR server
    add.times <- try(read.csv(paste0(groundhogR.url, "/differential.times.php?current_date=", max.existing.times.date))[-1])


    # 3.3.7 If sucess loading URL
    if (is.data.frame(add.times)) {
      # Get pkg_vrs for packages to add
      add.times.pkg_vrs <- paste0(add.times$Package, "_", add.times$Version)

      # Drop repeated rows in URL and existing (there is the last day of partial to full overlap)
      add.times.net <- add.times[!add.times.pkg_vrs %in% existing.times.pkg_vrs, ]

      # Add net
      cran.times <- rbind(existing.times, add.times.net)
      cran.times <<- cran.times # Save cran.to to environemnt


      # save to local drive
      write.csv(cran.times, file = gzfile(times.path))
    } # End 3.3.5 - if succeeded at downloading file from website



    # Feedback to user on existing cran.to
    message2()
    message1(
      "This computer had a database with a list of all versions available for each CRAN package up to ",
      max.existing.toc.date + 2, " for a total of N=", nrow(existing.toc), " package versions."
    ) # Add back the two days we took out
    if (is.data.frame(add.toc)) {
      message1(
        "We checked for additions to CRAN since then, and added ",
        nrow(add.toc.net), " additional entries to the list.\n"
      )
    }
    if (is.data.frame(add.toc)) {
      message1("We tried to update till today but it did not work")
    }

    message1("The file with the list is stored here: ", toc.path, "\n-------------------------------")
  } # End if local file exist


  # 3.4 if either cran.toc does not exist, download the entire up to date copy
  if (!file.exists(toc.path) | !file.exists(times.path)) {
    # 3.4.1 Attempt download
    d1 <- try(download.file(toc.url, toc.path))
    d2 <- try(download.file(times.url, times.path))

    # 3.4.2 if it fails (d1), give bad news
    if (inherits(d1, "try-error")) {
      message2()
      message1(
        "Could not load the database of packages available in CRAN neither locally from this\n",
        "computer nor from our server. Unfortunately this means you cannot use groundhog.libray()"
      )
      message1("This could happen if this is your first time using {groundhogR} and you are off-line.")
      stop("!")
    } # End if download did not work
    # 3.4.3 Load the toc
    cran.toc <- read.csv(file = gzfile(toc.path), stringsAsFactors = FALSE)[, -1]
    cran.toc$Published <- as.DateYMD(cran.toc$Published)
    cran.toc <<- cran.toc

    cran.times <- read.csv(file = gzfile(times.path), stringsAsFactors = FALSE)[, -1]
    cran.times$update.date <- as.DateYMD(cran.times$update.date)
    cran.times <<- cran.times



    # 3.4.4 tell users
    message2()
    message1(
      "GroundhogR requires a database listing all versions of every package ever in CRAN.\n",
      "The database was not found in this computer so it was just downloaded from http://groundhogR.com\n",
      "The database contains N = ", nrow(cran.toc), " entries. The most recent entry is from ", max(cran.toc$Published), ".\n",
      "The database will be automatically updated when the 'date' in groundhog.library('date') so requires.\n",
      "You may also update it by running: load.cran.toc(update.toc=TRUE)."
    )

    message1("The file is stored locally here:'", toc.path, "'\n-------------------------------")
  } # End if file does not exist
} # End of load cran.toc
