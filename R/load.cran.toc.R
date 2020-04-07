#' Load `cran.toc`
#'
#' Load a `data.frame`` listing all CRAN packages, with their dependencies and
#' publication date.
#'
#' @param update.toc logical (defaults to `FALSE`). Should `cran.toc` be updated
#'   from the server if it already exists locally.
#'
# FIXME: add @return
# @return
#'
#' @seealso [update_cran.toc_if.needed]
#'
#' @examples
#' \dontrun{
#' load.cran.toc()
#' }
#'
#' @importFrom utils read.csv write.csv
load.cran.toc <- function(update.toc = FALSE) {
  groundhogR.url <- "http://groundhogR.com"
  groundhogR.folder <- .pkgenv[["groundhogR.folder"]]

  # 3.0 Ensure directory for groundhogR exists
  dir.create(groundhogR.folder, showWarnings = FALSE) # Create if inexistent

  # 3.1 Paths two databases (toc and times:
  # LOCAL
  toc.path <- file.path(groundhogR.folder, "cran.toc.csv.xz")
  times.path <- file.path(groundhogR.folder, "cran.times.csv.xz")

  # 3.2 JUST LOAD
  if (!update.toc) {

    # TOC
    if (file.exists(toc.path)) {
      cran.toc <- read.csv(toc.path, stringsAsFactors = FALSE)
    } else {
      cran.toc <- read.csv(system.file("cran.toc.csv.xz", package = "groundhogR"),
                           stringsAsFactors = FALSE)
    }

    cran.toc$Published <- as.DateYMD(cran.toc$Published)

    # Move the cran.toc outside the function space, to global environment
    .pkgenv[["cran.toc"]] <- cran.toc

    # Times
    if (file.exists(times.path)) {
      cran.times <- read.csv(times.path, stringsAsFactors = FALSE)
    } else {
      cran.times <- read.csv(system.file("cran.times.csv.xz", package = "groundhogR"),
                             stringsAsFactors = FALSE)
    }

    cran.times$update.date <- as.DateYMD(cran.times$update.date)
    .pkgenv[["cran.times"]] <- cran.times
  } # End 3.2 - no update

  # 3.3 UPDATE
  else {
    if (file.exists(toc.path)) {
      existing.toc <- read.csv(toc.path, stringsAsFactors = FALSE)
    } else {
      # Fallback on versions bundled with the package on CRAN
      existing.toc <- read.csv(system.file("cran.toc.csv.xz", package = "groundhogR"),
                               stringsAsFactors = FALSE)
    }

    if (file.exists(times.path)) {
      existing.times <- read.csv(times.path, stringsAsFactors = FALSE)
    } else {
      existing.times <- read.csv(system.file("cran.times.csv.xz", package = "groundhogR"),
                                 stringsAsFactors = FALSE)
    }

    # 3.3.2 create pkg_vrs for unique identifyier of packages
    existing.toc.pkg_vrs <- paste0(existing.toc$Package, "_", existing.toc$Version)
    existing.times.pkg_vrs <- paste0(existing.times$Package, "_", existing.times$Version)

    # 3.3.3 highest date
    max.existing.toc.date <- max(as.DateYMD(existing.toc$Published)) - 2 # lookup two days prior to handle timezone and cran delays
    max.existing.times.date <- max(as.DateYMD(existing.times$update.date)) - 2 # lookup two days prior to handle timezone and cran delays

    # UPDATE TOC
    # 3.3.4 Try updating toc by downloading additional rows from groundhogR server
    add.toc <- try(read.csv(paste0(groundhogR.url, "/differential.toc.php?current_date=", max.existing.toc.date)))

    # 3.3.5 If sucess loading URL
    if (is.data.frame(add.toc)) {
      # Get pkg_vrs for packages to add
      add.toc.pkg_vrs <- paste0(add.toc$Package, "_", add.toc$Version)

      # Drop repeated rows in URL and existing (there is the last day of partial to full overlap)
      add.toc.net <- add.toc[!add.toc.pkg_vrs %in% existing.toc.pkg_vrs, ]

      # Add net
      cran.toc <- rbind(existing.toc, add.toc.net)
      .pkgenv[["cran.toc"]] <- cran.toc # Save cran.toc to environemnt

      # save to local drive
      write.csv(cran.toc, file = xzfile(toc.path), row.names = FALSE)
    } # End 3.3.5 - if succeeded at downloading file from website


    # UPDATE TIMES
    # 3.3.6 Try updating times by downloading additional rows from groundhogR server
    add.times <- try(read.csv(paste0(groundhogR.url, "/differential.times.php?current_date=", max.existing.times.date)))


    # 3.3.7 If sucess loading URL
    if (is.data.frame(add.times)) {
      # Get pkg_vrs for packages to add
      add.times.pkg_vrs <- paste0(add.times$Package, "_", add.times$Version)

      # Drop repeated rows in URL and existing (there is the last day of partial to full overlap)
      add.times.net <- add.times[!add.times.pkg_vrs %in% existing.times.pkg_vrs, ]

      # Add net
      cran.times <- rbind(existing.times, add.times.net)
      .pkgenv[["cran.times"]] <- cran.times # Save cran.times to environemnt

      # save to local drive
      write.csv(cran.times, file = xzfile(times.path), row.names = FALSE)
    } # End 3.3.5 - if succeeded at downloading file from website

    # Feedback to user on existing cran.toc
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
    } else {
      message1("We tried to update till today but it did not work")
    }

    message1("The file with the list is stored here: ", toc.path, "\n-------------------------------")
  }

}
