#' Load `cran.toc`
#'
#' Load a `data.frame` listing all CRAN packages, with their dependencies and
#' publication date.
#'
#' @param update.toc logical (defaults to `FALSE`). Should `cran.toc` be updated
#'   from the server if it already exists locally.
#'
#' @return (invisibly) `TRUE`/`FALSE` depending on the success/failure of this
#'   operation.
#'
#' @seealso [update_cran.toc_if.needed]
#'
#' @examples
#' load.cran.toc()
#'
#' @importFrom utils read.csv
#'
load.cran.toc <- function(update.toc = FALSE) {
  groundhogR.url <- "https://groundhogR.com/"
  groundhogR.folder <- get.groundhogr.folder()

  # 3.0 Ensure directory for groundhogR exists
  dir.create(groundhogR.folder, showWarnings = FALSE) # Create if inexistent

  # 3.1 Paths two databases (toc and times:
  # LOCAL
  toc.path <- file.path(groundhogR.folder, "cran.toc.rds")
  times.path <- file.path(groundhogR.folder, "cran.times.rds")

  # 3.2 JUST LOAD
  if (!update.toc) {

    # TOC
    if (file.exists(toc.path)) {
      cran.toc <- readRDS(toc.path)
    } else {
      cran.toc <- readRDS(system.file("cran.toc.rds", package = "groundhogR"))
    }

    # Move the cran.toc outside the function space, to global environment
    .pkgenv[["cran.toc"]] <- cran.toc

    # Times
    if (file.exists(times.path)) {
      cran.times <- readRDS(times.path)
    } else {
      cran.times <- readRDS(system.file("cran.times.rds", package = "groundhogR"))
    }

    .pkgenv[["cran.times"]] <- cran.times

  } else {

    dl_times <- try(download.file(paste0(groundhogR.url, "cran.times.rds"), times.path))
    dl_toc <- try(download.file(paste0(groundhogR.url, "cran.toc.rds"), toc.path))

    cran.times <- readRDS(times.path)
    cran.toc <- readRDS(toc.path)

    .pkgenv[["cran.times"]] <- cran.times
    .pkgenv[["cran.toc"]] <- cran.toc

  #   # Feedback to user on existing cran.toc
  #   message2()
  #   message1(
  #     "This computer had a database with a list of all versions available for each CRAN package up to ",
  #     max.existing.toc.date + 2, " for a total of N=", nrow(existing.toc), " package versions."
  #   ) # Add back the two days we took out
  #   if (is.data.frame(add.toc)) {
  #     message1(
  #       "We checked for additions to CRAN since then, and added ",
  #       nrow(add.toc.net), " additional entries to the list.\n"
  #     )
  #   } else {
  #     message1("We tried to update till today but it did not work")
  #   }
  #
  #   message1("The file with the list is stored here: ", toc.path, "\n-------------------------------")
    if (any(inherits(dl_times, "try-error"), inherits(dl_toc, "try-error"))) {
      return(invisible(FALSE))
    }
  }

  invisible(TRUE)
}
