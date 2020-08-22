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
#' \donttest{
#' groundhog:::load.cran.toc()
#' }
#'
#' @importFrom utils read.csv
#'
load.cran.toc <- function(update.toc = FALSE) {
  groundhogR.url <- "http://groundhogR.com/"
  groundhog.folder <- get.groundhog.folder()

  # 3.0 Ensure directory for groundhog exists
  dir.create(groundhog.folder, showWarnings = FALSE) # Create if inexistent

  # 3.1 Paths two databases (toc and times:
  # LOCAL
  toc.path <- file.path(groundhog.folder, "cran.toc.rds")
  times.path <- file.path(groundhog.folder, "cran.times.rds")
  mran.path <- file.path(groundhog.folder, "missing.mran.dates.rds")

  # 3.2 JUST LOAD
  if (!update.toc) {

    # TOC
    if (file.exists(toc.path)) {
      cran.toc <- readRDS(toc.path)
    } else {
      cran.toc <- readRDS(system.file("cran.toc.rds", package = "groundhog"))
    }

    # Move the cran.toc outside the function space, to global environment
    .pkgenv[["cran.toc"]] <- cran.toc

    # Times
    if (file.exists(times.path)) {
      cran.times <- readRDS(times.path)
    } else {
      cran.times <- readRDS(system.file("cran.times.rds", package = "groundhog"))
    }

    .pkgenv[["cran.times"]] <- cran.times


    # MRAN missing dates
    if (file.exists(mran.path)) {
      missing.mran.dates <- readRDS(mran.path)
    } else {
      missing.mran.dates <- readRDS(system.file("missing.mran.dates.rds", package = "groundhog"))
    }

    .pkgenv[["missing.mran.dates"]] <- missing.mran.dates
  } else {
    dl_times <- try(download.file(paste0(groundhogR.url, "cran.times.rds"), times.path, mode = "wb", method = "internal"))
    dl_toc <- try(download.file(paste0(groundhogR.url, "cran.toc.rds"), toc.path, mode = "wb", method = "internal"))
    dl_mran <- try(download.file(paste0(groundhogR.url, "missing.mran.dates.rds"), mran.path, mode = "wb", method = "internal"))

    cran.times <- readRDS(times.path)
    cran.toc <- readRDS(toc.path)
    missing.mran.dates <- readRDS(mran.path)

    .pkgenv[["cran.times"]] <- cran.times
    .pkgenv[["cran.toc"]] <- cran.toc
    .pkgenv[["missing.mran.dates"]] <- missing.mran.dates

    if (any(inherits(dl_times, "try-error"), inherits(dl_toc, "try-error"), inherits(dl_mran, "try-error"))) {
      return(invisible(FALSE))
    }
  }

  invisible(TRUE)
}
