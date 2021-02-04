#' Get date of binary package from MRAN, given \R version being used
#'
#' @param pkg_vrs character. The required package and its version separated by
#'   an underscore.
#' @param R_vrs character. The required \R version.
#'
#' @return the date of the binary the download from MRAN, in the format
#'   "%Y-%m-%d".
#'
#' @seealso [get.version()] for the opposite operation: determining the package
#'   version from a given date.
#'
# @examples
# \dontrun{
# groundhog:::get.R.pkg.date("magrittr_1.0.1", "3.6.0")
# }
#'
get.R.pkg.date <- function(pkg_vrs, R_vrs) {
  if (is.null(.pkgenv[["cran.toc"]])) {
    load.cran.toc()
  }

  cran.toc <- .pkgenv[["cran.toc"]]
  # 1. Get pkg from pkg_vrs
    pkg <- get.pkg(pkg_vrs)
    vrs <- get.vrs(pkg_vrs)

  # 2. cross.toc with R - all available pkg versions and R
    cross1 <- cross.toc(c(pkg, "R"))

  # 3, Which row have pkg_vrs vs R_vrs
    k.pkg <- which(pkg_vrs == paste0(cross1$Package, "_", cross1$Version))
    k.R <- which(paste0("R_", R_vrs) == paste0(cross1$Package, "_", cross1$Version))

  # It's possible that cran.toc is outdated and doesn't include the required
  # version
    if (length(k.R) == 0 | length(k.pkg) == 0) {
      load.cran.toc(update.toc = TRUE)
  
      cross1 <- cross.toc(c(pkg, "R"))
  
      k.pkg <- which(pkg_vrs == paste0(cross1$Package, "_", cross1$Version))
      k.R <- which(paste0("R_", R_vrs) == paste0(cross1$Package, "_", cross1$Version))
    }

  # Line indices that contain either pkg_vrs or R_vrs
    ks <- c(k.pkg, k.R)

  # 5. From one to the other (cross2: subset from cross1 where  pkg_vrs to R_vrs or vice versa)
    cross2 <- cross1[min(ks):max(ks), ]

  # 6. If the package came first:
    if (k.pkg < k.R) {
      # 6.1. If there is another version of the package in the subset, it means it changed before arriving at the desired R, so return ""
        if (sum(cross2$Package == pkg) > 1) {
        return(as.DateYMD("1970-01-01"))
      }
    # 6.2 If there is only one package in the set, then starting with last row, the desired package is available for that R, take midpoint till next
    if (sum(cross2$Package == pkg) == 1) {
      start.date <- cross1[k.R, ]$Published # start.date: start of period when pkg_vrs binary was available for this R-version

      # 6.3 If not using the most recent R, the midpoint is towardsthe next one, if most recent, halfway to today
      if (k.R < nrow(cross1)) {
        # If already replaced, when it was replaced, minus 2 days for caution
        end.date <- as.DateYMD(cross1[k.R + 1, ]$Published) - 2
      }
      if (k.R == nrow(cross1)) {
        # If not yet replaced, still current with today's MRAN, but use minus two days for caution
        end.date <- as.DateYMD(Sys.Date()) - 2
      }

      # 6.4  If end.date not yet in toc, update toc
      if (max(cran.toc$Published) < end.date) {
        load.cran.toc(TRUE)
      }
    } # ENd 6.2 --  if date will be found
  } # End if package came first

  # 7. If  R came first:
  if (k.pkg > k.R) {
    # 7.1. If there is another version of R, different minor, it changed
    if (sum(cross2$Package == "R") > 1) {
      return(as.DateYMD("1970-01-01"))
    }
    # 7.2 If there is only one version of R in the set,
    if (sum(cross2$Package == "R") == 1) {
      # Start date when pkg was available, is when it is released
      start.date <- cross1[k.pkg, ]$Published # start.date: start of period when pkg_vrs binary was avilable for this R-version
      # End date is either when then ext package version is released or the present.
      if (k.pkg < nrow(cross1)) {
        # If already replaced, when it was replaced, minus 2 days for caution
        end.date <- as.DateYMD(cross1[k.pkg + 1, ]$Published) - 2
      }
      if (k.pkg == nrow(cross1)) {
        # If not yet replaced, still current with today's MRAN, but use minus two days for caution
        end.date <- as.DateYMD(Sys.Date()) - 2
      }

      # If end.date not yet in toc, update toc
      if (max(cran.toc$Published) < end.date) {
        load.cran.toc(TRUE)
      }
    } # There is only 1 version of R within set
  } # R with after

  start.date <- as.DateYMD(start.date)
  end.date <- as.DateYMD(end.date)

  # Return mid-date among available dates
  mid.date <- get.available.mran.date(start.date, end.date) # Function below
  return(mid.date)
}

