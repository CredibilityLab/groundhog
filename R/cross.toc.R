#' Show toc table with multiple packages
#' @export
cross.toc <- function(pkgs, date1 = "1970-1-1", date2 = Sys.Date()) {

  # TODO: is this really necessary since this function calls toc() which does
  # the same check?
  if (is.null(.pkgenv[["cran.toc"]])) {
    load.cran.toc(update.toc = FALSE)
  }

  toc.all <- lapply(pkgs, function(pkg) {
    tock <- toc(pkg)
    tock$Package <- pkg
    return(tock)
  })

  toc.all <- do.call(rbind, toc.all)

  # Sort
  toc.all <- toc.all[order(toc.all$Published), ]
  # date subset
  return(toc.all[toc.all$Published > date1 & toc.all$Published < date2, ])
}
