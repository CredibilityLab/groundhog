#' Check snowball conflict
#'
#' @inheritParams estimate.seconds.left
#'
check.snowball.conflict <- function(snowball) {

  # Load active packages (function 2.14)
  active <- get.active()

  # How many match pkg vs pkg_vrs
  count.same.pkg <- sum(snowball$pkg %in% active$pkg)
  count.same.pkg_vrs <- sum(snowball$pkg_vrs %in% active$pkg_vrs)

  # If counts are different, give warning
  # 8.8.3 If different # of packages match pkg vs pkg_vrs, we have same packages  different vrs: stop
  if (count.same.pkg_vrs != count.same.pkg) {
    message2()
    message1(
      "A different version of the package you want to load, or one of its dependencies, is already have loaded.\n",
      "This can create reproducibility problems, as it will be ambiguous which of those version you are using\n",
      " with your script.  To solve this you have two options.\n\n",
      "Option 1.*RECOMMENDED* Restart R session (in R Studio press: CTRL/CMD-SHIFT-F10) to unload all packages\n",
      "Option 2. Run groundhog.library() with option: 'ignore.package.conflicts=TRUE'\n",
      "This option can cause errors. Avoid using it unless you are trying to run old scripts which were not written\n",
      "using groundhog.library() and thus you don't know the groundhog day which will render them reproducible.\n\n"
    )

    stop("----------------- package not loaded - see above -- PRESS: CTRL/CMD-SHIFT-F10  ----------------")
  } # End if different count
} # End fucntion 2.15
