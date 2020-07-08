#' Check snowball conflict
#'
#' @inheritParams estimate.seconds.left
#'
check.snowball.conflict <- function(snowball, force.install) {
  # short name for package being installed/loaded
  requested_pkg_vrs <- snowball$pkg_vrs[length(snowball$pkg_vrs)]
  # Load active packages
  active <- get.active()

  # Check if any package that needs to be installed are loaded; separte check from below because even SAME version created conflict
  if (force.install) {
    conflict.pkg <- snowball$pkg %in% active$pkg
    if (any(conflict.pkg)) {
      message2()
      message(
        "You selected 'force.install=TRUE' but the following packages that would be installed\n",
        "are currently loaded:", paste0(snowball$pkg[conflict.pkg], collapse = ",  "),
        "\n\nYou need to restart your R session to carry out the installation.\n",
        "(in R Studio press: CTRL/CMD-SHIFT-F10 to do so)"
      )
      exit()
    } # End conflict found for forced install
  } # End check force install

  # Compare already active package and package_version to find conflicts
  conflict.needed <- "" # Assume nothing is in conflict
  # These are packages that are needed and have a conflict with an active one
  conflict.needed <- snowball$pkg_vrs[!(snowball$pkg_vrs %in% active$pkg_vrs) & (snowball$pkg %in% active$pkg)]
  conflict.needed <- sort(conflict.needed)

  # These are packages that are active and have a conflict with a needed one
  conflict.active <- active$pkg_vrs[!(active$pkg_vrs %in% snowball$pkg_vrs) & (active$pkg %in% snowball$pkg)]
  conflict.active <- sort(conflict.active)

  # How many packages are needed and conflicted
  n.conflict <- length(conflict.needed)
  n.needed <- nrow(snowball)

  # Paste the package(s), which are vectors, into a string
  conflict.needed <- paste(conflict.needed, collapse = ",  ") # put a , between packages
  conflict.active <- paste(conflict.active, collapse = ",  ")


  # If different # of packages match pkg vs pkg_vrs, we have same packages  different vrs: stop
  if (conflict.needed != "") {
    message2()
    message1(
      "Cannot load the package '", requested_pkg_vrs, "' due to a package-version conflict.\n",
      "Specifically, in order to load, ", requested_pkg_vrs, " needs ", n.needed, " packages, ", n.conflict, " of which create a conflict\n",
      "because other versions of those same packages are already loaded.\n",
      "The package versions needed:\n", conflict.needed, "\n\n",
      "The package versions currently loaded:\n", conflict.active, "\n\n",
      "To solve this conflict you have two options.\n\n",
      "Option 1.*RECOMMENDED* Restart R session (in R Studio press: CTRL/CMD-SHIFT-F10) to unload all packages. Then:\n",
      "library(groundhogR)\ngroundhog.library(pkg,date)\n\n",
      "Option 2. Run groundhog.library() with option: 'ignore.package.conflicts=TRUE'\n",
      "This option will keep the loaded versions of the conflicting packages, possibly causing errors or unexpected behaviors.\n",
      "You may want to use this option to run multiple groundhog.library() commands, with different dates, within the same script.\n\n"
    )

    # Stop without error message
    exit(
      "groundhog.library() says: the package '", requested_pkg_vrs,
      "' was not loaded (read details above)\n\n-------------------------",
      "In R STUDIO: PRESS: CTRL/CMD-SHIFT-F10  ----------------"
    )
  } # End if some conflict found
} # End fucntion
