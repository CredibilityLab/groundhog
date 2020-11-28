#' Check snowball conflict
#'
#' @inheritParams estimate.seconds.left
#' @inheritParams install.snowball
#'
check.snowball.conflict <- function(snowball, force.install) {
  # short name for package being installed/loaded
  requested_pkg_vrs <- snowball$pkg_vrs[length(snowball$pkg_vrs)]
  # Load active packages
  active <- get.active()

  # Check if any package that needs to be installed are loaded; separate check from below because even SAME version created conflict
  if (force.install) {
    conflict.pkg <- (snowball$pkg %in% active$pkg) 
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
    message1("Some of the packages needed by '", requested_pkg_vrs, "' are currently loaded,",
             " but not with the version that is needed.\n",
            "To solve this: restart the R session. Note: you will need to do 'library(groundhog)' again.\n\n",
            "In R Studio press: CTRL/CMD-SHIFT-F10")
     message("The package '", requested_pkg_vrs,"' was *NOT* loaded")
     cat("needed conflict:\n",conflict.needed,"\n\nconflict active:\n", conflict.active)
    exit()
  } # End if some conflict found
} # End function
