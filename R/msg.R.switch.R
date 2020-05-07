#' Message about \R version swith
#'
#' This message is printed when the \R version you currently use does not match
#' the \R version in use at `date`.
#'
#' @inheritParams r.version.check
#'
#' @return (invisibly) the full \R version in use at `date`.
#'
#' @seealso [r.version.check()]
#'
#' @export
msg.R.switch <- function(date) {
  rv <- r.version.check(date)
  message1(
    "####################################################################################################\n",
    "INSTRUCTIONS TO RUN OLDER R VERSIONS IN R STUDIO:\n(copy-paste somewhere to have available when you close this window)\n\n",
    "WINDOWS:\n",
    "  1) Download R-", rv$r.need.full, " from https://cran.r-project.org/bin/windows/base/old/\n",
    "  2) Install it (e.g., double click on downloaded file)\n",
    "  3) Either: Restart R Studio pressing down the CTRL key\n",
    "     OR:     Within R Studio: Tools->Global Options->(first textbox)\n\n\n",
    "MAC:\n",
    "  1) Download R-", rv$r.need.full, " from\n",
    "     https://cran.r-project.org/bin/macosx/old/               [up to R-3.3.3]\n",
    "     https://cran.r-project.org/bin/macosx/el-capitan/base/   [since R-3.4.0]\n\n",

    "  2) Switch which R you use with a utility called 'R Switch'\navailable from: https://mac.r-project.org/RSwitch-1.2.dmg).\n\n\n",
    "LINUX and more details for Windows and Mac: https://groundhogR.com/olderR\n",
    " ####################################################################################################\n"
  )
  return(invisible(rv$r.need.full))
}
