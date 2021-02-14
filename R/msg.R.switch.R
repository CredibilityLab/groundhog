# Message about \R version switch
#
# This message is printed when the \R version you currently use does not match
# the \R version in use at `date`.
#
# @return (invisibly) the full \R version in use at `date`.
#
# @seealso [r.version.check()]
#
msg.R.switch <- function(date) {
  rv <- r.version.check(date)

  message1(
  "#####################################################################\n",
    "\n\nInstructions for running other versions of R on demand, in both Mac and Windows, available from http://groundhogR.com/many\n\n\n",
  "###################################################################"
  )

  return(invisible(rv$r.need.full))
}
