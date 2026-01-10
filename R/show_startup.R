#' Show suppressed startup messages
#'
#' Displays startup messages that were suppressed during the most recent
#' \code{\link[=groundhog.library]{groundhog.library()}} call. These messages
#' are formatted with proper newlines and package version information.
#'
#' @return Invisibly returns the startup messages string, or NULL if no
#'   messages were suppressed.
#'
#' @examples
#' \dontrun{
#' groundhog.library("some_package", "2024-01-01")
#' show_startup()  # View any suppressed startup messages
#' }
#'
#' @export
show_startup <- function() {
  msgs <- .pkgenv[['startup_messages']]
  if (is.null(msgs) || msgs == "") {
    message("No startup messages were suppressed.")
    return(invisible(msgs))
  }
  
  # Split into lines
  all_lines <- strsplit(msgs, "\n")[[1]]
  
  # Find lines that are pkg_vrs (no leading space, contains underscore followed by version pattern)
  # Pattern: starts with non-space, contains underscore, followed by version-like pattern
  pkg_vrs_indices <- grep("^[^ ].*_[0-9]", all_lines)
  
  # Build formatted output with header
  if (.pkgenv[["supportsANSI"]]) {
    formatted <- paste0("\033[1m", "Suppressed startup messages in most recent groundhog call", "\033[0m", "\n\n\n")
  } else {
    formatted <- "Suppressed startup messages in most recent groundhog call\n\n\n"
  }
  
  if (length(pkg_vrs_indices) > 0) {
    for (k in 1:length(pkg_vrs_indices)) {
      start_idx <- pkg_vrs_indices[k]
      end_idx <- if (k < length(pkg_vrs_indices)) pkg_vrs_indices[k + 1] - 1 else length(all_lines)
      
      # Get pkg_vrs (first line of this block)
      pkg_vrs <- all_lines[start_idx]
      
      # Add counter and package name (no newline after counter, just space)
      if (.pkgenv[["supportsANSI"]]) {
        formatted <- paste0(formatted, "\033[1m", k, ") ", pkg_vrs, "\033[0m", "\n")
      } else {
        formatted <- paste0(formatted, k, ") ", pkg_vrs, "\n")
      }
      
      # Add the rest of the messages for this package
      if (start_idx < end_idx) {
        for (i in (start_idx + 1):end_idx) {
          formatted <- paste0(formatted, all_lines[i], "\n")
        }
      }
      
      # Add separator (except after last package)
      if (k < length(pkg_vrs_indices)) {
        formatted <- paste0(formatted, "------------------------------\n")
      }
    }
  } else {
    # Fallback: just show messages as-is if we can't parse
    formatted <- paste0(formatted, msgs)
  }
  
  message(formatted)
  invisible(msgs)
}
