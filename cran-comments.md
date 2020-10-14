## Test environments

* local R installation, R 4.0.2 on Ubuntu 20.04.1 LTS
* GitHub actions:
  - R 3.2 on Ubuntu 16.04
  - R 3.3 on Ubuntu 16.04
  - R 3.4 on Ubuntu 16.04
  - R 3.5 on Ubuntu 16.04
  - R 3.6 on Ubuntu 16.04
  - R 4.0 on Ubuntu 16.04
  - R devel on Ubuntu 16.04
  - R release on windows
  - R release on macOS
* R devel and release on winbuilder
* rhub::check_for_cran()

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a re-submission following Gregor Seyer's feedback:

- Remaining examples in unexported functions have been removed
- One more example uses `\donttest{}` instead of `\dontrun{}`. We cannot change
this for the remaining examples as they produce permanent setting changes, or
write to the user home (at a user-specified location). We do not want users to
run these accidentally. This is similar to the example in `install.packages()`, 
which is wrapped in `\dontrun{}`
