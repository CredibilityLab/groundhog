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

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a re-submission following Gregor Seyer's feedback:

- Reduced title length to less than 65 characters
- Removed examples from unexported functions
- Removed calls to `installed.packages()` function

Answer to other points raised during the review:

> Please unwrap the examples if they are executable in < 5 sec, or replace 
\dontrun{} with \donttest{}. 

We have carefully checked which examples could be wrapped with donttest rather
than dontrun and only examples that take more than 5 sec are wrapped in dontrun.

> Please ensure that your functions do not write by default or in your 
examples/vignettes/tests in the user's home filespace (including the package 
directory and getwd()). This is not allowed by CRAN policies. In your 
examples/vignettes/tests you can write to tempdir(). 

This package will only write to a location explicitly chosen by the user (via 
the function `set.groundhog.folder()`. This emulates the behaviour of 
`install.packages()`.
Examples that would write in the user directory are wrapped with `dontrun()`.
All chunks from the vignette are set as `eval = FALSE`.
All tests run in the `tmp` directory.
