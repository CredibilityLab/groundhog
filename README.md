
<!-- README.md is generated from README.Rmd. Please edit that file -->

# groundhog

<!-- badges: start -->

[![CRAN
version](https://www.r-pkg.org/badges/version-ago/groundhog)](https://cran.r-project.org/package=groundhog)
[![R build
status](https://github.com/CredibilityLab/groundhog/workflows/R-CMD-check/badge.svg)](https://github.com/CredibilityLab/groundhog/actions)
[![Codecov test
coverage](https://codecov.io/gh/CredibilityLab/groundhog/branch/master/graph/badge.svg)](https://codecov.io/gh/CredibilityLab/groundhog?branch=master)
<!-- badges: end -->

For information about groundhog, check out the blogpost announcing it (http://datacolada.org/95) or groundhog's website (https://groundhogr.com)

CRAN contains the officially released versions. 

On github, here, you will find development versions which may change and could even introduce backwards incompatibilities (from one *un*released version to another). 
Typos and silly bugs are likely in this development version. Unless you are attempting to contribute to groundhog actively, you should stick to the CRAN version.


install.packages('groundhog') 

**Testing**
Because testing groundhog requires installing packages, restarting R sessions, evaluating multiple outcomes of an installation attempt, and artificially creating conflicts between groundhog's library and the local library, testing is not automatized via 'testthat' but rather conducted via a manually run script that is executed on a Windows, Mac and Unix machines. The testing involves:(1) generating the full set of possible conflicts between local and groundhog packages (e.g., attempting to attach a different version of a package that is already loaded, vs already attached), (2) proceeding to uninstall conflicts that are discovered, (3) listing those conflicts, (4) reinstalling them, and lastly (5) installing large numbers of packages (>100) in random order. The test script is not included in the package, but it is available here (https://github.com/CredibilityLab/groundhog/blob/master/test_groundhog.r).
