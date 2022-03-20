
<!-- README.md is generated from README.Rmd. Please edit that file -->

# groundhog

<!-- badges: start -->

[![CRAN
version](https://www.r-pkg.org/badges/version-ago/groundhog)](https://cran.r-project.org/package=groundhog)
<!-- badges: end -->

For information about groundhog, check out the blogpost announcing it (http://datacolada.org/95) or groundhog's website (https://groundhogr.com)

CRAN contains the officially released versions of groundhog. Here on github is the in-development version.

The in-development version may undergo backwards incompatible changes, and probably includes many (instead of few) typos and silly bugs.
Once it seems to be ready, it will be sent to CRAN and become v2.0.0. Aiming for April 2022. 

To use this in-development version:
remotes::install_github('CredibilityLab/groundhog')


**Major changes in v1.9.9.999 (versus v.1.5.0 on CRAN)***
1) Install/load packages from GitHub & GitLab, not just CRAN

   syntax:   `groundhog.library(usr/pkg,date)` 
   example:   `groundhog.library('crsh/papaja','2022-01-01')` 
   
3) Built in switch that if MRAN is down, installs everything from CRAN for next 5 hours (using source, instead of MRAN binaries, for older packages)
4) Speed increase for loading packages


**Testing**
Because testing groundhog requires installing packages, restarting R sessions, evaluating multiple outcomes of an installation attempt, and artificially creating conflicts between groundhog's library and the local library, testing is not automatized via 'testthat' but rather conducted via a manually run script that is executed on a Windows, Mac and Unix machines. The testing involves:(1) generating the full set of possible conflicts between local and groundhog packages (e.g., attempting to attach a different version of a package that is already loaded, vs already attached), (2) proceeding to uninstall conflicts that are discovered, (3) listing those conflicts, (4) reinstalling them, and lastly (5) installing large numbers of packages (>100) in random order. The test script is kept outside the package but you can see it in the main folder above: test_groundhog.r
