
<!-- README.md is generated from README.Rmd. Please edit that file -->

# groundhog

<!-- badges: start -->

[![CRAN
version](https://www.r-pkg.org/badges/version-ago/groundhog)](https://cran.r-project.org/package=groundhog)
<!-- badges: end -->

For information about groundhog, check out the blogpost announcing it (http://datacolada.org/95) or groundhog's website (https://groundhogr.com)

CRAN contains the officially released versions. 

On github, here, you will find development versions.

The version of groundhog on CRAN (v1.5.0) only works with CRAN packages.
The version of groundhog here on GitHub is experimental, and allows working also with GitHub and Gitlab packages.
This version is expeirmental and may undergo backwards incompatible changes. It may also include typos and silly bugs.
Once the version is considered mature it will be sent to CRAN and become v2.0.0. 

To use this experimental version, V1.5.0.9050, 
remotes::install_github('CredibilityLab/groundhog')

And then use the same groundhog.library() command to load (installling when necessary) packages from CRAN, GitHub or Gitlab.
groundhog.library('crsh/papaja','2022-01-01') 


**Testing**
Because testing groundhog requires installing packages, restarting R sessions, evaluating multiple outcomes of an installation attempt, and artificially creating conflicts between groundhog's library and the local library, testing is not automatized via 'testthat' but rather conducted via a manually run script that is executed on a Windows, Mac and Unix machines. The testing involves:(1) generating the full set of possible conflicts between local and groundhog packages (e.g., attempting to attach a different version of a package that is already loaded, vs already attached), (2) proceeding to uninstall conflicts that are discovered, (3) listing those conflicts, (4) reinstalling them, and lastly (5) installing large numbers of packages (>100) in random order. The test script is kept outside the package but you can see it in the main folder above: test_groundhog.r
