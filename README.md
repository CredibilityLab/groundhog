
<!-- README.md is generated from README.Rmd. Please edit that file -->

# groundhog

<!-- badges: start -->

[![CRAN
version](https://www.r-pkg.org/badges/version-ago/groundhog)](https://cran.r-project.org/package=groundhog)
<!-- badges: end -->

For information about groundhog, check out the blogpost announcing it (http://datacolada.org/95), announcing v2.0 (http://datacolada.org/100),  or groundhog's website (https://groundhogr.com)

CRAN contains the officially released versions of groundhog. Here on github are the in development versions.  Almost nobody, or perhaps nobody, should rely on the version of groundhog available here.

The changelog is available at: https://groundhogr.com/changelog

A big change is coming to groundhog with the shutting down of MRAN.
For the next release of groundhog, v2.3 or possibly v3.0, groundhog will rely on a custom repository of CRAN binaries, named GRAN (http://gran.groundhogr.com).
Groundhog will also swith to parallel installation for source, providing noticeable speed improvements, specially for multiple packages whose dependencies can be installed in parallel. This new release is expected no later than May 2023, hopefully earlier. 

**Testing**
Because testing groundhog requires installing packages, restarting R sessions, evaluating multiple outcomes of an installation attempt, and artificially creating conflicts between groundhog's library and the local library, testing is not automatized via 'testthat' but rather conducted via a manually run script that is executed on a Windows, Mac and Unix machines. The testing involves:(1) generating the full set of possible conflicts between local and groundhog packages (e.g., attempting to attach a different version of a package that is already loaded, vs already attached), (2) proceeding to uninstall conflicts that are discovered, (3) listing those conflicts, (4) reinstalling them, and lastly (5) installing large numbers of packages (>100) in random order. The test script is kept outside the package but you can see it in the main folder above: `test_groundhog.r`
