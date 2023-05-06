
<!-- README.md is generated from README.Rmd. Please edit that file -->

# groundhog

<!-- badges: start -->

[![CRAN
version](https://www.r-pkg.org/badges/version-ago/groundhog)](https://cran.r-project.org/package=groundhog)
<!-- badges: end -->

For information about groundhog, check out the blogpost announcing it (http://datacolada.org/95), announcing v2.0 (http://datacolada.org/100),  or groundhog's website (https://groundhogr.com)

CRAN contains the officially released versions of groundhog. Here on github are the in development versions.  Almost nobody, or perhaps nobody, should rely on the version of groundhog available here.

The changelog is available at: https://groundhogr.com/changelog
With version v3.0.0 groundhog stopped relying on MRAN, relying instead on the custom archive of CRAN called "GRAN".
A major redesign of many key architectural features followed this change, dramatically improving speed. Installation time dropped as much as 75%.

As may have been expected, an against-time major rewrite led to some bugs, they mostly impact Linux users. 
V3.1.0 fixes those bugs and made further improvements arising from addressing those bugs. See change log for details. Version 3.1.0 was submitted to CRAN on 2023-05-05.

**Testing**
Because testing groundhog requires installing packages, restarting R sessions, evaluating multiple outcomes of an installation attempt, and artificially creating conflicts between groundhog's library and the local library, testing is not automatized via 'testthat' but rather conducted via a manually run script that is executed on a Windows, Mac and Unix machines. The testing involves:(1) generating the full set of possible conflicts between local and groundhog packages (e.g., attempting to attach a different version of a package that is already loaded, vs already attached), (2) proceeding to uninstall conflicts that are discovered, (3) listing those conflicts, (4) reinstalling them, and lastly (5) installing large numbers of packages (>100) in random order. The test script is kept outside the package but you can see it in the main folder above: `test_groundhog.r`
