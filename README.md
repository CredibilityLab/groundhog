
<!-- README.md is generated from README.Rmd. Please edit that file -->

# groundhog

<!-- badges: start -->

[![CRAN
version](https://www.r-pkg.org/badges/version-ago/groundhog)](https://cran.r-project.org/package=groundhog)
<!-- badges: end -->

For information about groundhog, check out the blogpost announcing it (http://datacolada.org/95) or groundhog's website (https://groundhogr.com)

CRAN contains the officially released versions of groundhog. Here on github is the in-development version.

The in-development version may undergo backwards incompatible changes, and probably includes many (instead of few) typos and silly bugs.
Once it seems to be ready, it will be sent to CRAN and become v2.0.0. Aiming for July 2022. 

To use this in-development version:
`remotes::install_github('CredibilityLab/groundhog')`


**Very Major changes in v1.9.9.last_date (versus v.1.5.0 on CRAN)***
1) Install/load packages from GitHub & GitLab, not just CRAN

   syntax:   `groundhog.library(usr/pkg,date)` 
   
   example:   `groundhog.library('crsh/papaja','2022-01-01')` 
   
This is implemented using packages `git2r` and `remotes`. Specifically, the entire git project is cloned locally to the user's computer. From the local clone, the history of all commits is then used for version control, to identify the commit's sha based on the requested date, and then `remotes` installs off the local repository accordingly. These two packages are now 'suggested' and are dynamically loaded with `groundhog` if needed (if a user does indeed load a git package).
   
2) Large improvement in reducing package version conflicts, by copying files from groundhog's library, to the to default personal library. That is to say, upon installing the desired version of a package in the groundhog library, it is also copied to the default personal library (replacing existing versions if any). This means that when R Studio automatically loads packages, which it does off the default personal library if available, it loads the version groundhog is going to be relying on, eliminating pervasive and previously difficult to avoid conflicts caused by R Studio's policy of automatically loading packages (e.g., if they are referenced on the script, or used for creating notebooks, knitting, etec).  This new approach also addresses reproducibility challenges posed by background processes that would load from the default personal library, e.g., in parallel `foreach` loops when packages are exported and loaded by the 'workers' on background, off the default personal library if available. Note than an interim non-released 1.9.9 version of groundhog relied on 'disabling' packages. This proved a stepping stone towards the more reliable and reproducible solution of copying packages. Effectively, starting with groundhog v1.9.9.2022.06.28,  groundhog library can be thought of as the permanent package library, and R's default personal library as a temporary one which gets updated whenever the groundhog.day is changed.

**Other  changes**

3) Built in switch so that if MRAN is down, installs everything from CRAN for next 5 hours (using source files from CRAN, instead of binaries from MRAN, for older packages)
4) Speed increase for loading packages by saving the 'snowballs' (sequence of installation/loading) for a given package date, as an .rds file, instead of recomputing that sequence each time a package is loaded.
5) All dialogs providing important information now require active response by users to prevent missing errors or other critical information. Dialogs also have standardized formatting, with an "IMPORTANT" header, and "|    " as the left-margin.
6) Warning when different groundhog days are used within the same session, alerting to possible groundhog-base origin of package conflict.
7) If no CRAN repository is set, one is set by default to avoid errors.
8) If no personal library is set, users are prompted to accept creating and using a default personal library (this is important for major change #2 above). The location of that default library is that identified by `Sys.getenv("R_LIBS_USER")`
9) The final (back-end) step of every groundhog.library('pkg','date') call is now literally running `library('pkg')` so as to more fully match the behavior obtained when relying on library() instead of groundhog.library() (up to version v1.5.0, groundhog would instead `load` dependencies and `attach` the requested package.
10) More information given when RTools seems to be missing
11) Shortened most messages that do not require user action, provided more details on those that do
12) Fixed bug where .libPath() would not always return to its default upon finishing the groundhog.library() call
13) Fixed bugs involving base and recommended packages
14) Added some checks for missing and erroneously formatting function inputs (more to be done on this in future revisions)
15) Fixed bug that largely made groundhog incompatible with R-3.3.x (it would work with 3.2.x and with 3.4.x)




**Testing**
Because testing groundhog requires installing packages, restarting R sessions, evaluating multiple outcomes of an installation attempt, and artificially creating conflicts between groundhog's library and the local library, testing is not automatized via 'testthat' but rather conducted via a manually run script that is executed on a Windows, Mac and Unix machines. The testing involves:(1) generating the full set of possible conflicts between local and groundhog packages (e.g., attempting to attach a different version of a package that is already loaded, vs already attached), (2) proceeding to uninstall conflicts that are discovered, (3) listing those conflicts, (4) reinstalling them, and lastly (5) installing large numbers of packages (>100) in random order. The test script is kept outside the package but you can see it in the main folder above: `test_groundhog.r`
