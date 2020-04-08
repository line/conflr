## Test environments
* local macOS: release
* Travis Ubuntu: oldrel, release, devel
* win-builder: devel
* r-hub: devel

## R CMD check results

0 errors | 0 warnings | 0 note

* This is a maintainance release to fix the errors on CRAN checks:
    * Add pandoc to SystemRequirements on DESCRIPTION.
    * Skip tests when pandoc is not available.
