## Test environments
* local macOS: release
* Travis Ubuntu: oldrel, release, devel
* win-builder: devel
* r-hub: devel

## R CMD check results

0 errors | 0 warnings | 0 note

* This is a new release.
* This is fourth submission. The differences are
    - Added inst/extdata/example.Rmd and use it in examples, in response to the review comment.
    - The reviewer suggested "Please replace \dontrun{} by \donttest{} in your Rd-files, if the
      user do not need an API key," but the users do need username and password, so I didn't replace.

