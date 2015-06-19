## Release summary

* This fixes my dumb error of using <= 3.2.0 instead of > 3.3.0 
* It includes a couple of other small bug fixes/improvements.

## Test environments
* local OS X install, R 3.2.0
* ubuntu 12.04 (on travis-ci), R 3.2.0
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There were 2 NOTEs:

* checking dependencies in R code ... NOTE
  Missing or unexported object: ‘tools::nonS3methods’

  This is a new function only available in R-devel. The package has
  a fallback for older versions of R.

## Downstream dependencies
I have also run R CMD check on the three downstream dependencies of pryr 
(https://github.com/wch/checkresults/blob/master/pryr/r-release). None of the
problems appear related to pryr (which isn't surprising beacause this release
just includes a couple of tiny bug fixes.)
