# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.2.0 (2015-04-16) |
|system   |x86_64, darwin13.4.0         |
|ui       |RStudio (0.99.561)           |
|language |(EN)                         |
|collate  |en_US.UTF-8                  |
|tz       |America/Chicago              |

## Packages

|package |*  |version |date       |source         |
|:-------|:--|:-------|:----------|:--------------|
|Rcpp    |   |0.11.6  |2015-05-01 |CRAN (R 3.2.0) |
|stringr |   |1.0.0   |2015-04-30 |CRAN (R 3.2.0) |

# Check results
3 checked out of 3 dependencies 

## Ecfun (0.1-4)
Maintainer: Spencer Graves <spencer.graves@effectivedefense.org>

```
checking package dependencies ... NOTE
Packages suggested but not available for checking:
  ‘BMA’ ‘DescTools’ ‘Ecdat’
```
```
checking S3 generic/method consistency ... NOTE
Found the following apparent S3 methods exported but not registered:
  Interp.default interpChar.default interpChar.list interpPairs.call
  interpPairs.function interpPairs.list lines.qqnorm2
  parseCommas.data.frame parseCommas.default plot.qqnorm2 plot.qqnorm2s
  points.qqnorm2
See section ‘Registering S3 methods’ in the ‘Writing R Extensions’
manual.
```
```
checking Rd cross-references ... NOTE
Packages unavailable to check Rd xrefs: ‘EnvStats’, ‘drc’, ‘prodlim’, ‘TRAMPR’, ‘Ecdat’
```
```
checking examples ... ERROR
Running examples in ‘Ecfun-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: financialCrisisFiles
> ### Title: Files containing financial crisis data
> ### Aliases: financialCrisisFiles
> ### Keywords: IO
> 
> ### ** Examples
> 
> Ecdat.demoFiles <- system.file('demoFiles', package='Ecdat')
> Ecdat.xls <- dir(Ecdat.demoFiles, pattern='xls$',
+                  full.names=TRUE)
> if(require(gdata)){
+   tst <- financialCrisisFiles(Ecdat.xls)
+ }
Loading required package: gdata
gdata: read.xls support for 'XLS' (Excel 97-2004) files ENABLED.

gdata: read.xls support for 'XLSX' (Excel 2007+) files ENABLED.

Attaching package: ‘gdata’

The following object is masked from ‘package:stats’:

    nobs

The following object is masked from ‘package:utils’:

    object.size

Read NA; Error in if (substring(xls, 1, 7) == "http://" || substring(xls, 1, 6) ==  : 
  missing value where TRUE/FALSE needed
Calls: financialCrisisFiles -> <Anonymous> -> xls2sep
Error in file.exists(tfn) : invalid 'file' argument
Calls: financialCrisisFiles -> <Anonymous> -> xls2sep -> file.exists
Execution halted
```
```
DONE
Status: 1 ERROR, 3 NOTEs
```

## R6 (2.0.1)
Maintainer: "Winston Chang" <winston@stdout.org>

```
checking package dependencies ... NOTE
Packages suggested but not available for checking:
  ‘microbenchmark’ ‘testthat’
```
```
checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  Type 'license()' or 'licence()' for distribution details.
  
  R is a collaborative project with many contributors.
  Type 'contributors()' for more information and
  'citation()' on how to cite R or R packages in publications.
  
  Type 'demo()' for some demos, 'help()' for on-line help, or
  'help.start()' for an HTML browser interface to help.
  Type 'q()' to quit R.
  
  > library(testthat)
  Error in library(testthat) : there is no package called 'testthat'
  Execution halted
```
```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Quitting from lines 17-32 (Performance.Rmd) 
Error: processing vignette 'Performance.Rmd' failed with diagnostics:
there is no package called 'microbenchmark'
Execution halted

```
```
DONE
Status: 1 ERROR, 2 NOTEs
```

## radiant (0.1.83)
Maintainer: Vincent Nijs <radiant@rady.ucsd.edu>  
Bug reports: https://github.com/vnijs/radiant/issues

```
checking package dependencies ... NOTE
Package suggested but not available for checking: ‘testthat’
```
```
DONE
Status: 1 NOTE
```

