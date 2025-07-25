## Comments from Maintainer

- This is an update to underlying JS library (canvasXpress) and updates for compatibility with the upcoming
ggplot2 4.0 release

---  

## Test environments


RStudio Server (ubuntu 20.04)  

* R 4.3.3

CircleCI

* R 4.0.5
* R 4.3.3
* R 4.4.3
* R Latest

WinBuilder

* devtools::check_win_devel()  
* devtools::check_win_release()  


RHub (v2)

rhub::rhub_check(branch    = "cran", 
                 platforms = c("ubuntu-next", "ubuntu-release", "nosuggests",  # linux platforms
                               "linux", "macos", "windows"))             # other platforms


---  

## R CMD check results


```
devtools::check()  

0 errors ✓ | 0 warnings ✓ | 0 notes ✓
```

---  

## Reverse dependencies

* canvasXpress.data
* DGEobj.utils
* periscope
* MAFDash

```
> revdepcheck::cran_revdeps('canvasXpress', bioc = T)
[1] "dbparser"     "DGEobj.utils" "MAFDash"      "periscope"    "periscope2"  
```

```
> revdepcheck:: revdep_report_cran()
## revdepcheck results

We checked 5 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 1 packages

Issues with CRAN packages are summarised below.

### Failed to check

* MAFDash (NA)
```
