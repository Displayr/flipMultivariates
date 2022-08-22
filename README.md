[![](https://travis-ci.org/Displayr/flipMultivariates.svg?branch=master)](https://travis-ci.org/Displayr/flipMultivariates/)
[![Coverage Status](https://coveralls.io/repos/github/Displayr/flipMultivariates/badge.svg?branch=master)](https://coveralls.io/github/Displayr/flipMultivariates?branch=master)
# flipMultivariates

Multivariate models

To install from GitHub
```
require(devtools)
install_github("Displayr/flipMultivariates", dependencies = NA)
```

If you have not set up a GitHub Personal Access Token, you will likely need to do so to avoid 
GitHub rate limits, which will manifest as 403 errors when downloading packages via
`install_github`. Please see the documentation in the `usethis` package or see the 
instructions [here](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token) and [here](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token).

If you are using Windows, you will need to have a version of Rtools installed that matches your
version of R in order to build packages from source. Rtools can be downloaded from
[here](https://cran.r-project.org/bin/windows/Rtools/).

Specifying `dependencies = NA` in `install_github` will not install packages listed
in `Suggests` in the `DESCRIPTION` file (some of which may be proprietary and unavailable for download).

[![Displayr logo](https://mwmclean.github.io/img/logo-header.png)](https://www.displayr.com)
