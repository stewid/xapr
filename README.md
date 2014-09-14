[![Build Status](https://travis-ci.org/stewid/xapr.png)](https://travis-ci.org/stewid/xapr)

# Introduction

xapr is a package that aims to provide an interface to the
[Xapian](http://xapian.org/) search engine from R, allowing both
indexing and retrievel operations.

**NOTE:** The package is in a very early development phase. Functions
and documentation may be incomplete and subject to
change. Suggestions, bugs, forks and pull requests are
appreciated. Get in touch.

## Installation

The development files for the `Xapian` search engine must be
installed.

```
$ sudo apt-get install libxapian-dev
```

To install the development version of `xapr`, it's easiest to use the
devtools package:

```r
# install.packages("devtools")
library(devtools)
install_github("stewid/xapr")
```

# License

The `xapr` package is licensed under the GPL (>= 2). See these files
for additional details:

- [LICENSE](LICENSE)     - `xapr` package license
