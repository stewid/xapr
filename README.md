[![Build Status](https://travis-ci.org/stewid/xapr.png)](https://travis-ci.org/stewid/xapr)

# Introduction

`xapr` is an R package that provides an interface to the
[Xapian](http://xapian.org/) search engine from R, allowing both
indexing and retrievel operations. A great introduction to
[Xapian](http://xapian.org/) is the
[Getting Started with Xapian](http://getting-started-with-xapian.readthedocs.org/en/latest/).

## Indexing

Index the content of a \code{data.frame} to documents with the Xapian
search engine A `document` is the data returned from a search.

The index plan is specified symbolically. An index plan has the form
`data ~ terms` where `data` is the blob of data returned from a search
and `terms` is the basis for a search in Xapian. A first order term
index the text in the column as free text. A specification of the form
`first:second` indicates that the text in `second` should be indexed
with prefix `first`.

The prefix is a short string at the beginning of the term to indicate
which field the term indexes. Valid prefixes are: 'A' ,'D', 'E', 'G',
'H', 'I', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V',
'X', 'Y' and 'Z'. See http://xapian.org/docs/omega/termprefixes for a
list of conventional prefixes.

The specification `first*second` is the same as `second +
first:second`. The prefix `X` will create a user defined prefix by
appending the uppercase `second` to `X`. The prefix `Q` will use data
in the `second` column as a unique identifier for the document. `NA`
values in indexed columns are skipped.

No response e.g. `~ second + first:second` writes the row number as
data to the document.

The specification `~X*.` creates prefix terms with all columns plus
free text.

### Example

This is an `R` version of the `Python` example in the
[Getting Started with Xapian](http://getting-started-with-xapian.readthedocs.org/en/latest/practical_example/index.html)

We are going to build a simple search system based on museum catalogue
data released under the Creative Commons Attribution-NonCommercial-
ShareAlike license (http://creativecommons.org/licenses/by-nc-sa/3.0/)
by the Science Museum in London, UK.
(http://api.sciencemuseum.org.uk/documentation/collections/)

```r
library(xapr)
library(jsonlite)

## The first 100 rows of the museum catalogue data is distributed with
## the 'xapr' package
filename <- system.file("extdata/NMSI_100.csv", package="xapr")
nmsi <- read.csv(filename, as.is = TRUE)

## Create a temporary directory to hold the database
path <- tempfile(pattern="xapr-")
dir.create(path)

## Store all the fields for display purposes
nmsi$data <- sapply(seq_len(nrow(nmsi)), function(i) {
    as.character(toJSON(nmsi[i,]))
})

## Index the data
xindex(data ~ S*TITLE + X*DESCRIPTION + Q:id_NUMBER, nmsi, path)
```

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

Another alternative is to use `git` and `make`

```
$ git clone https://github.com/stewid/xapr.git
$ cd xapr
$ make install
```

**NOTE:** The package is in a very early development phase. Functions
and documentation may be incomplete and subject to
change. Suggestions, bugs, forks and pull requests are
appreciated. Get in touch.

# License

The `xapr` package is licensed under the GPL (>= 2). See these files
for additional details:

- [LICENSE](LICENSE)     - `xapr` package license
