[![Build Status](https://travis-ci.org/stewid/xapr.png)](https://travis-ci.org/stewid/xapr)

# Introduction

`xapr` is an R package that provides an interface to the
[Xapian](http://xapian.org/) search engine from R, allowing both
indexing and retrievel operations. A great introduction to
[Xapian](http://xapian.org/) is the
[Getting Started with Xapian](http://getting-started-with-xapian.readthedocs.org/en/latest/).

## Indexing

Index the content of a `data.frame` to documents with the Xapian
search engine A `document` is the data returned from a search.

The index plan is specified symbolically. An index plan has the form
`data ~ terms` where `data` is the blob of data returned from a search
and the `terms` are the basis for a search in Xapian. A first order
term index the text in the column as free text. A specification of the
form `first:second` indicates that the text in `second` should be
indexed with prefix `first`.

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

If the response contains more than one column, e.g. `col_1 + col_2 ~
X*.` the response is first converted to `JSON`. A compact form to
convert all fields to `JSON` is to use `. ~ terms`.

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

## The first 100 rows of the museum catalogue data is distributed with
## the 'xapr' package
filename <- system.file("extdata/NMSI_100.csv", package="xapr")
nmsi <- read.csv(filename, as.is = TRUE)

## Create a temporary directory to hold the database
path <- tempfile(pattern="xapr-")
dir.create(path)

## Index the 'TITLE' and 'DESCRIPTION' fields with both a suitable
## prefix and without a prefix for general search. Use the 'id_NUMBER'
## as unique identifier. Store all the fields as JSON for display
## purposes.
db <- xindex(. ~ S*TITLE + X*DESCRIPTION + Q:id_NUMBER, nmsi, path)

## Run a search and display docid (rowname) and TITLE from each match
xsearch(db, "watch", TITLE ~ .)
```

```
#>                                                           TITLE
#> 4                          Watch with Chinese duplex escapement
#> 18  Solar/Sidereal verge watch with epicyclic maintaining power
#> 13                                             Watch timer by P
#> 33 A device by Favag of Neuchatel which enables a stop watch to
#> 15  Ingersoll "Dan Dare" automaton pocket watch with pin-pallet
#> 36             Universal 'Tri-Compax' chronographic wrist watch
#> 46  Model by Dent of mechanism for setting hands and winding up
```

```r
## Run a search with multiple words
xsearch(db, "Dent watch", TITLE ~ .)
```

```
#>                                                                     TITLE
#> 46            Model by Dent of mechanism for setting hands and winding up
#> 4                                    Watch with Chinese duplex escapement
#> 18            Solar/Sidereal verge watch with epicyclic maintaining power
#> 13                                                       Watch timer by P
#> 94                                Model of a Lever Escapement , 1850-1883
#> 33           A device by Favag of Neuchatel which enables a stop watch to
#> 93                       Model of Graham's Cylinder Escapement, 1850-1883
#> 15            Ingersoll "Dan Dare" automaton pocket watch with pin-pallet
#> 36                       Universal 'Tri-Compax' chronographic wrist watch
#> 86 Model representing Earnshaw's detent chronometer escapement, 1950-1883
```

```r
## Run a search with prefix
xsearch(db, "title:sunwatch", TITLE ~ title:S)
```

```
#>                                    TITLE
#> 1 Ansonia Sunwatch (pocket compass dial)
```

```r
## Run a search with multiple prefixes
xsearch(db,
        "description:\"leather case\" AND title:sundial",
        TITLE ~ title:S + description:XDESCRIPTION)
```

```
#>                                      TITLE
#> 55 Silver altitude sundial in leather case
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
