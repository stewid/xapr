## xapr, R bindings to the Xapian search engine.
## Copyright (C) 2014 Stefan Widgren
##
##  xapr is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
##
##  xapr is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License along
##  with this program; if not, write to the Free Software Foundation, Inc.,
##  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

library(xapr)

path <- tempfile(pattern="xapr-")

xapr_index(
    path    = path,
    doc     = "This is a test",
    terms   = data.frame(A="Stefan Widgren"),
    content = "Hello world")

## Expect empty list.
stopifnot(identical(xapr_search(path, "Widgren"),
                    structure(list(), class = "xapian_search")))

## Expect one hit.
search_2 <- structure(list(
    structure(list( docid = 1L, rank = 0L, percent = 100L,
                   data = "This is a test"),
              .Names = c( "docid", "rank", "percent", "data"),
              class = "xapian_match")), class = "xapian_search")

prefix <- data.frame(field = "author", prefix = "A")

stopifnot(identical(xapr_search(path, "author:Widgren", prefix),
                    search_2))

## Expect empty list without prefix
stopifnot(identical(xapr_search(path, "author:Widgren"),
                    structure(list(), class = "xapian_search")))

## Cleanup
unlink(path, recursive=TRUE)
