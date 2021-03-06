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

df <- data.frame(data    = "This is a test",
                 author  = "Stefan Widgren",
                 content = "Hello world",
                 stringsAsFactors = FALSE)

db <- xindex(data ~ A:author + content, df, path)

## Expect no hit: NULL
stopifnot(identical(xsearch(db, "Widgren"), NULL))

## Expect one hit.
stopifnot(identical(xsearch(db, "author:Widgren",  ~author:A),
                    structure("This is a test", .Names = "1")))

## Expect no hit without prefix: NULL
stopifnot(identical(xsearch(db, "author:Widgren"), NULL))

## Cleanup
unlink(path, recursive=TRUE)
