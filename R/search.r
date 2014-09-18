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

##' Search a Xapian database
##'
##' @param path A character vector specifying the path to one or more
##' Xapian databases.
##' @param terms Search terms
##' @param offset Starting point within result set. Default 0.
##' @param pagesize Number of records to retrieve. Default 10.
##' @param wildcard Support searches using a trailing '*' wildcard,
##' which matches any number of trailing characters, so wildc* would
##' match wildcard, wildcarded, wildcards, wildcat, wildcats,
##' etc. Default is FALSE.
##' @return list with search result
##' @export
xapr_search <- function(path,
                        terms,
                        offset   = 0,
                        pagesize = 10,
                        wildcard = FALSE)
{
    .Call(
        "xapr_search",
        path,
        terms,
        as.integer(offset),
        as.integer(pagesize),
        wildcard,
        package = "xapr")
}

##' @export
print.xapian_match <- function(x)
{
    cat(sprintf("%i: #%3.3i %s\n",
                x$rank + 1,
                x$docid,
                x$data))
}

##' @export
print.xapian_search <- function(x)
{
    lapply(x, print)
}
