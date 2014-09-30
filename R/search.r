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

##' Search plan
##'
##' Extract the term prefixes
##' @param formula The term prefixes formula specification.
##' @return :TODO:DOCUMENTATION:
##' @keywords internal
search_plan <- function(formula) {
    term_prefixes <- c("A" ,"D", "E", "G", "H", "I", "K", "L", "M",
                       "N", "O", "P", "Q", "R", "S", "T", "U", "V",
                       "X", "Y", "Z")

    ## Extract response variable
    response <- attr(terms(formula), "response")
    if (response)
        stop("Invalid search formula")

    if (!all(sapply(attr(terms(formula), "order"), identical, 2L)))
        stop("Invalid search formula")

    prefix <- unique(attr(terms(formula), "term.labels"))
    prefix <- sapply(prefix,
                       function(prefix) {
                           ## Make sure the first term is the prefix
                           prefix <- unlist(strsplit(prefix, ":"))
                           if (substr(prefix[1], 1, 1) %in% term_prefixes)
                               return(paste0(rev(prefix), collapse=":"))
                           if (substr(prefix[2], 1, 1) %in% term_prefixes)
                               return(paste0(prefix, collapse=":"))
                           stop("Invalid search formula")
                       })
    names(prefix) <- NULL

    ## Extract field and prefix
    field <- sapply(strsplit(prefix, ":"), "[", 1)
    prefix <- sapply(strsplit(prefix, ":"), "[", 2)

    list(field  = field,
         prefix = prefix)
}

##' Search a Xapian database
##'
##' @param query A free-text query
##' @param path A character vector specifying the path to one or more
##' Xapian databases.
##' @param prefix A formula specification with term prefixes. Default
##' NULL. See 'Details'.
##' @param offset Starting point within result set. Default 0.
##' @param pagesize Number of records to retrieve. Default 10.
##' @param wildcard Support searches using a trailing '*' wildcard,
##' which matches any number of trailing characters, so wildc* would
##' match wildcard, wildcarded, wildcards, wildcat, wildcats,
##' etc. Default is FALSE.
##' @return \code{xapian_search} object with result
##' @details The term prefixes are specified symbolically. The
##' prefixes has the form '~field:prefix'.
##' @export
xsearch <- function(query,
                    path,
                    prefix = NULL,
                    offset   = 0,
                    pagesize = 10,
                    wildcard = FALSE)
{
    if (!is.null(prefix)) {
        if (!is(prefix, "formula"))
            stop("'prefix' must be a 'formula'")
        sp <- search_plan(prefix)
    } else {
        sp <- list(field  = character(0),
                   prefix = character(0))
    }

    .Call(
        "xapr_search",
        query,
        path,
        sp$field,
        sp$prefix,
        as.integer(offset),
        as.integer(pagesize),
        wildcard,
        package = "xapr")
}

##' @export
print.xapian_match <- function(x, ...)
{
    cat(sprintf("%i: #%3.3i %s\n",
                x$rank + 1,
                x$docid,
                x$data))
}

##' @export
print.xapian_search <- function(x, ...)
{
    lapply(x, print)
}
