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

##' Prefix plan
##'
##' Extract the term prefixes
##' @param formula The term prefixes formula specification.
##' @return :TODO:DOCUMENTATION:
##' @keywords internal
prefix_plan <- function(formula) {
    term_prefixes <- c("A" ,"D", "E", "G", "H", "I", "K", "L", "M",
                       "N", "O", "P", "Q", "R", "S", "T", "U", "V",
                       "X", "Y", "Z")

    ## Extract response variable
    response <- attr(terms(formula), "response")
    if (response)
        stop("Invalid prefix formula")

    if (!all(sapply(attr(terms(formula), "order"), identical, 2L)))
        stop("Invalid prefix formula")

    prefix <- unique(attr(terms(formula), "term.labels"))
    prefix <- sapply(prefix,
                       function(prefix) {
                           ## Make sure the first term is the prefix
                           prefix <- unlist(strsplit(prefix, ":"))
                           if (prefix[1] %in% term_prefixes)
                               return(paste0(rev(prefix), collapse=":"))
                           if (prefix[2] %in% term_prefixes)
                               return(paste0(prefix, collapse=":"))
                           stop("Invalid index formula")
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
##' @export
xsearch <- function(query,
                    path,
                    prefix = NULL,
                    offset   = 0,
                    pagesize = 10,
                    wildcard = FALSE)
{
    if (!is.null(prefix)) {
        if (!is.data.frame(prefix))
            stop("'prefix' must be a data.frame")
        if (is.factor(prefix$field))
            prefix$field <- as.character(prefix$field)
        if (!is.character(prefix$field))
            stop(paste0("'prefix$$field' must be a character"))
        if (is.factor(prefix$prefix))
            prefix$prefix <- as.character(prefix$prefix)
        if (!is.character(prefix$prefix))
            stop(paste0("'prefix$$prefix' must be a character"))
    }

    .Call(
        "xapr_search",
        query,
        path,
        prefix,
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
