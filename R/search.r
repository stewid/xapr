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
    response <- attr(terms(formula, allowDotAsName = TRUE), "response")
    if (response) {
        vars <- attr(terms(formula, allowDotAsName = TRUE), "variables")[-1]
        data <- as.character(vars[response])

        ## Handle "col1 + col2" etc
        data <- unlist(strsplit(data, "+", fixed=TRUE))
        data <- sub("^\\s", "", sub("\\s$", "", data))
    } else {
        data <- NULL
    }

    prefix <- attr(terms(formula, allowDotAsName = TRUE), "term.labels")
    prefix <- prefix[attr(terms(formula, allowDotAsName = TRUE), "order") == 2]
    prefix <- unique(prefix)
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
    if (length(prefix)) {
        field <- sapply(strsplit(prefix, ":"), "[", 1)
        prefix <- sapply(strsplit(prefix, ":"), "[", 2)
    } else {
        field <- character(0)
        prefix <- character(0)
    }

    list(data   = data,
         field  = field,
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
##' @param view Invoke a text editor on the result before
##' returning. This enables editing the result before it's
##' returned. The editor is only opened if there are any response
##' variables supplied in the prefix formula.
##' @return \code{xapian_search} object with result
##' @details The term prefixes are specified symbolically. The
##' prefixes has the form '~field:prefix'.
##' @import plyr
##' @export
xsearch <- function(query,
                    path,
                    prefix   = NULL,
                    offset   = 0,
                    pagesize = 10,
                    wildcard = FALSE,
                    view     = FALSE)
{
    if (!is.null(prefix)) {
        if (!is(prefix, "formula"))
            stop("'prefix' must be a 'formula'")
        sp <- search_plan(prefix)
    } else {
        sp <- list(data   = NULL,
                   field  = character(0),
                   prefix = character(0))
    }

    result <- .Call("xapr_search",
                    query,
                    path,
                    sp$field,
                    sp$prefix,
                    as.integer(offset),
                    as.integer(pagesize),
                    wildcard,
                    package = "xapr")

    if (!is.null(sp$data)) {
        if (length(result)) {
            rn <- sapply(result, "[[", "docid")
            if (identical(sp$data, ".")) {
                result <- lapply(result, function(x) {
                    fromJSON(x$data)
                })
            } else {
                result <- lapply(result, function(x) {
                    fromJSON(x$data)[,sp$data, drop=FALSE]
                })
            }
            result <- rbind.fill(result)
            rownames(result) <- rn

            if (view)
                result <- edit(result)
        } else {
            result <- lapply(seq_len(length(sp$data)), function(i) character(0))
            result <- as.data.frame(result, stringsAsFactors = FALSE)
            colnames(result) <- sp$data
        }
    }

    result
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
