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
##' @include prefixes.r
search_plan <- function(formula) {
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
                         if (substr(prefix[1], 1, 1) %in% term_prefixes())
                             return(paste0(rev(prefix), collapse=":"))
                         if (substr(prefix[2], 1, 1) %in% term_prefixes())
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
##' @rdname xsearch-methods
##' @docType methods
##' @param db A \code{linkS4class{xapian_database}} object.
##' @param query A free-text query
##' @param formula A formula with symbolic specification of the data
##' output and term prefixes. Default NULL. See 'Details'.
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
##' @keywords methods
##' @details The search plan for \code{xsearch} is specified
##' symbolically. A search plan has the form \code{data ~
##' field:prefix} where \code{data} is the blob of data returned from
##' a request as a \code{data.frame}. The columns of the
##' \code{data.frame} can be specified, e.g. 'col1 + col2 ~
##' field:prefix'. To search prefixed terms it possible to add a
##' human-readable prefix; for example 'data ~ title:S' to enable
##' queries such as 'title:xapr'.
##' @import plyr
##' @include S4_classes.r
setGeneric("xsearch",
           signature = c("db", "query"),
           function(db,
                    query,
                    prefix   = NULL,
                    offset   = 0,
                    pagesize = 10,
                    wildcard = FALSE,
                    view     = FALSE)
           standardGeneric("xsearch"))

##' @rdname xsearch-methods
##' @export
setMethod("xsearch",
          signature(db    = "xapian_database",
                    query = "character"),
          function(db,
                   query,
                   prefix,
                   offset,
                   pagesize,
                   wildcard,
                   view)
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
                    db@path,
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
    } else if (length(result)) {
        rn <- sapply(result, "[[", "docid")
        result <- sapply(result, function(x) {
            fromJSON(x$data)
        })
        names(result) <- rn
    }

    result
})

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
