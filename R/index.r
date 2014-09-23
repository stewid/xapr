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

##' Index plan
##'
##' Extract the index plan from a formula specification.
##' @param formula The index plan formula.
##' @return list with column names for data, text and prefixes.
##' @keywords internal
index_plan <- function(formula) {
    term_prefixes <- c("A" ,"D", "E", "G", "H", "I", "K", "L", "M",
                       "N", "O", "P", "Q", "R", "S", "T", "U", "V",
                       "X", "Y", "Z")

    ## Extract response variable
    response <- attr(terms(formula), "response")
    if (!response)
        stop("Invalid index formula")
    vars <- attr(terms(formula), "variables")[-1]
    data <- as.character(vars[response])

    ## Extract columns for free text indexing. Drop columns with a
    ## name equal to a term_prefix.
    text <- attr(terms(formula), "term.labels")
    text <- text[attr(terms(formula), "order") == 1]
    text <- text[!(text %in% term_prefixes)]

    ## Extract columns to prefix
    prefixes <- attr(terms(formula), "term.labels")
    prefixes <- prefixes[attr(terms(formula), "order") == 2]
    prefixes <- sapply(prefixes,
                       function(prefix) {
                           ## Make sure the first term is the prefix
                           prefix <- unlist(strsplit(prefix, ":"))
                           if (prefix[1] %in% term_prefixes)
                               return(paste0(prefix, collapse=":"))
                           if (prefix[2] %in% term_prefixes)
                               return(paste0(rev(prefix), collapse=":"))
                           stop("Invalid index formula")
                       })
    names(prefixes) <- NULL

    list(data     = data,
         text     = text,
         prefixes = prefixes)
}

##' Index
##'
##' Index the content of a \code{data.frame} with the Xapian search
##' engine.
##' @param formula A formula with a symbolic description of the index
##' plan for the columns in the data.frame. The details of the index
##' plan specification are given under ‘Details’.
##' @param path A character vector specifying the path to a Xapian
##' databases. If there is already a database in the specified
##' directory, it will be opened. If there isn't an existing database
##' in the specified directory, Xapian will try to create a new empty
##' database there.
##' @param language Either the English name for the language or the
##' two letter ISO639 code. Default is 'none'
##' @return NULL
##' @details The index plan for ‘xindex’ are specified
##' symbolically. An index plan has the form ‘data ~ terms’ where
##' ‘data’ is the blob of data returned from a request and ‘terms’ is
##' the basis for a search in Xapian. A first order term index the
##' text in the column as free text. A specification of the form
##' 'first:second' indicates that the text in 'second' should be
##' indexed with prefix 'first'. The prefix is a short string at the
##' beginning of the term to indicate which field the term
##' indexes. Valid prefixes are: 'A' ,'D', 'E', 'G', 'H', 'I', 'K',
##' 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'X', 'Y'
##' and 'Z'. See \url{http://xapian.org/docs/omega/termprefixes} for a
##' list of conventional prefixes. The specification ‘first*second’ is
##' the same as 'second + first:second’. The prefix 'X' will create a
##' user defined prefix by appending the uppercase 'second' to
##' 'X'. The prefix 'Q' will use data in the 'second' column as a
##' unique identifier for the document. NA values in columns to be
##' indexed are skipped.
##' @export
##' @examples
##' \dontrun{
##' library(jsonlite)
##'
##' ## Read data
##' filename <- system.file("extdata/NMSI_100.csv", package="xapr")
##' nmsi <- read.csv(filename, as.is = TRUE)
##'
##' ## Store all the fields for display purposes
##' nmsi$data <- sapply(seq_len(nrow(nmsi)), function(i) {
##'     as.character(toJSON(nmsi[i,]))
##' })
##'
##' ## Index the data to a temporary database
##' path <- tempfile(pattern="xapr-")
##' dir.create(path)
##' xindex(data ~ S*TITLE + X*DESCRIPTION + Q:id_NUMBER, nmsi, path)
##' }
xapr_index <- function(formula,
                       data,
                       path,
                       language = c(
                           "none",
                           "english", "en",
                           "danish", "da",
                           "dutch", "nl",
                           "english_lovins", "lovins",
                           "english_porter", "porter",
                           "finnish", "fi",
                           "french", "fr",
                           "german", "de", "german2",
                           "hungarian", "hu",
                           "italian", "it",
                           "kraaij_pohlmann",
                           "norwegian", "nb", "nn", "no",
                           "portuguese", "pt",
                           "romanian", "ro",
                           "russian", "ru",
                           "spanish", "es",
                           "swedish", "sv",
                           "turkish", "tr"))
{
    ## Check arguments
    if (missing(formula))
        stop("missing argument 'formula'")
    if (missing(data))
        stop("missing argument 'data'")
    if (missing(path))
        stop("missing argument 'path'")
    language <- match.arg(language)
    if (identical(language, "none"))
        language <- NULL

    ip <- index_plan(formula)

    ## .Call("xapr_index", data, path, language, package = "xapr")

    invisible(NULL)
}
