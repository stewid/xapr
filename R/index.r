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

##' Index a Xapian database
##'
##' @param path A character vector specifying the path to a Xapian
##' databases. If there is already a database in the specified
##' directory, it will be opened. If there isn't an existing database
##' in the specified directory, Xapian will try to create a new empty
##' database there.
##' @param doc A character vector with data stored in the document.
##' @param terms A \code{data.frame} with text to index with a
##' prefix. The prefix is a short string at the beginning of the term
##' to indicate which field the term indexes. See
##' \url{http://xapian.org/docs/omega/termprefixes} for a list of
##' conventional prefixes. The prefixes are the names of the
##' variables.
##' @param content A character vector with text to index.
##' @param id Optional identifier of the document.
##' @param language Either the English name for the language or the
##' two letter ISO639 code. Default is 'none'
##' @return NULL
##' @export
xapr_index <- function(path,
                       doc,
                       terms,
                       content,
                       id = NULL,
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
    language <- match.arg(language)
    if (identical(language, "none"))
        language <- NULL

    if (missing(terms))
        terms <- NULL
    if (!is.null(terms)) {
        if (!is.data.frame(terms))
            stop("'terms' must be a data.frame")
        colnames(terms) <- toupper(colnames(terms))
        for (i in seq_len(ncol(terms))) {
            if (is.factor(terms[,i]))
                terms[,i] <- as.character(terms[,i])
            if (!is.character(terms[,i])) {
                stop(paste0("'terms$",
                            colnames(terms)[i],
                            "' must be a character"))
            }
        }
    }

    .Call(
        "xapr_index",
        path,
        doc,
        terms,
        content,
        id,
        language,
        package = "xapr")

    invisible(NULL)
}
