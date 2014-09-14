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
##' @param filename A character vector specifying the file(s) to
##' index.
##' @param language Either the English name for the language or the
##' two letter ISO639 code. Default is 'english'
##' @return NULL
##' @export
xapr_index <- function(path,
                       doc,
                       filename,
                       language = c(
                           "english", "en",
                           "none",
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

    .Call(
        "xapr_index",
        path,
        doc,
        filename,
        language,
        package = "xapr")
}
