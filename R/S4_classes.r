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

##' Class \code{"xapian_database"}
##'
##' @title  S4 class to handle a xapian database
##' @section Slots:
##' \describe{
##'   \item{path}{
##'     Path to a Xapian database.
##'   }
##' }
##' @rdname xapian_database-class
##' @docType class
##' @keywords classes
##' @keywords methods
##' @export
setClass("xapian_database",
         slots = c(path = "character"),
         validity = function(object) {
             errors <- character()

             if (length(object@path) < 1)
                 errors <- c(errors, "No path to Xapian a database")
             if (any(sapply(object@path, is.na)))
                 errors <- c(errors, "'NA' not a valid path to a Xapian database")
             if (any(sapply(object@path, is.null)))
                 errors <- c(errors, "'NULL' not a valid path to a Xapian database")
             if (length(errors) == 0) TRUE else errors
         }
)
