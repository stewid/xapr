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

##' Brief summary of Xapian database
##'
##' @aliases show,xapian_database-methods
##' @docType methods
##' @param object The \code{\linkS4class{xapian_database}} object.
##' @return None (invisible 'NULL').
##' @keywords methods
##' @include S4_classes.r
##' @export
setMethod("show",
          signature(object = "xapian_database"),
          function(object)
          {
              cat("Xapian database: ", object@path, "\n")
          }
)

##' Summary of a Xapian database
##'
##' @aliases summary,xapian_database-methods
##' @docType methods
##' @param object The \code{\linkS4class{xapian_database}} object.
##' @return None (invisible 'NULL').
##' @keywords methods
##' @include S4_classes.r
##' @export
setMethod("summary",
          signature(object = "xapian_database"),
          function(object, ...)
          {
              show(object)

              cat("\n")

              result <- .Call("xapr_summary",
                              object@path,
                              package = "xapr")

              cat("UUID =", result[["UUID"]], "\n")
              cat("number of documents =", result[["doccount"]], "\n")
              cat("average document length =", result[["avlength"]], "\n")
              cat("document length lower bound =", result[["doclength_lower_bound"]], "\n")
              cat("document length upper bound =", result[["doclength_upper_bound"]], "\n")
              cat("highest document id ever used =", result[["lastdocid"]], "\n")
              cat("has positional information =", result[["has_positions"]], "\n")

              cat("\n")
          }
)
