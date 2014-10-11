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

##' Term Prefixes
##'
##' See \url{http://xapian.org/docs/omega/termprefixes}
##' @return Character vector with valid term prefixes
##' @keywords internal
term_prefixes <- function() {
    c("A" ,"D", "E", "G", "H", "I", "K", "L", "M", "N", "O", "P", "Q",
      "R", "S", "T", "U", "V", "X", "Y", "Z")
}
