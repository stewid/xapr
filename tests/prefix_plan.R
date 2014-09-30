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

library(xapr)

##
## Test case 1
##
sp_1_exp <- structure(list(field = c("author", "title"),
                           prefix = c("A", "S")),
                      .Names = c("field", "prefix"))
sp_1_obs <- xapr:::search_plan(~author:A+title:S)
str(sp_1_obs)
stopifnot(identical(sp_1_obs, sp_1_exp))

##
## Test case 2
##
sp_2_exp <- structure(list(field = "description",
                           prefix = "XDESCRIPTION"),
                      .Names = c("field", "prefix"))
sp_2_obs <- xapr:::search_plan(~description:XDESCRIPTION)
str(sp_2_obs)
stopifnot(identical(sp_2_obs, sp_2_exp))
