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

cols <- c("id_NUMBER", "ITEM_NAME", "TITLE", "MAKER", "DATE_MADE",
          "PLACE_MADE", "MATERIALS", "MEASUREMENTS", "DESCRIPTION",
          "WHOLE_PART", "COLLECTION", "data")

##
## Test case 1
##
ip_1_exp <- structure(list(data = 12L, text = c(3L, 9L),
                           prefix = structure(list(lbl = c("S", "XDESCRIPTION"),
                               col = c(3L, 9L), wdf = c(1L, 1L)),
                               .Names = c("lbl", "col", "wdf")), id = 1L),
                      .Names = c("data", "text", "prefix", "id"))
ip_1_obs <- xapr:::index_plan(data ~ S*TITLE + X*DESCRIPTION + Q:id_NUMBER, cols)
str(ip_1_obs)
stopifnot(identical(ip_1_obs, ip_1_exp))

##
## Test case 2
##
ip_2_exp <- structure(list(data = NULL, text = 1:12,
                           prefix = structure(list(lbl = character(0),
                               col = integer(0), wdf = integer(0)),
                               .Names = c("lbl", "col", "wdf")), id = NULL),
                      .Names = c("data", "text", "prefix", "id"))
ip_2_obs <- xapr:::index_plan(~., cols)
str(ip_2_obs)
stopifnot(identical(ip_2_obs, ip_2_exp))

##
## Test case 3
##
ip_3_exp <- structure(list(data = NULL, text = 1:12,
                           prefix = structure(list(lbl = character(0),
                               col = integer(0), wdf = integer(0)),
                               .Names = c("lbl", "col", "wdf")), id = NULL),
                      .Names = c("data", "text", "prefix", "id"))
ip_3_obs <- xapr:::index_plan(~.+data, cols)
str(ip_3_obs)
stopifnot(identical(ip_3_obs, ip_3_exp))

##
## Test case 4
##
ip_4_exp <- structure(list(data = NULL, text = 1:12,
                           prefix = structure(list(lbl = character(0),
                               col = integer(0), wdf = integer(0)),
                               .Names = c("lbl", "col", "wdf")), id = NULL),
                      .Names = c("data", "text", "prefix", "id"))
ip_4_obs <- xapr:::index_plan(~data+., cols)
str(ip_4_obs)
stopifnot(identical(ip_4_obs, ip_4_exp))

##
## Test case 5
##
ip_5_exp <- structure(list(data = NULL, text = 1:12,
                           prefix = structure(list(lbl = character(0),
                               col = integer(0), wdf = integer(0)),
                               .Names = c("lbl", "col", "wdf")), id = 1L),
                      .Names = c("data", "text", "prefix", "id"))
ip_5_obs <- xapr:::index_plan(~Q:id_NUMBER+., cols)
str(ip_5_obs)
stopifnot(identical(ip_5_obs, ip_5_exp))

##
## Test case 6
##
ip_6_exp <- structure(list(data = NULL, text = 1:12, prefix = structure(list(
                               lbl = c("XID_NUMBER", "XITEM_NAME", "XTITLE",
                                   "XMAKER", "XDATE_MADE", "XPLACE_MADE",
                                   "XMATERIALS", "XMEASUREMENTS", "XDESCRIPTION",
                                   "XWHOLE_PART", "XCOLLECTION", "XDATA"),
                               col = 1:12, wdf = c(1L, 1L, 1L, 1L, 1L, 1L, 1L,
                               1L, 1L, 1L, 1L, 1L)),
                               .Names = c("lbl", "col", "wdf")), id = NULL),
                      .Names = c("data", "text", "prefix", "id"))
ip_6_obs <- xapr:::index_plan(~X*., cols)
str(ip_6_obs)
stopifnot(identical(ip_6_obs, ip_6_exp))

##
## Test case 7
##
ip_7_exp <- structure(list(data = NULL, text = integer(0),
                           prefix = structure(list(lbl = c("XID_NUMBER",
                           "XITEM_NAME", "XTITLE", "XMAKER", "XDATE_MADE",
                           "XPLACE_MADE", "XMATERIALS", "XMEASUREMENTS",
                           "XDESCRIPTION", "XWHOLE_PART", "XCOLLECTION",
                           "XDATA"), col = 1:12, wdf = c(1L, 1L, 1L, 1L, 1L,
                           1L, 1L, 1L, 1L, 1L, 1L, 1L)), .Names = c("lbl",
                           "col", "wdf")), id = NULL),
                      .Names = c("data", "text", "prefix", "id"))
ip_7_obs <- xapr:::index_plan(~X:., cols)
str(ip_7_obs)
stopifnot(identical(ip_7_obs, ip_7_exp))

##
## Test case 8
##
ip_8_exp <- structure(list(data = NULL, text = integer(0),
                           prefix = structure(list(lbl = c("XID_NUMBER",
                           "XITEM_NAME", "XTITLE", "XMAKER", "XDATE_MADE",
                           "XPLACE_MADE", "XMATERIALS", "XMEASUREMENTS",
                           "XDESCRIPTION", "XWHOLE_PART", "XCOLLECTION",
                           "XDATA"), col = 1:12, wdf = c(1L, 1L, 1L, 1L, 1L,
                           1L, 1L, 1L, 1L, 1L, 1L, 1L)), .Names = c("lbl",
                           "col", "wdf")), id = NULL),
                      .Names = c("data", "text", "prefix", "id"))
ip_8_obs <- xapr:::index_plan(~X:. + X:id_NUMBER, cols)
str(ip_8_obs)
stopifnot(identical(ip_8_obs, ip_8_exp))

##
## Test case 9
##
ip_9_exp <- structure(list(data = NULL, text = 1:12,
                           prefix = structure(list(lbl = c("XID_NUMBER",
                           "XITEM_NAME", "XTITLE", "XMAKER", "XDATE_MADE",
                           "XPLACE_MADE", "XMATERIALS", "XMEASUREMENTS",
                           "XDESCRIPTION", "XWHOLE_PART", "XCOLLECTION",
                           "XDATA"), col = 1:12, wdf = c(1L, 1L, 1L, 1L, 1L,
                           1L, 1L, 1L, 1L, 1L, 1L, 1L)), .Names = c("lbl",
                           "col", "wdf")), id = NULL),
                      .Names = c("data", "text", "prefix", "id"))
ip_9_obs <- xapr:::index_plan(~X*. + X:id_NUMBER, cols)
str(ip_9_obs)
stopifnot(identical(ip_9_obs, ip_9_exp))

##
## Test case 10
##
ip_10_exp <- structure(list(data = 1:12, text = 1:12, prefix = structure(
                       list(lbl = character(0), col = integer(0),
                       wdf = integer(0)), .Names = c("lbl", "col", "wdf")),
                       id = NULL), .Names = c("data", "text", "prefix", "id"))
ip_10_obs <- xapr:::index_plan(.~., cols)
str(ip_10_obs)
stopifnot(identical(ip_10_obs, ip_10_exp))

##
## Test case 11
##
ip_11_exp <- structure(list(data = c(3L, 9L), text = c(3L, 9L), prefix = structure(
                       list(lbl = c("S", "XDESCRIPTION"), col = c(3L, 9L),
                       wdf = c(1L, 1L)), .Names = c("lbl", "col", "wdf")), id = 1L),
                       .Names = c("data", "text", "prefix", "id"))
ip_11_obs <-
    xapr:::index_plan(TITLE + DESCRIPTION ~ S*TITLE + X*DESCRIPTION + Q:id_NUMBER,
                      cols)
str(ip_11_obs)
stopifnot(identical(ip_11_obs, ip_11_exp))

##
## Test case 12
##
ip_12_exp <- structure(list(data = 1:12, text = c(3L, 9L), prefix = structure(
                       list(lbl = c("S", "XDESCRIPTION"), col = c(3L, 9L),
                       wdf = c(1L, 1L)), .Names = c("lbl", "col", "wdf")), id = 1L),
                       .Names = c("data", "text", "prefix", "id"))
ip_12_obs <-
    xapr:::index_plan(TITLE + DESCRIPTION + . ~ S*TITLE + X*DESCRIPTION + Q:id_NUMBER,
                      cols)
str(ip_12_obs)
stopifnot(identical(ip_12_obs, ip_12_exp))
