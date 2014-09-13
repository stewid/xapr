/*
 * xapr, R bindings to the Xapian search engine.
 * Copyright (C) 2014 Stefan Widgren
 *
 *  xapr is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  xapr is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with this program; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <xapian.h>
#include <Rinternals.h>

using namespace Xapian;
using namespace std;

const char xapr_err_integer_gte_zero_arg[] =
    "Error in '%s': '%s' must be an integer vector of length one with "
    "a value greater than or equal to zero";

/**
 * Check integer argument and that arg is greater than or equal to 0.
 *
 * @param arg the arg to check
 * @return true if OK, else false
 */
bool xapr_arg_check_integer(SEXP arg)
{
    if (R_NilValue == arg
        || !isInteger(arg)
        || 1 != length(arg)
        || NA_INTEGER == INTEGER(arg)[0]
        || 0 > INTEGER(arg)[0])
        return false;
    return true;
}

/**
 * Raise error
 *
 * @param format C string that contains the text to be written to
 * Rf_error
 * @param func_name The name of the function that raise the error
 * @param arg Optional text argument
 */
void xapr_error(const char *format, const char *func_name, const char *arg)
{
    if (arg)
        Rf_error(format, func_name, arg);
    else
        Rf_error(format, func_name);
}

/*
 * Search a Xapian database
 *
 * @param path A character vector specifying the path to one or more
 * Xapian databases.
 * @param terms Search terms
 * @param offset Starting point within result set
 * @param pagesize Number of records to retrieve
 * @return list with search result
 */
extern "C" SEXP
xapr_search(SEXP path, SEXP terms, SEXP offset, SEXP pagesize)
{
    SEXP result = R_NilValue;
    vector<string> queryterms;
    Database databases;

    if (!xapr_arg_check_integer(offset))
        xapr_error(xapr_err_integer_gte_zero_arg, "xapr_search", "offset");
    if (!xapr_arg_check_integer(pagesize))
        xapr_error(xapr_err_integer_gte_zero_arg, "xapr_search", "pagesize");

    size_t n = length(path);
    for (size_t i = 0; i < n; ++i)
        databases.add_database(Database(CHAR(STRING_ELT(path, i))));

    n = length(terms);
    for (size_t i = 0; i < n; ++i)
        queryterms.push_back(CHAR(STRING_ELT(terms, i)));

    Enquire enquire(databases);
    Query query(Query::OP_OR, queryterms.begin(), queryterms.end());
    enquire.set_query(query);

    size_t _offset = INTEGER(offset)[0];
    size_t _pagesize = INTEGER(pagesize)[0];
    MSet matches = enquire.get_mset(_offset, _pagesize);
    MSetIterator i;
    PROTECT(result = allocVector(VECSXP, matches.size()));
    for (i = matches.begin(); i != matches.end(); ++i) {
        const size_t n_items = 3;
        SEXP item, names;
        size_t j = 0;

        PROTECT(item = allocVector(VECSXP, n_items));
        PROTECT(names = allocVector(STRSXP, n_items));

        SET_STRING_ELT(names, j, mkChar("docid"));
        SET_VECTOR_ELT(item, j++, ScalarInteger(*i));

        SET_STRING_ELT(names, j, mkChar("rank"));
        SET_VECTOR_ELT(item, j++, ScalarInteger(i.get_rank()));

        SET_STRING_ELT(names, j, mkChar("percent"));
        SET_VECTOR_ELT(item, j++, ScalarInteger(i.get_percent()));

        setAttrib(item, R_NamesSymbol, names);
        SET_VECTOR_ELT(result, i.get_rank() - _offset, item);
        UNPROTECT(2);
    }

    if (R_NilValue != result)
        UNPROTECT(1);

    return result;
}
