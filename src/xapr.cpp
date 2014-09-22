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
 * @param query_string A free-text query
 * @param prefix A data.frame with term prefixes. First column field
 * and second column prefix.
 * @param offset Starting point within result set
 * @param pagesize Number of records to retrieve
 * @param wildcard Support trailing wildcard searches.
 * @return list with search result
 */
extern "C" SEXP
xapr_search(
    SEXP path,
    SEXP query_string,
    SEXP prefix,
    SEXP offset,
    SEXP pagesize,
    SEXP wildcard)
{
    SEXP result = R_NilValue;
    Database databases;

    if (!xapr_arg_check_integer(offset))
        xapr_error(xapr_err_integer_gte_zero_arg, "xapr_search", "offset");
    if (!xapr_arg_check_integer(pagesize))
        xapr_error(xapr_err_integer_gte_zero_arg, "xapr_search", "pagesize");

    size_t n = length(path);
    for (size_t i = 0; i < n; ++i)
        databases.add_database(Database(CHAR(STRING_ELT(path, i))));

    Enquire enquire(databases);
    QueryParser qp = QueryParser();
    qp.set_database(databases);
    unsigned flags = QueryParser::FLAG_DEFAULT;
    if (LOGICAL(wildcard)[0])
        flags |= QueryParser::FLAG_WILDCARD;

    // Check if there are any prefix
    size_t n_prefix = 0;
    if (R_NilValue != prefix)
        n_prefix = Rf_length(getAttrib(prefix, R_RowNamesSymbol));
    for (size_t i = 0; i < n_prefix; ++i) {
        SEXP field_item = STRING_ELT(VECTOR_ELT(prefix, 0), i);
        if (NA_STRING != field_item) {
            SEXP prefix_item = STRING_ELT(VECTOR_ELT(prefix, 1), i);
            if (NA_STRING != prefix_item)
                qp.add_prefix(CHAR(field_item), CHAR(prefix_item));
        }
    }

    Query query = qp.parse_query(CHAR(STRING_ELT(query_string, 0)), flags);
    enquire.set_query(query);

    size_t _offset = INTEGER(offset)[0];
    size_t _pagesize = INTEGER(pagesize)[0];
    MSet matches = enquire.get_mset(_offset, _pagesize);
    PROTECT(result = allocVector(VECSXP, matches.size()));
    for (MSetIterator i = matches.begin(); i != matches.end(); ++i) {
        const size_t n_items = 4;
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

        SET_STRING_ELT(names, j, mkChar("data"));
        SET_VECTOR_ELT(
            item,
            j++,
            ScalarString(mkChar(i.get_document().get_data().c_str())));

        setAttrib(item, R_NamesSymbol, names);
        setAttrib(item, R_ClassSymbol, mkString("xapian_match"));
        SET_VECTOR_ELT(result, i.get_rank() - _offset, item);
        UNPROTECT(2);
    }

    if (R_NilValue != result) {
        setAttrib(result, R_ClassSymbol, mkString("xapian_search"));
        UNPROTECT(1);
    }

    return result;
}

/**
 * Index a Xapian database
 *
 * @param path A character vector specifying the path to a Xapian databases.
 * @param doc A character vector with data stored in the document.
 * @param terms A \code{data.frame} with text to index with a
 * prefix. The prefix is a short string at the beginning of the term
 * to indicate which field the term indexes. See
 * \url{http://xapian.org/docs/omega/termprefixes} for a list of
 * conventional prefixes. The prefixes are the names of the variables.
 * @param content A character vector with text to index.
 * @param id Optional identifier of the document.
 * @param language Either the English name for the language or the two
 * letter ISO639 code.
 * @return R_NilValue
 */
extern "C" SEXP
xapr_index(
    SEXP path,
    SEXP doc,
    SEXP terms,
    SEXP content,
    SEXP id,
    SEXP language)
{
    if (R_NilValue != language)
        Rf_error("Stemmer not implemented. Sorry\n");

    // Open the database for update, creating a new database if necessary.
    WritableDatabase db(CHAR(STRING_ELT(path, 0)), DB_CREATE_OR_OPEN);

    TermGenerator indexer;

    // Check if there are any terms
    size_t columns = 0;
    if (R_NilValue != terms)
        columns = Rf_length(terms);

    size_t n = length(content);
    for (size_t i = 0; i < n; ++i) {
        Document document;
        document.set_data(CHAR(STRING_ELT(doc, i)));

        indexer.set_document(document);

        // Iterate of columns and index non NA terms with column name
        // as prefix.
        for (size_t j = 0; j < columns; ++j) {
            SEXP value = STRING_ELT(VECTOR_ELT(terms, j), i);
            if (NA_STRING != value) {
                string prefix =
                    CHAR(STRING_ELT(getAttrib(terms, R_NamesSymbol), j));

                indexer.index_text(CHAR(value), 1, prefix);
            }
        }

        if (NA_STRING != STRING_ELT(content, i))
            indexer.index_text(CHAR(STRING_ELT(content, i)));

        if (R_NilValue == id) {
            // Add the document to the database.
            db.add_document(document);
        } else {
            document.add_boolean_term(CHAR(STRING_ELT(id, i)));
            db.replace_document(CHAR(STRING_ELT(id, i)), document);
        }
    }

    db.commit();

    return R_NilValue;
}
