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

#include <sstream>
#include <xapian.h>
#include <Rinternals.h>

using namespace Xapian;

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
 * Summary of a Xapian database
 *
 * @param path A character vector specifying the path to one or more
 * Xapian databases.
 * @return list with summary result
 */
extern "C" SEXP
xapr_summary(SEXP path)
{
    size_t i = 0;
    SEXP result = R_NilValue;
    SEXP names  = R_NilValue;
    Database db;

    size_t n = length(path);
    for (size_t i = 0; i < n; ++i)
        db.add_database(Database(CHAR(STRING_ELT(path, i))));

    n = 7;
    PROTECT(result = allocVector(VECSXP, n));
    PROTECT(names = allocVector(STRSXP, n));
    SET_VECTOR_ELT(result, i,   ScalarString(mkChar(db.get_uuid().c_str())));
    SET_STRING_ELT(names,  i++, mkChar("UUID"));
    SET_VECTOR_ELT(result, i,   ScalarInteger(db.get_doccount()));
    SET_STRING_ELT(names,  i++, mkChar("doccount"));
    SET_VECTOR_ELT(result, i,   ScalarInteger(db.get_avlength()));
    SET_STRING_ELT(names,  i++, mkChar("avlength"));
    SET_VECTOR_ELT(result, i,   ScalarInteger(db.get_doclength_lower_bound()));
    SET_STRING_ELT(names,  i++, mkChar("doclength_lower_bound"));
    SET_VECTOR_ELT(result, i,   ScalarInteger(db.get_doclength_upper_bound()));
    SET_STRING_ELT(names,  i++, mkChar("doclength_upper_bound"));
    SET_VECTOR_ELT(result, i,   ScalarInteger(db.get_lastdocid()));
    SET_STRING_ELT(names,  i++, mkChar("lastdocid"));
    SET_VECTOR_ELT(result, i,   ScalarLogical(db.has_positions()));
    SET_STRING_ELT(names,  i++, mkChar("has_positions"));

    setAttrib(result, R_NamesSymbol, names);
    UNPROTECT(2);

    return result;
}

/*
 * Search a Xapian database
 *
 * @param query_string A free-text query
 * @param path A character vector specifying the path to one or more
 * Xapian databases.
 * @param field A character vector with field of the prefix
 * @param prefix A character vector with prefix for field.
 * @param offset Starting point within result set
 * @param pagesize Number of records to retrieve
 * @param wildcard Support trailing wildcard searches.
 * @return list with search result
 */
extern "C" SEXP
xapr_search(
    SEXP query_string,
    SEXP path,
    SEXP field,
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
        n_prefix = Rf_length(prefix);
    for (size_t i = 0; i < n_prefix; ++i) {
        SEXP field_item = STRING_ELT(field, i);
        if (NA_STRING != field_item) {
            SEXP prefix_item = STRING_ELT(prefix, i);
            if (NA_STRING != prefix_item)
                qp.add_prefix(CHAR(field_item), CHAR(prefix_item));
        }
    }

    Query query = qp.parse_query(CHAR(STRING_ELT(query_string, 0)), flags);
    enquire.set_query(query);

    size_t _offset = INTEGER(offset)[0];
    size_t _pagesize = INTEGER(pagesize)[0];
    MSet matches = enquire.get_mset(_offset, _pagesize);

    if (matches.size()) {
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
 * Index specified content of a data.frame with the Xapian search engine.
 * @param path A character vector specifying the path to a Xapian databases.
 * @param df The \code{data.frame} to index.
 * @param nrow Number of rows in the data.frame to index.
 * @param data The content to add to the document.
 * @param text The column(s) in 'df' with content to index. Note: 1
 * based column index.
 * @param prefix_lbl A character vector with the prefix to use for
 * content in prefix_col.
 * @param prefix_col The column(s) in 'df' with content to index with
 * the prefix_lbl. Note: 1 based column index.
 * @param prefix_wdf The The wdf increment to use for the content in
 * prefix_col.
 * @param id Optional identifier of the document.
 * @param language Either the English name for the language or the two
 * letter ISO639 code.
 * @return R_NilValue
 */
extern "C" SEXP
xapr_index(
    SEXP path,
    SEXP df,
    SEXP nrow,
    SEXP data,
    SEXP text,
    SEXP prefix_lbl,
    SEXP prefix_col,
    SEXP prefix_wdf,
    SEXP id,
    SEXP language)
{
    if (R_NilValue != language)
        Rf_error("Stemmer not implemented. Sorry\n");

    // Open the database for update, creating a new database if necessary.
    WritableDatabase db(CHAR(STRING_ELT(path, 0)), DB_CREATE_OR_OPEN);

    TermGenerator indexer;
    for (size_t row = 0, n_row = INTEGER(nrow)[0]; row < n_row; ++row) {
        Document document;

        if (R_NilValue == data) {
            std::ostringstream buf;
            buf << (row + 1); // Give the 1-based row number in the data.frame
            document.set_data(buf.str());
        } else {
            document.set_data(CHAR(STRING_ELT(data, row)));
        }
        indexer.set_document(document);

        // Iterate over prefix columns and index non NA values with
        // prefix label as prefix.
        for (size_t i = 0, n_prefix = Rf_length(prefix_lbl); i < n_prefix; ++i) {
            SEXP value = STRING_ELT(VECTOR_ELT(df, INTEGER(prefix_col)[i] - 1), row);
            if (NA_STRING != value) {
                indexer.index_text(
                    CHAR(value),
                    INTEGER(prefix_wdf)[i],
                    CHAR(STRING_ELT(prefix_lbl, i)));
            }
        }

        for (size_t i = 0, n_text = Rf_length(text); i < n_text; ++i) {
            SEXP value = STRING_ELT(VECTOR_ELT(df, INTEGER(text)[i] - 1), row);
            if (NA_STRING != value) {
                indexer.index_text(CHAR(value));
                indexer.increase_termpos();
            }
        }

        if (R_NilValue == id) {
            db.add_document(document);
        } else {
            SEXP value = STRING_ELT(VECTOR_ELT(df, INTEGER(id)[0] - 1), row);
            document.add_boolean_term(CHAR(value));
            db.replace_document(CHAR(value), document);
        }
    }

    db.commit();

    return R_NilValue;
}
