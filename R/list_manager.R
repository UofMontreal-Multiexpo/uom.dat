#' @include utils.R
NULL


#### Functions for search in lists ####

#' Extraction of first, last or \out{n<sup>th</sup>} values from a list
#' 
#' Extract the first, last or \out{n<sup>th</sup>} values of the vectors of a list.
#' 
#' @param x List of vectors whose `n`\out{<sup>th</sup>} values are to be extracted.
#' @param n Position of the elements to extract in each vector of `x`. Negative values start from the
#'  end.\cr
#'  `"first"` and `"last"` are special values for the first and last values of each vector.
#' @return Vector of the values at positions `n`.
#' 
#' @author Gauthier Magnin
#' 
#' @examples
#' l <- list(c(1), c(1,2), c(1,2,3))
#' nth_values(l, "first")
#' nth_values(l, "last")
#' nth_values(l, 2)
#' nth_values(l, -2)
#' 
#' @md
#' @export
nth_values = function(x, n) {
  if (n == "last") n = -1
  else if (n == "first") n = 1
  
  if (n > 0) return(sapply(x, function(v) v[n]))
  return(sapply(x, function(v) {
    index = length(v) + (n + 1)
    if (index > 0) return(v[index])
    return(NA)
  }))
}



#### Functions of computation on lists ####

#' Set operations on list
#' 
#' Perform an intersection or union operation on the elements of a list.
#' 
#' @param list List on which to perform the operation.
#' @param indices Indices or names of the elements on which to perform the operation.
#' @return Vector of values corresponding to the intersection or the union of all specified elements
#'  of the list.
#' 
#' @author Gauthier Magnin
#' @seealso [`table_on_list`].
#' 
#' @examples
#' l <- list("e1" = c("A", "B", "C"),
#'           "e2" = "B",
#'           "e3" = c("B", "C", "D"))
#' 
#' intersect_on_list(l)
#' intersect_on_list(l, indices = c(1, 3))
#' 
#' union_on_list(l)
#' union_on_list(l, indices = c("e1", "e2"))
#' 
#' @md
#' @name set_operations_on_list
NULL

#' @rdname set_operations_on_list
#' @aliases intersect_on_list
#' @export
intersect_on_list = function(list, indices = seq_along(list)) {
  set = list[[indices[1]]]
  for (i in indices[seq(2, length(indices))]) set = intersect(list[[i]], set)
  return(sort(set))
}

#' @rdname set_operations_on_list
#' @aliases union_on_list
#' @export
union_on_list = function(list, indices = seq_along(list)) {
  return(sort(unique(unlist(list[indices]))))
}


#' Frequency table on list
#' 
#' Build a frequency table of the counts of each value in a list.
#' 
#' @param list List for which to build the frequency table.
#' @param indices Indices or names of the elements on which to perform the operation. Can be a vector,
#'  a matrix or a list.
#' @param with_zero If `FALSE`, count only occurrences of effective values. If `TRUE`, add
#'  the frequencies of the values which are in `list` but not in the elements specified by `indices`.
#' @return Table, matrix or list of tables (according to `indices`) of the frequencies of the values of
#'  the specified elements of the list.
#' 
#' @author Gauthier Magnin
#' @seealso [`intersect_on_list`], [`union_on_list`].
#' 
#' @examples
#' l <- list(e1 = c("E", "F", "I"),
#'           e2 = "I",
#'           e3 = c("C", "I"),
#'           e4 = c("A", "C", "D", "F"),
#'           e5 = c("B", "D", "E", "G", "I"))
#' 
#' ## With indices as a vector
#' table_on_list(l)
#' table_on_list(l, indices = c(1,2,3))
#' table_on_list(l, indices = c("e1", "e2", "e3"), with_zero = TRUE)
#' 
#' ## With indices as a matrix
#' table_on_list(l, indices = matrix(c("e1", "e2", "e3",
#'                                     "e1", "e2", "e4",
#'                                     "e2", "e3", "e5"),
#'                                   nrow = 3, byrow = TRUE))
#' table_on_list(l, indices = matrix(c(1, 2, 3,
#'                                     1, 2, 4,
#'                                     2, 3, 5),
#'                                   nrow = 3, byrow = TRUE,
#'                                   dimnames = list(c("v1", "v2", "v3"))),
#'               with_zero = TRUE)
#' 
#' ## With indices as a list
#' table_on_list(l, indices = list(c("e1", "e2", "e3"),
#'                                 c("e1", "e2"),
#'                                 "e1"))
#' table_on_list(l, indices = list(v1 = c(1, 2, 3),
#'                                 v2 = c(1, 2),
#'                                 v3 = 1),
#'               with_zero = TRUE)
#' 
#' @md
#' @export
table_on_list = function(list, indices = seq_along(list), with_zero = FALSE) {
  
  # Ensemble des valeurs existantes
  if (with_zero) levels = sort(unique(unlist(list)))
  
  # Cas d'une liste d'indices
  if (is.list(indices)) {
    if (with_zero) return(t(sapply(indices, function(i) table(factor(unlist(list[i]), levels)) )))
    return(lapply(indices, function(i) table(unlist(list[i]), dnn = NULL) ))
  }
  
  # Cas d'un unique vecteur d'indices
  if (is.vector(indices)) {
    if (with_zero) return(table(factor(unlist(list[indices]), levels), dnn = NULL))
    return(table(unlist(list[indices]), dnn = NULL))
  }
  
  # Cas d'une matrice d'indices
  if (is.matrix(indices)) {
    if (with_zero) return(t(apply(indices, 1, function(set) table(factor(unlist(list[set]), levels)) )))
    return(apply(indices, 1, function(set) table(unlist(list[set]), dnn = NULL) ))
  }
  
  stop("indices must be a vector, a matrix or a list.")
}



#### Functions for transformation of lists ####

#' Turn a list into a character vector
#' 
#' Each element of the list is turned into a character value so the list is turned into a character vector.
#' 
#' @param x List to turn into a character vector.
#' @return Character vector corresponding to the input list.
#' 
#' @author Gauthier Magnin
#' @keywords internal
turn_list_into_char = function(x) {
  
  # Conversion des éléments de la liste en chaînes de caractères
  x = as.character(x)
  
  # Suppression des caractères "c()" liés aux vecteurs
  y = ifelse(substring(x, 1, 1) == "c",
             substr(x, start = 3, stop = nchar(x) - 1),
             x)
  
  # Suppression des guillemets liés aux vecteurs
  return(gsub("\"", "", y))
}


#' Inversion of list
#' 
#' Invert a list. Set the names of the list as the new values and set the values of the list as the
#'  new names.
#' 
#' @param x List to invert.
#' @param by_name  If `FALSE`, use the values of the elements of the list. If `TRUE`, use the names of
#'  these values.
#' @return List in which the values are the names of `x` and the names are the values found in the
#'  elements of `x` or the names of these values (according to `by_name`).
#' 
#' @author Gauthier Magnin
#' @seealso [`coerce_list`], [`coerce_to_list`].
#' 
#' @examples
#' l <- list(e1 = c(v1 = "E", v2 = "F", v3 = "I"),
#'           e2 = c(v1 = "I"),
#'           e3 = c(v1 = "C", v2 = "I"),
#'           e4 = c(v1 = "A", v2 = "C", v3 = "D", v4 = "F"),
#'           e5 = c(v1 = "B", v2 = "D", v3 = "E", v4 = "I", v5 = "G"))
#' 
#' invert_list(l, by_name = FALSE)
#' invert_list(l, by_name = TRUE)
#' 
#' @md
#' @export
invert_list = function(x, by_name = FALSE) {
  if (!is_named(x)[1]) stop("x must be a named list.")
  if (by_name && !is_named(x)[2]) stop("Values of the elements of x must be named.")
  
  return(apply(turn_list_into_logical_matrix(x, by_name), 2, function(n) names(x)[n]))
}


#' Turn a list into a matrix or a data frame
#' 
#' Turn a list into a logical matrix, a character matrix or a data frame, associating names of the list
#'  with its values or with the names of these values.
#' 
#' @param x List of vectors to turn into a matrix or a data frame.
#' @param to Data structure into which to transform the list. One of `"logical matrix"` (or `"lm"` for
#'  short), `"character matrix"` (or `"cm"`), `"data.frame"` (or `"df"`).
#' @param by_name If `FALSE`, use the values of the list. If `TRUE`, use the names of these values.
#' @param stringsAsFactors logical: should the character vectors be converted to factors?
#'  Ignored if `to` doesn't refer to data frame.
#' @return 
#' If `to` refers to logical matrix, logical matrix in which row names are the names of the list and
#'  column names are the values of the list or the names of these values (according to `by_name`).
#' 
#' Otherwise, character matrix or data frame (according to `to`) in which the first column contains the
#'  names of the list and the second one contains the related values or the names of these values
#'  (according to `by_name`).
#' 
#' @author Gauthier Magnin
#' @seealso [`invert_list`], [`coerce_to_list`].
#' 
#' @examples
#' l <- list(e1 = c(v1 = "E", v2 = "F", v3 = "I"),
#'           e2 = c(v1 = "I"),
#'           e3 = c(v1 = "C", v2 = "I"),
#'           e4 = c(v1 = "A", v2 = "C", v3 = "D", v4 = "F"),
#'           e5 = c(v1 = "B", v2 = "D", v3 = "E", v4 = "I", v5 = "G"))
#' 
#' coerce_list(l, "logical matrix")
#' coerce_list(l, "lm", by_name = TRUE)
#' 
#' coerce_list(l, "character matrix")
#' coerce_list(l, "cm", by_name = TRUE)
#' 
#' coerce_list(l, "data.frame")
#' coerce_list(l, "df", by_name = TRUE)
#' 
#' @md
#' @export
coerce_list = function(x, to, by_name = FALSE,
                       stringsAsFactors = default.stringsAsFactors()) {
  
  # Unification des termes possibles
  if (to == "logical matrix") to = "lm"
  else if (to == "character matrix") to = "cm"
  else if (to == "data.frame") to = "df"
  
  # Appel de la fonction adéquate
  if (to == "lm") return(turn_list_into_logical_matrix(x, by_name))
  else if (to == "cm") return(turn_list_into_char_matrix(x, by_name))
  else if (to == "df") return(turn_list_into_data_frame(x, by_name, stringsAsFactors))
  else stop("to must be one of \"logical matrix\", \"character matrix\", \"data.frame\".")
}


#' Turn a list into a logical matrix
#' 
#' Turn a list into a logical matrix associating names of the list with its values or with the names of
#'  these values.
#' 
#' @param x List of vectors to turn into a logical matrix.
#' @param by_name If `FALSE`, use the values of the list. If `TRUE`, use the names of these values.
#' @return Logical matrix in which row names are the names of the list and column names are the values
#'  of the list or the names of these values (according to `by_name`).
#' 
#' @author Gauthier Magnin
#' @seealso [`coerce_list`], [`turn_list_into_char_matrix`], [`turn_list_into_data_frame`],
#'          [`turn_logical_matrix_into_list`].
#' 
#' @examples
#' l <- list(e1 = c(v1 = "E", v2 = "F", v3 = "I"),
#'           e2 = c(v1 = "I"),
#'           e3 = c(v1 = "C", v2 = "I"),
#'           e4 = c(v1 = "A", v2 = "C", v3 = "D", v4 = "F"),
#'           e5 = c(v1 = "B", v2 = "D", v3 = "E", v4 = "I", v5 = "G"))
#' 
#' turn_list_into_logical_matrix(l)
#' turn_list_into_logical_matrix(l, by_name = TRUE)
#' 
#' @md
#' @keywords internal
turn_list_into_logical_matrix = function(x, by_name = FALSE) {
  if (by_name) {
    if (!is_named(x)[2]) stop("Values of the elements of x must be named.")
    columns = sort(unique(unlist(sapply(x, names))))
    y = t(sapply(x, function(row) columns %in% names(row)))
    
  } else {
    columns = sort(unique(unlist(x)))
    y = t(sapply(x, function(row) columns %in% row))
  }
  
  colnames(y) = columns
  return(y)
}


#' Turn a list into a char matrix
#' 
#' Turn a list into a character matrix associating names of the list with its values or with the names of
#'  these values.
#' 
#' @param x List of vectors to turn into a matrix.
#' @param by_name If `FALSE`, use the values of the list. If `TRUE`, use the names of these values.
#' @param inline If `TRUE` associations are made in line: one association per line. If `FALSE`:
#'  associations are made in column: one association per column.
#' @return Character matrix in which the first column (or row if `inline = FALSE`) contains the names of
#'  the list and the second one contains the related values or the names of these values (according to
#'  `by_name`).
#' 
#' @author Gauthier Magnin
#' @seealso [`coerce_list`], [`turn_list_into_logical_matrix`], [`turn_list_into_data_frame`],
#'          [`turn_char_matrix_into_list`].
#' 
#' @examples
#' l <- list(e1 = c(v1 = "E", v2 = "F", v3 = "I"),
#'           e2 = c(v1 = "I"),
#'           e3 = c(v1 = "C", v2 = "I"),
#'           e4 = c(v1 = "A", v2 = "C", v3 = "D", v4 = "F"),
#'           e5 = c(v1 = "B", v2 = "D", v3 = "E", v4 = "I", v5 = "G"))
#' 
#' turn_list_into_char_matrix(l)
#' turn_list_into_char_matrix(l, by_name = TRUE)
#' turn_list_into_char_matrix(l, inline = FALSE)
#' 
#' @md
#' @keywords internal
turn_list_into_char_matrix = function(x, by_name = FALSE, inline = TRUE) {
  
  if (by_name && !is_named(x)[2]) stop("Values of the elements of x must be named.")
  
  FUN = if (inline) cbind else rbind
  l_names = if (is.null(names(x))) seq_along(x) else names(x)
  
  return(FUN(rep.int(l_names, sapply(x, length)),
             if (by_name) unname(unlist(lapply(x, names))) else unname(unlist(x))))
}


#' Turn a list into a data frame
#' 
#' Turn a list into a data frame associating names of the list with its values or with the names of
#'  these values.
#' 
#' @param x List of vectors to turn into a data frame.
#' @param by_name If `FALSE`, use the values of the list. If `TRUE`, use the names of these values.
#' @inheritParams base::as.data.frame
#' @return Data frame in which the first column contains the names of the list and the second one
#'  contains the related values or the names of these values (according to `by_name`).
#' 
#' @author Gauthier Magnin
#' @seealso [`coerce_list`], [`turn_list_into_logical_matrix`], [`turn_list_into_char_matrix`],
#'          [`turn_data_frame_into_list`].
#' 
#' @examples
#' l <- list(e1 = c(v1 = "E", v2 = "F", v3 = "I"),
#'           e2 = c(v1 = "I"),
#'           e3 = c(v1 = "C", v2 = "I"),
#'           e4 = c(v1 = "A", v2 = "C", v3 = "D", v4 = "F"),
#'           e5 = c(v1 = "B", v2 = "D", v3 = "E", v4 = "I", v5 = "G"))
#' 
#' turn_list_into_data_frame(l)
#' turn_list_into_data_frame(l, by_name = TRUE)
#' 
#' @md
#' @keywords internal
turn_list_into_data_frame = function(x, by_name = FALSE,
                                     stringsAsFactors = default.stringsAsFactors()) {
  
  if (by_name && !is_named(x)[2]) stop("Values of the elements of x must be named.")
  
  l_names = if (is.null(names(x))) seq_along(x) else names(x)
  
  return(as.data.frame(cbind(rep.int(l_names, sapply(x, length)),
                             if (by_name) unname(unlist(lapply(x, names))) else unname(unlist(x))),
                       stringsAsFactors = stringsAsFactors))
}



#### Functions for transformation into lists ####

#' Turn a matrix or a data frame into a list
#' 
#' Turn a logical matrix, a two-column character matrix or a two-column data frame into a list.
#' 
#' @param x Matrix or data frame to turn into a list.
#' @param by_row Only considered if `x` is a logical matrix.
#'  If `TRUE`, use row names as names of the list and column names as values.
#'  If `FALSE`, use column names as names of the list and row names as values.
#' @return 
#' If `x` is a logical matrix, list whose values correspond to the values `TRUE` of each row or of each
#'  column (according to `by_row`).
#' 
#' Otherwise, list whose names correspond to the first column of `x` and whose values correspond to the
#'  second column of `x`.
#' 
#' @author Gauthier Magnin
#' @seealso [`invert_list`], [`coerce_list`].
#' 
#' @examples
#' m1 <- matrix(c(FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE,  FALSE, TRUE,
#'               FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,
#'               FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, TRUE,
#'               TRUE,  FALSE, TRUE,  TRUE,  FALSE, TRUE,  FALSE, FALSE,
#'               FALSE, TRUE,  FALSE, TRUE,  TRUE,  FALSE, TRUE,  TRUE),
#'             nrow = 5, byrow = TRUE,
#'             dimnames = list(c("e1", "e2", "e3", "e4", "e5"),
#'                             c("A", "B", "C", "D", "E", "F", "G", "I")))
#' 
#' coerce_to_list(m1)
#' coerce_to_list(m1, by_row = FALSE)
#' 
#' 
#' m2 <- matrix(c("e1", "e1", "e1", "e2", "e3", "e3", "e4",
#'                "e4", "e4", "e4", "e5", "e5", "e5", "e5", "e5",
#'                "E", "F", "I", "I", "C", "I", "A",
#'                "C", "D", "F", "B", "D", "E", "G", "I"),
#'              ncol = 2)
#' 
#' coerce_to_list(m2)
#' 
#' 
#' df <- data.frame(V1 = c("e1", "e1", "e1", "e2", "e3", "e3", "e4",
#'                         "e4", "e4", "e4", "e5", "e5", "e5", "e5", "e5"),
#'                  V2 = c("E", "F", "I", "I", "C", "I", "A",
#'                         "C", "D", "F", "B", "D", "E", "G", "I"),
#'                  stringsAsFactors = FALSE)
#' 
#' coerce_to_list(df)
#' 
#' @md
#' @export
coerce_to_list = function(x, by_row = TRUE) {
  
  if (is.matrix(x)) {
    if (is.logical(x)) turn_logical_matrix_into_list(x, by_row)
    else turn_char_matrix_into_list(x)
    
  }
  else if (is.data.frame(x)) turn_data_frame_into_list(x)
  else stop("x must be a matrix or a data frame.")
}


#' Turn a logical matrix into a list
#' 
#' Turn a logical matrix into a named list using the names of the columns and rows of the matrix.
#' 
#' @param x Logical matrix to turn into a list.
#' @param by_row If `TRUE`, use the names of the rows as names of the list. If `FALSE`, use the names of
#'  the columns as names of the list.
#' @return List whose values correspond to the values `TRUE` of each row or of each column (according to
#'  `by_row`).
#' 
#' @author Gauthier Magnin
#' @seealso [`coerce_to_list`], [`turn_char_matrix_into_list`], [`turn_data_frame_into_list`],
#'          [`turn_list_into_logical_matrix`].
#' 
#' @examples
#' m <- matrix(c(FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE,  FALSE, TRUE,
#'               FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,
#'               FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, TRUE,
#'               TRUE,  FALSE, TRUE,  TRUE,  FALSE, TRUE,  FALSE, FALSE,
#'               FALSE, TRUE,  FALSE, TRUE,  TRUE,  FALSE, TRUE,  TRUE),
#'             nrow = 5, byrow = TRUE,
#'             dimnames = list(c("e1", "e2", "e3", "e4", "e5"),
#'                             c("A", "B", "C", "D", "E", "F", "G", "I")))
#' 
#' turn_logical_matrix_into_list(m)
#' turn_logical_matrix_into_list(m, by_row = FALSE)
#' 
#' @md
#' @keywords internal
turn_logical_matrix_into_list = function(x, by_row = TRUE) {
  if (by_row && !is_named(x)[2]) stop("Columns of x must be named.")
  if (!by_row && !is_named(x)[1]) stop("Rows of x must be named.")
  
  if (by_row) return(apply(x, 1, function(row) names(row)[row]))
  return(apply(x, 2, function(column) names(column)[column]))
}


#' Turn a char matrix into a list
#' 
#' Turn two columns or two lines of a chararacter matrix into a named list.
#' 
#' @param x Character matrix to turn into a list.
#' @param indices Two-element vector indicating the names of the associated rows or columns (according
#'  to `inline`) to use to create the list 
#' @param inline `TRUE` if associations in `x` are made in line (i.e. there is one association per line).
#'  `FALSE` if associations are made in column (i.e. there is one association per column).
#' @return List whose names correspond to the column (or row if `inline = FALSE`) of `x`
#'  corresponding to the first element of `indices` and whose values correspond to the column (row if
#'  `inline = FALSE`) of `x` corresponding to the second element of `indices`.
#' 
#' @author Gauthier Magnin
#' @seealso [`coerce_to_list`], [`turn_logical_matrix_into_list`], [`turn_data_frame_into_list`],
#'          [`turn_list_into_char_matrix`].
#' 
#' @examples
#' m1 <- matrix(c("e1", "e1", "e1", "e2", "e3", "e3", "e4",
#'                "e4", "e4", "e4", "e5", "e5", "e5", "e5", "e5",
#'                "E", "F", "I", "I", "C", "I", "A",
#'                "C", "D", "F", "B", "D", "E", "G", "I"),
#'              ncol = 2)
#' 
#' turn_char_matrix_into_list(m1)
#' 
#' m2 <- matrix(c("e1", "e1", "e1", "e2", "e3", "e3", "e4",
#'                "e4", "e4", "e4", "e5", "e5", "e5", "e5", "e5",
#'                "E", "F", "I", "I", "C", "I", "A",
#'                "C", "D", "F", "B", "D", "E", "G", "I"),
#'              nrow = 2, byrow = TRUE)
#' 
#' turn_char_matrix_into_list(m2, inline = FALSE)
#' 
#' @md
#' @keywords internal
turn_char_matrix_into_list = function(x, indices = c(1, 2), inline = TRUE) {
  
  if (inline && ncol(x) < 2) stop("x must have at least 2 columns.")
  if (!inline && nrow(x) < 2) stop("x must have at least 2 lines.")
  if (length(indices) != 2) stop("indices must have a length of 2.")
  
  if (!inline) return(sapply(unique(x[indices[1], ]),
                             function(name) unname(x[indices[2], x[indices[1],] == name])))
  
  return(sapply(unique(x[, indices[1]]),
                function(name) unname(x[x[, indices[1]] == name, indices[2]])))
}


#' Turn a data frame into a list
#' 
#' Turn two columns of a data frame into a list.
#' 
#' @param x Data frame to turn into a list.
#' @param indices Two-element vector indicating the names of the associated columns to use to create
#'  the list.
#' @return List whose names correspond to the column of `x` corresponding to the first element
#'  of `indices` and whose values correspond to the column of `x` corresponding to the second element of
#'  `indices`.
#' 
#' @author Gauthier Magnin
#' @seealso [`coerce_to_list`], [`turn_logical_matrix_into_list`], [`turn_char_matrix_into_list`],
#'          [`turn_list_into_data_frame`].
#' 
#' @examples
#' df1 <- data.frame(V1 = c("e1", "e1", "e1", "e2", "e3", "e3", "e4", "e4",
#'                          "e4", "e4", "e5", "e5", "e5", "e5", "e5"),
#'                   V2 = c("E", "F", "I", "I", "C", "I", "A", "C",
#'                          "D", "F", "B", "D", "E", "G", "I"),
#'                   stringsAsFactors = FALSE)
#' 
#' turn_data_frame_into_list(df1)
#' 
#' df2 <- data.frame(V1 = c(1, 1, 1, 2, 3, 3, 4, 4,
#'                          4, 4, 5, 5, 5, 5, 5),
#'                   V2 = c("E", "F", "I", "I", "C", "I", "A", "C",
#'                          "D", "F", "B", "D", "E", "G", "I"),
#'                   stringsAsFactors = FALSE)
#' 
#' turn_data_frame_into_list(df2)
#' 
#' @md
#' @keywords internal
turn_data_frame_into_list = function(x, indices = c(1, 2)) {
  
  if (ncol(x) < 2) stop("x must have at least 2 columns.")
  if (length(indices) != 2) stop("indices must have a length of 2.")
  
  to_return = sapply(unique(x[, indices[1]]),
                     function(name) unname(x[x[, indices[1]] == name, indices[2]]))
  
  # Les noms ne sont pas définis automatiquement dans le cas où la colonne 1 est factor
  if (is.factor(x[, indices[1]])) names(to_return) = unique(x[, indices[1]])
  return(to_return)
}


