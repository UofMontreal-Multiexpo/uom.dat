
#### Utility functions for display ####

#' Decomposition of a time
#' 
#' Convert a period defined in seconds to a character string.
#' Examples of the format of the returned string: 1d 02h 25m 47s; 01m 05s; 19s.
#' 
#' @param t numeric. Time to convert, in seconds.
#' @return Character string corresponding to the converted time.
#' 
#' @references Stack Overflow topic:
#'  \href{https://stackoverflow.com/questions/27312292/convert-seconds-to-days-hoursminutesseconds}{Convert seconds to days:hours:minutes:seconds}
#' @keywords internal
dhms = function(t){
  t = round(t)
  d = t %/% (60*60*24)
  h = t %/% (60*60) %% 24
  m = t %/% 60 %% 60
  s = t %% 60
  
  to_return = paste0(formatC(s, width = 2, format = "d", flag = "0"), "s")
  if (t %/% 60 > 0) {
    to_return = paste0(formatC(m, width = 2, format = "d", flag = "0"), "m ", to_return)
    if (t %/% (60*60)) {
      to_return = paste0(formatC(h, width = 2, format = "d", flag = "0"), "h ", to_return)
      if (d > 0) to_return = paste0(d, "d ", to_return)
    }
  }
  
  return(to_return)
}


#' Print an execution time
#' 
#' Evaluate an expression and print the elapsed time according to the format defined by the
#'  \code{dhms} function.
#' Examples of the display format: [1d 02h 25m 47s]; [01m 05s]; [19s].
#' 
#' @details
#' A call to the garbage collector is made beforme timing.
#' 
#' @param expr Valid \code{R} expression to be timed.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{dhms}}, \code{\link{system.time}}.
#' @keywords internal
display_time = function(expr) {
  t = system.time(expr)
  cat(paste0("[", dhms(t[3]), "]"))
}


#' Capitalize a character string
#' 
#' Change the first letter of a string to an uppercase.
#' 
#' @param s Character string to be capitalized.
#' @return The character string corresponding to the argument \code{s}, starting with a capital letter.
#' 
#' @author Gauthier Magnin
#' @keywords internal
cap = function(s) {
  return(paste0(toupper(substring(s, 1, 1)), substring(s, 2)))
}


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


#' Turn a set notation into a vector notation
#' 
#' Convert itemsets written in mathematical notation into a list of character vectors.
#' Example of conversion: \code{{item i,item j}} becomes \code{c("item i", "item j")}.
#' 
#' @param sets Itemsets written in mathematical notation. Must be of vector type or of factor type.
#' @return List of character vectors corresponding to the itemsets \code{sets}.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{set_notation}}.
#' 
#' @examples
#' vector_notation(as.factor(c("{A}", "{B,C}", "{A,B}", "{A}")))
#' 
#' @export
vector_notation = function(sets) {
  return(strsplit(unname(sapply(as.character(sets),
                                function(x) substr(x, start = 2, stop = nchar(x) - 1))),
                  ","))
}


#' Turn a vector notation into a set notation
#' 
#' Convert a list of character vectors or a character vector into itemsets written in mathematical
#'  notation.
#' Example of conversion: \code{c("item i", "item j")} becomes \code{{item i,item j}}.
#' 
#' @param sets Itemsets to write in mathematical notation.
#' @param type Type of the return variable. One of \code{"character"}, \code{"factor"}.
#' @return Character vector or factor of the itemsets \code{sets} written in mathematical notation.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{vector_notation}}.
#' 
#' @examples
#' sets <- list("A", c("B", "C"), c("A", "B"), "A")
#' set_notation(sets, type = "character")
#' set_notation(sets, type = "factor")
#' 
#' @export
set_notation = function(sets, type = "character") {
  new_sets = paste0("{", lapply(sets, paste, collapse = ","), "}")
  
  if (type == "character") return(new_sets)
  if (type == "factor") return(as.factor(new_sets))
  stop("type must be \"character\" or \"factor\"")
}



#### Utility functions for plotting ####

#' Text shading
#' 
#' Draw text with an outline on a plot.
#' The outline is created by first drawing the text with the second color serveral times
#'  and with slight offsets.
#' 
#' @param x,y Coordinates where the text \code{labels} should be written.
#' @param labels Text to be written.
#' @param col Text color.
#' @param bg Outline color.
#' @param theta Angles to be used to create the outline.
#' @param r Outline size.
#' 
#' @references Greg Snow. Stack Overflow topic :
#'  \href{https://stackoverflow.com/questions/25631216/r-plots-is-there-any-way-to-draw-border-shadow-or-buffer-around-text-labels}{Any way to draw border, shadow or buffer around text labels}.
#' @keywords internal
shadowtext = function(x, y = NULL, labels, col = "black", bg = "white",
                      theta = seq(0, 2 * pi, length.out = 32), r = 0.1, ...) {
  
  xy = grDevices::xy.coords(x,y)
  xo = r * graphics::strwidth('A')
  yo = r * graphics::strheight('A')
  
  # Draw background text with small shift in x and y in background colour
  for (i in theta) {
    graphics::text(xy$x + cos(i)*xo, xy$y + sin(i)*yo, labels, col = bg, ...)
  }
  # Draw actual text in exact xy position in foreground colour
  graphics::text(xy$x, xy$y, labels, col = col, ...)
}



#### Utility functions for file management ####

#' Directory path
#' 
#' Check if a string can be used as a directory path.
#' Add a \code{"/"} character if it cannot.
#' 
#' @param path Character string to be used as a directory path.
#' @return Character string \code{path}, ending with \code{"/"}.
#' 
#' @author Gauthier Magnin
#' @keywords internal
turn_into_path = function(path) {
  if (substring(path, nchar(path)) != "/") return(paste0(path, "/"))
  return(path)
}


#' File extension
#' 
#' Check if a filename has the desired extension.
#' Add this extension if it has not.
#' 
#' @param filename Character string to be considered as a filename.
#' @param ext Desired extension.
#' @return Character string \code{filename}, ending with the extension \code{ext}.
#' 
#' @author Gauthier Magnin
#' @keywords internal
check_extension = function(filename, ext) {
  if (substr(ext, 1, 1) != ".") ext = paste0(".", ext)
  file_end = substring(filename, nchar(filename) - nchar(ext) + 1)
  if (file_end != ext && file_end != toupper(ext)) return(paste0(filename, ext))
  return(filename)
}



#### Utility functions to perform tests on data structures ####

#' Check if a data structure is named
#' 
#' Check if the elements of a data structure are named.
#' 
#' @param x A vector, matrix, data frame or list.
#' @return Logical value or logical vector of length 2.
#' * If `x` is a vector: `TRUE` or `FALSE` if the vector is named or not.
#' * If `x` is a matrix or a data frame: logical vector of length 2.
#'   The first value determines if the rows are named. The second one determines if the columns are named.
#' * If `x` is a list: logical vector of length 2.
#'   The first value determines if the list is named. The second one determines if the elements inside
#'   the list are named.
#' 
#' @author Gauthier Magnin
#' @md
#' @keywords internal
is.named = function(x) {
  
  if (is.list(x)) return(c(!is.null(names(x)), !is.null(names(unlist(unname(x))))))
  if (is.matrix(x) || is.data.frame(x)) return(c(!is.null(rownames(x)), !is.null(colnames(x))))
  if (is.vector(x)) return(!is.null(names(x)))
  
  stop("Unknown data structure.")
}



#### Utility functions for lists ####

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
#'           e5 = c("B", "D", "E", "I", "G"))
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
    if (with_zero) {
      return(t(sapply(indices, function(i) table(factor(unlist(list[i]), levels)) )))
    }
    return(lapply(indices, function(i) table(unlist(list[i]))))
  }
  
  # Cas d'un unique vecteur d'indices
  if (is.vector(indices)) {
    if (with_zero) return(table(factor(unlist(list[indices]), levels)))
    return(table(unlist(list[indices])))
  }
  
  # Cas d'une matrice d'indices
  if (is.matrix(indices)) {
    if (with_zero) {
      return(t(apply(indices, 1, function(set) table(factor(unlist(list[set]), levels)) )))
    }
    return(apply(indices, 1, function(set) table(unlist(list[set]))))
  }
  
  stop("indices must be a vector, a matrix or a list.")
}


#' Turn a list into a logical matrix
#' 
#' Turn a list into a logical matrix using the values of the list or the names of these values.
#' 
#' @param x List to turn into a logical matrix.
#' @param by_name  If `FALSE`, use the values of the elements of the list. If `TRUE`, use the names of
#'  these values.
#' @return Logical matrix in which row names are the names of the list and column names are the values
#'  found in the elements of the list or the names of these values (according to `by_name`).
#' 
#' @author Gauthier Magnin
#' @seealso [`turn_logical_matrix_into_list`], [`invert_list`].
#' 
#' @examples
#' l <- list(e1 = c(v1 = "E", v2 = "F", v3 = "I"),
#'           e2 = c(v1 = "I"),
#'           e3 = c(v1 = "C", v2 = "I"),
#'           e4 = c(v1 = "A", v2 = "C", v3 = "D", v4 = "F"),
#'           e5 = c(v1 = "B", v2 = "D", v3 = "E", v4 = "I", v5 = "G"))
#' 
#' turn_list_into_logical_matrix(l, by_name = FALSE)
#' turn_list_into_logical_matrix(l, by_name = TRUE)
#' 
#' @md
#' @export
turn_list_into_logical_matrix = function(x, by_name = FALSE) {
  if (by_name) {
    if (!is.named(x)[2]) stop("Values of the elements of x must be named.")
    columns = unique(unlist(sapply(x, names)))
    y = t(sapply(x, function(row) columns %in% names(row)))
    
  } else {
    columns = sort(unique(unlist(x)))
    y = t(sapply(x, function(row) columns %in% row))
  }
  
  colnames(y) = columns
  return(y)
}


#' Turn a logical matrix into a list
#' 
#' Turn a logical matrix into a named list using the names of the columns and rows of the matrix.
#' 
#' @param x Logical matrix to turn into a list.
#' @param by_row If `TRUE`, use the names of the rows as names of the list. If `FALSE`, use the names of
#'  the columns as names of the list.
#' @return List in which the values correspond to the `TRUE` values of each row or of each column
#'  (according to `by_row`).
#' 
#' @author Gauthier Magnin
#' @seealso [`turn_logical_matrix_into_list`], [`invert_list`].
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
#' turn_logical_matrix_into_list(m, by_row = TRUE)
#' turn_logical_matrix_into_list(m, by_row = FALSE)
#' 
#' @md
#' @export
turn_logical_matrix_into_list = function(x, by_row = TRUE) {
  if (by_row && !is.named(x)[2]) stop("Columns of x must be named.")
  if (!by_row && !is.named(x)[1]) stop("Rows of x must be named.")
  
  if (by_row) return(apply(x, 1, function(row) names(row)[row]))
  return(apply(x, 2, function(column) names(column)[column]))
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
#' @seealso [`turn_list_into_logical_matrix`], [`turn_logical_matrix_into_list`].
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
  if (!is.named(x)[1]) stop("x must be a named list.")
  if (by_name && !is.named(x)[2]) stop("Values of the elements of x must be named.")
  
  return(apply(turn_list_into_logical_matrix(x, by_name), 2, function(n) names(x)[n]))
}


