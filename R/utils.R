
#### Utility functions for display ####

#' Decomposition of a time
#' 
#' Convert a period defined in seconds to a character string.
#' Examples of the format of the returned string: 1d 02h 25m 47s; 01m 05s; 19s.
#' 
#' @template function_not_exported
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
#' @template function_not_exported
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
#' @template function_not_exported
#' 
#' @param x Character string to be capitalized.
#' @return The character string corresponding to the argument `x`, starting with a capital letter.
#' 
#' @author Gauthier Magnin
#' @seealso [`pluralize`], [`first_characters`], [`substr2`].
#' 
#' @md
#' @keywords internal
cap = function(x) {
  return(paste0(toupper(substring(x, 1, 1)), substring(x, 2)))
}


#' Pluzalize a character string
#' 
#' Give the plural form of the string if the given variable is numeric and is greater than \eqn{1} or
#'  has a length greater than \eqn{1}. Add an "s" at the end of the string or turn "y" into "ies".
#' 
#' @template function_not_exported
#' 
#' @param x Character string to be pluralized.
#' @param var Variable to consider whether to pluralize the string.
#' @return The character string corresponding to the argument `x`, possibly in its plural form.
#' 
#' @author Gauthier Magnin
#' @seealso [`cap`], [`first_characters`], [`substr2`].
#' 
#' @md
#' @keywords internal
pluralize = function(x, var) {
  if (is.numeric(var) && var > 1 || length(var) > 1) {
    if (substring(x, nchar(x)) == "y") return(paste0(substr(x, 1, nchar(x)-1), "ies"))
    return(paste0(x, "s"))
  }
  return(x)
}


#' Find the first characters
#' 
#' Search for the first character of each word and concatenate them.
#' 
#' @template function_not_exported
#' 
#' @param x Character string (one or more) from which the first characters are to be extracted.
#' @param sep Character string each of which is a word separator.
#' @return Character vector of size equal to that of `x` containing the first concatenated characters
#'  of all words.
#' 
#' @author Gauthier Magnin
#' @seealso [`cap`], [`pluralize`], [`substr2`].
#' 
#' @md
#' @keywords internal
first_characters = function(x, sep = " _-") {
  return(sapply(strsplit(x, split = paste0("[", sep, "]")),
                function(s) {
                  paste0(substr(s, start = 1, stop = 1), collapse = "")
                }))
}


#' Substrings of a character vector
#' 
#' Extract substrings in a character vector. Wrapper of the [`substr`] function allowing to use
#'  `NULL` value.
#' 
#' @template function_not_exported
#' 
#' @param x A character vector.
#' @param start Indice of the first character to extract from each element of `x`.
#' @param stop Indice of the last character to extract from each element of `x`.
#'  If `NULL`, all characters from `start` to the end of the element are extracted.
#' @return A character vector of the same length as `x` containing the extracted parts.
#' 
#' @author Gauthier Magnin
#' @seealso [`cap`], [`pluralize`], [`first_characters`].
#' 
#' @md
#' @keywords internal
substr2 = function(x, start = 1, stop = NULL) {
  if (is.null(stop)) {
    if (start == 1) return(x)
    return(substr(x, start, nchar(x)))
  } 
  return(substr(x, start, stop))
}


#' Turn into data frame
#' 
#' Turn a table, a matrix or a vector into a data frame.
#' 
#' @details
#' If `x` is a vector, the output is a one-line data frame.
#' Otherwise, the output has the same dimensions as `x`.
#' 
#' @template function_not_exported
#' 
#' @param x Table, matrix or vector to turn into a data frame.
#' @return Data frame corresponding to the input variable.
#' 
#' @author Gauthier Magnin
#' @md
#' @keywords internal
turn_into_data_frame = function(x) {
  
  if (is.table(x)) {
    df_x = as.data.frame(matrix(x, ncol = ncol(x)))
    rownames(df_x) = rownames(x)
    colnames(df_x) = colnames(x)
    
  } else if (is.matrix(x)) {
    df_x = as.data.frame(x)
    rownames(df_x) = rownames(x)
    colnames(df_x) = colnames(x)
    
  } else if (is.vector(x)) {
    df_x = as.data.frame(t(x))
  }
  
  return(df_x)
}


#' Turn a set notation into a vector notation
#' 
#' Convert itemsets written in mathematical notation into a list of character vectors.
#' Example of conversion: \code{{item i,item j}} becomes \code{c("item i", "item j")}.
#' 
#' @param sets Itemsets written in mathematical notation. Must be of character vector type or of
#'  factor type.
#' @return List of character vectors corresponding to the itemsets \code{sets}.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{set_notation}}.
#' 
#' @examples
#' vector_notation(c("{A}", "{B,C}", "{A,B}", "{A}"))
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



#### Utility functions for file management ####

#' Directory path
#' 
#' Check if a string can be used as a directory path.
#' Add a \code{"/"} character if it cannot.
#' 
#' @template function_not_exported
#' 
#' @param path Character string to be used as a directory path.
#' @return Character string \code{path}, ending with \code{"/"}.
#' 
#' @author Gauthier Magnin
#' @keywords internal
turn_into_path = function(path) {
  if (is.null(path)) return("./")
  if (substring(path, nchar(path)) != "/") return(paste0(path, "/"))
  return(path)
}


#' File extension
#' 
#' Check if a filename has the desired extension.
#' Add this extension if it has not.
#' 
#' @template function_not_exported
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


#' Resave data with the best compression method
#' 
#' Search for the best compression method to save existing '.RData' or '.rda' files and resave them
#'  with this method.
#' 
#' @details
#' Use of the maximum compression level (9).
#' 
#' @template function_not_exported
#' 
#' @param paths A character vector of paths to found data and save files.
#' 
#' @author Gauthier Magnin
#' @seealso [`tools::resaveRdaFiles`], [`tools::checkRdaFiles`].
#' @md
#' @keywords internal
resave_with_best_compression = function(paths){
  
  # Vérification de l'existence du package tools (inclus dans DESCRIPTION.Suggests)
  if (!requireNamespace("tools", quietly = TRUE)) {
    stop("Package \"tools\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  
  # Méthodes de compression
  methods = c("gzip", "bzip2", "xz")
  
  # Pour chaque fichier
  for (p in paths) {
    
    # Pour chaque méthode de compression
    sizes = sapply(methods, function(m) {
      # Compresse les données et retourne la taille du fichier
      tools::resaveRdaFiles(p, compress = m, compression_level = 9)
      return(tools::checkRdaFiles(p)$size)
    })
    names(sizes) = methods
    
    # Sélection de la meilleure méthode de compression
    best = methods[which.min(sizes)]
    
    # Affichage des résultats et du choix optimal
    if (length(paths) != 1) cat("File:", p,"\n")
    cat("File sizes according to compression method:\n")
    print(sizes)
    cat("Use of '", best, "' compression method.\n", sep = "")
    if (p != paths[length(paths)]) cat("\n")
    
    # Recompression selon la meilleur méthode
    tools::resaveRdaFiles(p, compress = best, compression_level = 9)
  }
}  



#### Utility functions to perform tests on variables and conditions ####

#' Check if a data structure is named
#' 
#' Check if the elements of a data structure are named.
#' 
#' @template function_not_exported
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
is_named = function(x) {
  
  if (is.list(x)) return(c(!is.null(names(x)), !is.null(names(unlist(unname(x))))))
  if (is.matrix(x) || is.data.frame(x)) return(c(!is.null(rownames(x)), !is.null(colnames(x))))
  if (is.vector(x)) return(!is.null(names(x)))
  
  stop("Unknown data structure.")
}


#' Validation of a parameter
#' 
#' Check that a parameter is of a specific type, is member of a set of values or is in a range of values.
#' Stop the execution and print an error message if not.
#' The expected type, range or possible values are detailed in the error message.
#' 
#' @details
#' Related validation is ignored if `types`, `values` or `range` is `NULL`.
#' 
#' Parameter type is checked using the [`base::mode`] function.
#' 
#' @template function_not_exported
#' 
#' @param x Parameter to validate.
#' @param types Types among which `x` must be.
#' @param values Values among which `x` must be.
#' @param range Two-element numeric vector. Range in which `x` must be. Ignored if `values` is not `NULL`.
#' @param quotes If `TRUE`, surround in quotes the possible values in the error message.
#' @param stop If `TRUE`, stop the execution and print an error message if the parameter is not valid.
#'  If `FALSE`, see 'Value' section.
#' @param prefix Text to be prefixed to the message.
#' @param suffix Text to be suffixed to the message.
#' @return `TRUE` or `FALSE` whether the parameter is valid.
#' 
#' @author Gauthier Magnin
#' @md
#' @keywords internal
check_param = function(x, types = NULL, values = NULL, range = NULL,
                       quotes = TRUE, stop = TRUE,
                       prefix = "", suffix = "") {
  
  # Nom de la variable dans l'appel de fonction
  var_name = deparse(substitute(x))
  
  # Vérification du type associé à la variable
  if (!is.null(types) && !is.element(mode(x), types)) {
    if (length(types) == 1) {
      error_msg = paste0("of type ", types)
    } else if (length(types) == 2) {
      error_msg = paste0("of type ", types[1], " or ", types[2])
    } else {
      error_msg = paste0("one of types ", paste(types, collapse = ", "))
    }
    if (stop) stop(prefix, var_name, " must be ", error_msg, suffix, ".")
    return(FALSE)
  }
  
  # Vérification de l'appartenance à un ensemble de valeurs
  if (!is.null(values) && !is.element(x, values)) {
    if (length(values) == 2) {
      if (!quotes) error_msg = paste0(values[1], " or ", values[2])
      else error_msg = paste0("\"", values[1], "\" or \"", values[2], "\"")
    } else {
      if (!quotes) error_msg = paste0("one of ", paste(values, collapse = ", "))
      else error_msg = paste0("one of ", paste0("\"", values, "\"", collapse = ", "))
    }
  }
  # Vérification de l'appartenance à une étendue
  else if (!is.null(range) && (x < range[1] || x > range[2])) {
    error_msg = paste0("in range [", range[1], ",", range[2], "]")
  }
  
  if (exists("error_msg", inherits = FALSE)) {
    if (stop) stop(prefix, var_name, " must be ", error_msg, suffix, ".")
    return(FALSE)
  }
  return(TRUE)
}


#' Improved 'if' statement
#' 
#' Perform a comparison between two elements and return the resulting logical value or evaluate
#'  an expression.
#' 
#' @template function_not_exported
#' 
#' @param x First element to be compared.
#' @param operator Relational operator to use to compare `x` and `y`.
#'  One of `"=="`, `"!="`, `"<"`, `">"`, `"<="`, `">="`.
#' @param y Second element to be compared.
#' @param expr Expression to evaluate if the condition is met.
#' @param alt.expr Expression to evaluate if the condition is not met.
#' @return
#'  If the condition expressed by `x`, `operator` and `y` is met and `expr` is not `NULL`:
#'  result of the evaluation of `expr`.
#'  
#'  If the condition is not met and `alt.expr` is not `NULL`:
#'  result of the evaluation of `alt.expr`.
#'  
#'  Logical value resulting of the comparison otherwise.
#' 
#' @author Gauthier Magnin
#' @seealso [`base::expression`], [`base::if`].
#' 
#' @md
#' @keywords internal
if_2.0 = function(x, operator, y, expr = NULL, alt.expr = NULL) {
  
  check_param(operator, values = c("==", "!=", "<", ">", "<=", ">="))
  check_param(expr, types = c("expression", "NULL"))
  check_param(alt.expr, types = c("expression", "NULL"))
  
  if (eval(parse(text = paste(x, operator, y)))) {
    if (is.null(expr)) return(TRUE)
    eval(expr, envir = parent.frame(n = 1))
  } else {
    if (is.null(alt.expr)) return(FALSE)
    eval(alt.expr, envir = parent.frame(n = 1))
  }
}


