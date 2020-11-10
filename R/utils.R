
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


#' Turn into data frame
#' 
#' Turn a table, a matrix or a vector into a data frame.
#' 
#' @details
#' If `x` is a vector, the output is a one-line data frame.
#' Otherwise, the output has the same dimensions as `x`.
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


#' Resave data with the best compression method
#' 
#' Search for the best compression method to save existing '.RData' or '.rda' files and resave them
#'  with this method.
#' 
#' @details
#' Use of the maximum compression level (9).
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


