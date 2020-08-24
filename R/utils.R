
#### Fonctions utiles à l'affichage ####

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
#' @keywords internal
cap = function(s) {
  return(paste0(toupper(substring(s, 1, 1)), substring(s, 2)))
}


#' Turn a set notation into a vector notation
#' 
#' Convert itemsets written in mathematical notation into a list of character vectors.
#' Example of conversion: \code{{item i, item j}} becomes \code{c("item i", "item j")}.
#' 
#' @param sets Itemsets written in mathematical notation. Must be of vector type or of factor type.
#' @return List of character vectors corresponding to the itemsets \code{sets}.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{set_notation}}.
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
#' Example of conversion: \code{c("item i", "item j")} becomes \code{{item i, item j}}.
#' 
#' @param sets Itemsets to write in mathematical notation.
#' @param type Type of the return variable. One of \code{"character"}, \code{"factor"}.
#' @return Character vector or factor of the itemsets \code{sets} written in mathematical notation.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{vector_notation}}.
#' @export
set_notation = function(sets, type = "character") {
  new_sets = paste0("{", lapply(sets, paste, collapse = ","), "}")
  
  if (type == "character") return(new_sets)
  if (type == "factor") return(as.factor(new_sets))
  stop("type must be \"character\" or \"factor\"")
}



#### Fonctions utiles au traçage de graphiques ####

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



#### Fonctions utiles à la gestion de fichiers ####

#' Directory path
#' 
#' Check if a string can be used as a directory path.
#' Add a \code{"/"} character if it cannot.
#' 
#' @param path Character string to be used as a directory path.
#' @return Character string \code{path}, ending with \code{"/"}.
#' 
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
#' @keywords internal
check_extension = function(filename, ext) {
  if (substr(ext, 1, 1) != ".") ext = paste0(".", ext)
  file_end = substring(filename, nchar(filename) - nchar(ext) + 1)
  if (file_end != ext && file_end != toupper(ext)) return(paste0(filename, ext))
  return(filename)
}


