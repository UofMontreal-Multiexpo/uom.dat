
#### Fonctions utiles à l'affichage ####

#' Décomposition d'un temps
#' 
#' Convertit en une chaîne de caractères une période définie en secondes.
#' Exemples du format de la chaîne retournée : 1d 02h 25m 47s ; 01m 05s ; 19s.
#' 
#' @param t Numeric. Temps à convertir, en secondes.
#' @return Chaîne de caractère correspondant au temps converti.
#' 
#' @references Stack Overflow topic :
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


#' Affiche un temps d'exécution
#' 
#' Évalue une expression et affiche le temps écoulé selon le format défini par la fonction \code{dhms}.
#' Exemples du format de la chaîne retournée : [1d 02h 25m 47s] ; [01m 05s] ; [19s].
#' 
#' @details
#' Un appel au garbage collector est effectué avant le chronométrage.
#' 
#' @param expr Expression R valide à chronométrer.
#' 
#' @seealso \code{\link{dhms}}.
#' @keywords internal
display_time = function(expr) {
  t = system.time(expr)
  cat(paste0("[", dhms(t[3]), "]"))
}


#' Capitalise une chaîne de caractères
#' 
#' Change la première lettre d'une chaîne de caractères en une majuscule.
#' 
#' @param s Chaîne de caractères à modifier.
#' @return La chaîne de caractères fournie par l'argument \code{s}, commençant par une majuscule.
#' 
#' @keywords internal
cap = function(s) {
  return(paste0(toupper(substring(s, 1, 1)), substring(s, 2)))
}


#' Ombrage de texte
#' 
#' Affiche du texte en y ajoutant un contour d'une autre couleur.
#' Le contour est créé en affichant d'abord le texte avec la seconde couleur, plusieurs fois
#'  et avec de légers décalages.
#' 
#' @param x,y Coordonnées du texte à afficher.
#' @param labels Texte à afficher.
#' @param col Couleur du texte.
#' @param bg Couleur du contour.
#' @param theta Angles à utiliser pour créer le contour.
#' @param r Taille du contour.
#' 
#' @references Greg Snow. Stack Overflow topic :
#'  \href{https://stackoverflow.com/questions/25631216/r-plots-is-there-any-way-to-draw-border-shadow-or-buffer-around-text-labels}{Any way to draw border, shadow or buffer around text labels}.
#' @keywords internal
shadowtext = function(x, y = NULL, labels, col = "black", bg = "white",
                       theta = seq(pi/4, 2*pi, length.out = 16), r = 0.3, ...) {
  
  xy = xy.coords(x,y)
  xo = r * strwidth('A')
  yo = r * strheight('A')
  
  # Draw background text with small shift in x and y in background colour
  for (i in theta) {
    text(xy$x + cos(i)*xo, xy$y + sin(i)*yo, labels, col = bg, ...)
  }
  # Draw actual text in exact xy position in foreground colour
  text(xy$x, xy$y, labels, col = col, ...)
}

