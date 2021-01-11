#' @include utils.R
NULL


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
#' @references Greg Snow. Stack Overflow topic:
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


#' Check color representation
#' 
#' Check if a character value is a valid color representation.
#' The three kinds of R color specifications are considered as valid representations (color name,
#'  hexadecimal string and positive integer).
#' 
#' @param x Character vector.
#' @param int Logical indicating whether to consider integer values as valid representations.
#' @return Logical vector giving for each value of `x` whether it is a valid color representation.
#' 
#' @references Josh O'Brien. Stack Overflow topic:
#'  [Check if character string is a valid color representation](https://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation).
#' @seealso Color names: [`colors`], hexadecimal values: [`rgb`], graphics palette: [`palette`].
#' 
#' Color to RGB conversion: [`col2rgb`].
#' @md
#' @keywords internal
is_color = function(x, int = TRUE) {
  is_valid_R_color = sapply(x, function(col) tryCatch(is.matrix(col2rgb(col)), error = function(e) FALSE))
  
  if (!int) {
    is_not_int = suppressWarnings(is.na(sapply(x, as.numeric)))
    return(is_not_int & is_valid_R_color)
  }
  return(is_valid_R_color)
}


#' Graphical units converter
#' 
#' Convert measures between graphical units considering the active graphics device.
#' 
#' @details
#' A measure in inches may not correspond to the same value in user coordinate unit along the X axis
#'  as along the Y axis.
#'  For example, a measure of \eqn{1} inch corresponding to the width of a text that will be rotated
#'  \eqn{90} degrees may correspond to a width of \eqn{0.09} in user unit before the rotation but a
#'  height of \eqn{0.05} in user unit after the rotation.
#'  The argument `rotation` allows to take this into account.
#' 
#' The `"user"` unit is only available after [`graphics::plot.new`] has been called.
#' 
#' The results of the functions [`graphics::strwidth`] and [`graphics::strheight`] applied to the
#'  character `"1"` and each different unit serve as reference values for conversions.
#' 
#' @param measures Values to convert.
#' @param from Graphical unit in which the measures are. One of `"user"`, `"inches"`, `"figure"`.
#' @param to Graphical unit in which to convert the measures. One of `"user"`, `"inches"`, `"figure"`.
#' @param dim Dimension on which measures were made (`"width"`, `"height"`, `"w"` or `"h"`).
#' @param rotation Logical indicating whether to consider a rotation of the measured elements.
#' @return Converted measure values.
#' 
#' @author Gauthier Magnin
#' @seealso [`fig_in_usr_coords`].
#' @md
#' @keywords internal
convert_gunits = function(measures, from, to = from, dim, rotation = FALSE) {
  
  check_param(from, values = c("user", "inches", "figure"))
  check_param(to, values = c("user", "inches", "figure"))
  check_param(dim, values = c("width", "height", "w", "h"))
  
  if (dim == "w") dim = "width"
  else if (dim == "h") dim = "height"
  
  if (rotation) {
    # Ratio largeur/hauteur = (étendue en Y / étendue en X selon le système de coordonnées actuelle) *
    #                         (largeur de la zone en inches / hauteur de la zone en inches)
    wh_ratio = c(user = ((par("usr")[4] - par("usr")[3]) / (par("usr")[2] - par("usr")[1])) *
                   (par("pin")[1] / par("pin")[2]),
                 inches = 1,
                 figure = par("fin")[1] / par("fin")[2])
    
    # Application du ratio pour effectuer la transition d'unité (largeur -> hauteur ou inversement)
    if (dim == "width") measures = measures * unname(wh_ratio[from])
    else measures = measures / unname(wh_ratio[from])
  }
  
  if (dim == "width" && !rotation || dim == "height" &&  rotation) 
    return(measures * graphics::strwidth(1, to) / graphics::strwidth(1, from))
  ##  dim == "width" &&  rotation || dim == "height" && !rotation 
  return(measures * graphics::strheight(1, to) / graphics::strheight(1, from))
}


#' User coordinates of the figure region
#' 
#' Compute the coordinates of the figure region in user coordinate units.
#' Only available after [`graphics::plot.new`] has been called.
#' 
#' @param n Numbers of the values to compute between `1` and `4`.
#'  The same numbering as for `par("fig")`, `par("plt")` and `par("usr")`: `x0`, `x1`, `y0`, `y1`.
#' @return User coordinates of the figure region.
#' 
#' @author Gauthier Magnin
#' @seealso [`convert_gunits`].
#' @md
#' @keywords internal
fig_in_usr_coords = function(n = 1:4) {
  return(c(
    graphics::par("usr")[1] - convert_gunits(graphics::par("plt")[1], "figure", "user", "w"),
    graphics::par("usr")[2] + convert_gunits(1 - graphics::par("plt")[2], "figure", "user", "w"),
    graphics::par("usr")[3] - convert_gunits(graphics::par("plt")[3], "figure", "user", "h"),
    graphics::par("usr")[4] + convert_gunits(1 - graphics::par("plt")[4], "figure", "user", "h")
  )[n])
}


#' Hierarchical Edge Bundling chart
#' 
#' Plot a hierarchical edge bundling chart: graph in which vertices are arranged circularly.
#' 
#' @details
#' The chart being plotted with the packages `ggraph` and `ggplot2`, it can be modified or completed
#'  afterwards using [`ggplot2::last_plot`] or the returned object.
#' 
#' @param hierarchy A two-column data frame representing the hierarchical network structure. A tree-like
#'  representation of the vertices with a root and any internal nodes (only leaves are plotted).
#' @param vertices Data frame containing the vertices to plot (i.e. the leaves of the tree
#'  representation). Must contain the columns:
#'  \describe{
#'    \item{`"name"`}{Name of the vertices in the `hierarchy`.}
#'    \item{`"label"`}{Labels to plot next to the vertices.}
#'    \item{`"group"`}{Optional. Additional column used to color the vertices.}
#'  }
#' @param edges A three-column data frame representing the edges of the graph (i.e. the connections
#'  between the leaves of the tree representation). Must contain two columns contaning the names of the
#'  vertices to connect and a third one giving the intensities of the connections.
#' @param limits The scale limits (related to the third column of `edges`).
#' @param legend_name,legend_values Name and values of the additional legend related to the column
#'  `group` of the data frame `vertices`.
#'  
#'  `NULL` if there is no group. Character for `legend_name` and named character vector for
#'  `legend_values`: colors values named with the group values.
#' @param vertex_size Size of the vertices.
#' @param vertex_alpha Opacity of the vertices (from 0 to 1).
#' @param vertex_margin Margin before the vertices (i.e. distance between the ends of the edges and the
#'  centers of the vertices).
#' @param label_size Size of the labels associated with the vertices.
#' @param label_margin Margin before the labels (i.e. distance between the centers of the vertices and
#'  the labels).
#' @param edge_tension Looseness of the connecting lines (from 0 to 1).
#'  The closer the value is to 0, the straighter the lines will be.
#'  The closer the value is to 1, the more the lines will be curved.
#' @param edge_alpha Opacity of the lines connecting vertices (from 0 to 1).
#' @param palette Name (or number) of the sequential palette to use for coloring the edges.
#'  One of `"Blues"`, `"BuGn"`, `"BuPu"`, `"GnBu"`, `"Greens"`, `"Greys"`, `"Oranges"`, `"OrRd"`,
#'  `"PuBu"`, `"PuBuGn"`, `"PuRd"`, `"Purples"`, `"RdPu"`, `"Reds"`, `"YlGn"`, `"YlGnBu"`, `"YlOrBr"`,
#'  `"YlOrRd"`.
#' @param palette_direction Direction in which to use the color palette.
#'  If `1`, colors are in original order: from the lightest to the darkest.
#'  If `-1`, color order is reversed: from the darkest to the lightest.
#' @return Graph created with the packages `ggraph` and `ggplot2`.
#' 
#' @author Gauthier Magnin
#' @md
#' @keywords internal
plot_heb_chart = function(hierarchy, vertices, edges, limits,
                          legend_name = NULL, legend_values = NULL,
                          vertex_size = 3, vertex_alpha = 1, vertex_margin = 0.05,
                          label_size = 3, label_margin = 0.05,
                          edge_tension = 0.8, edge_alpha = 1,
                          palette = "Blues", palette_direction = 1) {
  
  # Décalages qui seront appliquées aux sommets et labels
  vertices$vertex_coord_multiplier = 1 + vertex_margin
  vertices$label_coord_multiplier = 1 + vertex_margin + label_margin
  
  # Recherche des numéros des sommets à lier
  from = match(edges[, 2], vertices$name)
  to = match(edges[, 1], vertices$name)
  
  # Tri des liens selon l'ordre des sommets pour que les couleurs soient appliquées correctement
  the_order = order(from, to)
  from = from[the_order]
  to = to[the_order]
  edges = edges[the_order, ]
  
  # Graphe
  tree = igraph::graph_from_data_frame(hierarchy, vertices = vertices)
  
  graph = ggraph::ggraph(tree, layout = "dendrogram", circular = TRUE) +
    
    ggraph::geom_conn_bundle(data = ggraph::get_con(from = from, to = to,
                                                    colors = edges[, 3]),
                             ggplot2::aes(color = colors),
                             tension = edge_tension, alpha = edge_alpha) +
    ggraph::scale_edge_color_distiller("Co-occurrence", palette = palette, direction = palette_direction,
                                       # Définition de l'échelle (pour n'avoir que des entiers)
                                       limits = limits,
                                       breaks = unique(floor(pretty(seq(limits[1], limits[2])))),
                                       # Paramètre nécessaire si non-chargement de ggraph
                                       guide = ggraph::guide_edge_colorbar(order = 1)) +
    
    ggraph::geom_node_point(ggplot2::aes(x = x * vertex_coord_multiplier,
                                         y = y * vertex_coord_multiplier,
                                         filter = leaf,
                                         color = if (!is.null(legend_name)) group),
                            size = vertex_size, alpha = vertex_alpha) +
    
    ggraph::geom_node_text(ggplot2::aes(x = x * label_coord_multiplier,
                                        y = y * label_coord_multiplier,
                                        filter = leaf,
                                        label = label,
                                        angle = atan(y / x) * 180 / pi, # Angle en degré
                                        hjust = ifelse(x < 0, 1, 0),
                                        color = if (!is.null(legend_name)) group),
                           size = label_size, show.legend = FALSE) +
    ggplot2::theme_void() +
    ggplot2::coord_fixed()
  
  if (!is.null(legend_name)) {
    return(graph + ggplot2::scale_color_manual(legend_name,
                                               values = legend_values,
                                               guide = ggplot2::guide_legend(
                                                 order = 2,
                                                 override.aes = list(size = 1.5, alpha = 1))))
  }
  return(graph)
}


#' Itemset chart
#' 
#' Plot a chart of itemsets. A color scale can be associated to the items and additional data
#'  can be plotted under and over each itemset.
#' 
#' @param itemsets List of character vectors. Itemsets to plot.
#' @param items Data frame associating each item of `itemsets` (column `"item"`) with a label
#'  (column `"label"`) and possibly with a category (column named by the name of the category).
#' @param category Data frame associating each value of the category associated with the items (column
#'  `"value"`) with a color (column `"col"`) and with a label (column `"label"`).
#'  If not `NULL`, the items are colored according to the category values and a legend is displayed.
#' @param jitter If `FALSE`, non-equivalent itemsets of length \eqn{1} are aligned vertically.
#'  If `TRUE`, they are spread over several vertical lines to avoid overplotting while taking as little
#'  space as possible. If `NA`, they are plotted one after the other.
#' @param under Character vector. Text to display on the chart under the itemsets.
#' @param over Character vector. Text to display on the chart over the itemsets.
#'  Color values (color names or hexadecimal values) can be specified to plot colored squared instead of
#'  text.
#' @param over_legend If color values are given in the argument `over`, named character vector in which
#'  values are the colored used and names are associated labels to plot a specific legend.
#' @param title Chart title.
#' 
#' @author Delphine Bosson-Rieutort, Gauthier Magnin
#' @references Bosson-Rieutort D, Sarazin P, Bicout DJ, Ho V, Lavoué J (2020).
#'             Occupational Co-exposures to Multiple Chemical Agents from Workplace Measurements by the US Occupational Safety and Health Administration.
#'             *Annals of Work Exposures and Health*, Volume 64, Issue 4, May 2020, Pages 402–415.
#'             <https://doi.org/10.1093/annweh/wxaa008>.
#' @md
#' @keywords internal
plot_itemset_chart = function(itemsets, items, category = NULL,
                              jitter = TRUE, under = NULL, over = NULL, over_legend = NULL,
                              title = "Itemsets") {
  
  # Définition des marges et initialisation de la zone graphique
  if (!is.null(category)) graphics::par(mar = c(3.9, 0.5, 2.0, 0.5))
  else graphics::par(mar = c(0.5, 0.5, 2.0, 0.5))
  graphics::plot.new()
  
  
  ## Préparation de variables
  
  # Position des items sur la gauche du graphique (x = 0 ; y = ordre décroissant)
  items$x = 0
  items$y = rev(seq(nrow(items)))
  
  # Dimensions du graphique
  xmin = items$x[1]
  ymax = nrow(items) + 2 # Nombre d'items + 2 pour afficher les tailles des itemsets
  
  # Effectifs des tailles des itemsets
  length_tab = table(sapply(itemsets, length))
  # Affichage ou non de carrés colorés (cs = colored squares)
  display_cs = (!is.null(over_legend) && !is.null(over) && all(is_color(over, int = FALSE)))
  
  # Titre des tailles des itemsets et position en Y
  text_length = "Length:"
  y_lengths = max(items$y) + 1.25
  
  # Couleurs des items et nom de l'éventuelle catégorie
  if (!is.null(category)) {
    category_name = colnames(items)[!is.element(colnames(items), c("item", "label", "x", "y"))]
    item_colors = category$col[match(items[, category_name], category$value)]
  } else {
    item_colors = "black"
  }
  
  # Marge entre un itemset et la ligne séparatrice qui le suit ou entre deux itemsets
  margin = 0.5
  # Largeur d'un itemset
  width = 0.5
  
  # Ordonnées des items pour chaque itemset
  y_itemsets = lapply(itemsets, function(itemset) {
    sort(items[match(itemset, items$item), "y"])
  })
  
  # Abscisses des itemsets
  if ("1" %in% names(length_tab)) {
    
    # Placement des itemsets de taille 1
    if (is.na(jitter)) {
      x_itemsets = seq(0, length_tab[1] - 1) * width
    }
    else {
      x_itemsets = rep(0, length_tab[1])
      
      # Ordre des itemsets de taille 1 selon leur ordonnée
      one_order = order(unlist(y_itemsets[1:length_tab[1]]))
      
      if (jitter) {
        # Nombres d'itemsets pour chaque valeur de y pour lesquelles il y a au moins un itemset à dessiner
        y_tab = table(unlist(y_itemsets[seq_len(length_tab[1])]))
        
        # Placement des premiers itemsets (les plus bas)
        x_itemsets[seq_len(y_tab[1])] = seq_len(y_tab[1]) - 1
        
        # Pour chaque autre valeur de y
        for (i in seq_along(y_tab)[-1]) {
          y = names(y_tab)[i]
          
          # Si les itemsets ne se situent pas seulement une ligne au-dessus des précédents ou qu'il y a
          # la place pour tous les dessiner entre le bord gauche et la position en X des précédents
          if (as.numeric(y) != as.numeric(names(y_tab)[i-1]) + 1
              || x_itemsets[sum(y_tab[seq_len(i - 2)]) + 1] >= y_tab[y]) {
            # Placement à partir de X = 0
            positions = seq_len(y_tab[y]) - 1
          }
          else {
            # Placement après la dernière position en X
            positions = x_itemsets[sum(y_tab[seq_len(i - 1)])] + seq_len(y_tab[y])
          }
          x_itemsets[sum(y_tab[seq_len(i - 1)]) + seq_len(sum(y_tab[y]))] = positions
        }
        x_itemsets = x_itemsets * width
      }
      else {
        # Alignement vertical et décalage en X de ceux à la même hauteur
        for (i in seq(2, length(y_itemsets[one_order]))) {
          if (y_itemsets[one_order][[i]] == y_itemsets[one_order][[i-1]]) {
            x_itemsets[i] = x_itemsets[i-1] + width
          }
        }
      }
      
      x_itemsets = x_itemsets[order(one_order)]
    }
    
    # Position de la première ligne séparatrice d'itemsets
    x_lines = max(x_itemsets) + margin * 2
    
    # Itemsets de taille > 1 : les uns après les autres en considérant les lignes séparatrices
    x_itemsets = c(x_itemsets,
                   (width + margin) * seq(length(itemsets) - length(x_itemsets)) + max(x_itemsets) + 
                     margin * rep(seq_along(length_tab[-1]), length_tab[-1]))
    
    # Si plus de 2 tailles d'itemsets différentes
    if (length(length_tab) > 2) {
      other_itemsets = seq(length_tab[1] + 1, length(x_itemsets))
      
      # Abscisses des autres lignes séparatrices (milieux des itemsets les plus espacés)
      x_lines = c(x_lines,
                  x_itemsets[other_itemsets[-1]][x_itemsets[other_itemsets[-1]] >= 
                                                   (x_itemsets[other_itemsets[-length(other_itemsets)]] + width + margin * 2)] - margin)
    }
  } else {
    # Positionnement des itemsets les uns après les autres en considérant les lignes séparatrices
    # Taille d'un itemset et d'un espacement * nombre d'autres itemsets avant l'itemset
    #  + un espacement * nombre de lignes séparatrices avant l'itemset
    x_itemsets = (width + margin) * seq(0, length(itemsets)-1) + 
                   margin * rep(seq(0, length(length_tab)-1), length_tab)
    
    # Abscisses des lignes séparatrices (milieux des itemsets les plus espacés)
    x_lines = x_itemsets[-1][x_itemsets[-1] >= (x_itemsets[-length(x_itemsets)] + width + margin * 2)] - margin
  }
  
  # Largeur de la zone des itemsets : position du dernier itemset + largeur de l'itemset + un espacement
  area_width = max(x_itemsets) + width + margin
  
  
  ## Préparation de la zone graphique
  
  # Placement de la "plot region" pour avoir la place d'afficher les labels des items en-dehors
  # Espace à gauche : taille du plus grand label (ou du titre "Length") + taille de la marge
  #                   + taille d'un caractère * 0.5 (offset de placement des labels) ; en fraction de la "figure region"
  graphics::par(plt = c(max(graphics::strwidth(items$label, cex = 0.75, units = "figure"),
                            graphics::strwidth(text_length, cex = 1.05, units = "figure")) +
                          convert_gunits(graphics::par("mai")[2], "inches", to = "figure", "w") +
                          0.5 * graphics::strwidth("A", units = "figure"),
                        graphics::par("plt")[2:4]))
  
  # Option "new" pour que le graphique apparaisse par-dessus la zone précédemment définie
  graphics::par(new = TRUE)
  
  # Espace au bas du graphique : prise en compte de l'affichage du texte under
  if (!is.null(under)) {
    # Largeur des textes à afficher (en inches)
    under_width = graphics::strwidth(under, units = "inches", cex = 0.5)
    # Espace entre un itemset et le texte associé (en inches) :
    # taille d'un caractère * rapport approximatif entre un caractère et un point pch 15 * cex
    under_margin = graphics::par("cin")[2] * 0.41 * 0.5
    
    # Limite du texte qui apparaîtra le plus bas en fonction de sa taille et de la position de son itemset
    ymin_1 = min(nth_values(y_itemsets, "first") - 
                   convert_gunits(under_margin, "inches", to = "user", "h") - 
                   convert_gunits(under_width, "inches", to = "user", "w", rotation = TRUE))
  } else {
    ymin_1 = ymin_2 = min(items$y)
  }
  
  # Initialisation de la zone graphique
  graphics::plot(rbind(items[, c("x", "y")], # Autant de lignes (ordonnée max) que d'items distincts
                       data.frame(x = rep(0, 2), y = seq_len(2) + nrow(items))), # + 2 pour la légende des tailles
                 xlim = c(xmin, area_width - xmin),
                 ylim = c(ymin_1, ymax),
                 col = "white", pch = 20, bty = "n",
                 xaxt = "n", xaxs = "i", xlab = "",
                 yaxt = "n", yaxs = "i", ylab = "")
  
  # Espace ajouté à droite pour pouvoir afficher la dernière taille d'itemset s'il y a trop peu d'itemsets associés
  last_space = graphics::strwidth(utils::as.roman(names(length_tab[length(length_tab)]))) - 
    (length_tab[length(length_tab)] * (width + margin) + margin) # strwidth units="user" dépend du graphique
  if (last_space > 0) area_width = area_width + last_space
  
  # Recalcul de l'espace au bas du graphique
  if (!is.null(under)) {
    # Indice du texte qui débordera le plus de la partie de la plot region où seront affichés les itemsets
    index = which.min(nth_values(y_itemsets, "first") -
                        convert_gunits(under_margin, "inches", to = "user", "h") - 
                        convert_gunits(under_width, "inches", to = "user", "w", rotation = TRUE))

    repeat {
      # Définition temporaire de la plot region comme uniquement ce qui est au-dessus du texte identifié
      # (simulation d'une partie de la plot region du graphique finale)
      graphics::par(new = TRUE)
      par_plt = graphics::par("plt")
      graphics::par(plt = c(par_plt[1:2],
                            par_plt[3] + 
                              convert_gunits(under_width[index], "inches", to = "figure", "w", rotation = TRUE) +
                              convert_gunits(under_margin, "inches", to = "figure", "h"),
                            par_plt[4]))
      graphics::plot(rbind(items[c(y_itemsets[[index]][1], nrow(items)), c("x", "y")], # Autant de lignes que d'items au-dessus du texte
                           data.frame(x = rep(0, 2), y = seq_len(2) + nrow(items))), # + 2 pour la légende des tailles
                     xlim = c(xmin, area_width - xmin),
                     ylim = c(y_itemsets[[index]][1], ymax),
                     col = "white", pch = 20, bty = "n",
                     xaxt = "n", xaxs = "i", xlab = "",
                     yaxt = "n", yaxs = "i", ylab = "")

      # Calcul de la taille du texte (+ espacement entre le motif et le texte) en user coords
      text_size = convert_gunits(under_width[index], "inches", to = "user", "w", rotation = TRUE) +
        convert_gunits(under_margin, "inches", to = "user", "h")

      # Redéfinition normale de la plot région et calcul de l'ordonnée minimale à lui associer
      graphics::par(plt = par_plt)
      ymin_2 = y_itemsets[[index]][1] - text_size

      # Boucle car le ratio système de coordonnées inches ; système de coordonnées user est modifié :
      # le texte qui débordait le plus vers le bas n'est peut-être plus le même qu'avant (la taille d'un
      # texte en inches est invariable mais les items se resserrent)
      old_index = index
      index = which.min(nth_values(y_itemsets, "first") -
                          convert_gunits(under_margin, "inches", to = "user", "h") - 
                          convert_gunits(under_width, "inches", to = "user", "w", rotation = TRUE))
      
      if (index == old_index) break
    }
  }
  
  # Réinitialisation de la zone graphique en considérant les espacements à droite et en bas
  if (last_space > 0 || ymin_2 != ymin_1) {
    
    # Option "new" pour que le graphique apparaisse par-dessus la zone précédemment définie
    graphics::par(new = TRUE)
    graphics::plot(rbind(items[, c("x", "y")], # Autant de lignes (ordonnée max) que d'items distincts
                         data.frame(x = rep(0, 2), y = seq_len(2) + nrow(items))), # + 2 pour la légende des tailles
                   xlim = c(xmin, area_width - xmin),
                   ylim = c(ymin_2, ymax),
                   col = "white", pch = 20, bty = "n",
                   xaxt = "n", xaxs = "i", xlab = "",
                   yaxt = "n", yaxs = "i", ylab = "")
  }
  
  
  ## Préparation de variables dépendant de la zone graphique
  
  # Définition des abscisses des titres des tailles des itemsets
  if (length(x_lines) == 0) x_lengths = area_width / 2
  else {
    x_lengths = numeric(length(length_tab))
    x_lengths[1] = x_lines[1] / 2
    if (length(x_lines) > 1) {
      x_lengths[seq(2, length(x_lines))] = (x_lines[seq(2, length(x_lines))] + 
                                              x_lines[seq(length(x_lines)-1)]) / 2
    }
    x_lengths[length(x_lengths)] = (area_width + x_lines[length(x_lines)]) / 2
  }
  names(x_lengths) = names(length_tab)
  
  # Pour uniformiser l'espacement entre un itemset et les informations affichées (under/over) : taille d'un
  # point carré = taille d'un caractère * rapport approximatif entre un caractère et un point pch 15 * cex
  uo_margin = graphics::par("cxy")[2] * 0.41 * 0.5
  # (équivalent de convert_gunits(under_margin, "inches", "user", "h"))
  
  # Marges entre la plot region et la figure region (w = width, b = bottom, t = top)
  w_margin = convert_gunits(graphics::par("mai")[4], "inches", "user", "w")
  b_margin = convert_gunits(w_margin, "user", dim = "w", rotation = TRUE)
  t_margin = convert_gunits(graphics::par("mai")[3], "inches", "user", "h")
  
  # Dimensions des légendes (carrés colorés et catégorie)
  if (display_cs) {
    cs_legend = graphics::legend(x = 0, y = 0, plot = FALSE,
                                 bty = "n", horiz = TRUE, xpd = TRUE,
                                 pch = 15, cex = 0.85,
                                 col = over_legend,
                                 legend = names(over_legend))
  }
  if (!is.null(category)) {
    xcl = fig_in_usr_coords(1) + w_margin
    ycl = graphics::par("usr")[3]
    category_legend = graphics::legend(x = xcl, y = ycl, plot = FALSE,
                                       xpd = TRUE, bty = "n",
                                       title = cap(category_name), legend = category$label, cex = 0.85,
                                       col = category$col, pch = 20, ncol = ceiling(length(category$label) / 2))
  }
  
  
  ## Traçage du graphique
  
  # Lignes horizontales repérant les items
  graphics::segments(x0 = 0, x1 = area_width, y0 = items$y,
                     lwd = 0.02, lty = "dotted", col = "gray85")
  
  # Tailles des itemsets (titre et valeurs) et lignes séparatrices
  graphics::text(0, y_lengths, text_length,
                 col = "black", cex = 1.05, adj = c(1, 0.5), xpd = TRUE)
  graphics::text(x_lengths, y = y_lengths, labels = utils::as.roman(names(x_lengths)),
                 col = "black", cex = 1.05)
  graphics::abline(v = x_lines, col = "black", lwd = 0.5, lty = "dotted")
  
  # Segments verticaux entre les premiers et derniers items de chaque itemset
  graphics::segments(x0 = x_itemsets + width,
                     y0 = nth_values(y_itemsets, "first"), y1 = nth_values(y_itemsets, "last"),
                     lwd = 1.2, lty = 1, col = "black")
  
  # Segments horizontaux pour les items de chaque itemset
  graphics::segments(x0 = rep(x_itemsets, sapply(y_itemsets, length)),
                     x1 = rep(x_itemsets, sapply(y_itemsets, length)) + width,
                     y0 = unlist(y_itemsets),
                     lwd = 1.2, lty = 1, col = "black")
  
  # Affichage du texte contenu dans under au-dessous des itemsets
  if (!is.null(under)) {
    graphics::text(x_itemsets + width / 2,
                   nth_values(y_itemsets, "first") - uo_margin,
                   labels = under,
                   col = "black", cex = 0.5, srt = 90, adj = 1)
  }
  
  # Affichage du contenu de over au-dessus des itemsets
  if (!is.null(over)) {
    # Affichage de carrés colorés ou de texte
    if (display_cs) {
      graphics::points(x_itemsets + width / 2,
                       nth_values(y_itemsets, "last") + 1.5 * uo_margin,
                       col = over,
                       cex = 0.5, pch = 15)
      # Une coordonnée indépendante de la taille du point entraîne parfois un chevauchement avec l'itemset
      # Concernant under une coordonnée indépendante peut être utilisée grâce à adj
    } else {
      graphics::text(x_itemsets + width / 2,
                     nth_values(y_itemsets, "last") + uo_margin,
                     labels = over,
                     col = "black", cex = 0.5, srt = 90, adj = 0)
    }
  }
  
  
  ## Affichage du titre, des items et des légendes
  
  # Titre du graphique (fonction text au lieu de title pour placement précis avec des coordonnées)
  graphics::text(x = fig_in_usr_coords(1) + w_margin,
                 y = fig_in_usr_coords(4) - t_margin / 2,
                 title, cex = 1.3, font = 2, adj = c(0, 0.5), xpd = TRUE)
  
  # Légende de la catégorie
  if (!is.null(category)) {
    # Bornage de la taille de la légende à celle de la figure region (moins les marges)
    if (category_legend$rect$w > graphics::par("usr")[2] - xcl) {
      xycl = grDevices::xy.coords(x = c(xcl, graphics::par("usr")[2]),
                                  y = c(ycl, fig_in_usr_coords(3) + b_margin))
    } else {
      # Centrage de la légende
      xycl = grDevices::xy.coords(x = (graphics::par("usr")[2] + xcl) / 2 - category_legend$rect$w / 2,
                                  y = ycl)
    }
    graphics::legend(xycl, xpd = TRUE, bty = "n",
                     title = cap(category_name), legend = category$label, cex = 0.85,
                     col = category$col, pch = 20, ncol = ceiling(length(category$label) / 2))
  }
  
  # Légende des carrés colorés
  if (display_cs) {
    graphics::legend(x = graphics::par("usr")[2] - cs_legend$rect$w +
                       (max(graphics::strwidth(names(over_legend))) -
                          graphics::strwidth(names(over_legend)[length(over_legend)])),
                     y = graphics::par("usr")[4] + t_margin / 2 + cs_legend$rect$h / 2,
                     bty = "n", horiz = TRUE, xpd = TRUE,
                     pch = 15, cex = 0.85,
                     col = over_legend,
                     legend = names(over_legend))
  }
  
  # Pointage et affichage des noms des items
  graphics::points(items[, c("x", "y")], col = item_colors, pch = 20)
  graphics::text(items$x, items$y, items$label,
                 cex = 0.75, pos = 2, col = item_colors, xpd = TRUE)
}


