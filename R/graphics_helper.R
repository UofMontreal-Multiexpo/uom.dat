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
#' @seealso Color names: [`grDevices::colors`], hexadecimal values: [`grDevices::rgb`],
#'  graphics palette: [`grDevices::palette`].
#' 
#' Color to RGB conversion: [`grDevices::col2rgb`].
#' @md
#' @keywords internal
is_color = function(x, int = TRUE) {
  is_valid_R_color = sapply(x, function(col) tryCatch(is.matrix(grDevices::col2rgb(col)),
                                                      error = function(e) FALSE))
  
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
    par_usr = graphics::par("usr")
    wh_ratio = c(user = ((par_usr[4] - par_usr[3]) / (par_usr[2] - par_usr[1])) *
                   (graphics::par("pin")[1] / graphics::par("pin")[2]),
                 inches = 1,
                 figure = graphics::par("fin")[1] / graphics::par("fin")[2])
    
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
#' @note
#' If the value of the argument `edge_alpha` is not \eqn{1}, edges may not be displayed in
#'  the RStudio "Plots" pane. However, they will be actually displayed in the "Plot Zoom" window (by
#'  clicking on the "Zoom" button in the "Plots" pane) or while exporting the plot.
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
#' @param edge_looseness Looseness of the connecting lines (from 0 to 1).
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
                          edge_looseness = 0.8, edge_alpha = 1,
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
                             tension = edge_looseness, alpha = edge_alpha) +
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
  
  # Multiplicateurs (character expansion) et offsets associés aux textes à afficher
  cex_lengths = 1.05 # Tailles des itemsets
  cex_uo = 0.5       # Texte under et over
  cex_legend = 0.85  # Légendes
  cex_items = 0.75   # Items
  offset_items_inches = graphics::par("cin")[1] * cex_items
  
  # Définition des marges et initialisation de la zone graphique
  if (!is.null(category)) graphics::par(mar = c(3.9, 0.5, 2.0, 0.5))
  else graphics::par(mar = c(0.5, 0.5, 2.0, 0.5))
  graphics::plot.new()
  
  
  ## Préparation de variables
  
  # Position des items sur la gauche du graphique (x = 0 ; y = ordre décroissant)
  items$x = 0
  items$y = rev(seq(nrow(items)))
  
  # Dimensions du graphique
  xmin = min(items$x)
  ymax = max(items$y)
  
  # Effectifs des tailles des itemsets
  length_tab = table(sapply(itemsets, length))
  # Affichage ou non de carrés colorés (cs = colored squares)
  display_cs = (!is.null(over_legend) && !is.null(over) && all(is_color(over, int = FALSE)))
  
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
  y_itemsets = lapply(itemsets, function(itemset) sort(items[match(itemset, items$item), "y"]))
  
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
  
  # Agrandissement de la marge pour avoir la place d'afficher les labels des items
  # Espace à gauche : taille de la marge précédente + offset de placement des labels
  #                    + taille du plus grand label 
  graphics::par(mai = c(graphics::par("mai")[1],
                        graphics::par("mai")[2] + offset_items_inches +
                          max(graphics::strwidth(items$label, units = "inches", cex = cex_items)),
                        graphics::par("mai")[3:4]))
  
  # Exclusion temporaire de l'espace qui contiendra le texte associé aux tailles des itemsets
  length_text_height = graphics::strheight("IVXLCDM", "figure", cex = cex_lengths)
  graphics::par(plt = c(graphics::par("plt")[1:3],
                        graphics::par("plt")[4] - length_text_height * 2.5))
  
  # Espace entre un itemset et le texte associé (en inches)
  if (!is.null(under) || !is.null(over)) {
    # Taille d'un caractère * rapport approximatif entre un caractère et un point pch 15 * cex
    uo_margin_inches = graphics::par("cin")[2] * 0.41 * cex_uo
  }
  
  # Espace au bas du graphique : prise en compte de l'affichage du texte under
  if (!is.null(under)) {
    # Taille des textes à afficher (en inches) + espace entre un itemset et le texte
    under_size = convert_gunits(graphics::strwidth(under, units = "inches", cex = cex_uo),
                                "inches", dim = "w", rotation = TRUE) + uo_margin_inches
    
    # Limite du texte qui apparaîtra le plus bas en fonction de sa taille et de la position de son itemset
    ymin_1 = min(nth_values(y_itemsets, "first") - convert_gunits(under_size, "inches", to = "user", "h"))
  } else {
    # Position de l'item le plus bas - moitié de la taille du texte de l'item associé (nécessaire pour
    # que le point et les lignes qui correspondent à cet item ne soient pas tronqués)
    ymin_1 = min(items$y) - graphics::strheight("A", units = "user", cex = cex_items) / 2
  }
  
  # Espace en haut du graphique : prise en compte de l'affichage du texte over
  if (!is.null(over)) {
    # Taille des textes à afficher (en inches) + espace entre un itemset et le texte
    over_size = convert_gunits(graphics::strwidth(over, units = "inches", cex = cex_uo),
                               "inches", dim = "w", rotation = TRUE) + uo_margin_inches
    
    # Limite du texte qui apparaîtra le plus haut en fonction de sa taille et de la position de son itemset
    ymax = max(nth_values(y_itemsets, "last") + convert_gunits(over_size, "inches", to = "user", "h"))
  }
  
  # Système de coordonnées user mappant la plot region
  graphics::par(usr = c(xmin, area_width - xmin, ymin_1, ymax))
  
  # Espace ajouté à droite pour pouvoir afficher la dernière taille d'itemset s'il y a trop peu d'itemsets associés
  last_space = graphics::strwidth(utils::as.roman(names(length_tab[length(length_tab)]))) - 
    (length_tab[length(length_tab)] * (width + margin) + margin) # strwidth units = "user"
  if (last_space > 0) area_width = area_width + last_space
  
  # Nouvelle étendue des ordonnées sur le graphique
  if (is.null(over)) ymax_2 = ymax
  if (is.null(under)) {
    # Position de l'item le plus bas - moitié de la taille du texte de l'item associé (nécessaire pour
    # que le point et les lignes qui correspondent à cet item ne soient pas tronqués)
    ymin_2 = min(items$y) - graphics::strheight("A", units = "user", cex = cex_items) / 2
  }
  
  # Recalcul des espaces au bas et en haut du graphique en fonction de la taille du texte à afficher
  if (!is.null(under) || !is.null(over)) {
    index_u = old_index_u = index_o = old_index_o = 0
    
    if (!is.null(under)) {
      # Indice du texte qui débordera le plus au bas de la zone où seront affichés les itemsets
      index_u = which.min(nth_values(y_itemsets, "first") - 
                            convert_gunits(under_size, "inches", to = "user", "h"))
    }
    
    if (!is.null(over) && !display_cs) {
      # Indice du texte qui débordera le plus en haut de la zone où seront affichés les itemsets
      index_o = which.max(nth_values(y_itemsets, "last") + 
                            convert_gunits(over_size, "inches", to = "user", "h"))
    } else if (display_cs) {
      # Position de l'item le plus haut + taille d'un carré + espace entre un itemset et le carré
      ymax_2 = ymax + 2 * convert_gunits(uo_margin_inches, "inches", to = "user", "h")
    }
    
    while (index_u != old_index_u || index_o != old_index_o) {
      
      # Définition temporaire de la plot region comme uniquement ce qui est entre les textes identifiés
      # (simulation d'une partie de la plot region du graphique finale)
      par_plt = graphics::par("plt")
      graphics::par(plt = c(par_plt[1:2],
                            if (!is.null(under)) par_plt[3] + 
                              convert_gunits(under_size[index_u], "inches", to = "figure", "h")
                            else par_plt[3],
                            if (!is.null(over) && !display_cs) par_plt[4] - 
                              convert_gunits(over_size[index_o], "inches", to = "figure", "h")
                            else par_plt[4]))
      graphics::par(usr = c(xmin, area_width - xmin,
                            if (!is.null(under)) y_itemsets[[index_u]][1]
                            else ymin_2,
                            if (!is.null(over) && !display_cs) y_itemsets[[index_o]][length(y_itemsets[[index_o]])]
                            else ymax_2))
      
      # Calcul des tailles de ces textes (+ espacement entre l'itemset et le texte) en user coords
      # Calcul des ordonnées minimales et maximales à associer à la plot région finale
      # Recalcul des indices des textes qui entraînent les plus grands débordements en bas et en haut
      
      if (!is.null(under)) {
        text_size_u = convert_gunits(under_size[index_u], "inches", to = "user", "h")
        ymin_2 = y_itemsets[[index_u]][1] - text_size_u
        old_index_u = index_u
        index_u = which.min(nth_values(y_itemsets, "first") -
                              convert_gunits(under_size, "inches", to = "user", "h"))
      } else {
        # Position de l'item le plus bas - moitié de la taille du texte de l'item associé (nécessaire pour
        # que le point et les lignes qui correspondent à cet item ne soient pas tronqués)
        ymin_2 = min(items$y) - graphics::strheight("A", units = "user", cex = cex_items) / 2
      }
      
      if (!is.null(over) && !display_cs) {
        text_size_o = convert_gunits(over_size[index_o], "inches", to = "user", "h")
        ymax_2 = y_itemsets[[index_o]][length(y_itemsets[[index_o]])] + text_size_o
        old_index_o = index_o
        index_o = which.max(nth_values(y_itemsets, "last") + 
                              convert_gunits(over_size, "inches", to = "user", "h"))
      } else if (display_cs) {
        # Position de l'item le plus haut + taille d'un carré + espace entre un itemset et le carré
        ymax_2 = ymax + 2 * convert_gunits(uo_margin_inches, "inches", to = "user", "h")
      }
      
      # Redéfinition normale de la plot région
      graphics::par(plt = par_plt)
      
      # Boucle car le ratio système de coordonnées inches / système de coordonnées user est modifié :
      # les textes qui débordaient le plus ne sont peut-être plus les mêmes qu'avant (la taille d'un
      # texte en inches est invariable mais les items se resserrent)
    }
  }
  
  # Réattribution de l'espace associé au texte des tailles des itemsets
  graphics::par(plt = c(graphics::par("plt")[1:3],
                        graphics::par("plt")[4] + length_text_height * 2.5))
  
  length_text_height = convert_gunits(length_text_height, "figure", to = "user", "h")
  
  # # Réinitialisation de la zone graphique en considérant les espacements à droite, en bas et en haut
  # # Option new = TRUE pour que le graphique apparaisse par-dessus la zone précédemment définie
  # graphics::par(new = TRUE)
  # graphics::plot.new()
  # Redéfinition du système de coordonnées user en considérant les espacements à droite, en bas et en haut
  graphics::par(usr = c(xmin, area_width - xmin, ymin_2, ymax_2 + length_text_height * 2.5))
  
  
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
  
  # Ordonnée des titres des tailles des itemsets
  y_lengths = ymax_2 + length_text_height * 1.5
  
  # Pour uniformiser l'espacement entre un itemset et les informations affichées (under/over) : taille d'un
  # point carré = taille d'un caractère * rapport approximatif entre un caractère et un point pch 15 * cex
  uo_margin = graphics::par("cxy")[2] * 0.41 * cex_uo
  # (équivalent de convert_gunits(uo_margin_inches, "inches", "user", "h"))
  
  # Marges entre la plot region et la figure region (w = width, b = bottom, t = top)
  w_margin = convert_gunits(graphics::par("mai")[4], "inches", "user", "w")
  b_margin = convert_gunits(w_margin, "user", dim = "w", rotation = TRUE)
  t_margin = convert_gunits(graphics::par("mai")[3], "inches", "user", "h")
  
  # Dimensions des légendes (carrés colorés et catégorie)
  if (display_cs) {
    cs_legend = graphics::legend(x = 0, y = 0, plot = FALSE,
                                 bty = "n", horiz = TRUE, xpd = TRUE, pch = 15,
                                 cex = cex_legend, col = over_legend, legend = names(over_legend))
  }
  if (!is.null(category)) {
    xcl = fig_in_usr_coords(1) + w_margin
    ycl = graphics::par("usr")[3]
    category_legend = graphics::legend(x = xcl, y = ycl, plot = FALSE,
                                       bty = "n", xpd = TRUE, pch = 20,
                                       ncol = ceiling(length(category$label) / 2),
                                       cex = cex_legend, col = category$col, legend = category$label,
                                       title = cap(category_name))
  }
  
  
  ## Traçage du graphique
  
  # Lignes horizontales repérant les items
  graphics::segments(x0 = xmin, x1 = area_width - xmin, y0 = items$y,
                     lwd = 0.02, lty = "dotted", col = "gray85")
  
  # Tailles des itemsets et lignes séparatrices
  graphics::text(x_lengths, y = y_lengths, labels = utils::as.roman(names(x_lengths)),
                 col = "black", cex = cex_lengths)
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
    graphics::text(x = x_itemsets + width / 2,
                   y = nth_values(y_itemsets, "first") - uo_margin,
                   labels = under, col = "black", cex = cex_uo, srt = 90, adj = 1)
  }
  
  # Affichage du contenu de over au-dessus des itemsets
  if (!is.null(over)) {
    # Affichage de carrés colorés ou de texte
    if (display_cs) {
      graphics::points(x = x_itemsets + width / 2,
                       y = nth_values(y_itemsets, "last") + 1.5 * uo_margin,
                       pch = 15, col = over, cex = cex_uo)
      # Une coordonnée indépendante de la taille du point entraîne parfois un chevauchement avec l'itemset
      # Concernant du texte, une coordonnée indépendante peut être utilisée grâce à adj
    } else {
      graphics::text(x = x_itemsets + width / 2,
                     y = nth_values(y_itemsets, "last") + uo_margin,
                     labels = over, col = "black", cex = cex_uo, srt = 90, adj = 0)
    }
  }
  
  
  ## Affichage du titre, des légendes et des items
  
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
    graphics::legend(xycl, bty = "n", xpd = TRUE, pch = 20,
                     ncol = ceiling(length(category$label) / 2), cex = cex_legend,
                     col = category$col, legend = category$label, title = cap(category_name))
  }
  
  # Légende des carrés colorés
  if (display_cs) {
    graphics::legend(x = graphics::par("usr")[2] - cs_legend$rect$w +
                       (max(graphics::strwidth(names(over_legend))) -
                          graphics::strwidth(names(over_legend)[length(over_legend)])),
                     y = graphics::par("usr")[4] + t_margin / 2 + cs_legend$rect$h / 2,
                     bty = "n", horiz = TRUE, xpd = TRUE, pch = 15,
                     cex = cex_legend, col = over_legend, legend = names(over_legend))
  }
  
  # Pointage et affichage des noms des items
  graphics::points(items[, c("x", "y")], col = item_colors, pch = 20, xpd = TRUE)
  graphics::text(items$x - convert_gunits(offset_items_inches, "inches", to = "user", "w"),
                 items$y, items$label, adj = 1, cex = cex_items, col = item_colors, xpd = TRUE)
}


