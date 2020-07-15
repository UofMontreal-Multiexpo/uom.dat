#' @include utils.R
NULL


# État d'activation du mode débogage
DEBUG_MODE = FALSE
# Définit l'étape jusqu'à laquelle effectuer l'analyse avant d'arrêter le processus
UP_TO_STEP = Inf



#### Attributs et constructeur ####

# Création d'une classe correspondant à 2 types
setClassUnion("listORarray", c("list", "array"))

#' Analyseur Spectral
#' 
#' Classe d'objet S4 permettant une analyse spectrale.
#' 
#' @slot observations Liste des éléments retrouvés pour chaque observation.
#' @slot items Ensemble des codes des différents éléments retrouvés dans les observations.
#' @slot items_categories Catégories associées aux différents éléments observés.
#' @slot target Type de motifs à énumérer lors de l'analyse.
#' @slot count Nombre minimal d'apparition d'un motif pour être conservé lors de l'énumération des motifs.
#' @slot min_length Taille minimale qu'un motif doit avoir pour être conservé lors de l'énumération des motifs.
#' @slot max_length Taille maximale qu'un motif doit avoir pour être conservé lors de l'énumération des motifs.
#' @slot status_limit Intervalle de temps pour lequel caractériser le statut des motifs par rapport à la période totale d'observations.
#' @slot nodes_per_year Nombre de recrutements de chaque observation par année.
#' @slot nodes Ensemble des nœuds, c'est-à-dire ensemble des observations distinctes.
#' @slot n_links Ensemble des poids des liens entre les nœuds.
#' @slot nodes_links Ensemble des liens entre les nœuds et caractéristiques de ces liens.
#' @slot obs_patterns Ensemble des motifs associés à chaque observation disctincte.
#' @slot patterns_per_year Ensemble des motifs, par année.
#' @slot patterns Ensemble des motifs distincts et de leurs caractéristiques.
#' @slot p_links Ensemble des poids des liens entre les motifs.
#' @slot patterns_links Ensemble des liens entre les motifs et caractéristiques de ces liens.
#' @slot Class Liste des attributs de classe (indépendants de l'instance).
#' 
#' @author Gauthier Magnin
#' @aliases SpectralAnalyzer
#' @export
setClass(Class = "SpectralAnalyzer",
         representation = representation(
           
           observations = "listORarray",
           items = "vector",
           items_categories = "data.frame",
           
           target = "character",
           count = "numeric",
           min_length = "numeric",
           max_length = "numeric",
           status_limit = "numeric",
           
           nodes_per_year = "matrix",
           nodes = "data.frame",
           n_links = "matrix",
           nodes_links = "data.frame",
           
           obs_patterns = "matrix",
           
           patterns_per_year = "matrix",
           patterns = "data.frame",
           p_links = "matrix",
           patterns_links = "data.frame",
           
           Class = "list"
         ),
         validity = function(object) {
           
           # Validation du système de codage des items
           if(any(grepl("/", object@items))) stop("Item codes must not contain the character \"/\".")
           
           # Vérification des paramètres d'initialisation
           if(object@count < 1) stop("count must be greater than zero.")
           if(object@min_length < 1) stop("min_length must be greater than zero.")
           if(object@max_length < object@min_length) stop("max_length must be greater than or equal to min_length.")
           if(object@status_limit < 1) stop("status_limit must be greater than zero.")
           
           # Vérification du type des catégories associées aux items
           if (!all(tapply(seq_len(ncol(object@items_categories)),
                           seq_len(ncol(object@items_categories)),
                           function(c) is.factor(object@items_categories[, c])))) {
             stop("The categories associated with the items must be factor type.")
           }
           
           return(TRUE)
         })

# Initiateur
setMethod(f = "initialize",
          signature = "SpectralAnalyzer",
          definition = function(.Object, observations, items, target, count, min_length, max_length, status_limit) {
            
            .Object@observations = observations
            
            # Ensemble des éléments observés et catégories associées
            if (missing(items)) {
              .Object@items = get_all_items(observations)
              names(.Object@items) = .Object@items
            } else {
              .Object@items = items$item
              names(.Object@items) = if ("name" %in% colnames(items)) items$name else items$item
              .Object@items_categories = items[-which(colnames(items) %in% c("item", "name"))]
              rownames(.Object@items_categories) = items$item
            }
            
            # Descripteurs de la recherche de motifs
            .Object@target = target
            .Object@count = count
            .Object@min_length = min_length
            .Object@max_length = max_length
            .Object@status_limit = status_limit
            
            # Vérification des premiers attributs
            validObject(.Object)
            
            # Attrtibuts de classe
            .Object@Class = list("STATUS_PERSISTENT" = "Persistent",
                                 "STATUS_DECLINING" = "Declining",
                                 "STATUS_EMERGENT" = "Emergent",
                                 "STATUS_LATENT" = "Latent",
                                 "STATUS_COLORS" = c("red", "royalblue", "orange", "gray"))
            names(.Object@Class$STATUS_COLORS) = c(.Object@Class$STATUS_PERSISTENT, .Object@Class$STATUS_DECLINING,
                                                   .Object@Class$STATUS_EMERGENT, .Object@Class$STATUS_LATENT)
            
            # Initialisation des attributs restants
            reset(.Object, from = 1)
            
            validObject(.Object)
            return(.Object)
          })


#' Constructeur d'Analyseur Spectral
#' 
#' Construit un objet de classe SpectralAnalyzer.
#' 
#' @details
#' Si les items ne sont pas spécifiés via l'argument \code{items}, ils sont automatiquement listés
#'  à partir des valeurs de \code{CODE} dans l'argument \code{observations}, sans aucune catégorisation
#'  ni aucune dénomination.
#' 
#' @param observations Liste des éléments retrouvés pour chaque observation.
#'  Chaque observation est elle-même une liste sous la forme \code{list( CODE = character(), YEAR = numeric )}.
#'  Les valeurs de \code{CODE} ne doivent pas contenir le caractère "/".
#'  Une observation peut contenir des informations supplémentaires quelconques.
#' @param items Data frame associant un nom (colonne \code{name}) et une ou plusieurs catégories (colonnes
#'  supplémentaires) à chaque élément (colonne \code{item}).
#'  La valeur \code{NULL} par défaut précise qu'aucun nom et aucune catégorie ne sont définis.
#' @param target Type de motifs à énumérer.
#'  Choix parmi \code{"frequent itemsets"}, \code{"closed frequent itemsets"}, \code{"maximally frequent itemsets"}.
#'  Par défaut, \code{"closed frequent itemsets"}, fournissant une synthèse des motifs fréquents
#'  de sorte à économiser l'espace mémoire nécessaire.
#'  Pour énumérer l'intégralité des motifs possibles, utiliser \code{"frequent itemsets"}.
#' @param count Nombre minimal d'apparition d'un motif pour être considéré comme "fréquent", et par conséquent, conservé.
#' @param min_length Taille minimale qu'un motif doit avoir pour être conservé lors de l'énumération.
#' @param max_length Taille maximale qu'un motif doit avoir pour être conservé lors de l'énumération.
#'  La valeur \code{Inf} par défaut correspond à une recherche de motifs sans limite maximale de taille.
#' @param status_limit Intervalle de temps pour lequel caractériser le statut des motifs par rapport à la période totale d'observations (nombre d'années).
#' @return Nouvel objet de classe \code{\link{SpectralAnalyzer}}.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link[arules:ASparameter-class]{arules::APparameter}}.
#' @aliases spectral.analyzer
#' @export
spectral.analyzer = function(observations, items = NULL, target = "closed frequent itemsets", count = 1, min_length = 1, max_length = Inf, status_limit = 2) {
  
  # Installation des packages nécessaires au fonctionnement
  # Utile uniquement si les fonctions sont chargées sans charger le package (mode dev)
  packages = c("arules", "network", "sna")
  new_packages = packages[!(packages %in% installed.packages()[, "Package"])]
  
  if(length(new_packages) != 0) { 
    cat("Installing required packages:", paste(new_packages, collapse = ", "), "\n")
    install.packages(new_packages)
  }
  
  # Instanciation avec ou sans la liste des items et des catégories associées
  ifelse(is.null(items),
    return(new(Class = "SpectralAnalyzer", observations = observations, target = target, count = count, min_length = min_length, max_length = max_length, status_limit = status_limit)),
    return(new(Class = "SpectralAnalyzer", observations = observations, items = items, target = target, count = count, min_length = min_length, max_length = max_length, status_limit = status_limit)))
}


# Déclaration de la méthode de (ré)initialisation de l'analyseur spectral
setGeneric(name = "reset", def = function(object, from = 1){ standardGeneric("reset") })

#' Reconstruit en partie un analyseur spectral
#' 
#' Redéfinit les attributs d'un analyseur à partir d'une étape spécifique.
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @param from Étape à partir de laquelle recalculer les attributs.
#' 
#' @author Gauthier Magnin
#' @aliases reset
#' @export
setMethod(f = "reset",
          signature = "SpectralAnalyzer",
          definition = function(object, from = 1) {
            
            # Nom de l'objet pour modification interne dans l'environnement parent
            object_name = deparse(substitute(object))
            
            # Matrice des instructions à effectuer et descriptions, pour chaque étape
            steps = matrix(c(
              
              # Initialisation des attributs utiles à la construction d'un spectrosome des noeuds
                "*** Step 1/10:  Enumeration of separate observations per year... ",
                expression(  list_obs_per_year(object)  ),
                "\n*** Step 2/10:  Enumeration of the nodes and calculation of the number of occurrence... ",
                expression(  list_separate_obs(object)  ),
                "\n*** Step 3/10:  Counting the links between nodes... ",
                expression(  count_links(object, "nodes")  ),
                "\n*** Step 4/10:  Elaboration of links between nodes... ",
                expression(  search_links(object, "nodes")  ),
                
              # Initialisation des attributs utiles à la construction d'un spectre
                "\n*** Step 5/10:  Enumeration of separate patterns... ",
                expression(  list_separate_patterns(object, object@target, object@count,
                                                    object@min_length, object@max_length)  ),
                "\n*** Step 6/10:  Linking nodes to patterns... ",
                expression(  list_patterns_by_obs(object)  ),
                "\n*** Step 7/10:  Characterization of patterns per year... ",
                expression(  list_patterns_per_year(object)  ),
                "\n*** Step 8/10:  Calculation of pattern characteristics... ",
                expression(  compute_patterns_characteristics(object)  ),
                
              # Initialisation des attributs utiles à la construction d'un spectrosome des motifs
                "\n*** Step 9/10:  Counting the links between patterns... ",
                expression(  count_links(object, "patterns")  ),
                "\n*** Step 10/10: Elaboration of links between patterns... ",
                expression(  search_links(object, "patterns")  )
                
              ), ncol = 2, nrow = 10, byrow = TRUE)
            
            # Étapes à réaliser effectivement
            steps_todo = (from <= seq(nrow(steps))) & (!DEBUG_MODE | seq(nrow(steps)) <= UP_TO_STEP)
            
            # Réalisation
            for (i in seq(nrow(steps))) {
              if (steps_todo[i]) {
                cat(steps[i, 1][[1]])
                eval(parse(text = paste("display_time(", steps[i, 2], ")")))
              }
            }
            
            # Redéfinition de l'objet
            assign(object_name, object, envir = parent.frame())
            return(invisible())
          })



#### Méthodes print, show, plot, summary, length ####

# print : affichage en console
setMethod(f = "print",
          signature = "SpectralAnalyzer",
          definition = function(x, ...) {
            cat("SpectralAnalyzer\n")
            print(getSlots("SpectralAnalyzer"))
          })
  
# show : affichage sommaire en console
setMethod(f = "show",
          signature = "SpectralAnalyzer",
          definition = function(object) {
            cat("SpectralAnalyzer\n")
            print(slotNames(object))
          })

# summary : résumé de l'objet
setMethod(f = "summary",
          signature = "SpectralAnalyzer",
          definition = function(object, ...) {
            
            # Résumé des caractéristiques des motifs
            summaries = list()
            summaries[["year"]] = summary(object@patterns$year)
            summaries[["frequency"]] = summary(object@patterns$frequency)
            summaries[["weight"]] = summary(object@patterns$weight)
            
            summaries[["order"]] = as.data.frame(table(object@patterns$order))
            colnames(summaries[["order"]]) = c("order", "count")
            
            summaries[["specificity"]] = summary(object@patterns$specificity)
            
            summaries[["status"]] = as.data.frame(table(object@patterns$status))
            colnames(summaries[["status"]]) = c("status", "count")
            
            return(summaries)
          })



#### Sélecteurs et mutateurs ####

# Sélecteurs généraux
setMethod(f = "[",
          signature = "SpectralAnalyzer",
          definition = function(x, i, j, drop) {
            switch(EXPR = i,
                   "observations" = { return(x@observations) },
                   "items" = { return(x@items) },
                   "items_categories" = { return(x@items_categories) },
                   "target" = { return(x@target) },
                   "count" = { return(x@count) },
                   "min_length" = { return(x@min_length) },
                   "max_length" = { return(x@max_length) },
                   "status_limit" = { return(x@status_limit) },
                   "nodes_per_year" = { return(x@nodes_per_year) },
                   "nodes" = { return(x@nodes) },
                   "n_links" = { return(x@n_links) },
                   "nodes_links" = { return(x@nodes_links) },
                   "obs_patterns" = { return(x@obs_patterns) },
                   "patterns_per_year" = { return(x@patterns_per_year) },
                   "patterns" = { return(x@patterns) },
                   "p_links" = { return(x@p_links) },
                   "patterns_links" = { return(x@patterns_links) },
                   "Class" = { return(x@Class) },
                   stop("Unknown attribute."))
          })

# Mutateurs généraux
setReplaceMethod(f = "[",
                 signature = "SpectralAnalyzer",
                 definition = function(x, i, j, value) {
                   switch(EXPR = i,
                          "observations" = { x@observations = value },
                          "items" = { x@items = value },
                          "items_categories" = { x@items_categories = value },
                          "target" = { x@target = value },
                          "count" = { x@count = value },
                          "min_length" = { x@min_length = value },
                          "max_length" = { x@max_length = value },
                          "status_limit" = { x@status_limit = value },
                          "nodes_per_year" = { x@nodes_per_year = value },
                          "nodes" = { x@nodes = value },
                          "n_links" = { x@n_links = value },
                          "nodes_links" = { x@nodes_links = value },
                          "obs_patterns" = { x@obs_patterns = value },
                          "patterns_per_year" = { x@patterns_per_year = value },
                          "patterns" = { x@patterns = value },
                          "p_links" = { x@p_links = value },
                          "patterns_links" = { x@patterns_links = value },
                          "Class" = { stop("Final attribute.") },
                          stop("Unknown attribute."))
                   
                   validObject(x)
                   return(x)
                 })



#### Déclaration des méthodes ####

# Méthodes de calculs utiles à la construction des noeuds

setGeneric(name = "list_obs_per_year", def = function(object){ standardGeneric("list_obs_per_year") })

setGeneric(name = "list_separate_obs", def = function(object){ standardGeneric("list_separate_obs") })


# Méthodes de calculs utiles à la construction d'un spectrosome

setGeneric(name = "count_links", def = function(object, entities){ standardGeneric("count_links") })

setGeneric(name = "search_links", def = function(object, entities){ standardGeneric("search_links") })


# Méthodes de calculs utiles à la construction des motifs

setGeneric(name = "list_separate_patterns", def = function(object, target, count = 1, min_length = 1, max_length = Inf){ standardGeneric("list_separate_patterns") })

setGeneric(name = "list_patterns_by_obs", def = function(object){ standardGeneric("list_patterns_by_obs") })

setGeneric(name = "list_patterns_per_year", def = function(object){ standardGeneric("list_patterns_per_year") })

setGeneric(name = "compute_patterns_characteristics", def = function(object){ standardGeneric("compute_patterns_characteristics") })

setGeneric(name = "compute_specificity", def = function(object, patterns, frequencies, weights){ standardGeneric("compute_specificity") })

setGeneric(name = "compute_reporting_indexes", def = function(object, patterns, t = NULL, period = Inf){ standardGeneric("compute_reporting_indexes") })

setGeneric(name = "check_params_for_RI", def = function(object, t, period){ standardGeneric("check_params_for_RI") })

setGeneric(name = "compute_reporting_indexes_limits", def = function(object, patterns, first_limit, t = NULL, period = Inf){ standardGeneric("compute_reporting_indexes_limits") })

setGeneric(name = "compute_ksi_threshold", def = function(object, reporting_indexes){ standardGeneric("compute_ksi_threshold") })

setGeneric(name = "compute_ri_threshold", def = function(object, reporting_indexes, ksi = NULL){ standardGeneric("compute_ri_threshold") })

setGeneric(name = "define_dynamic_status", def = function(object, patterns, status_limit, t = NULL, period = Inf){ standardGeneric("define_dynamic_status") })


# Méthodes de création de graphiques de type spectre

setGeneric(name = "spectrum_chart", def = function(object, patterns_characteristics, path = getwd(), name = "spectrum_of_patterns.pdf", title = "Spectrum of patterns"){ standardGeneric("spectrum_chart") })

setGeneric(name = "plot_spectrum_chart", def = function(object, patterns_characteristics, weights_by_node_type, title = "Spectrum of patterns"){ standardGeneric("plot_spectrum_chart") })

setGeneric(name = "compute_pattern_distribution_in_nodes", def = function(object, patterns){ standardGeneric("compute_pattern_distribution_in_nodes") })


# Méthodes de création de graphiques de type spectrosome et de calcul d'indicateurs relatifs

setGeneric(name = "spectrosome_chart", def = function(object, entities, characteristics, nb_graphs = 1, min_link_weight = 1, vertex_size = "relative", vertex_col = "status", clusters = Inf, highlight = 3, use_names = TRUE, n.cutoff = NULL, c.cutoff = NULL, path = getwd(), name = paste0("spectrosome_of_", entities, ".png"), title = paste0("Network of ", entities), ...){ standardGeneric("spectrosome_chart") })

setGeneric(name = "cluster_text", def = function(object, graph, links, display = Inf, highlight = 3, use_names = TRUE, cutoff = NULL){ standardGeneric("cluster_text") })

setGeneric(name = "cluster_chart", def = function(object, entities, characteristics, item, use_name = TRUE, n.cutoff = NULL, vertex_size = "relative", vertex_col = "status", c.cutoff = NULL, path = getwd(), name = paste0(substr(entities, 1, nchar(entities) - 1), "_cluster_of_", item, ".png"), title = paste(cap(substr(entities, 1, nchar(entities) - 1)), "cluster of", item), ...){ standardGeneric("cluster_chart") })

setGeneric(name = "network_density", def = function(object, links){ standardGeneric("network_density") })

setGeneric(name = "degree", def = function(object, ID, links){ standardGeneric("degree") })


# Méthodes de création de graphiques de type arbre de la multi-association

setGeneric(name = "tree_chart", def = function(object, patterns_characteristics, use_names = TRUE, n.cutoff = NULL, display_status = TRUE, display_text = "ID", c.cutoff = NULL, path = getwd(), name = "multi-association_tree.pdf", title = "Multi-association tree"){ standardGeneric("tree_chart") })

setGeneric(name = "plot_tree_chart", def = function(object, patterns_characteristics, items_category, category = NULL, c.cutoff = NULL, use_names = TRUE, n.cutoff = NULL, display_status = TRUE, display_text = "ID", title = "Multi-association tree"){ standardGeneric("plot_tree_chart") })


# Méthodes de recherche et d'enregistrement

setGeneric(name = "save_characteristics", def = function(object, entities, characteristics, ...){ standardGeneric("save_characteristics") })

setGeneric(name = "extract_patterns_from_items", def = function(object, patterns_characteristics, items, target = "all"){ standardGeneric("extract_patterns_from_items") })

setGeneric(name = "extract_patterns_from_characteristic", def = function(object, patterns_characteristics, characteristic, value, condition = "EQ"){ standardGeneric("extract_patterns_from_characteristic") })

setGeneric(name = "extract_patterns_from_status", def = function(object, patterns_characteristics, value, condition = "EQ"){ standardGeneric("extract_patterns_from_status") })

setGeneric(name = "extract_patterns_from_category", def = function(object, patterns_characteristics, category, value, target){ standardGeneric("extract_patterns_from_category") })

setGeneric(name = "check_acces_for_category", def = function(object, category, value){ standardGeneric("check_acces_for_category") })

setGeneric(name = "extract_nodes_from_items", def = function(object, nodes_characteristics, items, target = "all"){ standardGeneric("extract_nodes_from_items") })

setGeneric(name = "extract_nodes_from_characteristic", def = function(object, nodes_characteristics, characteristic, value, condition = "EQ"){ standardGeneric("extract_nodes_from_characteristic") })

setGeneric(name = "extract_nodes_from_category", def = function(object, nodes_characteristics, category, value, target){ standardGeneric("extract_nodes_from_category") })

setGeneric(name = "extract_links", def = function(object, entities, characteristics){ standardGeneric("extract_links") })



#### Méthodes de calculs utiles à la construction des noeuds ####

#' Dénombrement des observations par année
#' 
#' Identifie les observations distinctes par année et calcule le nombre de recrutements
#' de chacune de ces observations.
#' La matrice résultante est assignée à l'attribut \code{nodes_per_year}.
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @return Invisible. Matrice du nombre d'occurrences de chaque observation distincte, par année.
#'  Les lignes correspondent aux observations. Les colonnes correspondent aux années.
#' 
#' @author Gauthier Magnin
#' @aliases list_obs_per_year
#' @keywords internal
setMethod(f = "list_obs_per_year",
          signature = "SpectralAnalyzer",
          definition = function(object){
            
            # Nom de l'objet pour modification interne dans l'environnement parent
            object_name = deparse(substitute(object))
            
            
            # Conversion de la liste d'observations en une data.frame (et tri des items de chaque observation)
            obs_df = data.frame(year = sapply(object@observations, "[[", "YEAR"))
            obs_df$node = sapply(sapply(object@observations, "[[", "CODE"), sort)
            
            # Concaténation des identifiants des items (nécessaire pour la fonction "table" et un tri plus rapide)
            obs_df$node = sapply(obs_df$node, paste0, collapse = "/")
            
            # Calcul de la distribution des ensembles distincts d'items par année et conversion en matrice
            nodes_df = as.data.frame(table(obs_df), stringsAsFactors = FALSE)
            nodes_mat = with(nodes_df, tapply(Freq, list(node, year), sum))
            
            # Redécomposition des items composant chaque noeud pour pouvoir calculer leur longueur
            nodes = strsplit(rownames(nodes_mat), split = "/")
            
            # Tri par longueur et poids total décroissants puis par ordre alphanumérique
            the_order = order(sapply(nodes, length),
                              rowSums(nodes_mat),
                              order(order(rownames(nodes_mat), decreasing = TRUE)),
                              decreasing = TRUE)
            nodes_mat = nodes_mat[the_order, ]
            rownames(nodes_mat) = nodes[the_order]
            
            # Définition de l'attribut et retour
            object@nodes_per_year = nodes_mat
            assign(object_name, object, envir = parent.frame())
            return(invisible(nodes_mat))
          })


#' Énumération des nœuds
#' 
#' Identifie les observations distinctes et calcule la taille et le nombre de recrutements
#' de chacune de ces observations.
#' La data frame résultante est assignée à l'attribut \code{nodes}.
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @return Invisible. Data frame des observations distinctes et de leurs caractéristiques
#'  (longueur et poids).
#' 
#' @author Gauthier Magnin
#' @aliases list_separate_obs
#' @keywords internal
setMethod(f = "list_separate_obs",
          signature = "SpectralAnalyzer",
          definition = function(object){
            
            # Nom de l'objet pour modification interne dans l'environnement parent
            object_name = deparse(substitute(object))
            
            # Poids des noeuds par année
            nodes_per_year = object@nodes_per_year
            
            # Calcul du poids total pour chaque noeud (= chaque observation distincte)
            nodes_df = data.frame("weight" = unname(rowSums(nodes_per_year)))
            nodes_df$node = lapply(strsplit(rownames(nodes_per_year), 'c\\("|", "|")'),
                                   function(node) {
                                     if (length(node) > 1) { return(node[-1]) }
                                     return(node)
                                   })
            
            # Calcul de la longueur de chaque noeud et réordonnement des colonnes
            nodes_df$length = sapply(nodes_df$node, length)
            nodes_df = nodes_df[, c("node", "length", "weight")]
            
            # Tri par longueur et poids décroissants puis par ordre alphanumérique
            nodes_df = nodes_df[order(nodes_df$length,
                                      nodes_df$weight,
                                      order(order(sapply(nodes_df$node, paste0, collapse = "/"), decreasing = TRUE)),
                                      decreasing = TRUE), ]
            rownames(nodes_df) = NULL
            
            # Définition de l'attribut et retour
            object@nodes = nodes_df
            assign(object_name, object, envir = parent.frame())
            return(invisible(nodes_df))
          })



#### Méthodes de calculs utiles à la construction d'un spectrosome ####

#' Comptage des liens
#' 
#' Compte le nombre d'éléments en commun entre chaque nœud ou motif.
#' La matrice résultante est assignée à l'attribut \code{n_links} ou \code{p_links} respectivement.
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @param entities Type d'élément pour lequel compter les liens (nœuds ou motifs).
#'  Choix parmi \code{"nodes"}, \code{"patterns"}.
#' @return Invisible. Matrice d'adjacence : matrice du nombre de liens entre chaque nœud ou motif.
#' 
#' @author Gauthier Magnin
#' @aliases count_links
#' @keywords internal
setMethod(f = "count_links",
          signature = "SpectralAnalyzer",
          definition = function(object, entities){
            
            # Nom de l'objet pour modification interne dans l'environnement parent
            object_name = deparse(substitute(object))
            
            # Ensemble des noeuds ou motifs
            if (entities == "nodes") to_link = object@nodes$node
            else if (entities == "patterns") to_link = object@patterns$pattern
            else stop("entities must be \"nodes\" or \"patterns\".")
            
            # Compte le nombre d'items en commun pour chaque paire d'éléments à lier
            names(to_link) = sapply(to_link, paste0, collapse = "/")
            n_intersections = crossprod(table(stack(to_link)))
            
            # Nommage des colonnes et lignes par les itemsets correspondants
            dimnames(n_intersections) = NULL
            colnames(n_intersections) = rownames(n_intersections) = to_link
            
            # Minimisation de la taille en mémoire
            class(n_intersections) = "integer"
            
            # Définition de l'attribut et retour
            if (entities == "nodes") object@n_links = n_intersections
            else if (entities == "patterns") object@p_links = n_intersections
            assign(object_name, object, envir = parent.frame())
            return(invisible(n_intersections))
          })


#' Élaboration des liens
#' 
#' Identifie les liens selon les éléments en commun, entre les nœuds ou les motifs.
#' La data frame résultante est assignée à l'attribut \code{nodes_links} ou \code{patterns_links} respectivement.
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @param entities Type d'élément pour lequel compter les liens (nœuds ou motifs).
#'  Choix parmi \code{"nodes"}, \code{"patterns"}.
#' @return Invisible. Data frame détaillant les liens entre les paires de nœuds ou de motifs.
#'  Les nœuds ou motifs isolés (c'est-à-dire sans lien avec aucune autre entité) apparaîssent au bas
#'  de la data frame.
#' 
#' @author Delphine Bosson-Rieutort, Gauthier Magnin
#' @aliases search_links
#' @keywords internal
setMethod(f = "search_links",
          signature = "SpectralAnalyzer",
          definition = function(object, entities){
            
            # Nom de l'objet pour modification interne dans l'environnement parent
            object_name = deparse(substitute(object))
            
            # Ensemble des noeuds ou motifs
            if (entities == "nodes") {
              to_link = object@nodes$node
              entities_links = object@n_links
            } else if (entities == "patterns") {
              to_link = object@patterns$pattern
              entities_links = object@p_links
            } else stop("entities must be \"nodes\" or \"patterns\".")
            
            
            # Recherche des indices des éléments liés
            linked_indexes = which(apply(entities_links, 1,
                                         function(x) sum(x) != x[parent.frame()$i[]]))
            names(linked_indexes) = NULL
            
            # Utilisation de la propriété de symétrie de la matrice pour compter le nombre de liens
            nb_links = (sum(entities_links != 0) - nrow(entities_links)) / 2
            links = matrix(NA, nrow = nb_links, ncol = ifelse(entities == "patterns", 5, 4))
            
            link_counter = 0
            loop_index = 0
            
            # Recherche des liens entre chaque paire d'éléments à lier
            for(i in linked_indexes[1:(length(linked_indexes) - 1)]) {
              loop_index = loop_index + 1
              
              for(j in linked_indexes[(loop_index + 1):length(linked_indexes)]) {
                if (entities_links[i, j] != 0) {
                  # Nouveau lien identifié
                  link_counter = link_counter + 1
                  intersection = to_link[[j]][to_link[[j]] %in% to_link[[i]]]
                  
                  # Élément i, élément j, numéro de lien, items en communs, nb items en communs (, année d'apparition du lien)
                  if (entities == "patterns") {
                    links[link_counter, ] = c(i, j,
                                              paste(intersection, collapse = "/"), entities_links[i, j],
                                              max(object@patterns[i, "year"], object@patterns[j, "year"]))
                  } else {
                    links[link_counter, ] = c(i, j, paste(intersection, collapse = "/"), entities_links[i, j])
                  }
                }
              }
            }
            
            
            # Recherche des éléments isolés
            isolated_indexes = which(apply(entities_links, 1,
                                           function(x) sum(x) == x[parent.frame()$i[]]))
            names(isolated_indexes) = NULL
            
            # Matrice des éléments isolés qui complète celle des paires de éléments liés
            if (length(isolated_indexes) != 0) {
              no_links = t(sapply(isolated_indexes, entity = entities,
                                  function(x, entity) {
                                    if (entity == "patterns") {
                                      return(c(x, x, "", 0, object@patterns[parent.frame()$i[], "year"]))
                                    }
                                    return(c(x, x, "", 0))
                                  }))
            } else {
              # Matrice vide pour la fusion qui suit (sans avoir à tester aucune des deux)
              no_links = matrix(NA, nrow = 0, ncol = ifelse(entities == "patterns", 5, 4))
            }
            
            
            # Fusion des listes en une data frame unique
            merged = as.data.frame(rbind(links, no_links), stringsAsFactors = FALSE)
            
            # Affectation de noms de colonnes et rétablissement des types
            if (entities == "patterns") {
              colnames(merged) = c("endpoint.1", "endpoint.2", "items", "weight", "year")
              class(merged$year) = "integer"
            } else {
              colnames(merged) = c("endpoint.1", "endpoint.2", "items", "weight")
            }
            rownames(merged) = NULL
            class(merged$endpoint.1) = class(merged$endpoint.2) = class(merged$weight) = "integer"
            
            
            # Définition de l'attribut et retour
            if (entities == "nodes") object@nodes_links = merged
            else if (entities == "patterns") object@patterns_links = merged
            assign(object_name, object, envir = parent.frame())
            return(invisible(object@nodes_links))
          })



#### Méthodes de calculs utiles à la construction des motifs ####

#' Énumération des motifs
#' 
#' Identifie l'ensemble des motifs distincts générés à partir des observations.
#' La data frame résultante est assignée à l'attribut \code{patterns}.
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @param target Type de motifs à énumérer.
#'  Choix parmi \code{"frequent itemsets"}, \code{"closed frequent itemsets"}, \code{"maximally frequent itemsets")}.
#' @param count Nombre minimal d'apparition d'un motif pour être considéré comme "fréquent", et par conséquent, conservé.
#' @param min_length Taille minimale qu'un motif doit avoir pour être conservé lors de l'énumération.
#' @param max_length Taille maximale qu'un motif doit avoir pour être conservé lors de l'énumération.
#'  La valeur \code{Inf} par défaut correspond à une recherche de motifs sans limite maximale de taille.
#' @return Invisible. Data frame dans laquelle une ligne est une association entre un motif et sa
#'  fréquence dans l'ensemble des observations.
#' 
#' @author Gauthier Magnin
#' @seealso [arules:ASparameter-class]{arules::APparameter}
#' @aliases list_separate_patterns
#' @keywords internal
setMethod(f = "list_separate_patterns",
          signature = "SpectralAnalyzer",
          definition = function(object, target, count = 1, min_length = 1, max_length = Inf) {
            
            # Nom de l'objet pour modification interne dans l'environnement parent
            object_name = deparse(substitute(object))
            
            
            # Liste des items retrouvés pour chaque observation et vecteurs des identifiants des items
            data = sapply(sapply(object@observations, "[", "CODE"), as.character)
            labels = as.character(object@items)
            
            # Transformation en objet itemMatrix puis en objet transaction : une ligne par observation, une colonne par item
            item_matrix = arules::encode(data, labels)
            transact = as(item_matrix, "transactions")
            
            # Énumération des motifs recherchés
            params = list(supp = count/dim(transact)[1], 
                          minlen = min_length,
                          maxlen = ifelse(max_length == Inf, dim(transact)[2], max_length), 
                          target = target)
            invisible(capture.output({
              result <- arules::eclat(transact, parameter = params)
              res <- arules::inspect(result, linebreak = FALSE) # Permet aussi d'obtenir le support
            }))
            
            # Exraction des motifs issus du résultat puis transformation
            patterns = res$items
            patterns = tapply(patterns, seq_along(patterns), as.character)
            patterns = tapply(patterns, seq_along(patterns), function(x) {
                                substr(x, start = 2, stop = nchar(x) - 1)
                              })
            patterns = strsplit(patterns, ",")
            patterns = lapply(patterns, as.character)
            
            # Rassemblement des motifs dans une data.frame
            patterns_df = data.frame(pattern = numeric(length(patterns)))
            patterns_df$pattern = sapply(patterns, c)
            patterns_df$weight = res$count
            
            # Tri et renommage des lignes selon le nouvel ordre de la data.frame
            patterns_df = patterns_df[order(patterns_df$weight, decreasing = TRUE), ]
            rownames(patterns_df) = seq(nrow(patterns_df))
            
            # Définition de l'attribut et retour
            object@patterns = patterns_df
            assign(object_name, object, envir = parent.frame())
            return(invisible(patterns_df))
          })


#' Liaison des nœuds aux motifs
#' 
#' Associe à chaque observation distincte (c'est-à-dire, chaque nœud) les motifs qui y sont inclus.
#' La matrice résultante est assignée à l'attribut \code{obs_patterns}.
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @return Invisible. Matrice de booléens dans laquelle les lignes correspondent aux observations
#'  et les colones aux motifs.
#'  Une case ayant la valeur \code{TRUE} signifie que le motif est inclus dans l'observation (ou le nœud).
#' 
#' @author Gauthier Magnin
#' @aliases list_patterns_by_obs
#' @keywords internal
setMethod(f = "list_patterns_by_obs",
          signature = "SpectralAnalyzer",
          definition = function(object) {
            
            # Nom de l'objet pour modification interne dans l'environnement parent
            object_name = deparse(substitute(object))
            
            
            # Pour chaque motif et chaque noeud, recherche si le motif est inclus dans le noeud
            associations = sapply(object@patterns$pattern,
                                  function(pattern) {
                                    sapply(object@nodes$node, pattern = pattern,
                                           function(node, pattern) {
                                             return(all(pattern %in% node))
                                           })
                                  })
            
            # Renommage des lignes et colonnes
            rownames(associations) = object@nodes$node
            colnames(associations) = object@patterns$pattern
            
            # Définition de l'attribut et retour
            object@obs_patterns = associations
            assign(object_name, object, envir = parent.frame())
            return(invisible(object@obs_patterns))
          })


#' Dénombrement des motifs par année
#' 
#' Compte le nombre d'apparitions de chaque motif selon l'année.
#' La matrice résultante est assignée à l'attribut \code{patterns_per_year}.
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @return Invisible. Matrice des poids de chaque motif, par année.
#'  Les lignes correspondent aux motifs. Les colonnes correspondent aux années.
#' 
#' @author Gauthier Magnin
#' @aliases list_patterns_per_year
#' @keywords internal
setMethod(f = "list_patterns_per_year",
          signature = "SpectralAnalyzer",
          definition = function(object) {
            
            # Nom de l'objet pour modification interne dans l'environnement parent
            object_name = deparse(substitute(object))
            
            # Poids des noeuds par année
            nodes_per_year = object@nodes_per_year
            
            # Calcul des poids par année pour chaque motif
            weights = lapply(seq_along(object@patterns$pattern), function(p) {
              # Sélection des noeuds associées au motif
              nodes_names = object@obs_patterns[, p]
              nodes = nodes_per_year[rownames(nodes_per_year) %in% names(nodes_names[nodes_names]), ]
              
              # Somme des poids selon l'année
              if (is.matrix(nodes)) { return(colSums(nodes)) }
              return(nodes)
            })
            
            # Matrice des poids des motifs par année
            ppy = do.call(rbind, weights)
            rownames(ppy) = object@patterns$pattern
            
            # Définition de l'attribut et retour
            object@patterns_per_year = ppy
            assign(object_name, object, envir = parent.frame())
            return(invisible(ppy))
          })


#' Calcul des caractéristiques des motifs
#' 
#' Calcule les caractéristiques des différents motifs (fréquence, poids, ordre, spécificté, caractère dynamique).
#' La data frame résultante est assignée à l'attribut \code{patterns}.
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @return Invisible. Data frame dans laquelle une ligne est une association entre un motif
#'  et ses caractéristiques.
#' 
#' @author Gauthier Magnin
#' @references Bosson-Rieutort D, de Gaudemaris R, Bicout DJ (2018).
#'             \emph{The spectrosome of occupational health problems}. PLoS ONE 13(1): e0190196.
#'             \url{https://doi.org/10.1371/journal.pone.0190196}.
#' @aliases compute_patterns_characteristics
#' @keywords internal
setMethod(f = "compute_patterns_characteristics",
          signature = "SpectralAnalyzer",
          definition = function(object) {
            
            # Nom de l'objet pour modification interne dans l'environnement parent
            object_name = deparse(substitute(object))
            
            # Nombre de noeuds contenant le motif
            frequencies = tapply(seq_along(object@patterns$pattern), seq_along(object@patterns$pattern),
                                 function(p) {
                                   sum(object@obs_patterns[, p])
                                 })
            
            # Association de nouvelles caractéristiques aux motifs
            object@patterns$frequency = frequencies
            object@patterns$order = sapply(object@patterns$pattern, length)
            object@patterns$year = apply(object@patterns_per_year, 1, function(x) {
                                                                        # Année d'apparition du motif
                                                                        as.numeric(names(x[x > 0])[1])
                                                                      })
            
            # Calcul de la spécificité et du statut dynamique de chaque motif
            specificity = compute_specificity(object, object@patterns$pattern, object@patterns$frequency, object@patterns$weight)
            object@patterns$specificity = specificity
            object@patterns$status = define_dynamic_status(object, object@patterns$pattern, object@status_limit)$status
            
            # Changement de l'ordre des colonnes
            object@patterns = object@patterns[, c("pattern", "year", "frequency", "weight", "order", "specificity", "status")]
            
            # Définition de l'attribut et retour
            assign(object_name, object, envir = parent.frame())
            return(invisible(object@patterns))
          })


#' Calcul de spécificité
#' 
#' Calcule la spécificité de l'information portée par chaque motif.
#' La spécificité correspond au caractère d'un motif spécifique d'une combinaison particulière
#'  ou ubiquitaire et permettant la formation de nombreux agrégats.
#'  
#' @param object Objet de classe SpectralAnalyzer.
#' @param patterns Liste des motifs dont la spécificité est à calculer.
#' @param frequencies Vecteur des fréquences associées aux motifs contenus dans \code{patterns}.
#' @param weights Vecteur des poids associées aux motifs contenus dans \code{patterns}.
#' @return Vecteur contenant la spécificité de chaque motif contenu dans \code{patterns}.
#' 
#' @author Gauthier Magnin
#' @references Bosson-Rieutort D, de Gaudemaris R, Bicout DJ (2018).
#'             \emph{The spectrosome of occupational health problems}. PLoS ONE 13(1): e0190196.
#'             \url{https://doi.org/10.1371/journal.pone.0190196}.
#' @aliases compute_specificity
#' @keywords internal
setMethod(f = "compute_specificity",
          signature = "SpectralAnalyzer",
          definition = function(object, patterns, frequencies, weights) {
            
            # Article d'où sont tirées la formule et la notation : cf. doi.org/10.1371/journal.pone.0190196
            u = frequencies
            w = weights
            h = numeric(length(patterns))
            
            # Pour chaque motif
            for (i in seq_along(patterns)) {
              # Recherche des poids des noeuds qui contiennent le motif
              a = object@nodes$weight[object@obs_patterns[, i]]
              
              # Calcul de h
              h_m = -1 * sum((a / w[i] * log(a / w[i])))
              h[i] = h_m
            }
            
            # Calcul de la spécificité de chaque motif
            h_max = max(log(u))
            h_min = min(log(w / (w - u + 1)) + ((u - 1) / w * log(w - u + 1)))
            specificity = (h_max - h) / (h_max - h_min)
            
            # Les spécificités calculées valent NaN si les motifs n'apparaîssent que dans un seul noeud
            nan = is.nan(specificity)
            if (any(nan)) {
              # Rétablissement de la spécificité à 1
              specificity[nan] = 1
            }
            
            return(specificity)
          })


#' Calcul d'indice de recrutement
#' 
#' Calcule l'indice de recrutement de chaque motif pour une période donnée.
#' Cet indice informe sur la proportion et l'importance du recrutement d'un motif en tenant compte
#'  du recrutement des autres motifs.
#'  
#' @param object Objet de classe SpectralAnalyzer.
#' @param patterns Liste des motifs dont l'indice de recrutement est à calculer.
#' @param t Année de fin de la période, c'est-à-dire, date à laquelle caractériser le motif.
#'  \code{NULL} indique que la caractérisation doit se faire par rapport à la dernière année
#'  couverte par les observations.
#' @param period Intervalle de temps sur lequel calculer l'indice de recrutement (nombre d'années).
#'  Si \code{t = 2015} et \code{period = 2}, alors le calcul est fait sur la période [2014 - 2015].
#'  \code{Inf} indique que la période considérée couvre un intervalle commençant à la date de la 
#'  plus ancienne observation et terminant à l'année \code{t}.
#' @return Data frame associant un indice de recrutement à chaque motif.
#' 
#' @author Gauthier Magnin
#' @references Bosson-Rieutort D, de Gaudemaris R, Bicout DJ (2018).
#'             \emph{The spectrosome of occupational health problems}. PLoS ONE 13(1): e0190196.
#'             \url{https://doi.org/10.1371/journal.pone.0190196}.
#' @seealso \code{\link{compute_reporting_indexes_limits}}.
#' @aliases compute_reporting_indexes
#' @keywords internal
setMethod(f = "compute_reporting_indexes",
          signature = "SpectralAnalyzer",
          definition = function(object, patterns, t = NULL, period = Inf) {
            
            # Valeurs ajustées des paramètres
            params = check_params_for_RI(object, t, period)
            t = params$t
            period = params$period
            
            # Années maximale et minimale
            max_year = as.numeric(rev(colnames(object@nodes_per_year))[1])
            min_year = as.numeric(colnames(object@nodes_per_year)[1])
            
            # Liste des motifs et ensembles des motifs par année
            ppy = object@patterns_per_year
            ri = numeric(length(patterns))
            
            # Si la période sur laquelle calculer RI correspond à celle couverte par l'ensemble des observations
            if (t == max_year && (t - period + 1) == min_year) {
              
              ri_denominator = sum(ppy) # Somme des poids de tous les motifs sur toute la période
              
              for (i in seq_along(patterns)) {
                ri_numerator = sum(ppy[i, ]) # Poids total du motif sur toute la période
                ri[i] = ri_numerator / ri_denominator
              }
            } else {
              
              # Année de début = année de fin - nombre d'années considérées + 1
              t0 = t - period + 1
              
              ri_denominator = 0 # Somme des poids de tous les motifs sur la période recherchée
              ri_numerator = numeric(length(ri)) # Somme du poids total de chaque motif sur la période
              
              # Calcul du dénominateur (indépendant du motif) et des numérateurs (un par motif)
              for (i in seq_along(patterns)) {
                ri_numerator[i] = sum(ppy[i, (as.numeric(colnames(ppy)) <= t)
                                             & (as.numeric(colnames(ppy)) >= t0)])
                ri_denominator = ri_denominator + ri_numerator[i]
              }
              
              # Calcul du Reporting Index de chaque motif
              ri = ri_numerator / ri_denominator
            }
            
            # Retour
            ri = data.frame(ri)
            ri$pattern = patterns
            ri = ri[c("pattern", "ri")]
            
            return(ri)
          })


#' Validation de paramètres de calcul de RI
#' 
#' Vérifie la validité des valeurs des paramètres donnés en arguments pour le calcul d'indices de recrutement.
#' Adapte leurs valeurs si elles n'entrent pas dans l'intervalle adéquate et affiche un message d'information.
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @param t Année de fin de la période, c'est-à-dire, date à laquelle caractériser le motif.
#'  \code{NULL} indique que la caractérisation doit se faire par rapport à la dernière année
#'  couverte par les observations.
#' @param period Intervalle de temps sur lequel calculer l'indice de recrutement (nombre d'années).
#'  Si \code{t = 2015} et \code{period = 2}, alors le calcul est fait sur la période [2014 - 2015].
#'  \code{Inf} indique que la période considérée couvre un intervalle commençant à la date de la 
#'  plus ancienne observation et terminant à l'année \code{t}.
#' @return Liste contenant la valeur finale de \code{t} et celle de \code{period}.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{compute_reporting_indexes}}, \code{\link{compute_reporting_indexes_limits}}.
#' @aliases check_params_for_RI
#' @keywords internal
setMethod(f = "check_params_for_RI",
          signature = "SpectralAnalyzer",
          definition = function(object, t, period) {
            
            # Années maximale et minimale
            max_year = as.numeric(rev(colnames(object@nodes_per_year))[1])
            min_year = as.numeric(colnames(object@nodes_per_year)[1])
            
            # Validation du premier paramètre
            if (is.null(t) || t > max_year) {
              # La date de fin donnée est supérieure à celles considérées par les observations
              if (!is.null(t)) {
                warning("t must be less than or equal to the most recent year among data. Running with t = NULL. See documentation for more details.")
              }
              t = max_year
            }
            else if (t < min_year) {
              # La date de fin donnée est inférieure à celles considérées par les observations
              warning(paste0("t must be greater than or equal to the earliest year among data. Running with t equal to the earliest one (",
                            min_year,
                            "). See documentation for more details."))
              t = min_year
            }
            
            # Validation du second paramètre
            if (period < 1) {
              # L'indice de recrutement ne peut être calculé sur moins d'un an
              warning("period must be greater than 0. Running with period = 1. See documentation for more details.")
              period = 1
            }
            else if (is.infinite(period) || period > t - min_year + 1) {
              # La période demandée excède celle couverte par les observations
              if (!is.infinite(period)) {
                warning("period must not exceed the earliest year among data. Running with period = Inf. See documentation for more details.")
              }
              period = t - min_year + 1
            }
            
            return(list(t = t, period = period))
          })


#' Calcul de RI à des limites temporelles
#' 
#' Calcule les indices de recrutement aux limites temporelles utilisées pour caractériser les motifs.
#' La première correspond à l'indice de recrutement calculé sur \code{first_limit} années.
#' La seconde correspond à l'indice de recrutement calculé sur la période définie par les paramètres \code{t} et \code{period}.
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @param patterns Liste des motifs dont les limites sont à calculer.
#' @param first_limit Intervalle de temps sur lequel calculer la première limite (nombre d'années).
#' @param t Année de fin de la période, c'est-à-dire, date à laquelle caractériser le motif.
#'  \code{NULL} indique que le calcul doit se faire par rapport à la dernière année
#'  couverte par les observations.
#' @param period Intervalle de temps sur lequel faire le calcul (nombre d'années).
#'  Si \code{t = 2015} et \code{period = 2}, alors le calcul est fait sur la période [2014 - 2015].
#'  \code{Inf} indique que la période considérée couvre un intervalle commençant à la date de la 
#'  plus ancienne observation et terminant à l'année \code{t}.
#' @return Data frame associant à chaque motif ses deux limites.
#' 
#' @author Gauthier Magnin
#' @references Bosson-Rieutort D, de Gaudemaris R, Bicout DJ (2018).
#'             \emph{The spectrosome of occupational health problems}. PLoS ONE 13(1): e0190196.
#'             \url{https://doi.org/10.1371/journal.pone.0190196}.
#' @seealso \code{\link{compute_reporting_indexes}}.
#' @aliases compute_reporting_indexes_limits
#' @keywords internal
setMethod(f = "compute_reporting_indexes_limits",
          signature = "SpectralAnalyzer",
          definition = function(object, patterns, first_limit, t = NULL, period = Inf) {
            
            ri_2 = compute_reporting_indexes(object, patterns, t, first_limit)["ri"]
            ri_period = compute_reporting_indexes(object, patterns, t, period)["ri"]
            
            ri_limits = data.frame(ri_2, ri_period)
            colnames(ri_limits) = c("ri_2", "ri_period")
            ri_limits$pattern = patterns
            ri_limits = ri_limits[c("pattern", "ri_2", "ri_period")]
            
            return(ri_limits)
          })


#' Calcul de seuil ksi
#' 
#' Calcule le nombre de motifs permettant d'expliquer l'essentiel des indices de recrutement.
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @param reporting_indexes Indices de recrutement associées aux motifs.
#' @return Seuil calculé.
#' 
#' @author Gauthier Magnin
#' @references Bosson-Rieutort D, de Gaudemaris R, Bicout DJ (2018).
#'             \emph{The spectrosome of occupational health problems}. PLoS ONE 13(1): e0190196.
#'             \url{https://doi.org/10.1371/journal.pone.0190196}.
#' @seealso \code{\link{compute_reporting_indexes}}, \code{\link{compute_reporting_indexes_limits}},
#'          \code{\link{compute_ri_threshold}}.
#' @aliases compute_ksi_threshold
#' @keywords internal
setMethod(f = "compute_ksi_threshold",
          signature = "SpectralAnalyzer",
          definition = function(object, reporting_indexes) {
            
            return(1 / sum(reporting_indexes ^ 2))
          })


#' Calcul de seuil de RI
#' 
#' Calcule la valeur limite séparant deux statuts dynamiques par rapport aux indices de recrutement.
#' Les motifs sont ordonnés par ordre décroissant de leur valeur d'indice de recrutement et séparé 
#' par le seuil \code{ksi}.
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @param reporting_indexes Indices de recrutement associées aux motifs.
#' @param ksi Nombre de motifs à considérer avant de fixer le seuil.
#'  Est calculé si valeur \code{NULL}.
#' @return Seuil calculé.
#' 
#' @author Gauthier Magnin
#' @references Bosson-Rieutort D, de Gaudemaris R, Bicout DJ (2018).
#'             \emph{The spectrosome of occupational health problems}. PLoS ONE 13(1): e0190196.
#'             \url{https://doi.org/10.1371/journal.pone.0190196}.
#' @seealso \code{\link{compute_reporting_indexes}}, \code{\link{compute_reporting_indexes_limits}},
#'          \code{\link{compute_ksi_threshold}}.
#' @aliases compute_ri_threshold
#' @keywords internal
setMethod(f = "compute_ri_threshold",
          signature = "SpectralAnalyzer",
          definition = function(object, reporting_indexes, ksi = NULL) {
            
            # Calcul du seuil ksi si non fourni en paramètre et arrondi à l'entier le plus proche
            if (is.null(ksi)) {
              ksi = compute_ksi_threshold(object, reporting_indexes)
            }
            ksi = round(ksi)
            
            # Extraction de la valeur de RI du ksi_ème élément (ordonnés par RI)
            return(sort(reporting_indexes, decreasing = TRUE)[ksi])
          })


#' Attribution de statut dynamique
#' 
#' Définit le statut dynamique de chaque motif.
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @param patterns Liste des motifs dont le statut dynamique est à définir.
#' @param status_limit Intervalle de temps sur lequel caractériser le statut des motifs
#'  par rapport à la période définie par les paramètres \code{t} et \code{period}.
#' @param t Année de fin de la période, c'est-à-dire, date à laquelle caractériser le motif.
#'  \code{NULL} indique que la caractérisation doit se faire par rapport à la dernière année
#'  couverte par les observations.
#' @param period Intervalle de temps à considérer pour caractériser le motif (nombre d'années).
#'  Si \code{t = 2015} et \code{period = 2}, alors le calcul est fait sur la période [2014 - 2015].
#'  \code{Inf} indique que la période considérée couvre un intervalle commençant à la date de la 
#'  plus ancienne observation et terminant à l'année \code{t}.
#' @return Data frame associant à chaque motif son statut dynamique.
#' 
#' @author Gauthier Magnin
#' @references Bosson-Rieutort D, de Gaudemaris R, Bicout DJ (2018).
#'             \emph{The spectrosome of occupational health problems}. PLoS ONE 13(1): e0190196.
#'             \url{https://doi.org/10.1371/journal.pone.0190196}.
#' @seealso \code{\link{compute_reporting_indexes}}, \code{\link{compute_reporting_indexes_limits}},
#'          \code{\link{compute_ksi_threshold}}, \code{\link{compute_ri_threshold}}.
#' @aliases define_dynamic_status
#' @keywords internal
setMethod(f = "define_dynamic_status",
          signature = "SpectralAnalyzer",
          definition = function(object, patterns, status_limit, t = NULL, period = Inf) {
            
            # Calcul des limites et des 2 seuils associés
            ri_limits = compute_reporting_indexes_limits(object, patterns, status_limit, t, period)
            ri_thresholds = apply(ri_limits[, c("ri_2", "ri_period")], 2,
                               function(column) { compute_ri_threshold(object, column) })
            
            # Mise en évidence des RI ayant une valeur supérieur aux seuils
            substantially_recorded_2 = ri_limits$ri_2 >= ri_thresholds["ri_2"]
            substantially_recorded_period = ri_limits$ri_period >= ri_thresholds["ri_period"]
            
            # Interprétation
            status = rep(NA, length(patterns))
            status[( substantially_recorded_period &  substantially_recorded_2)] = object@Class$STATUS_PERSISTENT
            status[(!substantially_recorded_period &  substantially_recorded_2)] = object@Class$STATUS_DECLINING
            status[( substantially_recorded_period & !substantially_recorded_2)] = object@Class$STATUS_EMERGENT
            status[(!substantially_recorded_period & !substantially_recorded_2)] = object@Class$STATUS_LATENT
            
            # Retour
            status = data.frame(status, stringsAsFactors = FALSE)
            status$pattern = patterns
            status = status[c("pattern", "status")]
            
            return(status)
          })



#### Méthodes de création de graphiques de type spectre ####

#' Spectre des motifs
#' 
#' Construit un graphique de type spectre et l'enregistre dans un fichier au format PDF.
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @param patterns_characteristics Ensemble des caractéristiques des motifs dont le spectre est à tracer.
#' @param path Chemin du dossier dans lequel enregistrer le graphique.
#'  Par défaut, le graphique est enregistré dans le répertoire de travail.
#' @param name Nom du fichier dans lequel enregistrer le graphique.
#' @param title Titre du graphique.
#' @return Data frame des motifs et caractéristiques utilisées, associés aux identifiants visibles sur le graphique.
#' 
#' @author Delphine Bosson-Rieutort, Gauthier Magnin
#' @references Bosson-Rieutort D, de Gaudemaris R, Bicout DJ (2018).
#'             \emph{The spectrosome of occupational health problems}. PLoS ONE 13(1): e0190196.
#'             \url{https://doi.org/10.1371/journal.pone.0190196}.
#' @aliases spectrum_chart
#' @export
setMethod(f = "spectrum_chart",
          signature = "SpectralAnalyzer",
          definition = function(object, patterns_characteristics, path = getwd(), name = "spectrum_of_patterns.pdf", title = "Spectrum of patterns") {
            
            # Attribution d'identifiants aux motifs
            patterns_characteristics$ID = seq(nrow(patterns_characteristics))
            
            # Ensembles des poids et longueurs des noeuds contenant les motifs
            patterns_distributions = compute_pattern_distribution_in_nodes(object, patterns_characteristics$pattern)
            weight_distribution = patterns_distributions[["weight_distribution"]]
            length_distribution = patterns_distributions[["length_distribution"]]
            
            # Tri des motifs selon spécificité, statut, poids, longueur
            sorting_vector = order(1 - patterns_characteristics$specificity,
                                   patterns_characteristics$status,
                                   abs(patterns_characteristics$weight - max(patterns_characteristics$weight)),
                                   patterns_characteristics$order)
            
            patterns_characteristics = patterns_characteristics[sorting_vector, ]
            weight_distribution = weight_distribution[sorting_vector]
            length_distribution = length_distribution[sorting_vector]
            
            # Décomposition des poids des motifs selon le type de noeuds (simple ou complexe)
            weights = data.frame(complex_nodes = sapply(seq(nrow(patterns_characteristics)), function(x) {
              sum(weight_distribution[[x]][which(length_distribution[[x]] > 1)])
            }))
            weights$simple_node = patterns_characteristics$weight - weights$complex_nodes
            
            
            # Traçage du graphique dans un fichier PDF
            pdf(paste0(turn_into_path(path), check_extension(name, "pdf")), 15, 8, pointsize = 10)
            plot_spectrum_chart(object, patterns_characteristics, weights, title)
            dev.off()
            
            # Motifs et caractéristiques, ordonnés selon ID (replacé en 1ère colonne)
            return(patterns_characteristics[order(patterns_characteristics$ID),
                                            c(ncol(patterns_characteristics), seq(ncol(patterns_characteristics)-1))])
          })


#' Spectre des motifs
#' 
#' Dessine un graphique de type spectre.
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @param patterns_characteristics Ensemble des caractéristiques des motifs dont le spectre est à tracer.
#' @param weights_by_node_type Data frame contenant, pour chaque motif, son poids dans des nœuds complexes
#'  et son poids dans des nœuds simples.
#' @param title Titre du graphique.
#' 
#' @author Delphine Bosson-Rieutort, Gauthier Magnin
#' @references Bosson-Rieutort D, de Gaudemaris R, Bicout DJ (2018).
#'             \emph{The spectrosome of occupational health problems}. PLoS ONE 13(1): e0190196.
#'             \url{https://doi.org/10.1371/journal.pone.0190196}.
#' @seealso \code{\link{spectrum_chart}}, \code{\link{compute_pattern_distribution_in_nodes}}.
#' @aliases plot_spectrum_chart
#' @keywords internal
setMethod(f = "plot_spectrum_chart",
          signature = "SpectralAnalyzer",
          definition = function(object, patterns_characteristics, weights_by_node_type, title = "Spectrum of patterns") {
            
            # Définition des couleurs des barres du barplot
            bars_colors = object@Class$STATUS_COLORS[patterns_characteristics$status]
            
            
            ## Bar chart relatif au poids
            par(mfrow = c(1, 1))
            par(mar = c(7.1, 5, 4.1, 5) + .1)
            
            # Diagramme en barres selon le poids des motifs
            las = ifelse(length(patterns_characteristics$pattern) <= 20, 1, 2)
            y_lim_bar = max(patterns_characteristics$weight) * 1.25
            bar_plot = barplot(t(weights_by_node_type), col = NA, space = 0, main = title,
                               xlim = c(-0.5, nrow(patterns_characteristics) + 0.5), xaxs = "i",
                               ylim = c(0, y_lim_bar),
                               lwd = 2, xlab = "Patterns IDs", ylab = "Weight", names.arg = patterns_characteristics$ID,
                               cex.main = 1.3, cex.lab = 1.5, cex.axis = 1.5, cex.names = 0.9, las = las, font.axis = 2)
            bar_width_2 = diff(bar_plot[1:2]) / 2
            
            # Coloration des barres
            for (i in seq(nrow(weights_by_node_type))) {
              y = c(0, cumsum(c(weights_by_node_type[i, ])))
              rect(bar_plot[i] - bar_width_2,  y[ - length(y)],  bar_plot[i] + bar_width_2,  y[ - 1], 
                   col = bars_colors[i], density = c(300, 15), border = "black")
            }
            
            
            ## Line chart relatif à la spécificité
            par(new = TRUE)
            
            # Ligne de la spécificité et seuil
            y_lim_line = 1.039
            plot(x = seq(0.5, nrow(patterns_characteristics) - 0.5),
                 y = patterns_characteristics$specificity,
                 lwd = 3, bty = "n", type = "b", col = "black", pch = 20,
                 xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = "",
                 xlim = c(-0.5, nrow(patterns_characteristics) + 0.5), xaxs = "i",
                 ylim = c(0, y_lim_line), yaxs = "i")
            segments(x0 = 0, x1 = nrow(patterns_characteristics) + 0.5, y0 = 0.5,
                     lwd = 0.5, lty = "dotted")
            
            # Axe et titre à droite
            axis(4, yaxp = c(0, 1, 5), lwd = 2, cex.axis = 1.5, font.axis = 2)
            mtext("Specificity", side = 4, line = 3.1, cex = 1.5)
            
            
            ## Texte relatif à l'ordre des motifs
            # Changement du système de coordonnées du au changement de graphique (bar -> line)
            new_y = patterns_characteristics$weight * y_lim_line / y_lim_bar
            shadowtext(bar_plot, new_y, as.roman(patterns_characteristics$order),
                       col = "black", bg = "white", cex = 0.8, pos = 3, offset = 1)
            
            
            ## Légendes du graphique complet
            legend("bottom", bty = "n", horiz = TRUE, xpd = NA, inset = c(0.06, -0.18),
                   pch = 15, col = object@Class$STATUS_COLORS,
                   legend = names(object@Class$STATUS_COLORS), cex = 1.1)
            legend("topright", bty = "n", xpd = NA, adj = 0, inset = c(0.18, -0.07), pch = "I",
                   legend = "- Order", cex = 1.1)
            legend("topright", bty = "n", xpd = NA, adj = 0, inset = c(0.18, -0.04), pch = 20, lty = 1,
                   legend = "Specificity", cex = 1.1)
            legend("topright", bty = "n", xpd = NA, inset = c(0.02, -0.07), fill = "red", density = c(600, 15),
                   legend = c("Weight in complex nodes", "Weight in simple nodes"), cex = 1.1)
          })


#' Distribution des motifs parmi les nœuds
#' 
#' Calcule les distributions des poids et longueurs des nœuds dans lesquels sont inclus chaque motif.
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @param patterns Liste des motifs dont les distributions sont à calculer.
#' @return Liste contenant :
#'  \describe{
#'    \item{\code{weight_distribution}}{La distribution, pour chaque motif, des poids des nœuds dans lequel ils sont inclus.}
#'    \item{\code{length_distribution}}{La distribution, pour chaque motif, des longueurs des nœuds dans lequel ils sont inclus.}
#'  }
#'  
#' @author Gauthier Magnin
#' @aliases compute_pattern_distribution_in_nodes
#' @keywords internal
setMethod(f = "compute_pattern_distribution_in_nodes",
          signature = "SpectralAnalyzer",
          definition = function(object, patterns) {
            
            # Ensembles des poids et longueurs des noeuds contenant les motifs
            weight_distribution = list()
            length_distribution = list()
            
            # Pour chaque motif
            for (i in seq_along(patterns)) {
              # Ensemble des noeuds contenant le motif
              pat = as.character(patterns[i])
              nodes = object@nodes[object@obs_patterns[, pat], ]
              
              weight_distribution[[i]] = nodes$weight
              length_distribution[[i]] = nodes$length
            }
            
            return(list(weight_distribution = weight_distribution, length_distribution = length_distribution))
          })



#### Méthodes de création de graphiques de type spectrosome ####

#' Spectrosome
#' 
#' Construit un ou plusieurs graphiques de type spectrosome et les enregistre au format PNG.
#' 
#' @details
#' Si des catégories sont associées aux items, chaque catégorie génère un spectrosome.
#'  Le nom de la catégorie est ajouté à la fin du nom du fichier.
#' 
#' Si des liens mixtes sont relatifs à des valeurs de catégorie qui ne sont pas représentées par des
#'  liens simples, ces valeurs apparaîssent dans la légende en dessous de "Mixt", sans couleur associée.
#'  
#' Si \code{min_link_weight} est supérieur à 1, certains nœuds ou motifs peuvent devenir isolés
#'  du fait que leurs liens avec les autres éléments peuvent ne plus être considérés. Ces nouveaux
#'  sommets isolés sont déplacés à la fin de la data frame de retour \code{edges}.
#' Les \code{n} lignes additionnelles sont numérotées \code{"A1"..."An"}.
#' 
#' Les couleurs associées aux valeurs de chaque catégorie représentée sont sélectionnées
#'  de manière circulaire parmi les 20 couleurs de la palette \code{category20} de D3 (cf.
#'  \code{ggsci::pal_d3("category20")}).
#' Par conséquent, si le nombre de valeurs dépasse \code{20}, certaines couleurs seront utilisées
#'  plusieurs fois. Par exemple, la \out{22<sup>e</sup>} valeur partagera la couleur de la
#'  \out{2<sup>e</sup>} valeur.
#' 
#' Les noms des clusters confondus, du fait que l'intégralité de leurs liens sont des liens mixtes,
#'  ne sont pas affichés.
#' 
#' Des arguments supplémentaires peuvent être fournis à la fonction en charge du traçage du graphe.
#'  Voir la liste des paramètres : \code{\link[sna:gplot]{sna::gplot}}.
#' Parmi eux, les paramètres suivants sont déjà définis et ne peuvent pas être modifiés : \code{dat},
#'  \code{gmode}, \code{vertex.sides}, \code{vertex.cex}, \code{vertex.col}, \code{edge.col}.
#' Les paramètres suivants, pouvant être redéfinis, ont pour valeurs :
#'  \itemize{
#'    \item{\code{mode = "fruchtermanreingold"}}
#'    \item{\code{layout.par = list(repulse.rad = 4 ^ (log(nrow(nop_links), 10)))},
#'      où \code{nrow(nop_links)} correspond à la somme du nombre de liens et du nombre d'éléments isolés.
#'      Peut aussi être un objet de type \code{expression} ou \code{NULL}.}
#'    \item{\code{displaylabels = TRUE}}
#'    \item{\code{label.pos = 0}}
#'    \item{\code{boxed.labels = TRUE}}
#'    \item{\code{displayisolates = TRUE}}
#'  }
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @param entities Type d'élément pour lequel construire le spectrosome (nœuds ou motifs).
#'  Choix parmi \code{"nodes"}, \code{"patterns"}.
#' @param characteristics Ensemble des caractéristiques des nœuds ou motifs dont le spectrosome est à tracer.
#' @param nb_graphs Nombre de graphiques à générer et enregistrer. Le placement des sommets diffère entre chaque exemplaire.
#' @param min_link_weight Nombre minimum d'items en commun entre deux entités pour afficher le lien sur le graphe.
#' @param vertex_size Façon dont les tailles des sommets du graphe doivent être définies.
#'  Choix parmi \code{"relative"}, \code{"grouped"}, \code{"absolute"}, \code{"equal"}.
#'  \describe{
#'    \item{\code{"relative"}}{La taille d'un sommet dépend de l'intervalle de valeurs des différents poids.}
#'    \item{\code{"grouped"}}{Les poids des motifs sont regroupés selon des intervalles. À chaque intervalle correspond une taille.}
#'    \item{\code{"absolute"}}{La taille d'un sommet est définie directement en fonction du poids du motif.}
#'    \item{\code{"equal"}}{Les sommets ont tous la même taille.}
#'  }
#' @param vertex_col Façon dont les couleurs des sommets du graphe doivent être définies.
#'  Choix parmi \code{"status"}, \code{NULL}.
#'  Si \code{"status"} et \code{entities = "patterns"}, coloration selon les statuts des motifs.
#'  Dans tous les autres cas, le sommets sont de couleur grise.
#' @param clusters Nombre maximum de clusters à nommer sur le graphe.
#'  Si le nombre de clusters est supérieur, les noms des plus petits clusters ne sont pas affichés.
#' @param highlight Nombre de clusters à mettre en évidence parmi ceux nommés sur le graphe.
#'  Les noms des plus grands clusters sont affichés en gras.
#' @param use_names Si \code{TRUE}, affiche les noms des items s'ils sont définis. Affiche leurs codes sinon.
#' @param n.cutoff Si \code{use_names = TRUE}, nombre limite de caractères à afficher concernant les noms des items affichés.
#' @param c.cutoff Nombre limite de caractères à afficher dans la légende concernant les catégories représentées.
#' @param path Chemin du dossier dans lequel enregistrer les graphiques.
#'  Par défaut, les graphiques sont enregistrés dans le répertoire de travail.
#' @param name Nom du fichier dans lequel enregistrer le graphique.
#'  Si \code{nb_graphs} est supérieur à \code{1}, un numéro est ajouté automatiquement à la fin du nom du fichier.
#' @param title Titre du graphique.
#' @param ... Arguments supplémentaires fournis à la fonction \code{\link[sna:gplot]{sna::gplot}}
#'  pour le traçage du graphe. Cf. section Details.
#' @return Liste contenant :
#'  \describe{
#'    \item{\code{vertices}}{Data frame des nœuds ou motifs et caractéristiques utilisées,
#'                           associés aux identifiants des sommets du graphe et à leurs degrés dans le graphe.}
#'    \item{\code{edges}}{Data frame des informations relatives aux arêtes du graphe.}
#'    \item{\code{coords}}{Liste contenant les matrices des coordonnées des sommets du graphe.
#'                         Autant de matrices que de graphiques (\code{nb_graphs}), pouvant être
#'                         réutilisées via l'argument \code{coord} (cf. \code{...}).}
#'  }
#' 
#' @author Delphine Bosson-Rieutort, Gauthier Magnin
#' @references Bosson-Rieutort D, de Gaudemaris R, Bicout DJ (2018).
#'             \emph{The spectrosome of occupational health problems}. PLoS ONE 13(1): e0190196.
#'             \url{https://doi.org/10.1371/journal.pone.0190196}.
#' @seealso \code{\link{degree}}, \code{\link[sna:gplot]{sna::gplot}}.
#' @aliases spectrosome_chart
#' @export
setMethod(f = "spectrosome_chart",
          signature = "SpectralAnalyzer",
          definition = function(object, entities, characteristics,
                                nb_graphs = 1, min_link_weight = 1, 
                                vertex_size = "relative", vertex_col = "status",
                                clusters = Inf, highlight = 3,
                                use_names = TRUE, n.cutoff = NULL, c.cutoff = NULL,
                                path = getwd(), name = paste0("spectrosome_of_", entities, ".png"), title = paste0("Network of ", entities),
                                ...) {
            
            if (entities != "nodes" && entities != "patterns")
              stop("entities must be \"nodes\" or \"patterns\".")
            
            if (nrow(characteristics) < 2)
              stop("\"characteristics\" must have at least 2 rows to draw a spectrosome.")
            
            
            # Extraction des liens pour les éléments à visualiser (nop_links = nodes or patterns links)
            nop_links = extract_links(object, entities, characteristics)
            
            if (entities == "nodes") {
              # Renommage d'une colonne pour plus tard (cf. vertices_shapes)
              colnames(characteristics)[colnames(characteristics) == "length"] = "order"
              
              # Texte affiché sur le graphique
              nop_subtitle_1 = "\nNodes: "
              nop_subtitle_3 = "; Isolated nodes: "
              
              not_identical = !identical(object@nodes, characteristics)
              
            } else if (entities == "patterns") {
              # Texte affiché sur le graphique
              nop_subtitle_1 = "\nPatterns: "
              nop_subtitle_3 = "; Isolated patterns: "
              
              not_identical = !identical(object@patterns, characteristics)
            }
            
            # Identifiants des sommets du graphe
            vertices_id = seq(nrow(characteristics))
            
            if (not_identical) {
              # Nouvelle numérotation des éléments conservés
              names(vertices_id) = rownames(characteristics)
              nop_links$endpoint.1 = vertices_id[as.character(nop_links$endpoint.1)]
              nop_links$endpoint.2 = vertices_id[as.character(nop_links$endpoint.2)]
            }
            
            # Retrait des liens entre les sommets qui ont moins de min_link_weight items en commun
            if (min_link_weight > 1) {
              all_vertices = unique(c(t(nop_links[, 1:2]))) # unlist horizontalement
              nop_links = nop_links[nop_links$weight >= min_link_weight | nop_links$weight == 0, ]
              
              # Redéfinition des sommets maintenant sans lien
              missing_vertices = as.data.frame(t(
                sapply(setdiff(all_vertices, unique(unlist(nop_links[, 1:2]))),
                       function(x){
                         if (entities == "nodes") return(c(x, x, "", 0))
                         return(c(x, x, "", 0, object@patterns[x, "year"]))
                       })), stringsAsFactors = FALSE)
              
              # Réattribution des noms et classes des colonnes avant concaténation à la data frame des liens
              colnames(missing_vertices) = colnames(nop_links)
              for (c_name in colnames(missing_vertices)) class(missing_vertices[c_name]) = class(nop_links[c_name])
              class(missing_vertices$endpoint.1) = class(missing_vertices$endpoint.2) = class(missing_vertices$weight) = "integer"
              if(entities == "patterns") class(missing_vertices$year) = "integer"
              nop_links = rbind(nop_links, missing_vertices)
              
              # Attribution d'index aux nouvelles lignes, différents de ceux de la data frame générale (l'attribut)
              rownames(nop_links) = c(rownames(nop_links)[1:(nrow(nop_links) - nrow(missing_vertices))],
                                      paste0("A", seq_len(nrow(missing_vertices))))
            }
            # Attribution d'identifiants aux liens
            nop_links$ID = seq_len(nrow(nop_links))
            if (entities == "nodes") nop_links = nop_links[, c("ID", "endpoint.1", "endpoint.2", "items", "weight")]
            else nop_links = nop_links[, c("ID", "endpoint.1", "endpoint.2", "items", "weight", "year")]
            
            
            # Couleurs et légendes pour chaque catégorie existante
            categories_colors = list()
            links_colors = list()
            
            # Définition des couleurs des liens en recherchant les catégories associées aux items formants les liens
            if (length(object@items_categories) == 0) {
              # Si aucune catégorie n'est associée aux éléments, aucune ne l'est aux liens
              categories_colors[[1]] = character(0)
              links_colors[[1]] = 1
              
            } else {
              # Pour chaque type de catégorie
              for (category in seq_len(ncol(object@items_categories))) {
                
                # S'il n'y a aucun lien
                if (sum(nop_links$weight != 0) == 0) {
                  categories_colors[[category]] = character(0)
                  links_colors[[category]] = rep("white", nrow(nop_links))
                  
                } else if (length(levels(object@items_categories[, category])) > 1) {
                    
                  # Catégories associées aux liens
                  categories_links = lapply(strsplit(nop_links$items, "/"),
                                            function(x) sort(unique(as.character(object@items_categories[x, category]))))
                  category_values = unique(unlist(categories_links))
                  categories_links = unlist(lapply(categories_links, function(x) {
                    if (length(x) == 1) return(x)
                    if (length(x) > 1) return("Mixt")
                    return("Isolated")
                  }))
                  
                  # Séparation des valeurs de la catégorie qui sont uniquement inclus dans des liens mixtes
                  category_mixed = sort(setdiff(category_values, unique(unlist(categories_links))))
                  category_not_mixed = sort(setdiff(category_values, category_mixed))
                  
                  # Sélection circulaire parmi les 20 couleurs d'une palette de D3
                  categories_colors[[category]] = c(ggsci::pal_d3("category20")(20)[seq_along(category_not_mixed) %% 21],
                                                    "black", "white")
                  names(categories_colors[[category]]) = c(category_not_mixed, "Mixt", "Isolated")
                  
                  # Couleurs des liens tracés sur le graphique
                  links_colors[[category]] = categories_colors[[category]][categories_links]
                  
                  # Retrait du noir associé aux liens mixtes s'il n'y en a pas et retrait du blanc
                  # associé aux isolés, pour ne pas les afficher ultérieurement dans la légende
                  if (length(category_mixed) == 0) {
                    categories_colors[[category]] = categories_colors[[category]][seq(length(categories_colors[[category]])-2)]
                  } else {
                    categories_colors[[category]] = categories_colors[[category]][seq(length(categories_colors[[category]])-1)]
                    
                    # Ajout des valeurs de catégorie inclus uniquement dans des liens mixtes
                    new_names = c(names(categories_colors[[category]]), category_mixed)
                    categories_colors[[category]] = append(categories_colors[[category]], rep("white", length(category_mixed)))
                    names(categories_colors[[category]]) = new_names
                  }
                  
                } else if(length(levels(object@items_categories[, category])) == 1) {
                  # Une unique catégorie
                  categories_colors[[category]] = c("black", "white")
                  names(categories_colors[[category]]) = c(levels(object@items_categories[, category]), "Isolated")
                  
                  categories_links = ifelse(nop_links$weight == 0, "Isolated", levels(object@items_categories[, category]))
                  
                  # Couleurs des liens tracés sur le graphique
                  links_colors[[category]] = categories_colors[[category]][categories_links]
                  
                  # Retrait du blanc associé aux isolés pour ne pas l'afficher ultérieurement dans la légende
                  categories_colors[[category]] = categories_colors[[category]][seq(length(categories_colors[[category]])-1)]
                  
                } else {
                  stop("The categories associated with the items must be factor type and it must have at least one factor.")
                }
              }
            }
            
            
            # Définition des couleurs des sommets en fonction du statut et nombre pour chaque statut
            if (entities == "patterns" && !is.null(vertex_col) && vertex_col == "status") {
              vertices_colors = object@Class$STATUS_COLORS[characteristics$status]
              count_status = sapply(names(object@Class$STATUS_COLORS),
                                    function(status) sum(characteristics$status == status))
              
              # Légende associée
              legend_1 = c(paste(names(object@Class$STATUS_COLORS), paste0("(", count_status, ")")),
                           "", "Single items", "Multiple items")
              col_1 = c(object@Class$STATUS_COLORS, "white", "black", "black")
            } else {
              vertices_colors = rep("grey", nrow(characteristics))
              count_status = c(0,0,0,0)
              
              # Légende associée
              legend_1 = c("Single items", "Multiple items")
              col_1 = c("black", "black")
            }
            
            # Sommets à plusieurs items en cercle ; triangle sinon
            vertices_shapes = rep(100 , length(vertices_colors))
            vertices_shapes[characteristics$order == 1] = 3
            
            # Définition des tailles des sommets
            switch(EXPR = vertex_size,
                   "relative" = {
                     # Interpolation linéaire des poids aux valeurs [0.5, 2.5]
                     if (min(characteristics$weight) != max(characteristics$weight)) {
                       func = approxfun(x = c(min(characteristics$weight), max(characteristics$weight)),
                                        y = c(0.5, 2.5))
                       vertices_sizes = func(characteristics$weight)
                     } else {
                       vertices_sizes = rep(1, length(characteristics$weight))
                     }
                   },
                   "grouped" = {
                     # Groupement des valeurs des poids
                     breaks = round(quantile(unique(characteristics$weight), prob = seq(0, 1, 0.2)))
                     intervals = cut(characteristics$weight, breaks = breaks, include.lowest = TRUE, dig.lab = 5)
                     sizes = seq(0.5, 2.5, length.out = length(levels(intervals)))
                     vertices_sizes = sizes[intervals]
                   },
                   "absolute" = {
                     # Utilisation d'un log
                     vertices_sizes = log10(characteristics$weight)
                     vertices_sizes[vertices_sizes < 0.5] = 0.5
                   },
                   "equal" = {
                     # Taille par défaut (valeur par défaut de l'argument vertex.cex de la fonction gplot())
                     vertices_sizes = 1
                   },
                   stop("Unknown value for vertex_size. Must be one of c(\"relative\", \"grouped\", \"absolute\", \"equal\")."))
            
            
            # Réseau généré avec le package network
            links = as.matrix(nop_links[, c("endpoint.1", "endpoint.2")], ncol = 2)
            network_data = network::network(links, directed = FALSE, matrix.type = "edgelist")
            vertices_names = network::network.vertex.names(network_data)
            
            # Récupération des arguments additionnels et détermination de valeurs par défaut pour sna::gplot
            args = list(...)
            if(!("mode" %in% names(args))) args$mode = "fruchtermanreingold"
            if(!("layout.par" %in% names(args)) && args$mode == "fruchtermanreingold")
              args$layout.par = list(repulse.rad = 4 ^ (log(nrow(nop_links), 10)))
            if("layout.par" %in% names(args) && is.expression(args$layout.par))
              args$layout.par = eval(args$layout.par)
            if(!("displaylabels" %in% names(args))) args$displaylabels = TRUE
            if(!("label.pos" %in% names(args))) args$label.pos = 0
            if(!("boxed.labels" %in% names(args))) args$boxed.labels = TRUE
            if(!("displayisolates" %in% names(args))) args$displayisolates = TRUE
            
            # Duplication de la fonction utilisée par l'argument "mode" de la fonction sna::gplot
            # pour fonctionner sans avoir à charger le package
            eval(parse(text = paste0("gplot.layout.", args$mode, " <- sna::gplot.layout.", args$mode)))
            
            # Nombre de variantes du graphique
            nb_categories = ifelse(length(object@items_categories) == 0, 1, ncol(object@items_categories))
            file_name = check_extension(name, "png")
            
            # Réutilisation ou non de coordonnées
            if ("coord" %in% names(args)) {
              coord = args$coord
              args$coord = NULL
              is.missing = is.null(coord)
              
            } else { is.missing = TRUE }
            coords_list = list()
            
            # Traçage des graphiques
            for (i in seq(nb_graphs)) {
              # Coordonnées qui seront réutilisées
              if (is.missing) coord = NULL
              
              for (j in seq(nb_categories)) {
                
                # Nom du graphique en fonction du nombre
                file_name = ifelse(nb_graphs == 1, file_name, sub(".png", paste0("-", i, ".png"), file_name))
                file_name = ifelse(nb_categories == 1,
                                   file_name,
                                   sub(".png", paste0("-", colnames(object@items_categories)[j], ".png"), file_name))
                
                # Traçage des graphiques dans des fichiers PNG
                png(paste0(turn_into_path(path), file_name), 950, 700)
                par(mar = c(2,0.5,4,0.5))
                
                # Dessin du graphe : appel de sna::gplot avec les arguments de ... modifiés (variable args)
                coord = do.call(sna::gplot, c(list(
                                  dat = network_data, gmode = "graph",
                                  coord = coord,
                                  vertex.sides = vertices_shapes,
                                  vertex.cex = vertices_sizes,
                                  vertex.col = vertices_colors,
                                  edge.col = links_colors[[j]]
                               ), args))
                
                # Titre du graphique
                title(main = title, cex.main = 1.5)
                title(main = paste0(nop_subtitle_1, nrow(characteristics),
                                    "; Links: ", sum(nop_links$weight != 0),
                                    nop_subtitle_3, length(sna::isolates(network_data))),
                      font.main = 3, line = 0)
                
                # Préparation des formes de la légende du graphique
                if (entities == "patterns" && !is.null(vertex_col) && vertex_col == "status") {
                  legend_pt.cex = c(rep(2, length(object@Class$STATUS_COLORS)), 1.7, 1.9, rep(2, length(categories_colors[[j]])))
                  legend_pch = c(rep(15, length(object@Class$STATUS_COLORS)), 0, 2, 1, 0, rep(20, length(categories_colors[[j]])))
                } else {
                  legend_pt.cex = c(1.9, rep(2, length(categories_colors[[j]])))
                  legend_pch = c(2, 1, 0, rep(20, length(categories_colors[[j]])))
                }
                
                # Préparation de la légende des liens, à la suite des statuts et sommets
                if (is.null(c.cutoff)) {
                  legend_legend = c(legend_1, "", names(categories_colors[[j]]))
                } else {
                  legend_legend = c(legend_1, "", substr(names(categories_colors[[j]]), 1, c.cutoff))
                }
                legend_col = c(col_1, "white", categories_colors[[j]])
                
                # Affichage de la légende
                legend("topleft", bty = "n", xpd = NA, pt.cex = legend_pt.cex, pch = legend_pch,
                       legend = legend_legend, col = legend_col)
                
                # S'il y a bien des liens, identification et affichage des noms des clusters
                if (sum(nop_links$weight != 0)) {
                  cluster_text(object, coord, nop_links, clusters, highlight, use_names, n.cutoff)
                }
                
                # Fermeture du fichier PNG
                dev.off()
              }
              
              # Récupération des coordonnées des sommets du graphe
              coords_list[[i]] = coord
            }
            
            # Calcul du degré de chaque sommet dans le graphe
            degrees = sapply(vertices_id, function(ID) degree(object, ID, nop_links))
            
            # Noeuds ou motifs, caractéristiques, identifiants sur le graphique et degrés dans le graphe
            return(list(vertices = data.frame(ID = vertices_id, characteristics, degree = degrees),
                        edges = nop_links,
                        coords = coords_list))
          })


#' Affichage des noms des clusters
#' 
#' Identifie et affiche les noms des clusters sur le graphe fourni en argument.
#' Les noms des clusters confondus, du fait que l'intégralité de leurs liens sont des liens mixtes,
#'  ne sont pas affichés.
#' Les textes sont écrits sur le périphérique graphique actif.
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @param graph Graphe généré par la fonction \code{\link[sna:gplot]{sna::gplot}} :
#'  "A two-column matrix containing the vertex positions as x,y coordinates.".
#' @param links Liens des nœuds ou motifs utilisés pour générer \code{graph}.
#' @param display Nombre maximum de clusters à nommer sur le graphe.
#'  Si le nombre de clusters est supérieur, les noms des plus petits clusters ne sont pas affichés.
#' @param highlight Nombre de clusters à mettre en évidence parmi ceux nommés sur le graphe.
#'  Les noms des plus grands clusters sont affichés en gras.
#' @param use_names Si \code{TRUE}, affiche les noms des items s'ils sont définis. Affiche leurs codes sinon.
#' @param cutoff Si \code{use_names = TRUE}, nombre limite de caractères à afficher concernant les noms des items affichés.
#' 
#' @author Delphine Bosson-Rieutort, Gauthier Magnin
#' @seealso \code{\link{spectrosome_chart}}, \code{\link[sna:gplot]{sna::gplot}}.
#' @aliases cluster_text
#' @keywords internal
setMethod(f = "cluster_text",
          signature = "SpectralAnalyzer",
          definition = function(object, graph, links, display = Inf, highlight = 3, use_names = TRUE, cutoff = NULL){
            
            # Calcul des coordonnées des milieux des liaisons
            coord_e1 = graph[links$endpoint.1, ] # Coordonnées des premiers sommets des liens
            coord_e2 = graph[links$endpoint.2, ] # Coordonnées des seconds sommets des liens
            # S'il y a plusieurs liens
            if (!is.vector(coord_e1)) {
              coord_L = data.frame(X = rowMeans(cbind(coord_e1[, "x"], coord_e2[, "x"])), 
                                   Y = rowMeans(cbind(coord_e1[, "y"], coord_e2[, "y"])),
                                   LABEL = links$items)
            } else {
              # S'il n'y a qu'un seul lien et que deux sommets
              coord_L = data.frame(X = mean(c(coord_e1["x"], coord_e2["x"])), 
                                   Y = mean(c(coord_e1["y"], coord_e2["y"])),
                                   LABEL = links$items)
            }
            # Regroupement en fonction du type de liaison ("LABEL")
            coord_L = coord_L[order(coord_L$LABEL), ]
            
            # Décomposition des liens multiples et calcul du nombre de liaisons réelles de chaque item
            clusters = sort(table(unlist(strsplit(as.character(coord_L$LABEL[coord_L$LABEL != ""]), "/"))), decreasing = TRUE)
            clusters = names(clusters)
            
            # Moyenne des coordonnées des liens pour chaque type de lien ("LABEL")
            coords = aggregate(data.frame(MOY.X = coord_L$X), by = list(LABEL = coord_L$LABEL), mean)
            coords$MOY.Y = tapply(coord_L$Y, coord_L$LABEL, mean)
            
            # Association des coordonnées moyennes des liens exactes (non multiples et non décomposés) aux noms des items ayant générés le plus de liaisons
            coords = coords[match(clusters, as.character(coords$LABEL)), ]
            #! Les coordonnées ne sont donc pas la moyenne de tous les liens correspondant à l'item
            #! mais uniquement de ceux qui correspondent exactement à cet item (pas de combinaisons)
            #! bien que la variable "cluster" a recherché le nombre de liens correspondant à l'item, qu'il y ait une combinaison ou non.
            #! => Permet une sorte d'attraction du label vers les sommets partageant uniquement l'item.
            
            # Extraction des noms des items ayant générés le plus de liaisons
            coords = coords[complete.cases(coords), ]
            if (nrow(coords) >= display) coords = coords[seq_len(display), ]
            
            # Affichage des noms des "clusters" retenus
            if (use_names) {
              clusters = names(object@items)[match(coords$LABEL, object@items)]
              if (!is.null(cutoff)) clusters = substr(clusters, 1, cutoff)
            }
            shadowtext(coords$MOY.X, coords$MOY.Y, clusters, r = 0.3,
                       col = "black", bg = "white", cex = 0.9,
                       font = ifelse(clusters %in% clusters[seq_len(highlight)], 2, 1))
          })


#' Cluster : sous-graphe du spectrosome
#' 
#' Identifie le cluster associé à l'item fourni en argument et en dessine un spectrosome.
#' 
#' @details
#' Si des catégories sont associées aux items, chaque catégorie génère un spectrosome.
#'  Le nom de la catégorie est ajouté à la fin du nom du fichier.
#' 
#' Si des liens mixtes sont relatifs à des valeurs de catégorie qui ne sont pas représentées par des
#'  liens simples, ces valeurs apparaîssent dans la légende en dessous de "Mixt", sans couleur associée.
#' 
#' Des arguments supplémentaires peuvent être fournis à la fonction en charge du traçage du graphe.
#'  Voir la liste des paramètres : \code{\link[sna:gplot]{sna::gplot}}.
#' Parmi eux, les paramètres suivants sont déjà définis et ne peuvent pas être modifiés : \code{dat},
#'  \code{gmode}, \code{vertex.sides}, \code{vertex.cex}, \code{vertex.col}, \code{edge.col}.
#' Les paramètres suivants, pouvant être redéfinis, ont pour valeurs :
#'  \itemize{
#'    \item{\code{mode = "fruchtermanreingold"}}
#'    \item{\code{layout.par = list(repulse.rad = 4 ^ (log(nrow(nop_links), 10)))},
#'      où \code{nrow(nop_links)} correspond à la somme du nombre de liens et du nombre d'éléments isolés.
#'      Peut aussi être un objet de type \code{expression} ou \code{NULL}.}
#'    \item{\code{displaylabels = TRUE}}
#'    \item{\code{label.pos = 0}}
#'    \item{\code{boxed.labels = TRUE}}
#'    \item{\code{displayisolates = TRUE}}
#'  }
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @param entities Type d'élément pour rechercher un cluster (nœuds ou motifs).
#'  Choix parmi \code{"nodes"}, \code{"patterns"}.
#' @param characteristics Ensemble des caractéristiques des nœuds ou motifs dont l'un des clusters est à tracer.
#' @param item Code de l'item dont le cluster est à mettre en évidence.
#' @param use_name Si \code{TRUE}, affiche le nom de l'item s'il est défini. Affiche son code sinon.
#' @param n.cutoff Si \code{use_names = TRUE}, nombre limite de caractères à afficher concernant le nom de l'item.
#' @param vertex_size Façon dont les tailles des sommets du graphe doivent être définies.
#'  Choix parmi \code{"relative"}, \code{"grouped"}, \code{"absolute"}, \code{"equal"}.
#'  \describe{
#'    \item{\code{"relative"}}{La taille d'un sommet dépend de l'intervalle de valeurs des différents poids.}
#'    \item{\code{"grouped"}}{Les poids des motifs sont regroupés selon des intervalles. À chaque intervalle correspond une taille.}
#'    \item{\code{"absolute"}}{La taille d'un sommet est définie directement en fonction du poids du motif.}
#'    \item{\code{"equal"}}{Les sommets ont tous la même taille.}
#'  }
#' @param vertex_col Façon dont les couleurs des sommets du graphe doivent être définies.
#'  Choix parmi \code{"status"}, \code{NULL}.
#'  Si \code{"status"}, coloration selon les statuts des motifs. Sinon, couleur grise.
#' @param c.cutoff Nombre limite de caractères à afficher dans la légende concernant les catégories représentées.
#' @param path Chemin du dossier dans lequel enregistrer les graphiques.
#'  Par défaut, les graphiques sont enregistrés dans le répertoire de travail.
#' @param name Nom du fichier dans lequel enregistrer le graphique.
#'  Par défaut, le nom dépend des arguments \code{entities} et \code{item}.
#'  Exemple de nom par défaut : \code{"node_cluster_of_25"}.
#' @param title Titre du graphique.
#'  Par défaut, le titre dépend des arguments \code{entities} et \code{item}.
#'  Exemple de titre par défaut : \code{"Node cluster of 25"}.
#' @param ... Arguments supplémentaires fournis à la fonction \code{\link[sna:gplot]{sna::gplot}}
#'  pour le traçage du graphe. Cf. section Details.
#' @return \code{NULL} si aucun ou un seul nœud ou motif contient l'item recherché. \cr
#'  Sinon, liste contenant :
#'  \describe{
#'    \item{\code{vertices}}{Data frame des nœuds ou motifs et caractéristiques utilisées,
#'                           associés aux identifiants des sommets du graphe et à leurs degrés dans le graphe.}
#'    \item{\code{edges}}{Data frame des informations relatives aux arêtes du graphe.}
#'    \item{\code{coords}}{Matrice des coordonnées des sommets du graphe.}
#'  }
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{spectrosome_chart}}, \code{\link{degree}}, \code{\link[sna:gplot]{sna::gplot}}.
#' @aliases cluster_chart
#' @export
setMethod(f = "cluster_chart",
          signature = "SpectralAnalyzer",
          definition = function(object, entities, characteristics, item,
                                use_name = TRUE, n.cutoff = NULL,
                                vertex_size = "relative", vertex_col = "status", c.cutoff = NULL,
                                path = getwd(),
                                name = paste0(substr(entities, 1, nchar(entities) - 1), "_cluster_of_", item, ".png"),
                                title = paste(cap(substr(entities, 1, nchar(entities) - 1)), "cluster of", item),
                                ...) {
            
            # Vérifie qu'un seul item est mentionné
            if (length(item) != 1 && entities == "nodes")
              stop("item must refer to only one item. For more, check out the functions extract_nodes_from_items and spectrosome_chart.")
            if (length(item) != 1 && entities == "patterns")
              stop("item must refer to only one item. For more, check out the functions extract_patterns_from_items and spectrosome_chart.")
            
            # Extraction des noeuds ou motifs contenant l'item recherché (nop = nodes or patterns)
            if (entities == "nodes") nop = extract_nodes_from_items(object, characteristics, item)
            else if (entities == "patterns") nop = extract_patterns_from_items(object, characteristics, item)
            else stop("entities must be \"nodes\" or \"patterns\".")
            
            # Pas de cluster à construire si un seul ou aucun noeud/motif ne contient l'item
            if (nrow(nop) > 1) {
              # Construction du spectrosome associé
              to_return = spectrosome_chart(object, entities, nop,
                                            vertex_size = vertex_size, vertex_col = vertex_col,
                                            use_names = use_name, n.cutoff = n.cutoff, c.cutoff = c.cutoff,
                                            path = path, name = name, title = title, ...)
              return(list(vertices = to_return$vertices, edges = to_return$edges, coords = to_return$coords[[1]]))
              
            } else {
              warning(paste0("There is no cluster for item ", item,
                             " (", nrow(nop), " ", substr(entities, 1, nchar(entities) - 1), ")."))
              return(NULL)
            }
          })


#' Densité d'un réseau
#' 
#' Calcule la densité du graphe comme étant le ratio entre le nombre de liens identifiés
#'  et le nombre maximal de liens possibles.
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @param links Data frame des liens (ou arêtes) d'un graphe de type spectrosome.
#' @return Densité du réseau.
#' 
#' @author Gauthier Magnin
#' @aliases network_density
#' @export
setMethod(f = "network_density",
          signature = "SpectralAnalyzer",
          definition = function(object, links) {
            
            # Nombre d'arêtes et de sommets
            nb_edges = nrow(links) - sum(links$weight == 0)
            nb_vertices = length(unique(c(links$endpoint.1, links$endpoint.2)))
            
            # Nombre maximal d'arêtes (1 entre chaque paire de sommets, sans boucle)
            nb_edges_max = nb_vertices * (nb_vertices - 1) / 2
            
            return(nb_edges / nb_edges_max)
          })


#' Degré d'un sommet
#' 
#' Calcule le degré d'un sommet dans un graphe, c'est-à-dire, le nombre de sommets
#'  auxquels il est adjacent.
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @param ID Identifiant du sommet (nœud ou motif) dont le degré est à calculer.
#' @param links Data frame des liens (ou arêtes) d'un graphe de type spectrosome.
#' @return Degré du sommet.
#' 
#' @author Gauthier Magnin
#' @aliases degree
#' @export
setMethod(f = "degree",
          signature = "SpectralAnalyzer",
          definition = function(object, ID, links) {
            
            return(sum(xor(links$endpoint.1 == ID, links$endpoint.2 == ID)))
          })



#### Méthodes de création de graphiques de type arbre de la multi-association ####

#' Arbre de la multi-association
#' 
#' Construit un graphique de type arbre de la multi-association et l'enregistre dans un fichier au
#' format PDF.
#' 
#' @details
#' Si des catégories sont associées aux items, chaque catégorie génère un arbre.
#'  Le nom de la catégorie est ajouté à la fin du nom du fichier.
#' 
#' Les motifs d'ordre 1 ne sont pas dessinés. Seuls les items inclus dans les motifs d'ordre supérieur
#'  le sont.
#' 
#' Les motifs sont triés selon leurs poids.
#' 
#' Les couleurs associées aux valeurs de l'éventuelle catégorie représentée sont sélectionnées
#'  de manière circulaire parmi les 20 couleurs de la palette \code{category20} de D3 (cf.
#'  \code{ggsci::pal_d3("category20")}).
#' Par conséquent, si le nombre de valeurs dépasse \code{20}, certaines couleurs seront utilisées
#'  plusieurs fois. Par exemple, la \out{22<sup>e</sup>} valeur partagera la couleur de la
#'  \out{2<sup>e</sup>} valeur.
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @param patterns_characteristics Ensemble des caractéristiques des motifs dont l'arbre est à tracer.
#' @param use_names Si \code{TRUE}, affiche les noms des items s'ils sont définis. Affiche leurs codes sinon.
#' @param n.cutoff Si \code{use_names = TRUE}, nombre limite de caractères à afficher concernant les noms des items affichés.
#' @param display_status Si \code{TRUE}, affiche les statuts des motifs.
#' @param display_text Texte à afficher sur le graphique à côté des motifs.
#'  Choix entre les identifiants \code{"ID"} des motifs ou l'une des autres caractéristiques
#'  (\code{"weight"}, \code{"frequency"}, \code{"specificity"}, \code{"year"}).
#'  La valeur \code{NULL} précise qu'aucune de ces informations ne doit être affichée.
#' @param c.cutoff Nombre limite de caractères à afficher dans la légende concernant les catégories représentées.
#' @param path Chemin du dossier dans lequel enregistrer le graphique.
#'  Par défaut, le graphique est enregistré dans le répertoire de travail.
#' @param name Nom du fichier dans lequel enregistrer le graphique.
#' @param title Titre du graphique.
#' @return Data frame des motifs représentées sur le graphique, associés à leurs caractéristiques et
#'  identifiants (visibles sur le graphique si \code{display_text = "ID"}).
#' 
#' @author Delphine Bosson-Rieutort, Gauthier Magnin
#' @references Bosson-Rieutort D, Sarazin P, Bicout DJ, Ho V, Lavoué J (2020).
#'             Occupational Co-exposures to Multiple Chemical Agents from Workplace Measurements by the US Occupational Safety and Health Administration.
#'             \emph{Annals of Work Exposures and Health}, Volume 64, Issue 4, May 2020, Pages 402–415.
#'             \url{https://doi.org/10.1093/annweh/wxaa008}.
#' @aliases tree_chart
#' @export
setMethod(f = "tree_chart",
          signature = "SpectralAnalyzer",
          definition = function(object, patterns_characteristics, use_names = TRUE, n.cutoff = NULL, display_status = TRUE, display_text = "ID", c.cutoff = NULL, path = getwd(), name = "multi-association_tree.pdf", title = "Multi-association tree") {
            
            # Motifs d'ordre > 1, triés par taille croissant, puis par poids décroissant
            pat_charac = patterns_characteristics[patterns_characteristics$order != 1, ]
            pat_charac = pat_charac[order(pat_charac$order,
                                          max(pat_charac$weight) - pat_charac$weight), ]
            pat_charac$ID = seq(nrow(pat_charac))
            
            # Ensemble des items distincts parmi les motifs, associés d'une catégorie
            items_cat = data.frame(item = unique(unlist(pat_charac$pattern)))
            
            # Une variante du graphique par catégorie
            nb_categories = ifelse(length(object@items_categories) == 0, 1, ncol(object@items_categories))
            file_name = check_extension(name, "pdf")
            
            for (c in seq(nb_categories)) {
              
              # S'il n'y a pas de catégorie
              if (length(object@items_categories) == 0) {
                # Tri des items alphanumériquement
                items_cat$category = NA
                items_cat = items_cat[order(items_cat$item), ]
                category = NULL
              } else {
                # Tri des items selon une catégorie
                items_cat$category = object@items_categories[as.character(items_cat$item), c]
                items_cat = items_cat[order(items_cat$category), ]
                category = colnames(object@items_categories)[c]
              }
              rownames(items_cat) = NULL
              
              # Nom du graphique en fonction de la catégorie
              file_name = ifelse(nb_categories == 1,
                                 file_name,
                                 sub(".pdf", paste0("-", category, ".pdf"), file_name))
              
              # Traçage du graphique dans un fichier PDF
              pdf(paste0(turn_into_path(path), file_name), 14, 10, paper = "a4r", pointsize = 11)
              plot_tree_chart(object, pat_charac, items_cat, category, c.cutoff, use_names, n.cutoff, display_status, display_text, title)
              dev.off()
            }
            
            # Motifs et caractéristiques, ordonnés selon ID (replacé en 1ère colonne)
            return(pat_charac[, c(ncol(pat_charac), seq(ncol(pat_charac)-1))])
          })


#' Arbre de la multi-association
#' 
#' Dessine un graphique de type arbre de la multi-association.
#' 
#' @details
#' Les couleurs associées aux valeurs de l'éventuelle catégorie représentée sont sélectionnées
#'  de manière circulaire parmi les 20 couleurs de la palette \code{category20} de D3 (cf.
#'  \code{ggsci::pal_d3("category20")}).
#' Par conséquent, si le nombre de valeurs dépasse \code{20}, certaines couleurs seront utilisées
#'  plusieurs fois. Par exemple, la \out{22<sup>e</sup>} valeur partagera la couleur de la
#'  \out{2<sup>e</sup>} valeur
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @param patterns_characteristics Ensemble des caractéristiques des motifs dont l'arbre est à tracer.
#' @param items_category Data frame des items et d'une catégorie associée.
#' @param category Nom de la catégorie à représenter sur l'arbre, utilisé comme titre de légende.
#' @param c.cutoff Nombre limite de caractères à afficher dans la légende concernant la catégorie représentée.
#' @param use_names Si \code{TRUE}, affiche les noms des items s'ils sont définis. Affiche leurs codes sinon.
#' @param n.cutoff Si \code{use_names = TRUE}, nombre limite de caractères à afficher concernant les noms des items affichés.
#' @param display_status Si \code{TRUE}, affiche les statuts des motifs.
#' @param display_text Texte à afficher sur le graphique à côté des motifs.
#'  Choix entre les identifiants \code{"ID"} des motifs ou l'une des autres caractéristiques
#'  (\code{"weight"}, \code{"frequency"}, \code{"specificity"}, \code{"year"}).
#'  La valeur \code{NULL} précise qu'aucune de ces informations ne doit être affichée.
#' @param title Titre du graphique.
#' 
#' @author Delphine Bosson-Rieutort, Gauthier Magnin
#' @references Bosson-Rieutort D, Sarazin P, Bicout DJ, Ho V, Lavoué J (2020).
#'             Occupational Co-exposures to Multiple Chemical Agents from Workplace Measurements by the US Occupational Safety and Health Administration.
#'             \emph{Annals of Work Exposures and Health}, Volume 64, Issue 4, May 2020, Pages 402–415.
#'             \url{https://doi.org/10.1093/annweh/wxaa008}.
#' @seealso \code{\link{tree_chart}}.
#' @aliases plot_tree_chart
#' @keywords internal
setMethod(f = "plot_tree_chart",
          signature = "SpectralAnalyzer",
          definition = function(object, patterns_characteristics, items_category, category = NULL, c.cutoff = NULL, use_names = TRUE, n.cutoff = NULL, display_status = TRUE, display_text = "ID", title = "Multi-association tree") {
            
            # Définition des marges
            par(mar = c(3, 1.1, 1.1, 1.1))
            
            # Préparation de la position des items sur le graphique (x = 0 ; y = ordre décroissant)
            items_category$x = 0
            items_category$y = rev(seq(nrow(items_category)))
            
            # Initialisation de la zone graphique
            plot(rbind(items_category[, c("x", "y")], # Autant de lignes (ordonnée max) que d'items distincts
                       data.frame(x = rep(0, 3), y = seq(3) + nrow(items_category))), # + 3 pour placer du texte
                 xlim = c(-max(nchar(as.character(items_category$item))) - 1,
                          nrow(patterns_characteristics) + length(unique(patterns_characteristics$order))),
                 ylim = c(0, nrow(items_category) + 3),
                 col = "white", pch = 20,
                 xaxt = "n", yaxt = "n", bty = "n",
                 xaxs = "i", yaxs = "i")
            
            # Espace à gauche pour affichage des items et du titre "Order"
            # et à droite pour afficher la dernière taille de motif s'il n'y a qu'un seul motif de cette taille
            if (use_names) {
              text_labels = names(object@items)[match(items_category$item, object@items)]
              if (!is.null(n.cutoff)) text_labels = substr(text_labels, 1, n.cutoff)
              text_area = max(strwidth(text_labels, cex = 0.75) + strwidth("12", cex = 0.75), strwidth("Order", cex = 1.05))
              # Ajout de la place pour 2 caractères en plus (stridwidth("12")) car le texte est parfois tronqué
            } else {
              text_labels = as.character(items_category$item)
              text_area = max(strwidth(text_labels, cex = 0.75), strwidth("Order", cex = 1.05))
            }
            data_area = nrow(patterns_characteristics) + length(unique(patterns_characteristics$order)) + strwidth(1)
            
            # Option "new" pour ne pas générer une page blanche à cause du premier plot
            par(new = TRUE)
            
            # Réinitialisation de la zone graphique en considérant la taille d'un caractère (strwidth, dépend du graphique)
            plot(rbind(items_category[, c("x", "y")], # Autant de lignes (ordonnée max) que d'items distincts
                       data.frame(x = rep(0, 3), y = seq(3) + nrow(items_category))), # + 3 pour placer du texte
                 xlim = c(-text_area, data_area),
                 ylim = c(-strwidth(1, cex = 0.5), # Taille d'un caractère pour affichage d'une caractéristique
                          nrow(items_category) + 3),
                 col = "white", pch = 20,
                 xaxt = "n", yaxt = "n", bty = "n",
                 xaxs = "i", yaxs = "i")
            title(main = title, line = 0.35, cex.main = 1.3)
            
            # Traçage des lignes horizontales
            for (y_i in items_category$y) {
              segments(x0 = 0, x1 = data_area - strwidth(1), y0 = y_i,
                       lwd = 0.02, lty = 3, col = "gray85")
            }
            
            # Titre taille des motifs
            text(0, nrow(items_category) + 1.5,
                 "Order", col = "black", cex = 1.05, adj = c(1, 0.5))
            
            
            # Effectifs cumulés des tailles des motifs
            order_cumfreq = cumsum(table(patterns_characteristics$order))
            
            # Préparation des couleurs des lignes séparatrices
            vcolor = c("white", rep("black", length(order_cumfreq)-1))
            names(vcolor) = names(order_cumfreq)
            width = -1
            
            # Pour chaque motif à dessiner
            for (m in 1:nrow(patterns_characteristics)) {
              
              # Nouvelle taille de motifs
              if (m %in% (1 + c(0, order_cumfreq))) {
                order_nb = patterns_characteristics$order[m]
                width = width + 1
                
                # Séparation verticale et affichage de la taille des prochains motifs
                abline(v = width,
                       col = vcolor[as.character(order_nb)], lwd = 0.5, lty = "dotted")
                # Positionnement en X : (nombres de motifs et de lignes déjà placés - la première placée à 0)
                #                       + (nombre de motifs de la même taille
                #                          + 1 espace entre dernier motif et prochaine ligne) / 2
                text(x = (m - 1 + which(names(order_cumfreq) == as.character(order_nb)) - 1) +
                           (order_cumfreq[as.character(order_nb)] - m + 1 + 1) / 2,
                     y = nrow(items_category) + 1.5,
                     as.roman(order_nb), col = "black", cex = 1.05)
              }
              
              # Ordonnées (y) des items du motif (m)
              y_m = sort(items_category[match(patterns_characteristics[m, "pattern"][[1]], items_category$item), "y"])
              
              # Segment vertical entre les premier et dernier items du motif
              lines(c(width + 1, width + 1),
                    c(y_m[1], y_m[length(y_m)]),
                    lwd = 1.2, lty = 1, col = "black")
              # Segments horizontaux pour les items du motif
              for (y in y_m) {
                lines(c(width + 0.5, width + 1), c(y, y),
                      lwd = 1.2, lty = 1, col = "black", pch = 20, cex = 0.8)
              }
              
              # Affichage de l'identifiant ou de l'une des caractéristiques du motif
              if (!is.null(display_text)) {
                text(0.75 + width, y_m[1] - 0.25,
                     patterns_characteristics[m, display_text],
                     col = "black", cex = 0.5, srt = 90, adj = 1)
              }
              # Affichage du statut du motif
              if (display_status) {
                points(0.75 + width, y_m[length(y_m)] + 0.25,
                       cex = 0.5, pch = 15,
                       col = object@Class$STATUS_COLORS[patterns_characteristics$status[m]])
              }
              
              width = width + 1
            }
            
            
            # Couleurs de catégorie
            if (!is.null(category)) {
              # Sélection circulaire parmi les 20 couleurs d'une palette de D3
              category_colors = ggsci::pal_d3("category20")(20)[seq_along(unique(items_category$category)) %% 21]
              names(category_colors) = unique(items_category$category)
              
              final_colors = category_colors[match(items_category$category, names(category_colors))]
              
              # Légende de catégorie
              if (is.null(c.cutoff)) {
                category_legend = unique(items_category$category)
              } else {
                category_legend = substr(unique(items_category$category), 1, c.cutoff)
              }
              legend("bottom", xpd = NA, bty = "n", inset = c(0, -0.09),
                     title = cap(category), cex = 0.85,
                     legend = category_legend,
                     col = category_colors,
                     pch = 20, ncol = ceiling(length(category_legend) / 2))
            } else {
              final_colors = "black"
            }
            
            # Pointage et affichage des items
            points(items_category[, c("x", "y")],
                   col = final_colors, pch = 20)
            text(items_category$x, items_category$y, text_labels,
                 cex = 0.75, pos = 2,
                 col = final_colors)
            
            # Légende des statuts
            if (display_status) {
              legend("topright", bty = "n", horiz = TRUE, xpd = NA, inset = c(-0.02, -0.04),
                     pch = 15, cex = 0.85,
                     col = object@Class$STATUS_COLORS,
                     legend = names(object@Class$STATUS_COLORS))
            }
          })



#### Méthodes de recherche et d'enregistrement ####

#' Enregistrement de nœuds ou de motifs
#' 
#' Enregistre au format CSV un ensemble de nœuds ou motifs ainsi que leurs caractéristiques.
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @param entities Type d'élément associé aux caractéristiques (nœuds ou motifs).
#'  Choix parmi \code{"nodes"}, \code{"patterns"}.
#' @param characteristics Data frame des caractéristiques des nœuds ou des motifs.
#' @param ... Arguments supplémentaires fournis à la fonction \code{\link[utils:write.table]{utils::write.csv2}}.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link[utils:write.table]{utils::write.csv2}}.
#' @aliases save_characteristics
#' @export
setMethod(f = "save_characteristics",
          signature = "SpectralAnalyzer",
          definition = function(object, entities, characteristics, ...) {
            
            if (entities != "nodes" & entities != "patterns")
              stop("entities must be \"nodes\" or \"patterns\".")
            
            # Nom de la colonne dans laquelle chercher les vecteurs à convertir
            column = substr(entities, 1, nchar(entities) - 1)
  
            # Conversion des noeuds ou motifs en chaînes de caractères
            itemsets = tapply(characteristics[, column],
                              seq_along(characteristics[, column]),
                              function(x) {
                                x = as.character(x)
                                
                                # Suppression des caractères liés aux vecteurs
                                ifelse (substring(x, 1, 1) == "c",
                                        substr(x, start = 3, stop = nchar(x) - 1),
                                        x)
                              })
            
            characteristics[, column] = unlist(itemsets)
            
            # Enregistrement des données
            utils::write.csv2(x = characteristics, ...)
          })


#' Recherche de nœuds par item
#' 
#' Extrait les nœuds contenant un ou plusieurs items recherchés.
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @param nodes_characteristics Data frame des nœuds et de leurs caractéristiques.
#' @param items Éléments recherchés (un ou plusieurs).
#' @param target Condition pour qu'un nœud soit extrait. Choix parmi \code{"all"}, \code{"any"}.
#'  \describe{
#'    \item{\code{"all"}}{L'intégralité des éléments recherchés doivent faire partie d'un nœud pour que ce nœud soit extrait.}
#'    \item{\code{"any"}}{Au moins un des éléments recherchés doit faire partie d'un nœud pour que ce nœud soit extrait.}
#'  }
#' @return Sous-ensemble de la data frame fournie en argument contenant les nœuds correspondant au critère de recherche.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{extract_nodes_from_characteristic}}, \code{\link{extract_nodes_from_category}}.
#' @aliases extract_nodes_from_items
#' @export
setMethod(f = "extract_nodes_from_items",
          signature = "SpectralAnalyzer",
          definition = function(object, nodes_characteristics, items, target = "all") {
            
            if (!(target %in% c("all", "any"))) stop("target must be \"all\" or \"any\".")
            
            if (target == "all") func = all
            else if (target == "any") func = any
            
            return(subset(nodes_characteristics, sapply(nodes_characteristics$node,
                                                        function(x) func(items %in% x))))
          })


#' Recherche de nœuds par caractéristique
#' 
#' Extrait les nœuds satisfaisant un critère de recherche.
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @param nodes_characteristics Data frame des nœuds et de leurs caractéristiques.
#' @param characteristic Nom de la caractéristique sur laquelle faire la recherche.
#'  Choix parmi \code{"length"}, \code{"weight"}.
#' @param value Valeur recherchée pour la caractéristique spécifiée par l'argument \code{characteristic}.
#' @param condition Condition de la recherche.
#'  Choix parmi \code{"EQ"}, \code{"NE"}, \code{"LT"}, \code{"GT"}, \code{"LE"}, \code{"GE"}.
#'  \describe{
#'    \item{\code{"EQ"}}{\strong{EQ}ual : la valeur de la caractéristique doit être égale à celle recherchée.}
#'    \item{\code{"NE"}}{\strong{N}ot \strong{E}qual : la valeur de la caractéristique doit être différente de celle recherchée.}
#'    \item{\code{"LT"}}{\strong{L}ess \strong{T}han : la valeur de la caractéristique doit être inférieure à celle recherchée.}
#'    \item{\code{"GT"}}{\strong{G}reater \strong{T}han : la valeur de la caractéristique doit être supérieure à celle recherchée.}
#'    \item{\code{"LE"}}{\strong{L}ess than or \strong{E}qual : la valeur de la caractéristique doit être inférieure ou égale à celle recherchée.}
#'    \item{\code{"GE"}}{\strong{G}reater than or \strong{E}qual : la valeur de la caractéristique doit être supérieure ou égale à celle recherchée.}
#'  }
#' @return Sous-ensemble de la data.frame fournie en argument pour les nœuds satisfaisant le critère de recherche.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{extract_nodes_from_items}}, \code{\link{extract_nodes_from_category}}.
#' @aliases extract_nodes_from_characteristic
#' @export
setMethod(f = "extract_nodes_from_characteristic",
          signature = "SpectralAnalyzer",
          definition = function(object, nodes_characteristics, characteristic, value, condition = "EQ") {
            
            if (!(characteristic %in% c("length", "weight")))
              stop("characteristic must be one of c(\"length\", \"weight\").")
            
            switch(EXPR = condition,
                   "EQ" = { return(nodes_characteristics[nodes_characteristics[characteristic] == value, ]) },
                   "NE" = { return(nodes_characteristics[nodes_characteristics[characteristic] != value, ]) },
                   "LT" = { return(nodes_characteristics[nodes_characteristics[characteristic] < value, ]) },
                   "GT" = { return(nodes_characteristics[nodes_characteristics[characteristic] > value, ]) },
                   "LE" = { return(nodes_characteristics[nodes_characteristics[characteristic] <= value, ]) },
                   "GE" = { return(nodes_characteristics[nodes_characteristics[characteristic] >= value, ]) },
                   stop("value must be one of c(\"EQ\", \"NE\", \"LT\", \"GT\", \"LE\", \"GE\")."))
          })


#' Recherche de nœuds par catégorie
#' 
#' Extrait les nœuds correspondant à une valeur de catégorie recherchée
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @param nodes_characteristics Data frame des nœuds et de leurs caractéristiques.
#' @param category Nom ou numéro de la catégorie sur laquelle effectuer la recherche
#'  (numérotation selon l'ordre des colonnes de \code{object["items_categories"]}).
#' @param value Valeur recherchée pour la catégorie spécifiée par l'argument \code{category}.
#' @param target Condition pour qu'un nœud soit extrait. Choix parmi \code{"vertices"}, \code{"edges"}.
#'  \describe{
#'    \item{\code{"vertices"}}{Recherche des nœuds contenant un item associé à la valeur de catégorie recherchée.}
#'    \item{\code{"edges"}}{Recherche de nœuds générant un lien corresopndant à la valeur de catégorie recherchée.}
#'  }
#' @return Data frame des nœuds correspondant aux critères de recherche.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{extract_nodes_from_items}}, \code{\link{extract_nodes_from_characteristic}}.
#' @aliases extract_nodes_from_category
#' @export
setMethod(f = "extract_nodes_from_category",
          signature = "SpectralAnalyzer",
          definition = function(object, nodes_characteristics, category, value, target) {
            
            # Validation des paramètres
            if (!(target %in% c("vertices", "edges"))) stop("target must be \"vertices\" or \"edges\".")
            check_acces_for_category(object, category, value)
            
            if (target == "vertices") {
              # Recherche des items correspondant à la catégorie recherchée
              items = rownames(subset(object@items_categories, object@items_categories[category] == value))
              # Extraction des noeuds contenant ces items
              return(extract_nodes_from_items(object, nodes_characteristics, items, target = "any"))
              
            } else if (target == "edges") {
              # Recherche de l'ensemble de liens correspondant aux motifs
              links = extract_links(object, "nodes", nodes_characteristics)
              # Valeurs associées à chaque lien pour le type de catégorie recherché
              categories_links = lapply(strsplit(links$items, "/"),
                                        function(x) sort(unique(as.character(object@items_categories[x, category]))))
              # Extraction des liens qui correspondent à la valeur de catégorie recherchée
              links = links[sapply(categories_links, function(x) value %in% x), ]
              # Récupération des noeuds associés
              return(nodes_characteristics[unique(unlist(links[, 1:2])), ])
            }
          })


#' Validation de paramètres de recherche par catégorie
#' 
#' Vérifie que les paramètres fournis correspondent à une catégorie existante.
#' Affiche un message d'erreur si ce n'est pas le cas.
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @param category Nom ou numéro de la catégorie à laquelle accéder (numérotée selon l'ordre des colonnes de \code{object["items_categories"]}).
#' @param value Valeur recherchée pour la catégorie spécifiée par l'argument \code{category}.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{extract_patterns_from_category}}, \code{\link{extract_nodes_from_category}}.
#' @aliases check_acces_for_category
#' @keywords internal
setMethod(f = "check_acces_for_category",
          signature = "SpectralAnalyzer",
          definition = function(object, category, value) {
            
            # Vérification que le type de catégorie recherché existe
            if (is.character(category) & !(category %in% colnames(object@items_categories))) {
              stop("category must be one of ", paste0("\"", colnames(object@items_categories), "\"",
                                                      collapse = ", ") ,".")
            } else if (is.numeric(category) & (category < 1 | category > ncol(object@items_categories))) {
              stop(paste0("category must be in range [1,", ncol(object@items_categories), "]."))
            }
            
            # Vérification que la valeur de la catégorie recherchée existe
            if (!(value %in% levels(object@items_categories[, category]))) {
              stop("value must be one of ", paste0("\"", levels(object@items_categories[, category]), "\"",
                                                   collapse = ", ") ,".")
            }
          })


#' Recherche de motifs par item
#' 
#' Extrait les motifs contenant un ou plusieurs items recherchés.
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @param patterns_characteristics Data frame des motifs et de leurs caractéristiques.
#' @param items Éléments recherchés (un ou plusieurs).
#' @param target Condition pour qu'un motif soit extrait. Choix parmi \code{"all"}, \code{"any"}.
#'  \describe{
#'    \item{\code{"all"}}{L'intégralité des éléments recherchés doivent faire partie d'un motif pour que ce motif soit extrait.}
#'    \item{\code{"any"}}{Au moins un des éléments recherchés doit faire partie d'un motif pour que ce motif soit extrait.}
#'  }
#' @return Sous-ensemble de la data frame fournie en argument contenant les motifs correspondant au critère de recherche.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{extract_patterns_from_characteristic}}, \code{\link{extract_patterns_from_status}},
#'          \code{\link{extract_patterns_from_category}}.
#' @aliases extract_patterns_from_items
#' @export
setMethod(f = "extract_patterns_from_items",
          signature = "SpectralAnalyzer",
          definition = function(object, patterns_characteristics, items, target = "all") {
            
            if (!(target %in% c("all", "any"))) stop("target must be \"all\" or \"any\".")
            
            if (target == "all") func = all
            else if (target == "any") func = any
            
            return(subset(patterns_characteristics, sapply(patterns_characteristics$pattern,
                                                           function(x) func(items %in% x))))
          })


#' Recherche de motifs par caractéristique
#' 
#' Extrait les motifs satisfaisant un critère de recherche relatif à une caractéristique.
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @param patterns_characteristics Data frame des motifs et de leurs caractéristiques.
#' @param characteristic Nom de la caractéristique sur laquelle faire la recherche.
#'  Choix parmi \code{"year"}, \code{"frequency"}, \code{"weight"}, \code{"order"}, \code{"specificity"}.
#' @param value Valeur recherchée pour la caractéristique spécifiée par l'argument \code{characteristic}.
#' @param condition Condition de la recherche.
#'  Choix parmi \code{"EQ"}, \code{"NE"}, \code{"LT"}, \code{"GT"}, \code{"LE"}, \code{"GE"}.
#'  \describe{
#'    \item{\code{"EQ"}}{\strong{EQ}ual : la valeur de la caractéristique doit être égale à celle recherchée.}
#'    \item{\code{"NE"}}{\strong{N}ot \strong{E}qual : la valeur de la caractéristique doit être différente de celle recherchée.}
#'    \item{\code{"LT"}}{\strong{L}ess \strong{T}han : la valeur de la caractéristique doit être inférieure à celle recherchée.}
#'    \item{\code{"GT"}}{\strong{G}reater \strong{T}han : la valeur de la caractéristique doit être supérieure à celle recherchée.}
#'    \item{\code{"LE"}}{\strong{L}ess than or \strong{E}qual : la valeur de la caractéristique doit être inférieure ou égale à celle recherchée.}
#'    \item{\code{"GE"}}{\strong{G}reater than or \strong{E}qual : la valeur de la caractéristique doit être supérieure ou égale à celle recherchée.}
#'  }
#' @return Sous-ensemble de la data frame fournie en argument contant les motifs satisfaisant le critère de recherche.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{extract_patterns_from_items}}, \code{\link{extract_patterns_from_status}},
#'          \code{\link{extract_patterns_from_category}}.
#' @aliases extract_patterns_from_characteristic
#' @export
setMethod(f = "extract_patterns_from_characteristic",
          signature = "SpectralAnalyzer",
          definition = function(object, patterns_characteristics, characteristic, value, condition = "EQ") {
            
            if (!(characteristic %in% c("year", "frequency", "weight", "order", "specificity")))
              stop("characteristic must be one of c(\"year\", \"frequency\", \"weight\", \"order\", \"specificity\").")
            
            switch(EXPR = condition,
                   "EQ" = { return(patterns_characteristics[patterns_characteristics[characteristic] == value, ]) },
                   "NE" = { return(patterns_characteristics[patterns_characteristics[characteristic] != value, ]) },
                   "LT" = { return(patterns_characteristics[patterns_characteristics[characteristic] < value, ]) },
                   "GT" = { return(patterns_characteristics[patterns_characteristics[characteristic] > value, ]) },
                   "LE" = { return(patterns_characteristics[patterns_characteristics[characteristic] <= value, ]) },
                   "GE" = { return(patterns_characteristics[patterns_characteristics[characteristic] >= value, ]) },
                   stop("value must be one of c(\"EQ\", \"NE\", \"LT\", \"GT\", \"LE\", \"GE\")."))
          })


#' Recherche de motifs par statut
#' 
#' Extrait les motifs dont le statut correspond à une valeur recherchée.
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @param patterns_characteristics Data frame des motifs et de leurs caractéristiques.
#' @param value Valeur de statut recherché (une ou plusieurs).
#' @param condition Condition de la recherche.
#'  Choix parmi \code{"EQ"}, \code{"NE"}.
#'  \describe{
#'    \item{\code{"EQ"}}{\strong{EQ}ual : le statut du motif doit être l'une des valeurs recherchées.}
#'    \item{\code{"NE"}}{\strong{N}ot \strong{E}qual : le statut du motif doit être différente des valeurs recherchées.}
#'  }
#' @return Sous-ensemble de la data frame fournie en argument contenant les motifs satisfaisant le critère de recherche.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{extract_patterns_from_items}}, \code{\link{extract_patterns_from_characteristic}},
#'          \code{\link{extract_patterns_from_category}}.
#' @aliases extract_patterns_from_status
#' @export
setMethod(f = "extract_patterns_from_status",
          signature = "SpectralAnalyzer",
          definition = function(object, patterns_characteristics, value, condition = "EQ") {
            
            switch(EXPR = condition,
                   "EQ" = { return(patterns_characteristics[patterns_characteristics$status %in% value, ]) },
                   "NE" = { return(patterns_characteristics[!(patterns_characteristics$status %in% value), ]) },
                   stop("condition must be \"EQ\" or \"NE\"."))
          })


#' Recherche de motifs par catégorie
#' 
#' Extrait les motifs correspondant à une valeur de catégorie recherchée
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @param patterns_characteristics Data frame des motifs et de leurs caractéristiques.
#' @param category Nom ou numéro de la catégorie sur laquelle effectuer la recherche
#'  (numérotation selon l'ordre des colonnes de \code{object["items_categories"]}).
#' @param value Valeur recherchée pour la catégorie spécifiée par l'argument \code{category}.
#' @param target Condition pour qu'un motif soit extrait. Choix parmi \code{"vertices"}, \code{"edges"}.
#'  \describe{
#'    \item{\code{"vertices"}}{Recherche des motifs contenant un item associé à la valeur de catégorie recherchée.}
#'    \item{\code{"edges"}}{Recherche des motifs générant un lien correspondant à la valeur de catégorie recherchée.}
#'  }
#' @return Data frame des motifs correspondant aux critères de recherche.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{extract_patterns_from_items}}, \code{\link{extract_patterns_from_characteristic}},
#'          \code{\link{extract_patterns_from_status}}.
#' @aliases extract_patterns_from_category
#' @export
setMethod(f = "extract_patterns_from_category",
          signature = "SpectralAnalyzer",
          definition = function(object, patterns_characteristics, category, value, target) {
            
            # Validation des paramètres
            if (!(target %in% c("vertices", "edges"))) stop("target must be \"vertices\" or \"edges\".")
            check_acces_for_category(object, category, value)
            
            if (target == "vertices") {
              # Recherche des items correspondant à la catégorie recherchée
              items = rownames(subset(object@items_categories, object@items_categories[category] == value))
              # Extraction des motifs contenant ces items
              return(extract_patterns_from_items(object, patterns_characteristics, items, target = "any"))
              
            } else if (target == "edges") {
              # Recherche de l'ensemble de liens correspondant aux motifs
              links = extract_links(object, "patterns", patterns_characteristics)
              # Valeurs associées à chaque lien pour le type de catégorie recherché
              categories_links = lapply(strsplit(links$items, "/"),
                                        function(x) sort(unique(as.character(object@items_categories[x, category]))))
              # Extraction des liens qui correspondent à la valeur de catégorie recherchée
              links = links[sapply(categories_links, function(x) value %in% x), ]
              # Récupération des motifs associés
              return(patterns_characteristics[unique(unlist(links[, 1:2])), ])
            }
          })


#' Extraction de liens
#' 
#' Extrait de l'ensemble des liens ceux correspondant aux nœuds ou motifs recherchés.
#' 
#' @details
#' Si parmi les nœuds ou motifs, certains deviennent isolés du fait que les autres éléments
#'  auxquels ils sont normalement liés ne font pas partie de \code{characteristics}, ces nœuds ou
#'  motifs sont ajoutés à la fin de la data frame de retour.
#' Ces éventuelles \code{n} lignes additionnelles sont numérotées \code{"A1"..."An"}.
#' 
#' @param object Objet de classe SpectralAnalyzer.
#' @param entities Type d'élément pour lequel rechercher les liens.
#'  Choix parmi \code{"nodes"}, \code{"patterns"}.
#' @param characteristics Data frame des caractéristiques des nœuds ou motifs dont les liens sont à rechercher.
#' @return Data frame associant les nœuds ou motifs liés.
#' 
#' @author Gauthier Magnin
#' @aliases extract_links
#' @export
setMethod(f = "extract_links",
          signature = "SpectralAnalyzer",
          definition = function(object, entities, characteristics) {
            
            if (entities != "nodes" && entities != "patterns")
              stop("entities must be \"nodes\" or \"patterns\".")
            
            # Si les liens recherchés correspondent à l'intégralité des liens
            if (entities == "nodes" && identical(object@nodes, characteristics)) {
              return(object@nodes_links)
            }
            if (entities == "patterns" && identical(object@patterns, characteristics)) {
              return(object@patterns_links)
            }
            
            # Sinon...
            search_nodes = (entities == "nodes")
            all_links = if(search_nodes) object@nodes_links else object@patterns_links
            
            # Sous-ensemble des liens pour lesquels les deux sommets sont à afficher
            # (nop_links = nodes or patterns links)
            nop_links = all_links[all_links$endpoint.1 %in% rownames(characteristics)
                                  & all_links$endpoint.2 %in% rownames(characteristics), ]
            
            # Identification des nouveaux sommets isolés
            isolated = lapply(rownames(characteristics),
                              function(x) {
                                if (!(x %in% unlist(nop_links[, 1:2]))) {
                                  if (search_nodes) return(c(x, x, "", 0))
                                  return(c(x, x, "", 0, object@patterns[x, "year"]))
                                }
                                return(NULL)
                              })
            
            # S'il y a de nouveaux isolés
            if (any(sapply(isolated, function(x) !is.null(x)))) {
              
              # Ajout à l'ensemble des liens/sommets
              no_links = do.call(rbind, isolated)
              colnames(no_links) = colnames(nop_links)
              nop_links = rbind(nop_links, no_links, stringsAsFactors = FALSE)
              class(nop_links$endpoint.1) = class(nop_links$endpoint.2) = class(nop_links$weight) = "integer"
              if(!search_nodes) class(nop_links$year) = "integer"
              
              # Attribution d'index aux nouvelles lignes, différents de ceux de la data frame générale (l'attribut)
              if (nrow(no_links) == nrow(nop_links)) {
                # Si toutes les lignes ne sont que des nouveaux isolés
                rownames(nop_links) = paste0("A", seq_len(nrow(nop_links)))
              } else {
                rownames(nop_links) = c(rownames(nop_links)[1:(nrow(nop_links) - nrow(no_links))],
                                        paste0("A", seq_len(nrow(no_links))))
              }
            }
            
            return(nop_links)
          })
          
