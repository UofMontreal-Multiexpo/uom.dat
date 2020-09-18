#' @include observation_maker.R utils.R
NULL


# Debug mode activation status
DEBUG_MODE = FALSE
# Defines the step up to which to perform the analysis before stopping the process
UP_TO_STEP = Inf



#### Attributes and constructor ####

# Creation of a class corresponding to 2 types
setClassUnion("listORarray", c("list", "array"))

#' Spectral Analyzer
#' 
#' S4 object class allowing spectral analyzes.
#' 
#' @details
#' Default colors are assigned to the values of each category associated with the items.
#'  These colors are selected circularly among the 20 colors of the palette \code{category20}
#'  from D3 (see \code{ggsci::pal_d3("category20")}).
#' Therefore, if the number of values exceeds \code{20}, some colors will be used more than once.
#'  For example, the \out{22<sup>nd</sup>} value will share the color of the \out{2<sup>nd</sup>}
#'  value.
#' See the attribute \code{categories_colors} to reassign colors to the category values.
#' 
#' @slot observations List of observations containing the items corresponding to each observation.
#' @slot items Set of codes identifying the items found in the observations.
#' @slot items_categories Categories associated with the items.
#' @slot categories_colors Colors associated with the values of the categories associated with the items.
#' @slot target Type of patterns to enumerate during the analysis.
#' @slot count Minimum number of occurrences of a pattern to be kept when enumerating patterns.
#' @slot min_length Minimum number of items that a pattern must have to be kept when enumerating patterns.
#' @slot max_length Maximum number of items that a pattern must have to be kept when enumerating patterns.
#' @slot status_limit Time interval for which to characterize the status of the patterns in relation to the total period of observations.
#' @slot nodes Set of nodes: set of separate observations, and characteristics of these nodes.
#' @slot nodes_per_year Number of occurrences of each node per year.
#' @slot n_links Set of weights of the links between the nodes.
#' @slot nodes_links Set of links between the nodes and characteristics of these links.
#' @slot obs_patterns Set of associations between patterns and observation.
#' @slot patterns Set of separate patterns and their characteristics.
#' @slot patterns_per_year Number of occurrences of each pattern per year.
#' @slot p_links Set of weights of the links between the patterns.
#' @slot patterns_links Set of links between the patterns and characteristics of these links.
#' @slot Class List of class attributes (independent of the instance).
#' 
#' @author Gauthier Magnin
#' @references Bosson-Rieutort D, de Gaudemaris R, Bicout DJ (2018).
#'             The spectrosome of occupational health problems.
#'             \emph{PLoS ONE} 13(1): e0190196.
#'             \url{https://doi.org/10.1371/journal.pone.0190196}.
#'             
#'             Bosson-Rieutort D, Sarazin P, Bicout DJ, Ho V, Lavoué J (2020).
#'             Occupational Co-exposures to Multiple Chemical Agents from Workplace Measurements by the US Occupational Safety and Health Administration.
#'             \emph{Annals of Work Exposures and Health}, Volume 64, Issue 4, May 2020, Pages 402–415.
#'             \url{https://doi.org/10.1093/annweh/wxaa008}.
#' @seealso The \code{SpectralAnalyzer} constructor: \code{\link{spectral.analyzer}}.
#' 
#' An example object of class \code{SpectralAnalyzer}: \code{\link{SA_instance}}.
#' @aliases SpectralAnalyzer
#' @export
setClass(Class = "SpectralAnalyzer",
         representation = representation(
           
           observations = "listORarray",
           items = "vector",
           items_categories = "data.frame",
           categories_colors = "list",
           
           target = "character",
           count = "numeric",
           min_length = "numeric",
           max_length = "numeric",
           status_limit = "numeric",
           
           nodes = "data.frame",
           nodes_per_year = "matrix",
           n_links = "matrix",
           nodes_links = "data.frame",
           
           obs_patterns = "matrix",
           
           patterns = "data.frame",
           patterns_per_year = "matrix",
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

# Initiator
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
              
              # Attribution de couleurs aux valeurs de chaque catégorie
              if (length(.Object@items_categories) != 0) {
                .Object@categories_colors = lapply(.Object@items_categories, function(category) {
                  # Sélection circulaire parmi les 20 couleurs d'une palette de D3
                  colors = ggsci::pal_d3("category20")(20)[seq_along(levels(category)) %% 21]
                  return(stats::setNames(colors, levels(category)))
                })
              }
            }
            
            # Descripteurs de la recherche de motifs
            .Object@target = target
            .Object@count = count
            .Object@min_length = min_length
            .Object@max_length = max_length
            .Object@status_limit = status_limit
            
            # Vérification des premiers attributs
            methods::validObject(.Object)
            
            # Attrtibuts de classe
            .Object@Class = list("NODES" = "nodes",
                                 "PATTERNS" = "patterns",
                                 "RULES" = "rules",
                                 "NODES_OR_PATTERNS" = "nop",
                                 "NODES_PATTERNS_OR_RULES" = "npr",
                                 "STATUS_PERSISTENT" = "Persistent",
                                 "STATUS_DECLINING" = "Declining",
                                 "STATUS_EMERGENT" = "Emergent",
                                 "STATUS_LATENT" = "Latent",
                                 "STATUS_COLORS" = c("red", "royalblue", "orange", "gray"))
            names(.Object@Class$STATUS_COLORS) = c(.Object@Class$STATUS_PERSISTENT, .Object@Class$STATUS_DECLINING,
                                                   .Object@Class$STATUS_EMERGENT, .Object@Class$STATUS_LATENT)
            
            # Initialisation des attributs restants
            reset(.Object, from = 1)
            
            methods::validObject(.Object)
            return(.Object)
          })


#' Spectral Analyzer constructor
#' 
#' Create and initialize an object of class \code{SpectralAnalyzer}.
#' 
#' @details
#' If items are not specified using the argument \code{items}, they are automatically listed from
#'  the values of \code{CODE} in the argument \code{observations} without any categorization or
#'  specific denomination.
#' 
#' @param observations List of observations containing the items corresponding to each observation.
#'  Each observation is itself a list containing at least two elements named \code{"CODE"} and
#'  \code{"YEAR"}. \code{"YEAR"} must be numeric and \code{"CODE"} must be character or numeric values.
#'  Values of \code{CODE} must not contain the character \code{"/"}.
#'  An observation can contain any additional information in its list.
#' @param items Data frame associating a name (column \code{name}) and one or more categories
#'  (additional columns) to each item (column \code{item}). Each category must be of type \code{factor}.
#'  The columns \code{item} and \code{name} must be of type \code{character}. The default value,
#'  \code{NULL}, specifies that no name or category is defined.
#' @param target Type of patterns to enumerate. One of \code{"frequent itemsets"},
#'  \code{"closed frequent itemsets"}, \code{"maximally frequent itemsets"}.
#'  By default, \code{"closed frequent itemsets"} provide a summary of frequent patterns in order to
#'  save the necessary memory space. To use even less memory, use \code{"maximally frequent itemsets"}.
#'  To enumerate all possible patterns, use \code{"frequent itemsets"}.
#' @param count Minimum number of occurrences of a pattern to be considered as "frequent".
#' @param min_length Minimum number of items that a pattern must have to be kept when enumerating.
#' @param max_length Maximum number of items that a pattern must have to be kept when enumerating.
#'  The default \code{Inf} corresponds to a pattern search without maximum size limit.
#' @param status_limit Time interval for which to characterize the status of the patterns in relation
#'  to the total period of observations (number of years).
#' @return New object of class \code{SpectralAnalyzer}.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{SpectralAnalyzer}}, \code{\link{reset}}.
#' 
#' @examples
#' ## Creating a SpectralAnalyzer from a list of observations
#' obs <- make_observations(oedb_sample, by = "ID",
#'                          additional = c("CODE", "NAME", "YEAR"))
#' 
#' sa_object_1 <- spectral.analyzer(obs)
#' 
#' ## Creating a SpectralAnalyzer after associating item identifiers with
#' ## names and one category
#' items_ids <- get_all_items(obs)
#' category_1 <- substances_classification[match(items_ids,
#'                                               substances_classification$CODE),
#'                                         "SUBFAMILY"]
#' category_1[is.na(category_1)] <- "Unknown"
#' names <- substances_classification[match(items_ids,
#'                                          substances_classification$CODE),
#'                                    "NAME"]
#' 
#' items <- data.frame(item = items_ids, name = names, family = category_1)
#' sa_object_2 <- spectral.analyzer(obs, items)
#' 
#' @export
spectral.analyzer = function(observations, items = NULL, target = "closed frequent itemsets", count = 1, min_length = 1, max_length = Inf, status_limit = 2) {
  
  # Installation des packages nécessaires au fonctionnement
  # Utile uniquement si les fonctions sont chargées sans charger le package (mode dev)
  packages = c("arules", "ggplot2", "ggsci", "graphics", "grDevices", "mathjaxr", "methods", "network", "sna", "stats", "utils")
  new_packages = packages[!(packages %in% utils::installed.packages()[, "Package"])]
  
  if(length(new_packages) != 0) { 
    cat("Installing required packages:", paste(new_packages, collapse = ", "), "\n")
    utils::install.packages(new_packages)
  }
  
  # Instanciation avec ou sans la liste des items et des catégories associées
  ifelse(is.null(items),
    return(methods::new(Class = "SpectralAnalyzer", observations = observations, target = target, count = count, min_length = min_length, max_length = max_length, status_limit = status_limit)),
    return(methods::new(Class = "SpectralAnalyzer", observations = observations, items = items, target = target, count = count, min_length = min_length, max_length = max_length, status_limit = status_limit)))
}


# Declaration of the SpectralAnalyzer (re)set method
setGeneric(name = "reset", def = function(object, from = 1){ standardGeneric("reset") })

#' Partial reset of a spectral analyzer
#' 
#' Redefine the attributes of a spectral analyzer from a specific step.
#' 
#' @details
#' The steps of construction of a spectral analyzer are:
#' \enumerate{
#'   \item{Enumeration of separate observations per year.}
#'   \item{Enumeration of the nodes and calculation of the number of occurrence.}
#'   \item{Counting the links between nodes.}
#'   \item{Elaboration of links between nodes.}
#'   \item{Enumeration of separate patterns.}
#'   \item{Linking nodes to patterns.}
#'   \item{Characterization of patterns per year.}
#'   \item{Computation of pattern characteristics.}
#'   \item{Counting the links between patterns.}
#'   \item{Elaboration of links between patterns.}
#' }
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @param from Step from which to recompute the attributes.
#' 
#' @author Gauthier Magnin
#' @seealso The \code{\link{SpectralAnalyzer}} constructor: \code{\link{spectral.analyzer}}.
#' 
#' @examples
#' ## Change one attribute (for instance, the pattern enumeration target) and
#' ## enumerate the patterns again for a new analysis
#' SA_instance["target"] <- "frequent itemsets"
#' reset(SA_instance, from = 5)
#' 
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
                "\n*** Step 8/10:  Computation of pattern characteristics... ",
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



#### Methods print, show, plot, summary, length ####

# print: display in console
setMethod(f = "print",
          signature = "SpectralAnalyzer",
          definition = function(x, ...) {
            cat("SpectralAnalyzer\n")
            print(methods::getSlots("SpectralAnalyzer"))
          })

# show: short display in console
setMethod(f = "show",
          signature = "SpectralAnalyzer",
          definition = function(object) {
            cat("SpectralAnalyzer\n")
            print(methods::slotNames(object))
          })

# summary: object summary
setMethod(f = "summary",
          signature = "SpectralAnalyzer",
          definition = function(object, ...) {
            
            summaries = list()
            
            # Résumé des caractéristiques des motifs
            main = cbind(
              "year" = summary(object@patterns$year),
              "frequency" = summary(object@patterns$frequency),
              "weight" = summary(object@patterns$weight),
              "specificity" = summary(object@patterns$specificity)
            )
            colnames(main) = c("year", "frequency", "weight", "specificity")
            
            summaries[["patterns"]] = list(main = main)
            summaries[["patterns"]][["order"]] = as.data.frame(table(object@patterns$order))
            summaries[["patterns"]][["status"]] = as.data.frame(table(object@patterns$status))
            colnames(summaries[["patterns"]][["order"]]) = c("order", "count")
            colnames(summaries[["patterns"]][["status"]]) = c("status", "count")
            
            # Tailles des attributs principaux
            summaries[["count"]] = c(observations = length(object@observations),
                                     items = length(object@items),
                                     categories = ncol(object@items_categories),
                                     nodes = nrow(object@nodes),
                                     patterns = nrow(object@patterns))
            
            return(summaries)
          })



#### Selectors and mutators ####

#' Extract or replace parts of a \code{SpectralAnalyzer} object
#' 
#' General selector and mutator to access the attributes of an object of class \code{SpectralAnalyzer}.
#' Extraction and replacement can be done by using an attribute name or its numeric value in the order
#'  of the attributes.
#' 
#' @inheritParams base::Extract
#' 
#' @author Gauthier Magnin
#' 
#' @examples
#' SA_instance["items"]
#' SA_instance["patterns"]
#' SA_instance[1]
#' 
#' @aliases [,SpectralAnalyzer-method
#' @export
setMethod(f = "[",
          signature = "SpectralAnalyzer",
          definition = function(x, i, j, drop) {
            switch(EXPR = i,
                   "observations" = { return(x@observations) },
                   "items" = { return(x@items) },
                   "items_categories" = { return(x@items_categories) },
                   "categories_colors" = { return(x@categories_colors) },
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

#' @rdname sub-SpectralAnalyzer-ANY-ANY-ANY-method
#' 
#' @examples
#' SA_instance["target"] <- "maximally frequent itemsets"
#' SA_instance["count"] <- 2
#' SA_instance["max_length"] <- 3
#' 
#' @aliases [<-,SpectralAnalyzer-method
#' @export
setReplaceMethod(f = "[",
                 signature = "SpectralAnalyzer",
                 definition = function(x, i, j, value) {
                   switch(EXPR = i,
                          "observations" = { x@observations = value },
                          "items" = { x@items = value },
                          "items_categories" = { x@items_categories = value },
                          "categories_colors" = { x@categories_colors = value },
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
                   
                   methods::validObject(x)
                   return(x)
                 })



#### Declaration of the methods ####

# Computation methods used for the construction of the nodes

setGeneric(name = "list_obs_per_year", def = function(object){ standardGeneric("list_obs_per_year") })

setGeneric(name = "list_separate_obs", def = function(object){ standardGeneric("list_separate_obs") })


# Computation methods used for the construction of spectrosomes

setGeneric(name = "count_links", def = function(object, entities){ standardGeneric("count_links") })

setGeneric(name = "search_links", def = function(object, entities){ standardGeneric("search_links") })


# Computation methods used for the construction of the patterns

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


# Methods for creating spectrum graphs

setGeneric(name = "spectrum_chart", def = function(object, pc, identifiers = "original", sort = TRUE, title = "Spectrum of patterns", path = getwd(), name = "spectrum_of_patterns.pdf"){ standardGeneric("spectrum_chart") })

setGeneric(name = "plot_spectrum_chart", def = function(object, pc, weights_by_node_type, title = "Spectrum of patterns"){ standardGeneric("plot_spectrum_chart") })

setGeneric(name = "compute_pattern_distribution_in_nodes", def = function(object, patterns){ standardGeneric("compute_pattern_distribution_in_nodes") })


# Methods for creating spectrosome graphs and computing related indicators

setGeneric(name = "spectrosome_chart", def = function(object, nopc, identifiers = "original", nb_graphs = 1, min_link_weight = 1, vertex_size = "relative", size_range = c(0.5, 2.5), vertex_col = "status", clusters = Inf, highlight = 3, use_names = TRUE, n.cutoff = NULL, c.cutoff = NULL, display_mixt = TRUE, title = "Spectrosome", path = getwd(), name = "spectrosome.png", ...){ standardGeneric("spectrosome_chart") })

setGeneric(name = "cluster_text", def = function(object, graph, links, display = Inf, highlight = 3, use_names = TRUE, cutoff = NULL){ standardGeneric("cluster_text") })

setGeneric(name = "cluster_chart", def = function(object, nopc, item, identifiers = "original", use_name = TRUE, n.cutoff = NULL, vertex_size = "relative", size_range = c(0.5, 2.5), vertex_col = "status", c.cutoff = NULL, display_mixt = TRUE, title = paste("Cluster of", item), path = getwd(), name = paste0("cluster_of_", item, ".png"), ...){ standardGeneric("cluster_chart") })

setGeneric(name = "network_density", def = function(object, links){ standardGeneric("network_density") })

setGeneric(name = "degree", def = function(object, ID, links){ standardGeneric("degree") })


# Methods for creating multi-association tree graphs

setGeneric(name = "tree_chart", def = function(object, pc, identifiers = "original", use_names = TRUE, n.cutoff = NULL, display_status = TRUE, display_text = "ID", c.cutoff = NULL, sort_by = "category", title = "Multi-association tree", path = getwd(), name = "multi-association_tree.pdf"){ standardGeneric("tree_chart") })

setGeneric(name = "plot_tree_chart", def = function(object, pc, items_category, category = NULL, c.cutoff = NULL, use_names = TRUE, n.cutoff = NULL, display_status = TRUE, display_text = "ID", title = "Multi-association tree"){ standardGeneric("plot_tree_chart") })


# Association rule extraction methods

setGeneric(name = "extract_rules", def = function(object, from, pruning = FALSE, as_sets = FALSE, ...){ standardGeneric("extract_rules") })


# Methods for search and save

setGeneric(name = "save_characteristics", def = function(object, characteristics, ...){ standardGeneric("save_characteristics") })

setGeneric(name = "get_nodes", def = function(object, nc, element, value, condition = "default"){ standardGeneric("get_nodes") })

setGeneric(name = "get_nodes_from_items", def = function(object, nc, items, condition = "all"){ standardGeneric("get_nodes_from_items") })

setGeneric(name = "get_nodes_from_characteristic", def = function(object, nc, characteristic, value, condition = "EQ"){ standardGeneric("get_nodes_from_characteristic") })

setGeneric(name = "get_nodes_from_category", def = function(object, nc, category, value, condition){ standardGeneric("get_nodes_from_category") })

setGeneric(name = "get_patterns", def = function(object, pc, element, value, condition = "default"){ standardGeneric("get_patterns") })

setGeneric(name = "get_patterns_from_items", def = function(object, pc, items, condition = "all"){ standardGeneric("get_patterns_from_items") })

setGeneric(name = "get_patterns_from_characteristic", def = function(object, pc, characteristic, value, condition = "EQ"){ standardGeneric("get_patterns_from_characteristic") })

setGeneric(name = "get_patterns_from_status", def = function(object, pc, value, condition = "EQ"){ standardGeneric("get_patterns_from_status") })

setGeneric(name = "get_patterns_from_category", def = function(object, pc, category, value, condition){ standardGeneric("get_patterns_from_category") })

setGeneric(name = "get_links", def = function(object, nopc){ standardGeneric("get_links") })

setGeneric(name = "get_isolates", def = function(object, nopc){ standardGeneric("get_isolates") })

setGeneric(name = "get_non_isolates", def = function(object, nopc){ standardGeneric("get_non_isolates") })

setGeneric(name = "get_complexes", def = function(object, nopc, category = NULL, condition = NULL, min_nb_values = 2){ standardGeneric("get_complexes") })


# Other specific methods

setGeneric(name = "check_access_for_category", def = function(object, category, value, stop = TRUE){ standardGeneric("check_access_for_category") })

setGeneric(name = "get_nopc", def = function(object, nopc, entities = object@Class$NODES_OR_PATTERNS){ standardGeneric("get_nopc") })

setGeneric(name = "which_entities", def = function(object, npr, entities = object@Class$NODES_OR_PATTERNS){ standardGeneric("which_entities") })



#### Computation methods used for the construction of the nodes ####

#' Enumeration of separate observations per year
#' 
#' Identify the separate observations per year and count their number of occurrences for each one.
#' The resulting matrix is assigned to the attribute \code{nodes_per_year} of \code{object}.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @return Invisible. Matrix of the number of occurrences of each separate observation, per year.
#'  The lines correspond to the observations. The columns correspond to the years.
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


#' Enumeration of nodes
#' 
#' Identify the separate observations and compute their size and number of occurrences.
#' The resulting data frame is assigned to the attribute \code{nodes} of \code{object}.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @return Invisible. Data frame of the separate observations and their characteristics (length and
#'  weight).
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



#### Computation methods used for the construction of spectrosomes ####

#' Counting of links
#' 
#' Count the number of items in common between each pair of nodes or patterns.
#' The resulting matrix is assigned respectively to the attribute \code{n_links} or \code{p_links}
#'  of \code{object}.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @param entities Type of entities for which to count links (nodes or patterns).
#'  One of \code{"nodes"}, \code{"patterns"}.
#' @return Invisible. Adjacency matrix: matrix of the number of links between each pair of nodes
#'  or patterns.
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
            n_intersections = crossprod(table(utils::stack(to_link)))
            
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


#' Elaboration of links
#' 
#' Identify the links according to items in common between nodes or patterns.
#' The resulting data frame is assigned respectively to the attribute \code{nodes_links} or
#'  \code{patterns_links} of \code{object}.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @param entities Type of entities for which to elaborate links (nodes or patterns).
#'  One of \code{"nodes"}, \code{"patterns"}.
#' @return Invisible. Data frame detailing the links between pairs of nodes or patterns.
#'  Isolated nodes or patterns (i.e. unrelated to any other entity) appear at the bottom of the data
#'  frame.
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
                  
                  # Élément i, élément j, items en communs, nb items en communs (, année d'apparition du lien)
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



#### Computation methods used for the construction of the patterns ####

#' Enumeration of patterns
#' 
#' Identify the patterns generated from the observations.
#' The resulting data frame is assigned to the attribute \code{patterns} of \code{object}.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @param target Type of patterns to enumerate. One of \code{"frequent itemsets"},
#'  \code{"closed frequent itemsets"}, \code{"maximally frequent itemsets"}.
#' @param count Minimum number of occurrences of a pattern to be considered as "frequent".
#' @param min_length Minimum number of items that a pattern must have to be kept when enumerating.
#' @param max_length Maximum number of items that a pattern must have to be kept when enumerating.
#'  The default \code{Inf} corresponds to a pattern search without maximum size limit.
#' @return Invisible. Data frame in which a line is an association between a pattern and its
#'  frequency in the set of observations.
#' 
#' @author Gauthier Magnin
#' @aliases list_separate_patterns
#' @keywords internal
setMethod(f = "list_separate_patterns",
          signature = "SpectralAnalyzer",
          definition = function(object, target, count = 1, min_length = 1, max_length = Inf) {
            
            # Nom de l'objet pour modification interne dans l'environnement parent
            object_name = deparse(substitute(object))
            
            
            # Conversion des observations en transactions : une ligne par observation, une colonne par item
            transact = turn_obs_into_transactions(object@observations, "CODE")
            
            # Énumération des motifs recherchés
            params = list(supp = count/dim(transact)[1], 
                          minlen = min_length,
                          maxlen = ifelse(max_length == Inf, dim(transact)[2], max_length), 
                          target = target)
            result = arules::eclat(transact, parameter = params, control = list(verbose = FALSE))
            res = as(result, "data.frame") # Contient aussi le support
            
            # Exraction des motifs issus du résultat et transformation en liste de vecteurs
            patterns = vector_notation(res$items)
            
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


#' Linking nodes to patterns
#' 
#' Associate each separate observation (i.e. each node) with the patterns included in it.
#' The resulting matrix is assigned to the attribute \code{obs_patterns} of \code{object}.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @return Invisible. Logical matrix in which rows correspond to observations and columns correspond
#'  to patterns. A value of \code{TRUE} means the pattern is included in the observation (or the node).
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


#' Enumeration of patterns per year
#' 
#' Count the number of occurrences of each pattern pear year.
#' The resulting matrix is assigned to the attribute \code{patterns_per_year} of \code{object}.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @return Invisible. Matrix of the weights of each pattern, pear year.
#'  The lines correspond to the patterns. The column correspond to the years.
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


#' Computation of pattern characteristics
#' 
#' Compute the characteristics of the patterns (frequency, weight, order, specificity, dynamic status).
#' The resulting data frame is assigned to the attribute \code{patterns} of \code{object}.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @return Invisible. Data frame in which a line is an association between a pattern and its
#'  characteristics.
#' 
#' @author Gauthier Magnin
#' @references Bosson-Rieutort D, de Gaudemaris R, Bicout DJ (2018).
#'             The spectrosome of occupational health problems. \emph{PLoS ONE} 13(1): e0190196.
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


#' Specificity computation
#' 
#' Compute the specificity of the information conveyed by each pattern.
#' The specitificity corresponds to the character of a pattern of being specific of a particular
#'  combination or ubiquitous and allowing the formation of numerous aggregates.
#'  
#' @param object \code{SpectralAnalyzer} class object.
#' @param patterns Patterns whose specificity is to be computed.
#' @param frequencies Vector of frequencies associated with the patterns contained in \code{patterns}.
#' @param weights Vector of weights associated with the patterns contained in \code{patterns}.
#' @return Vector containing the specificity of each pattern contained in \code{patterns}.
#' 
#' @author Gauthier Magnin
#' @references Bosson-Rieutort D, de Gaudemaris R, Bicout DJ (2018).
#'             The spectrosome of occupational health problems. \emph{PLoS ONE} 13(1): e0190196.
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


#' Compute the reporting index (RI)
#' 
#' Compute the reporting index of each pattern for a given period.
#' This index provides information on the proportion and importance of the occurrences of a pattern,
#'  taking into account the occurrences of the other patterns.
#'  
#' @param object \code{SpectralAnalyzer} class object.
#' @param patterns Patterns whose reporting indexes are to be computed.
#' @param t Year of the end of the period, i.e. the date on which to characterize the patterns.
#'  \code{NULL} specifies that the characterization must be done in relation to the last year covered
#'  by the observations.
#' @param period Time interval over which to compute the reporting indexes (number of years).
#'  For example, if \code{t = 2015} and \code{period = 2} then the computation is made over the
#'  period [2014 - 2015].
#'  \code{Inf} specifies that the period considered covers an interval starting on the date of the
#'  oldest observation and ending in the year \code{t}.
#' @return Data frame associating one reporting index with each pattern.
#' 
#' @author Gauthier Magnin
#' @references Bosson-Rieutort D, de Gaudemaris R, Bicout DJ (2018).
#'             The spectrosome of occupational health problems. \emph{PLoS ONE} 13(1): e0190196.
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


#' Validation of RI computation parameters
#' 
#' Check the validity of the values of the parameters given for the computation of reporting indexes.
#' Adapt their values if they do not fall within the correct range and print a warning message.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @param t Year of the end of the period, i.e. the date on which to characterize the patterns.
#'  \code{NULL} specifies that the characterization must be done in relation to the last year covered
#'  by the observations.
#' @param period Time interval over which to compute the reporting indexes (number of years).
#'  For example, if \code{t = 2015} and \code{period = 2} then the computation is made over the
#'  period [2014 - 2015].
#'  \code{Inf} specifies that the period considered covers an interval starting on the date of the
#'  oldest observation and ending in the year \code{t}.
#' @return List containing the final values of \code{t} and \code{period}.
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


#' Computation of RI at temporal limits 
#' 
#' Compute the reporting indexes at the temporal limits used to characterize the patterns.
#' The first one corresponds to the reporting index computed over the \code{first_limit} years.
#' The second one corresponds to the reporting index computed over the period defined by the arguments
#'  \code{t} and \code{period}.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @param patterns Patterns whose limits are to be computed.
#' @param first_limit Time interval over which to compute the first limit (number of years).
#' @param t Year of the end of the period, i.e. the date on which to characterize the pattern.
#'  \code{NULL} specifies that the characterization must be done in relation to the last year covered
#'  by the observations.
#' @param period Time interval over which to do the computation (number of years).
#'  For example, if \code{t = 2015} and \code{period = 2} then the computation is made over the
#'  period [2014 - 2015].
#'  \code{Inf} specifies that the period considered covers an interval starting on the date of the
#'  oldest observation and ending in the year \code{t}.
#' @return Data frame associating each pattern to its two  limits.
#' 
#' @author Gauthier Magnin
#' @references Bosson-Rieutort D, de Gaudemaris R, Bicout DJ (2018).
#'             The spectrosome of occupational health problems. \emph{PLoS ONE} 13(1): e0190196.
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


#' Computation of threshold ksi
#' 
#' Compute the number of patterns allowing to explain the main part of the reporting indexes.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @param reporting_indexes Reporting indexes associated with the patterns.
#' @return Computed threshold.
#' 
#' @author Gauthier Magnin
#' @references Bosson-Rieutort D, de Gaudemaris R, Bicout DJ (2018).
#'             The spectrosome of occupational health problems. \emph{PLoS ONE} 13(1): e0190196.
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


#' RI threshold computation
#' 
#' Compute the limit value separating two dynamic profiles with respect to the reporting indexes.
#' The patterns are ordered in descending order of their reporting index value and separated by
#'  the threshold \code{ksi}.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @param reporting_indexes Reporting indexes associated with the patterns.
#' @param ksi Number of patterns to consider before setting the RI threshold.
#'  Is computed if \code{NULL}.
#' @return Computed threshold.
#' 
#' @author Gauthier Magnin
#' @references Bosson-Rieutort D, de Gaudemaris R, Bicout DJ (2018).
#'             The spectrosome of occupational health problems. \emph{PLoS ONE} 13(1): e0190196.
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


#' Dynamic status assignment
#' 
#' Define the dynamic status of each pattern.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @param patterns Patterns whose dynamic status are to be defined.
#' @param status_limit Time interval over which to characterize the status of the patterns in relation
#' to the period defined by the arguments \code{t} and \code{period}.
#' @param t Year of the end of the period, i.e. the date on which to characterize the pattern.
#'  \code{NULL} specifies that the characterization must be done in relation to the last year covered
#'  by the observations.
#' @param period Time interval over which to characterize the patterns (number of years).
#'  For example, if \code{t = 2015} and \code{period = 2} then the computation is made over the
#'  period [2014 - 2015].
#'  \code{Inf} specifies that the period considered covers an interval starting on the date of the
#'  oldest observation and ending in the year \code{t}.
#' @return Data frame associating each pattern with its dynamic status.
#' 
#' @author Gauthier Magnin
#' @references Bosson-Rieutort D, de Gaudemaris R, Bicout DJ (2018).
#'             The spectrosome of occupational health problems. \emph{PLoS ONE} 13(1): e0190196.
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



#### Methods for creating spectrum graphs ####

#' Pattern spectrum
#' 
#' Plot a spectrum chart and save it as a PDF file.
#' 
#' @details
#' The patterns are sorted according to their specificities (desc.), status (\code{"Persistent"},
#'  \code{"Declining"}, \code{"Emergent"}, \code{"Latent"}), weights (desc.) and sizes (asc.).
#'  If two patterns have the same characteristics concerning thse ones, they are ordered relative to
#'  each other in the order they are given.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @param pc Data frame of \strong{p}atterns and their \strong{c}haracteristics. Patterns whose spectrum
#'  is to be plotted. Any subset of \code{object["patterns"]}.\cr
#'  \code{"patterns"} and \code{"p"} are specific values for \code{object["patterns"]}.
#' @param identifiers Which IDs to use to identify the patterns on the chart and in the return data frame.
#'  One of \code{"original"}, \code{"new"}.
#'  \describe{
#'    \item{\code{"original"}}{Use of the original identifiers.}
#'    \item{\code{"new"}}{Use of new identifiers based on the sorting of patterns or on the order in
#'                        \code{pc}.}
#'  }
#' @param sort If \code{TRUE}, the patterns are sorted on the chart as described in 'Details' section.
#'  Otherwise, they are ordered on the chart in the order in which they are given.
#' @param title Chart title.
#' @param path Path of the directory in which to save the chart.
#'  By default, the chart is saved in the working directory.
#' @param name Name of the file in which to save the chart.
#' @return Data frame of the patterns and characteristics used, associated with the identifiers visible
#'  on the chart.
#' 
#' @author Delphine Bosson-Rieutort, Gauthier Magnin
#' @references Bosson-Rieutort D, de Gaudemaris R, Bicout DJ (2018).
#'             The spectrosome of occupational health problems. \emph{PLoS ONE} 13(1): e0190196.
#'             \url{https://doi.org/10.1371/journal.pone.0190196}.
#' 
#' @examples
#' spectrum_1 <- spectrum_chart(SA_instance, "patterns")
#' spectrum_2 <- spectrum_chart(SA_instance, SA_instance["patterns"][1:15, ],
#'                              name = "spectrum_of_patterns_1-15")
#' 
#' @aliases spectrum_chart
#' @export
setMethod(f = "spectrum_chart",
          signature = "SpectralAnalyzer",
          definition = function(object, pc, identifiers = "original", sort = TRUE,
                                title = "Spectrum of patterns", path = getwd(), name = "spectrum_of_patterns.pdf") {
            
            # Récupération des patterns
            pc = get_nopc(object, pc, object@Class$PATTERNS)
            
            if (identifiers != "original" && identifiers != "new")
              stop("identifiers must be \"original\" or \"new\".")
            
            # Ensembles des poids et longueurs des noeuds contenant les motifs
            patterns_distributions = compute_pattern_distribution_in_nodes(object, pc$pattern)
            weight_distribution = patterns_distributions[["weight_distribution"]]
            length_distribution = patterns_distributions[["length_distribution"]]
            
            # Tri des motifs selon spécificité, statut, poids, longueur
            if (sort) {
              sorting_vector = order(1 - pc$specificity,
                                     match(pc$status, names(object@Class$STATUS_COLORS)),
                                     max(pc$weight) - pc$weight,
                                     pc$order)
              
              pc = pc[sorting_vector, ]
              weight_distribution = weight_distribution[sorting_vector]
              length_distribution = length_distribution[sorting_vector]
            }
            
            # Attribution d'identifiants aux motifs
            if (identifiers == "new") pc$ID = seq(nrow(pc))
            else pc$ID = as.numeric(rownames(pc))
            
            # Décomposition des poids des motifs selon le type de noeuds (simple ou complexe)
            weights = data.frame(complex_nodes = sapply(seq(nrow(pc)), function(x) {
              sum(weight_distribution[[x]][which(length_distribution[[x]] > 1)])
            }))
            weights$simple_node = pc$weight - weights$complex_nodes
            
            
            # Traçage du graphique dans un fichier PDF
            grDevices::pdf(paste0(turn_into_path(path), check_extension(name, "pdf")),
                           15, 8, pointsize = 10)
            plot_spectrum_chart(object, pc, weights, title)
            grDevices::dev.off()
            
            # Motifs et caractéristiques, ordonnés selon ID (replacé en 1ère colonne)
            return(pc[order(pc$ID), c(ncol(pc), seq(ncol(pc)-1))])
          })


#' Pattern spectrum plotting
#' 
#' Plot a spectrum chart.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @param pc Data frame of \strong{p}atterns and their \strong{c}haracteristics. Patterns whose spectrum
#'  is to be plotted. Any subset of \code{object["patterns"]}.
#' @param weights_by_node_type Data frame containing for each pattern, its wieght in complexe nodes
#'  and its weight in simple nodes.
#' @param title Chart title.
#' 
#' @author Delphine Bosson-Rieutort, Gauthier Magnin
#' @references Bosson-Rieutort D, de Gaudemaris R, Bicout DJ (2018).
#'             The spectrosome of occupational health problems. \emph{PLoS ONE} 13(1): e0190196.
#'             \url{https://doi.org/10.1371/journal.pone.0190196}.
#' @seealso \code{\link{spectrum_chart}}, \code{\link{compute_pattern_distribution_in_nodes}}.
#' 
#' @aliases plot_spectrum_chart
#' @keywords internal
setMethod(f = "plot_spectrum_chart",
          signature = "SpectralAnalyzer",
          definition = function(object, pc, weights_by_node_type, title = "Spectrum of patterns") {
            
            # Définition des couleurs des barres du barplot
            bars_colors = object@Class$STATUS_COLORS[pc$status]
            
            # Limite maximale de l'ordonnée concernant la spécificité
            # Écart à 1 ajouté pour que la ligne ne soit pas tronquée en haut du graphique
            # et que les tailles des motifs ayant le poids max ne soient pas masquées par la marge
            y_lim_line = 1.045
            
            # Marge entre les barres et les axes à gauche et à droite
            x_margin = 0.03 * nrow(pc)
            
            ## Bar chart relatif au poids
            graphics::par(mfrow = c(1, 1))
            graphics::par(mar = c(7.1, 5.6, 4.1, 5.6) + 0.1)
            
            # Diagramme en barres selon le poids des motifs
            las = if (length(pc$pattern) < 50) 1 else 3
            y_lim_bar = max(pc$weight) * y_lim_line # Poids max aligné avec specificité de 1
            bar_plot = graphics::barplot(t(weights_by_node_type), main = title,
                                         col = NA, space = 0, lwd = 2,
                                         xlim = c(-x_margin, nrow(pc) + x_margin), xaxs = "i",
                                         ylim = c(0, y_lim_bar), yaxt = "n",
                                         xlab = "Patterns IDs", ylab = "",
                                         names.arg = pc$ID, las = las, font.axis = 2,
                                         cex.main = 1.3, cex.lab = 1.5, cex.axis = 1.5, cex.names = 0.9)
            bar_width_2 = diff(bar_plot[1:2]) / 2
            
            # Axe à gauche : suppression des nombres à virgule, orientation en fonction du nombre
            # et affichage éventuel d'un tick supplémentaire pour délimiter l'axe en haut du graphique
            ticks = unique(trunc(graphics::axTicks(2)))
            if (max(ticks) < max(pc$weight)) {
               ticks = append(ticks, max(pc$weight))
            }
            graphics::axis(2, lwd = 2, cex.axis = 1.5, font.axis = 2,
                           at = ticks, las = if (any(ticks >= 10)) 3 else 1)
            graphics::mtext("Weight", side = 2, line = 3.1, cex = 1.5,
                            at = max(pc$weight) / 2)
            
            # Coloration des barres
            for (i in seq(nrow(weights_by_node_type))) {
              y = c(0, cumsum(c(weights_by_node_type[i, ])))
              graphics::rect(bar_plot[i] - bar_width_2, y[ - length(y)],
                             bar_plot[i] + bar_width_2, y[ - 1],
                             col = bars_colors[i], density = c(300, 15), border = "black")
            }
            
            
            ## Line chart relatif à la spécificité
            graphics::par(new = TRUE)
            
            # Ligne de la spécificité et seuil
            graphics::plot(x = seq(0.5, nrow(pc) - 0.5),
                           y = pc$specificity,
                           lwd = 3, type = "b", col = "black", pch = 20,
                           bty = "n", xlab = "", ylab = "", main = "",
                           xlim = c(-x_margin, nrow(pc) + x_margin), xaxt = "n", xaxs = "i",
                           ylim = c(0, y_lim_line), yaxt = "n", yaxs = "i")
            graphics::segments(x0 = 0, y0 = 0.5,
                               x1 = nrow(pc) * (1 + x_margin),
                               lwd = 0.5, lty = "dotted")
            
            # Axe et titre à droite
            graphics::axis(4, yaxp = c(0, 1, 5), lwd = 2, cex.axis = 1.5, font.axis = 2)
            graphics::mtext("Specificity", side = 4, line = 3.1, cex = 1.5, at = 0.5)
            
            
            ## Texte relatif aux tailles des motifs
            # Changement du système de coordonnées du au changement de graphique (bar -> line)
            new_y = pc$weight * y_lim_line / y_lim_bar
            shadowtext(bar_plot, new_y, utils::as.roman(pc$order),
                       col = "black", bg = "white", cex = 0.8, pos = 3, offset = 1)
            
            
            ## Légendes
            graphics::legend("bottomleft", bty = "n", horiz = TRUE, xpd = NA, inset = c(0, -0.15),
                             pch = 15, col = object@Class$STATUS_COLORS,
                             legend = names(object@Class$STATUS_COLORS), cex = 1.1)
            graphics::legend("bottomright", bty = "n", xpd = NA, adj = 0, inset = c(0.165, -0.135),
                             pch = NA_integer_,
                             legend = paste0(utils::as.roman(min(pc$order)), " ... ",
                                             utils::as.roman(max(pc$order)), ":  Order"),
                             cex = 1.1)
            graphics::legend("bottomright", bty = "n", xpd = NA, adj = 0, inset = c(0.165, -0.165),
                             pch = 20, lty = 1,
                             legend = "Specificity", cex = 1.1)
            graphics::legend("bottomright", bty = "n", xpd = NA, inset = c(0.005, -0.165),
                             fill = "red", density = c(600, 15),
                             legend = c("Weight in complex nodes", "Weight in simple nodes"), cex = 1.1)
          })


#' Distribution of patterns among nodes
#' 
#' Compute the distributions of the weights and lenghts of the nodes in which each pattern is included.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @param patterns Patterns whose distributions are to be calculated.
#' @return
#'  \describe{
#'    \item{\code{weight_distribution}}{The distribution, for each pattern, of the weights of nodes in
#'                                      which they are included.}
#'    \item{\code{length_distribution}}{The distribution, for each pattern, of the lengths of nodes in
#'                                      which they are included.}
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



#### Methods for creating spectrosome graphs and computing related indicators ####

#' Spectrosome
#' 
#' Plot one or more spectrosome charts and save them in PNG format.
#' 
#' @details
#' If categories are associated with the items, each category generates a spectrosome.
#'  The category name is appended to the end of the file name.
#' 
#' If mixed links are relative to category values that are not represented by single links, these
#'  values are present in the legend below "Mixt", with no associated color.
#' The same is true for mixed vertices if the argument \code{vertex_col} is \code{"categories"}.
#' 
#' If \code{min_link_weight} is greater than 1 or the chart is to be plotted from a subset of
#'  all nodes or patterns, some of them may become isolated because their links to the other
#'  entities may no longer be considered.
#' These new isolated vertices are moved to the end of the return data frame \code{edges}.
#' The \code{n} related lines are numbered \code{"A1"..."An"}.
#' 
#' The colors associated with the values of each category represented are selected circularly
#'  among the 20 colors of the palette \code{category20} from D3 (see \code{ggsci::pal_d3("category20")}).
#' Therefore, if the number of values exceeds \code{20}, some colors will be used more than once.
#'  For example, the \out{22<sup>nd</sup>} value will share the color of the \out{2<sup>nd</sup>}
#'  value.
#' See the attribute \code{categories_colors} of \code{object} to reassign colors to the category values.
#' 
#' The names of clusters confused because all of their links are mixed links, are not displayed.
#' 
#' Additional arguments can be supplied to the function in charge of plotting the graph.
#'  See the list of parameters: \code{\link[sna:gplot]{sna::gplot}}.
#' Among them, the following parameters are already defined and cannot be modified: \code{dat},
#'  \code{gmode}, \code{vertex.sides}, \code{vertex.cex}, \code{vertex.col}, \code{edge.col}.
#' The following parameters, which can be redefined, have the following values:
#'  \itemize{
#'    \item{\code{mode = "fruchtermanreingold"}}
#'    \item{\code{displaylabels = TRUE}}
#'    \item{\code{label.pos = 0}}
#'    \item{\code{boxed.labels = TRUE}}
#'    \item{\code{displayisolates = TRUE}}
#'  }
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @param nopc Data frame of \strong{n}odes \strong{o}r \strong{p}atterns and their
#'  \strong{c}haracteristics. Nodes or patterns whose spectrosome is to be plotted. Any subset of
#'  \code{object["nodes"]} or \code{object["patterns"]}.\cr
#'  \code{"nodes"}, \code{"n"}, \code{"patterns"} and \code{"p"} are specific values for
#'  \code{object["nodes"]} and \code{object["patterns"]}.
#' @param identifiers Which IDs to use to identify the nodes or patterns on the chart and in the
#'  return data frames. One of \code{"original"}, \code{"new"}.
#'  \describe{
#'    \item{\code{"original"}}{Use of the original identifiers.}
#'    \item{\code{"new"}}{Use of new identifiers ordered according to the subset corresponding to
#'                        \code{nopc}.}
#'  }
#' @param nb_graphs Number of graphics to generate and save. The place of the vertices differs between
#'  each copy.
#' @param min_link_weight Minimum number of items in common between two entities to plot their link on
#'  the chart.
#' @param vertex_size Way how the sizes of the vertices of the graph should be defined.
#'  One of \code{"relative"}, \code{"grouped"}, \code{"absolute"}, \code{"equal"}.
#'  \describe{
#'    \item{\code{"relative"}}{The sizes are defined by a linear interpolation of the weights of the
#'                             entities in \code{size_range}.}
#'    \item{\code{"grouped"}}{The weights of the entities are grouped according to 5 intervals
#'                            defined by quantiles. The 5 corresponding size values are taken in a
#'                            regular sequence bounded by \code{size_range}.}
#'    \item{\code{"absolute"}}{The size of a vertex is defined directly according to the weight of
#'                             the entity.}
#'    \item{\code{"equal"}}{The vertices are all the same size of 1.}
#'  }
#' @param size_range If \code{vertex_size} is \code{"relative"} or \code{"grouped"}.
#'  Range of vertex size values (given as expansion factors).
#' @param vertex_col Way how the colors of the vertices of the graph should be defined.
#'  One of \code{"status"}, \code{"categories"}, \code{"none"}.
#'  If \code{"status"} and \code{entities = "patterns"}, coloring according to the status of the patterns.
#'  If \code{"categories"}, coloring according to the categories associated with the items of the
#'  entities represented.
#'  In all other cases, the vertices are colored gray.
#' @param clusters Maximum number of clusters to name on the graph.
#'  If the actual number of clusters is greater, the names of the smaller clusters are not displayed.
#' @param highlight Number of clusters to highlight among those named on the graph.
#'  The names of the largest clusters are displayed in bold.
#' @param use_names If \code{TRUE}, display item names if they are defined. Display their identification
#'  codes otherwise.
#' @param n.cutoff If \code{use_names = TRUE}, limit number of characters to display concerning the names
#'  of the represented items.
#' @param c.cutoff Limit number of characters to display in the legend for the categories represented.
#' @param display_mixt If \code{TRUE}, display in the legend the category values included only in mixed
#'  links (or in mixed vertices, if \code{vertex_col = "categories"}).
#' @param title Chart title.
#' @param path Path of the directory in which to save the charts.
#'  By default, the charts are saved in the working directory.
#' @param name Name of the file in which to save the chart.
#'  If \code{nb_graphs} is greater than \code{1}, a number is automatically added to the end of the
#'  file name.
#' @param ... Additional arguments to the function \code{\link[sna:gplot]{gplot}} from the package
#'  \code{sna} for plotting the graph. See Details section.
#' @return
#'  \describe{
#'    \item{\code{vertices}}{Data frame of the nodes or patterns and characteristics used,
#'                           associated with the identifiers of the vertices of the graph and their
#'                           degrees in the graph.}
#'    \item{\code{edges}}{Data frame of information relating to the edges of the graph.}
#'    \item{\code{coords}}{List containing the coordinate matrices of the vertices of the graph.
#'                         As many matrices as there are charts (\code{nb_graphs}), which can be
#'                         reused via the argument \code{coord} (see \code{...}).}
#'  }
#' 
#' @author Delphine Bosson-Rieutort, Gauthier Magnin
#' @references Bosson-Rieutort D, de Gaudemaris R, Bicout DJ (2018).
#'             The spectrosome of occupational health problems. \emph{PLoS ONE} 13(1): e0190196.
#'             \url{https://doi.org/10.1371/journal.pone.0190196}.
#' @seealso \code{\link{cluster_chart}}, \code{\link{degree}}, \code{\link[sna:gplot]{sna::gplot}}.
#' 
#' @examples
#' spectrosome_1 <- spectrosome_chart(SA_instance, "nodes",
#'                                    name = "spectrosome_of_nodes")
#' spectrosome_2 <- spectrosome_chart(SA_instance, SA_instance["patterns"])
#' spectrosome_3 <- spectrosome_chart(SA_instance, SA_instance["patterns"][1:15, ],
#'                                    name = "spectrosome_of_patterns_1-15")
#' 
#' @aliases spectrosome_chart
#' @export
setMethod(f = "spectrosome_chart",
          signature = "SpectralAnalyzer",
          definition = function(object, nopc,
                                identifiers = "original",
                                nb_graphs = 1, min_link_weight = 1,
                                vertex_size = "relative", size_range = c(0.5, 2.5), vertex_col = "status",
                                clusters = Inf, highlight = 3,
                                use_names = TRUE, n.cutoff = NULL, c.cutoff = NULL, display_mixt = TRUE,
                                title = "Spectrosome", path = getwd(), name = "spectrosome.png",
                                ...) {
            
            # Récupération des noeuds/patterns et recherche du type d'entités fourni
            nopc = get_nopc(object, nopc)
            entities = which_entities(object, nopc)
            
            # Validation des paramètres
            if (nrow(nopc) < 2)
              stop("There must be at least 2 nodes or patterns to draw a spectrosome.")
            
            if (identifiers != "original" && identifiers != "new")
              stop("identifiers must be \"original\" or \"new\".")
            
            if (vertex_col != "status" && vertex_col != "categories" && vertex_col != "none")
              stop("vertex_col must be \"status\", \"categories\" or \"none\".")
            
            
            # Extraction des liens pour les éléments à visualiser (nop_links = nodes or patterns links)
            nop_links = get_links(object, nopc)
            
            if (all(nop_links$weight == 0) && "displayisolates" %in% names(list(...)) && !(list(...)$displayisolates)) {
              warning("There is no graph to plot: displayisolates = FALSE and all entities are isolated.")
              return(NULL)
            }
            
            if (entities == "nodes") {
              # Renommage de colonnes pour simplification ultérieure (cf. vertices_colors et vertices_shapes)
              colnames(nopc)[colnames(nopc) == "node"] = "pattern"
              colnames(nopc)[colnames(nopc) == "length"] = "order"
              
              # Texte affiché sur le graphique
              nop_subtitle_1 = "Nodes: %d (%d isolate"
              nop_subtitle_2 = "); Links: %d"
              
              not_identical = !identical(object@nodes, nopc)
              
            } else if (entities == "patterns") {
              # Texte affiché sur le graphique
              nop_subtitle_1 = "Patterns: %d (%d isolate"
              nop_subtitle_2 = "); Links: %d"
              
              not_identical = !identical(object@patterns, nopc)
            }
            
            # Identifiants des sommets du graphe
            vertices_id = seq(nrow(nopc))
            
            if (not_identical) {
              # Nouvelle numérotation des éléments conservés
              names(vertices_id) = rownames(nopc)
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
                  links_categories = lapply(strsplit(nop_links$items, "/"),
                                            function(x) sort(unique(as.character(object@items_categories[x, category]))))
                  category_values = unique(unlist(links_categories))
                  links_categories = unlist(lapply(links_categories, function(x) {
                    if (length(x) == 1) return(x)
                    if (length(x) > 1) return("Mixt")
                    return("Isolated")
                  }))
                  
                  # Séparation des valeurs de la catégorie qui sont uniquement inclus dans des liens mixtes
                  category_mixed = sort(setdiff(category_values, unique(links_categories)))
                  category_not_mixed = sort(setdiff(category_values, category_mixed))
                  
                  # Sélection des couleurs associées
                  categories_colors[[category]] = c(object@categories_colors[[category]][category_not_mixed],
                                                    "black", "white")
                  names(categories_colors[[category]]) = c(category_not_mixed, "Mixt", "Isolated")
                  
                  # Couleurs des liens tracés sur le graphique
                  links_colors[[category]] = categories_colors[[category]][links_categories]
                  
                  # Retrait du noir associé aux liens mixtes s'il n'y en a pas et retrait du blanc
                  # associé aux isolés, pour ne pas les afficher ultérieurement dans la légende
                  if (length(category_mixed) == 0 && !("Mixt" %in% links_categories)) {
                    categories_colors[[category]] = categories_colors[[category]][seq(length(categories_colors[[category]])-2)]
                  } else {
                    categories_colors[[category]] = categories_colors[[category]][seq(length(categories_colors[[category]])-1)]
                    
                    # Ajout des valeurs de catégorie inclus uniquement dans des liens mixtes
                    if (display_mixt) {
                      new_names = c(names(categories_colors[[category]]), category_mixed)
                      categories_colors[[category]] = append(categories_colors[[category]], rep("white", length(category_mixed)))
                      names(categories_colors[[category]]) = new_names
                    }
                  }
                  
                } else if(length(levels(object@items_categories[, category])) == 1) {
                  # Une unique catégorie
                  categories_colors[[category]] = c("black", "white")
                  names(categories_colors[[category]]) = c(levels(object@items_categories[, category]), "Isolated")
                  
                  links_categories = ifelse(nop_links$weight == 0, "Isolated", levels(object@items_categories[, category]))
                  
                  # Couleurs des liens tracés sur le graphique
                  links_colors[[category]] = categories_colors[[category]][links_categories]
                  
                  # Retrait du blanc associé aux isolés pour ne pas l'afficher ultérieurement dans la légende
                  categories_colors[[category]] = categories_colors[[category]][seq(length(categories_colors[[category]])-1)]
                  
                } else {
                  stop("The categories associated with the items must be factor type and it must have at least one factor.")
                }
              }
            }
            
            
            # Définition des couleurs des sommets en fonction du statut et nombre pour chaque statut
            if (entities == "patterns" && vertex_col == "status") {
              vertices_colors = object@Class$STATUS_COLORS[nopc$status]
              count_status = sapply(names(object@Class$STATUS_COLORS),
                                    function(status) sum(nopc$status == status))
              
              # Légende associée
              legend_1 = c(names(object@Class$STATUS_COLORS), "", "Single items", "Multiple items")
              col_1 = c(object@Class$STATUS_COLORS, "white", "black", "black")
              
            } else if (vertex_col != "categories") {
              # Noeuds ou motifs sans affichage du statut : couleur grise
              vertices_colors = rep("grey", nrow(nopc))
              
              # Légende associée
              legend_1 = c("Single items", "Multiple items")
              col_1 = c("black", "black")
              
            } else {
              # Couleurs en fonction de la catégorie
              v.categories_colors = list()
              vertices_colors = list()
              legend_1 = list()
              col_1 = list()
              
              if (length(object@items_categories) == 0) {
                v.categories_colors[[1]] = character(0)
                vertices_colors[[1]] = rep("grey", nrow(nopc))
                legend_1[[1]] = c("Single items", "Multiple items")
                col_1[[1]] = c("black", "black")
                
              } else {
                # Pour chaque catégorie
                for (category in seq_len(ncol(object@items_categories))) {
                  
                  # Valeurs de la catégorie associées aux sommets
                  vertices_categories = lapply(nopc$pattern,
                                               function(x) sort(unique(as.character(object@items_categories[x, category]))))
                  category_values = unique(unlist(vertices_categories))
                  vertices_categories = unlist(lapply(vertices_categories, function(x) {
                    if (length(x) == 1) return(x)
                    if (length(x) > 1) return("Mixt")
                  }))
                  
                  # Séparation des valeurs de la catégorie qui sont uniquement inclus dans des sommets mixtes
                  category_mixed = sort(setdiff(category_values, unique(vertices_categories)))
                  category_not_mixed = sort(setdiff(category_values, category_mixed))
                  
                  # Sélection des couleurs associées
                  v.categories_colors[[category]] = c(object@categories_colors[[category]][category_not_mixed],
                                                      "black")
                  names(v.categories_colors[[category]]) = c(category_not_mixed, "Mixt")
                  
                  # Couleurs des sommets tracés sur le graphique
                  vertices_colors[[category]] = v.categories_colors[[category]][vertices_categories]
                  
                  # Retrait du noir associé aux sommets mixtes s'il n'y en a pas, pour ne pas l'afficher ultérieurement dans la légende
                  if (length(category_mixed) == 0) {
                    v.categories_colors[[category]] = v.categories_colors[[category]][seq(length(v.categories_colors[[category]])-1)]
                  } else {
                    # Ajout des valeurs de catégorie inclus uniquement dans des sommets mixtes
                    if (display_mixt) {
                      new_names = c(names(v.categories_colors[[category]]), category_mixed)
                      v.categories_colors[[category]] = append(v.categories_colors[[category]], rep("white", length(category_mixed)))
                      names(v.categories_colors[[category]]) = new_names
                    }
                  }
                  
                  # Légende associée
                  legend_1[[category]] = c(names(v.categories_colors[[category]]),
                                           "", "Single items", "Multiple items")
                  col_1[[category]] = c(v.categories_colors[[category]], "white", "black", "black")
                }
              }
            }
            
            # Sommets à plusieurs items en cercle ; triangle sinon
            vertices_shapes = rep(100 , nrow(nopc))
            vertices_shapes[nopc$order == 1] = 3
            
            # Définition des tailles des sommets
            switch(EXPR = vertex_size,
                   "relative" = {
                     # Interpolation linéaire des poids aux valeurs [size_range[1], size_range[2]]
                     if (min(nopc$weight) != max(nopc$weight)) {
                       func = stats::approxfun(x = c(min(nopc$weight),
                                                     max(nopc$weight)),
                                               y = size_range)
                       vertices_sizes = func(nopc$weight)
                     } else {
                       vertices_sizes = rep(mean(size_range), length(nopc$weight))
                     }
                   },
                   "grouped" = {
                     # Groupement des valeurs des poids selon 5 quantiles
                     breaks = round(stats::quantile(nopc$weight, prob = seq(0, 1, 0.2)))
                     intervals = cut(nopc$weight, breaks = unique(breaks), include.lowest = TRUE)
                     sizes = seq(size_range[1], size_range[2], length.out = length(levels(intervals)))
                     vertices_sizes = sizes[intervals]
                   },
                   "absolute" = {
                     vertices_sizes = nopc$weight
                   },
                   "equal" = {
                     # Valeur par défaut de l'argument vertex.cex de la fonction sna::gplot()
                     vertices_sizes = 1
                   },
                   stop("Unknown value for vertex_size. Must be one of \"relative\", \"grouped\", \"absolute\", \"equal\"."))
            
            
            # Réseau généré avec le package network
            links = as.matrix(nop_links[, c("endpoint.1", "endpoint.2")], ncol = 2)
            network_data = network::network(links, directed = FALSE, matrix.type = "edgelist")
            vertices_names = network::network.vertex.names(network_data)
            
            # Récupération des arguments additionnels et détermination de valeurs par défaut pour sna::gplot
            args = list(...)
            if(!("mode" %in% names(args))) args$mode = "fruchtermanreingold"
            if(!("displaylabels" %in% names(args))) args$displaylabels = TRUE
            if(!("label.pos" %in% names(args))) args$label.pos = 0
            if(!("boxed.labels" %in% names(args))) args$boxed.labels = TRUE
            if(!("displayisolates" %in% names(args))) args$displayisolates = TRUE
            if(!("label" %in% names(args))) {
              if (identifiers == "new") args$label = vertices_id
              else args$label = rownames(nopc)
            }
            
            # Duplication de la fonction utilisée par l'argument "mode" de la fonction sna::gplot
            # pour fonctionner sans avoir à charger le package
            eval(parse(text = paste0("gplot.layout.", args$mode, " <- sna::gplot.layout.", args$mode)))
            
            # Nombre de variantes du graphique
            nb_categories = ifelse(length(object@items_categories) == 0, 1, ncol(object@items_categories))
            name = check_extension(name, "png")
            
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
                file_name = ifelse(nb_graphs == 1, name, sub(".png", paste0("-", i, ".png"), name))
                file_name = ifelse(nb_categories == 1,
                                   file_name,
                                   sub(".png", paste0("-", colnames(object@items_categories)[j], ".png"), file_name))
                
                # Traçage des graphiques dans des fichiers PNG
                grDevices::png(paste0(turn_into_path(path), file_name), 950, 700)
                graphics::par(mar = c(0.5, 0.5, 4.5, 0.5))
                graphics::plot.new()
                
                # Titres du graphique
                title(main = title, cex.main = 1.5, line = 2.5)
                nb_isolates = length(sna::isolates(network_data))
                title(main = paste0(sprintf(nop_subtitle_1, nrow(nopc), nb_isolates),
                                    if (nb_isolates < 2) "" else "s",
                                    sprintf(nop_subtitle_2, sum(nop_links$weight != 0))),
                      font.main = 3, line = 1)
                
                # Préparation des formes de la légende du graphique
                if (entities == "patterns" && vertex_col == "status") {
                  legend_pt.cex = c(rep(2, length(object@Class$STATUS_COLORS)), 1.7, 1.9, rep(2, length(categories_colors[[j]])))
                  legend_pch = c(rep(15, length(object@Class$STATUS_COLORS)), 0, 2, 1, 0, rep(20, length(categories_colors[[j]])))
                } else if (vertex_col != "categories" || length(object@items_categories) == 0) {
                  legend_pt.cex = c(1.9, rep(2, length(categories_colors[[j]])))
                  legend_pch = c(2, 1, 0, rep(20, length(categories_colors[[j]])))
                } else {
                  legend_pt.cex = c(rep(2, length(v.categories_colors[[j]])), 1.7, 1.9, rep(2, length(categories_colors[[j]])))
                  legend_pch = c(rep(15, length(v.categories_colors[[j]])), 0, 2, 1, 0, rep(20, length(categories_colors[[j]])))
                }
                
                # Préparation de la légende des liens, à la suite des statuts et sommets
                if (is.null(c.cutoff)) {
                  legend_legend = c(if (vertex_col == "categories") legend_1[[j]] else legend_1,
                                    "", names(categories_colors[[j]]))
                } else {
                  if (vertex_col == "categories") {
                    if (length(object@items_categories) != 0) {
                      # Application du cutoff sur la légende des couleurs des sommets également
                      nb_vertices_leg = length(legend_1[[j]])
                      legend_legend = c(substr(legend_1[[j]][1:(nb_vertices_leg-3)], 1, c.cutoff),
                                        legend_1[[j]][(nb_vertices_leg-2):nb_vertices_leg],
                                        "", substr(names(categories_colors[[j]]), 1, c.cutoff))
                    } else {
                      legend_legend = legend_1[[1]]
                    }
                  } else {
                    legend_legend = c(legend_1, "", substr(names(categories_colors[[j]]), 1, c.cutoff))
                  }
                }
                legend_col = c(if (vertex_col == "categories") col_1[[j]] else col_1,
                               "white", categories_colors[[j]])
                
                # Affichage de la légende
                graphics::legend("topleft", bty = "n", xpd = NA, pt.cex = legend_pt.cex, pch = legend_pch,
                                 legend = legend_legend, col = legend_col)
                # Taille de la légende (labels + pch + espace à droite)
                legend_size = graphics::strwidth(legend_legend, units = "inches") + 
                              graphics::strwidth("1", units = "inches") * 4.5
                
                # Légende supplémentaire concernant la distribution des statuts
                if (entities == "patterns" && vertex_col == "status") {
                  status_legend = paste0("(", count_status, ")")
                  legend_size[1:4] = legend_size[1:4] + graphics::strwidth(status_legend, units = "inches") + 
                                                        graphics::strwidth("1", units = "inches") * 2.5
                  
                  graphics::legend("topleft", bty = "n", xpd = NA, inset = c(0.065, 0), legend = status_legend)
                }
                
                
                # Réinitialisation des marges de la zone graphique pour séparer légende et plot
                graphics::par(new = TRUE, mai = graphics::par()$mai + c(0, max(legend_size), 0, 0))
                
                # Dessin du graphe : appel de sna::gplot avec les arguments de ... modifiés (variable args)
                coord = do.call(sna::gplot, c(list(
                  dat = network_data, gmode = "graph",
                  coord = coord,
                  vertex.sides = vertices_shapes,
                  vertex.cex = vertices_sizes,
                  vertex.col = if (vertex_col == "categories") vertices_colors[[j]] else vertices_colors,
                  edge.col = links_colors[[j]]
                ), args))
                
                # S'il y a bien des liens, identification et affichage des noms des clusters
                if (sum(nop_links$weight != 0)) {
                  cluster_text(object, coord, nop_links, clusters, highlight, use_names, n.cutoff)
                }
                
                # Fermeture du fichier PNG
                grDevices::dev.off()
              }
              
              # Récupération des coordonnées des sommets du graphe
              coords_list[[i]] = coord
            }
            
            # Calcul du degré de chaque sommet dans le graphe
            degrees = sapply(vertices_id, function(ID) degree(object, ID, nop_links))
            # Renommage initial des colonnes avant retour
            if (entities == "nodes") {
              colnames(nopc)[colnames(nopc) == "pattern"] = "node"
              colnames(nopc)[colnames(nopc) == "order"] = "length"
            }
            
            # Réattribution des ID d'origine (non compatibles avec sna::gplot)
            if (identifiers == "original" && not_identical) {
              nop_links$endpoint.1 = names(vertices_id[nop_links$endpoint.1])
              nop_links$endpoint.2 = names(vertices_id[nop_links$endpoint.2])
              
              vertices_id = as.numeric(rownames(nopc))
            }
            
            # Noeuds ou motifs, caractéristiques, identifiants sur le graphique et degrés dans le graphe
            return(list(vertices = data.frame(ID = vertices_id, nopc, degree = degrees),
                        edges = nop_links,
                        coords = coords_list))
          })


#' Display of cluster names
#' 
#' Identify and display the names of the clusters on the graph provided as an argument.
#' The names of clusters confused because all of their links are mixed links, are not displayed.
#' Texts are written on the active graphics device.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @param graph Graph generated by the function \code{\link[sna:gplot]{gplot}} from the package
#'  \code{sna}: "A two-column matrix containing the vertex positions as x,y coordinates".
#' @param links Links of nodes or patterns used to generate \code{graph}.
#' @param display Maximum number of clusters to name on the graph.
#'  If the actual number of clusters is greater, the names of the smaller clusters are not displayed.
#' @param highlight Number of clusters to highlight among those named on the graph.
#'  The names of the largest clusters are displayed in bold.
#' @param use_names If \code{TRUE}, display item names if they are defined. Display their identification
#'  codes otherwise.
#' @param cutoff If \code{use_names = TRUE}, limit number of characters to display concerning the names
#'  of the represented items.
#' 
#' @author Delphine Bosson-Rieutort, Gauthier Magnin
#' @seealso \code{\link{spectrosome_chart}}, \code{\link{cluster_chart}}, \code{\link[sna:gplot]{sna::gplot}}.
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
            coords = stats::aggregate(data.frame(MOY.X = coord_L$X),
                                      by = list(LABEL = coord_L$LABEL), mean)
            coords$MOY.Y = tapply(coord_L$Y, coord_L$LABEL, mean)
            
            # Association des coordonnées moyennes des liens exactes (non multiples et non décomposés) aux noms des items ayant générés le plus de liaisons
            coords = coords[match(clusters, as.character(coords$LABEL)), ]
            #! Les coordonnées ne sont donc pas la moyenne de tous les liens correspondant à l'item
            #! mais uniquement de ceux qui correspondent exactement à cet item (pas de combinaisons)
            #! bien que la variable "cluster" a recherché le nombre de liens correspondant à l'item, qu'il y ait une combinaison ou non.
            #! => Permet une sorte d'attraction du label vers les sommets partageant uniquement l'item.
            
            # Extraction des noms des items ayant générés le plus de liaisons
            coords = coords[stats::complete.cases(coords), ]
            if (nrow(coords) >= display) coords = coords[seq_len(display), ]
            
            # S'il y a effectivement des clusters à nommer (ce n'est pas le cas s'il n'y a que des liens mixtes)
            if (nrow(coords) > 0) {
              # Affichage des noms des "clusters" retenus
              if (use_names) {
                clusters = names(object@items)[match(coords$LABEL, object@items)]
                if (!is.null(cutoff)) clusters = substr(clusters, 1, cutoff)
              }
              shadowtext(coords$MOY.X, coords$MOY.Y, clusters, r = 0.3,
                         col = "black", bg = "white", cex = 0.9,
                         font = ifelse(clusters %in% clusters[seq_len(highlight)], 2, 1))
            }
          })


#' Cluster: subgraph of a spectrosome
#' 
#' Identify the cluster associated with one specific item and plot a spectrosome of this cluster.
#' 
#' @details
#' If categories are associated with the items, each category generates a spectrosome.
#'  The category name is appended to the end of the file name.
#' 
#' If mixed links are relative to category values that are not represented by single links, these
#'  values are present in the legend below "Mixt", with no associated color.
#' The same is true for mixed vertices if the parameter \code{vertex_col} is \code{"categories"}.
#' 
#' If the chart is to be plotted from a subset of all nodes or patterns and some become isolated
#'  because the other entities to which they are normally linked are not part of the subset \code{nopc},
#'  these nodes or patterns are placed at the end of the return data frame \code{edges}.
#' These possible \code{n} additional lines are numbered \code{"A1"..."An"}.
#' 
#' Additional arguments can be supplied to the function in charge of plotting the graph.
#'  See the list of parameters: \code{\link[sna:gplot]{sna::gplot}}.
#' Among them, the following parameters are already defined and cannot be modified: \code{dat},
#'  \code{gmode}, \code{vertex.sides}, \code{vertex.cex}, \code{vertex.col}, \code{edge.col}.
#' The following parameters, which can be redefined, have the following values:
#'  \itemize{
#'    \item{\code{mode = "fruchtermanreingold"}}
#'    \item{\code{displaylabels = TRUE}}
#'    \item{\code{label.pos = 0}}
#'    \item{\code{boxed.labels = TRUE}}
#'    \item{\code{displayisolates = TRUE}}
#'  }
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @param nopc Data frame of \strong{n}odes \strong{o}r \strong{p}atterns and their
#'  \strong{c}haracteristics. Nodes or patterns of which one of the clusters is to be plotted. Any subset
#'  of \code{object["nodes"]} or \code{object["patterns"]}.\cr
#'  \code{"nodes"}, \code{"n"}, \code{"patterns"} and \code{"p"} are specific values for
#'  \code{object["nodes"]} and \code{object["patterns"]}.
#' @param item Identification code of the item whose cluster is to be plotted.
#' @param identifiers Which IDs to use to identify the nodes or patterns on the chart and in the
#'  return data frames. One of \code{"original"}, \code{"new"}.
#'  \describe{
#'    \item{\code{"original"}}{Use of the original identifiers.}
#'    \item{\code{"new"}}{Use of new identifiers ordered according to the subset corresponding to
#'                        \code{nopc}.}
#'  }
#' @param use_name If \code{TRUE}, display the item name if it is defined. Display its identification
#'  code otherwise.
#' @param n.cutoff If \code{use_names = TRUE}, limit number of characters to display concerning the name
#'  of the item.
#' @param vertex_size Way how the sizes of the vertices of the graph should be defined.
#'  One of \code{"relative"}, \code{"grouped"}, \code{"absolute"}, \code{"equal"}.
#'  \describe{
#'    \item{\code{"relative"}}{The sizes are defined by a linear interpolation of the weights of the
#'                             entities in \code{size_range}.}
#'    \item{\code{"grouped"}}{The weights of the entities are grouped according to 5 intervals
#'                            defined by quantiles. The 5 corresponding size values are taken in a
#'                            regular sequence bounded by \code{size_range}.}
#'    \item{\code{"absolute"}}{The size of a vertex is defined directly according to the weight of
#'                             the entity.}
#'    \item{\code{"equal"}}{The vertices are all the same size of 1.}
#'  }
#' @param size_range If \code{vertex_size} is \code{"relative"} or \code{"grouped"}.
#'  Range of vertex size values (given as expansion factors).
#' @param vertex_col Way how the colors of the vertices of the graph should be defined.
#'  One of \code{"status"}, \code{"categories"}, \code{"none"}.
#'  If \code{"status"} and \code{entities = "patterns"}, coloring according to the status of the patterns.
#'  If \code{"categories"}, coloring according to the categories associated with the items of the
#'  entities represented.
#'  In all other cases, the vertices are colored gray.
#' @param c.cutoff Limit number of characters to display in the legend for the categories represented.
#' @param display_mixt If \code{TRUE}, display in the legend the category values included only in mixed
#'  links (or in mixed vertices, if \code{vertex_col = "categories"}).
#' @param title Chart title.
#'  By default, the title depends on the argument \code{item}.
#'  Example of default title: \code{"Cluster of 25"} if \code{item = 25}.
#' @param path Path of the directory in which to save the charts.
#'  By default, the charts are saved in the working directory.
#' @param name Name of the file in which to save the chart.
#'  By default, the name depends on the argument \code{item}.
#'  Example of default name: \code{"cluster_of_25.png"} if \code{item = 25}.
#' @param ... Additional arguments to the function \code{\link[sna:gplot]{gplot}} from the package
#'  \code{sna} for plotting the graph. See Details section.
#' @return \code{NULL} if none or only one node or pattern contains the sought item. \cr
#'  Otherwise:
#'  \describe{
#'    \item{\code{vertices}}{Data frame of the nodes or patterns and characteristics used,
#'                           associated with the identifiers of the vertices of the graph and their
#'                           degrees in the graph.}
#'    \item{\code{edges}}{Data frame of information relating to the edges of the graph.}
#'    \item{\code{coords}}{Matrix of the coordinates of the vertices of the graph.
#'                         Can be reused via the parameter \code{coord} (see \code{...}).}
#'  }
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{spectrosome_chart}}, \code{\link{degree}}, \code{\link[sna:gplot]{sna::gplot}}.
#' 
#' @examples
#' cluster_1 <- cluster_chart(SA_instance, "nodes", item = 3146,
#'                            name = "node_cluster_of_3146")
#' cluster_2 <- cluster_chart(SA_instance, SA_instance["patterns"], item = 3146)
#' 
#' @aliases cluster_chart
#' @export
setMethod(f = "cluster_chart",
          signature = "SpectralAnalyzer",
          definition = function(object, nopc, item,
                                identifiers = "original",
                                use_name = TRUE, n.cutoff = NULL,
                                vertex_size = "relative", size_range = c(0.5, 2.5), vertex_col = "status",
                                c.cutoff = NULL, display_mixt = TRUE,
                                title = paste("Cluster of", item),
                                path = getwd(), name = paste0("cluster_of_", item, ".png"),
                                ...) {
            
            # Récupération des noeuds/patterns et recherche du type d'entités fourni
            nopc = get_nopc(object, nopc)
            entities = which_entities(object, nopc)
            
            # Vérifie qu'un seul item est mentionné
            if (length(item) != 1 && entities == "nodes")
              stop("item must refer to only one item. For more, check out the functions get_nodes and spectrosome_chart.")
            if (length(item) != 1 && entities == "patterns")
              stop("item must refer to only one item. For more, check out the functions get_patterns and spectrosome_chart.")
            
            # Extraction des noeuds ou motifs contenant l'item recherché ("nop" = "nodes or patterns")
            if (entities == "nodes") nop = get_nodes_from_items(object, nopc, item)
            else if (entities == "patterns") nop = get_patterns_from_items(object, nopc, item)
            
            # Pas de cluster à construire si un seul ou aucun noeud/motif ne contient l'item
            if (nrow(nop) > 1) {
              # Construction du spectrosome associé
              to_return = spectrosome_chart(object, nop,
                                            identifiers = identifiers,
                                            vertex_size = vertex_size, vertex_col = vertex_col,
                                            use_names = use_name, n.cutoff = n.cutoff, c.cutoff = c.cutoff,
                                            display_mixt = display_mixt,
                                            title = title, path = path, name = name, ...)
              return(list(vertices = to_return$vertices, edges = to_return$edges, coords = to_return$coords[[1]]))
              
            } else {
              warning(paste0("There is no cluster for item ", item,
                             " (", nrow(nop), " ", substr(entities, 1, nchar(entities) - 1), ")."))
              return(NULL)
            }
          })


#' Network density
#' 
#' Compute the density of the graph as the ratio between the number of links identified and
#'  the maximum number of possible links.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @param links Data frame of the links (or edges) of a spectrosome graph.
#' @return Density of the network.
#' 
#' @author Gauthier Magnin
#' 
#' @examples
#' spectrosome <- spectrosome_chart(SA_instance, SA_instance["patterns"][1:15, ])
#' network_density(SA_instance, spectrosome[["edges"]])
#' 
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


#' Degree of a vertex
#' 
#' Compute the degree of a vertex in a graph, i.e. the number of vertices to which it is adjacent.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @param ID Identifier of the vertex (node or pattern) whose degree is to be calculated.
#' @param links Data frame of the links (or edges) of a spectrosome graph.
#' @return Degree of the vertex.
#' 
#' @author Gauthier Magnin
#' 
#' @examples
#' spectrosome <- spectrosome_chart(SA_instance, SA_instance["patterns"][1:15, ])
#' degree(SA_instance, 7, spectrosome[["edges"]])
#' 
#' @aliases degree
#' @export
setMethod(f = "degree",
          signature = "SpectralAnalyzer",
          definition = function(object, ID, links) {
            
            return(sum(xor(links$endpoint.1 == ID, links$endpoint.2 == ID)))
          })



#### Methods for creating multi-association tree graphs ####

#' Multi-association tree
#' 
#' Plot a multi-association tree chart and save it as a PDF file.
#' 
#' @details
#' If categories are associated with items, each category generates a tree.
#'  The category name is appended to the end of the file name.
#' 
#' Patterns of order 1 are not drawn. Only items included in higher-order patterns are.
#' 
#' The patterns are sorted according to their order values, then to their weights.
#' 
#' The colors associated with the values of the possible category represented are selected circularly
#'  among the 20 colors of the palette \code{category20} from D3 (see \code{ggsci::pal_d3("category20")}).
#' Therefore, if the number of values exceeds \code{20}, some colors will be used more than once.
#'  For example, the \out{22<sup>nd</sup>} value will share the color of the \out{2<sup>nd</sup>}
#'  value.
#' See the attribute \code{categories_colors} of \code{object} to reassign colors to the category values.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @param pc Data frame of \strong{p}atterns and their \strong{c}haracteristics. Patterns whose tree
#'  is to be plotted. Any subset of \code{object["patterns"]}.\cr
#'  \code{"patterns"} and \code{"p"} are specific values for \code{object["patterns"]}.
#' @param identifiers Which IDs to use to identify the patterns on the chart and in the return data frame.
#'  One of \code{"original"}, \code{"new"}.
#'  \describe{
#'    \item{\code{"original"}}{Use of the original identifiers.}
#'    \item{\code{"new"}}{Use of new identifiers based on pattern sorting (see 'Details' section to learn
#'                        more about the sort that is performed).}
#'  }
#' @param use_names If \code{TRUE}, display item names if they are defined. Display their identification
#'  codes otherwise.
#' @param n.cutoff If \code{use_names = TRUE}, limit number of characters to display concerning the names
#'  of the represented items.
#' @param display_status If \code{TRUE}, display pattern status.
#' @param display_text Text to display on the chart next to the patterns.
#'  Pattern identifiers (\code{"ID"}) or one of the other characteristics (\code{"weight"},
#'  \code{"frequency"}, \code{"specificity"}, \code{"year"}).
#'  The \code{NULL} value specifies that none of this information should be displayed.
#' @param c.cutoff Limit number of characters to display in the legend for the categories represented.
#' @param sort_by Sorting method of displayed items. One of \code{"category"}, \code{"item"}.
#' @param title Chart title.
#' @param path Path of the directory in which to save the chart.
#'  By default, the chart is saved in the working directory.
#' @param name Name of the file in which to save the chart.
#' @return Data frame of the patterns represented on the chart, associated with their characteristics
#'  and identifiers (visible on the chart if \code{display_text = "ID"}).
#' 
#' @author Delphine Bosson-Rieutort, Gauthier Magnin
#' @references Bosson-Rieutort D, Sarazin P, Bicout DJ, Ho V, Lavoué J (2020).
#'             Occupational Co-exposures to Multiple Chemical Agents from Workplace Measurements by the US Occupational Safety and Health Administration.
#'             \emph{Annals of Work Exposures and Health}, Volume 64, Issue 4, May 2020, Pages 402–415.
#'             \url{https://doi.org/10.1093/annweh/wxaa008}.
#' 
#' @examples
#' tree_1 <- tree_chart(SA_instance, "patterns",
#'                      n.cutoff = 20, c.cutoff = 17)
#' tree_2 <- tree_chart(SA_instance, SA_instance["patterns"][1:15, ],
#'                      c.cutoff = 17, name = "multi-association_tree_1-15")
#' 
#' @aliases tree_chart
#' @export
setMethod(f = "tree_chart",
          signature = "SpectralAnalyzer",
          definition = function(object, pc, identifiers = "original",
                                use_names = TRUE, n.cutoff = NULL,
                                display_status = TRUE, display_text = "ID",
                                c.cutoff = NULL, sort_by = "category",
                                title = "Multi-association tree", path = getwd(), name = "multi-association_tree.pdf") {
            
            # Récupération des patterns
            pc = get_nopc(object, pc, object@Class$PATTERNS)
            
            if (identifiers != "original" && identifiers != "new")
              stop("identifiers must be \"original\" or \"new\".")
            
            # Motifs d'ordre > 1, triés par taille croissant, puis par poids décroissant
            pat_charac = pc[pc$order != 1, ]
            pat_charac = pat_charac[order(pat_charac$order,
                                          max(pat_charac$weight) - pat_charac$weight), ]
            
            # Attribution d'identifiants aux motifs
            if (identifiers == "new") pat_charac$ID = seq(nrow(pat_charac))
            else pat_charac$ID = as.numeric(rownames(pat_charac))
            
            # Ensemble des items distincts parmi les motifs, associés d'une catégorie
            items_cat = data.frame(item = unique(unlist(pat_charac$pattern)))
            
            # Une variante du graphique par catégorie
            nb_categories = ifelse(length(object@items_categories) == 0, 1, ncol(object@items_categories))
            name = check_extension(name, "pdf")
            
            for (c in seq(nb_categories)) {
              
              # Association des items à leur valeur de catégorie s'il y a des catégories
              if (length(object@items_categories) == 0) {
                sort_by = "item"
                items_cat$category = NA
                category = NULL
              } else {
                items_cat$category = object@items_categories[as.character(items_cat$item), c]
                category = colnames(object@items_categories)[c]
              }
              
              # Tri des items
              if (use_names && sort_by == "item") {
                # Par nom
                items_cat = items_cat[order(names(object@items)[match(items_cat$item, object@items)]), ]
              } else if (sort_by == "item") {
                # Par code
                items_cat = items_cat[order(match(items_cat$item, object@items)), ]
              } else {
                # Selon une catégorie
                items_cat = items_cat[order(items_cat[[sort_by]]), ]
              }
              rownames(items_cat) = NULL
              
              # Nom du graphique en fonction de la catégorie
              file_name = ifelse(nb_categories == 1,
                                 name,
                                 sub(".pdf", paste0("-", category, ".pdf"), name))
              
              # Traçage du graphique dans un fichier PDF
              grDevices::pdf(paste0(turn_into_path(path), file_name),
                             14, 10, paper = "a4r", pointsize = 11)
              plot_tree_chart(object, pat_charac, items_cat, category, c.cutoff, use_names, n.cutoff, display_status, display_text, title)
              grDevices::dev.off()
            }
            
            # Motifs et caractéristiques, ordonnés selon ID (replacé en 1ère colonne)
            return(pat_charac[order(pat_charac$ID),
                              c(ncol(pat_charac), seq(ncol(pat_charac)-1))])
          })


#' Multi-association tree plotting
#' 
#' Plot a multi-association tree chart
#' 
#' @details
#' The colors associated with the values of the possible category represented are selected circularly
#'  among the 20 colors of the palette \code{category20} from D3 (see \code{ggsci::pal_d3("category20")}).
#' Therefore, if the number of values exceeds \code{20}, some colors will be used more than once.
#'  For example, the \out{22<sup>nd</sup>} value will share the color of the \out{2<sup>nd</sup>}
#'  value.
#' See the attribute \code{categories_colors} of \code{object} to reassign colors to the category values.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @param pc Data frame of \strong{p}atterns and their \strong{c}haracteristics. Patterns whose tree
#'  is to be plotted. Any subset of \code{object["patterns"]}.
#' @param items_category Data frame of items and one associated category.
#' @param category Name of the category to represent on the tree, used as the legend title.
#' @param c.cutoff Limit number of characters to display in the legend for the category represented.
#' @param use_names If \code{TRUE}, display item names if they are defined. Display their identification
#'  codes otherwise.
#' @param n.cutoff If \code{use_names = TRUE}, limit number of characters to display concerning the names
#'  of the represented items.
#' @param display_status If \code{TRUE}, display pattern status.
#' @param display_text Text to display on the chart next to the patterns.
#'  Pattern identifiers (\code{"ID"}) or one of the other characteristics (\code{"weight"},
#'  \code{"frequency"}, \code{"specificity"}, \code{"year"}).
#'  The \code{NULL} value specifies that none of this information should be displayed.
#' @param title Chart title.
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
          definition = function(object, pc, items_category, category = NULL,
                                c.cutoff = NULL, use_names = TRUE, n.cutoff = NULL,
                                display_status = TRUE, display_text = "ID",
                                title = "Multi-association tree") {
            
            # Définition des marges et initialisation de la zone graphique
            if (!is.null(category)) graphics::par(mar = c(3.0, 0.5, 1.9, 0.5))
            else graphics::par(mar = c(0.5, 0.5, 1.9, 0.5))
            graphics::plot.new()
            
            # Préparation de la position des items sur le graphique (x = 0 ; y = ordre décroissant)
            items_category$x = 0
            items_category$y = rev(seq(nrow(items_category)))
            
            
            ## Affichage des légendes, centrées, avant le graphique (avant modification de la "plot region")
            
            title(main = title, line = 1.1, cex.main = 1.3)
            
            # Couleurs de catégorie
            if (!is.null(category)) {
              item_colors = object@categories_colors[[category]][items_category$category]
              category_colors = unique(item_colors)[order(unique(items_category$category))]
              
              # Légende de catégorie
              if (is.null(c.cutoff)) {
                category_legend = sort(unique(items_category$category))
              } else {
                category_legend = substr(sort(unique(items_category$category)), 1, c.cutoff)
              }
              
              graphics::legend("bottom", xpd = NA, bty = "n", inset = c(0, -0.09),
                               title = cap(category), cex = 0.85,
                               legend = category_legend,
                               col = category_colors,
                               pch = 20, ncol = ceiling(length(category_legend) / 2))
            } else {
              item_colors = "black"
            }
            
            # Légende des statuts
            if (display_status) {
              # La différence de marges implique la nécessité une différence d'inset
              status_inset = if (!is.null(category)) c(-0.02, -0.064) else c(-0.02, -0.060)
              
              graphics::legend("topright", bty = "n", horiz = TRUE, xpd = NA, inset = status_inset,
                               pch = 15, cex = 0.85,
                               col = object@Class$STATUS_COLORS,
                               legend = names(object@Class$STATUS_COLORS))
            }
            
            
            ## Préparation de variables
            
            # Marge entre un motif et la ligne séparatrice qui le suit
            line_margin = 0.5
            
            # Table des tailles des motifs et effectifs cumulés
            order_tab = table(pc$order)
            order_cumfreq = cumsum(order_tab)
            
            # Titre des taille des motifs et position en Y
            order_text = "Order:"
            order_y = nrow(items_category) + 1.25
            
            # Labels des items (codes ou noms)
            if (use_names) {
              text_labels = names(object@items)[match(items_category$item, object@items)]
              if (!is.null(n.cutoff)) text_labels = substr(text_labels, 1, n.cutoff)
            } else {
              text_labels = as.character(items_category$item)
            }
            
            # Préparation des couleurs des lignes séparatrices
            vcolor = c("white", rep("black", length(order_cumfreq)-1))
            names(vcolor) = names(order_cumfreq)
            
            # Largeur de la zone déjà tracée ; utilisé comme coordonnée X pour afficher le prochain élément
            width = -1
            
            
            ## Préparation de la zone graphique
            
            # Espace à droite : nombre de motifs - espace inexistant avant le premier motif
            #                   + nombre de lignes verticales
            data_area = nrow(pc) - line_margin + length(unique(pc$order) - 1) * line_margin
            # Espace ajouté pour afficher la dernière taille de motif s'il y a trop peu de motifs associés
            last_space = graphics::strwidth(utils::as.roman(names(order_tab[length(order_tab)]))) - (order_tab[length(order_tab)] + line_margin)
            if (last_space > 0) data_area = data_area + last_space
            
            # Placement de la "plot region" pour avoir la place d'afficher les labels des items
            # Espace à gauche : taille du plus grand label (ou du titre "Order") + taille de la marge
            #                   + taille d'un caractère * 0.5 (offset de placement des labels) ; en fraction de la "figure region"
            graphics::par(plt = c(max(graphics::strwidth(text_labels, cex = 0.75, units = "figure"), graphics::strwidth(order_text, cex = 1.05, units = "figure")) +
                                    graphics::par("mai")[2] * graphics::strwidth(1, units = "figure") / graphics::strwidth(1, units = "inches") + # Conversion marge en figure units
                                    0.5 * graphics::strwidth("A", units = "figure"),
                          graphics::par("plt")[2:4]))
            
            # Option "new" pour que le graphique n'apparaîssent pas sur une seconde page
            graphics::par(new = TRUE)
            
            # Réinitialisation de la zone graphique en considérant la taille d'un caractère (strwidth, dépend du graphique)
            graphics::plot(rbind(items_category[, c("x", "y")], # Autant de lignes (ordonnée max) que d'items distincts
                                 data.frame(x = rep(0, 2), y = seq(2) + nrow(items_category))), # + 2 pour placer du texte
                           xlim = c(0, data_area),
                           ylim = c(-graphics::strwidth(1, cex = 0.5), # - taille d'un caractère pour affichage d'une caractéristique
                                    nrow(items_category) + 2), # + 2 pour affichage tailles des motifs
                           col = "white", pch = 20, bty = "n",
                           xaxt = "n", xaxs = "i", xlab = "",
                           yaxt = "n", yaxs = "i", ylab = "")
            
            
            ## Traçage du graphique
            
            # Traçage des lignes horizontales
            for (y_i in items_category$y) {
              graphics::segments(x0 = 0, x1 = data_area, y0 = y_i, lwd = 0.02, lty = 3, col = "gray85")
            }
            
            # Titre taille des motifs
            graphics::text(0, order_y, order_text, col = "black", cex = 1.05, adj = c(1, 0.5), xpd = TRUE)
            
            # Pour chaque motif à dessiner
            for (m in 1:nrow(pc)) {
              
              # Nouvelle taille de motifs
              if (m %in% (1 + c(0, order_cumfreq))) {
                order_nb = pc$order[m]
                width = width + line_margin
                
                # Séparation verticale
                graphics::abline(v = width, col = vcolor[as.character(order_nb)], lwd = 0.5, lty = "dotted")
                
                # Affichage de la taille des prochains motifs
                if (m == 1) {
                  order_x = order_cumfreq[as.character(order_nb)] / 2 
                } else if (m == nrow(pc)) {
                  order_x = width + (data_area - width) / 2
                } else {
                  # Positionnement en X : position de la ligne qui vient d'être tracée
                  #                       + (nombre de motifs de la nouvelle taille
                  #                          + 1 espace entre dernier motif et prochaine ligne) / 2
                  order_x = width + (order_cumfreq[as.character(order_nb)] - m + 1 + line_margin) / 2
                }
                graphics::text(x = order_x, y = order_y, utils::as.roman(order_nb), col = "black", cex = 1.05)
              }
              
              # Ordonnées (y) des items du motif (m)
              y_m = sort(items_category[match(pc[m, "pattern"][[1]], items_category$item), "y"])
              
              # Segment vertical entre les premier et dernier items du motif
              graphics::lines(c(width + 1, width + 1),
                              c(y_m[1], y_m[length(y_m)]),
                              lwd = 1.2, lty = 1, col = "black")
              # Segments horizontaux pour les items du motif
              for (y in y_m) {
                graphics::lines(c(width + 0.5, width + 1), c(y, y),
                                lwd = 1.2, lty = 1, col = "black", pch = 20, cex = 0.8)
              }
              
              # Affichage de l'identifiant ou de l'une des caractéristiques du motif
              if (!is.null(display_text)) {
                graphics::text(0.75 + width, y_m[1] - 0.25,
                               pc[m, display_text],
                               col = "black", cex = 0.5, srt = 90, adj = 1)
              }
              # Affichage du statut du motif
              if (display_status) {
                graphics::points(0.75 + width, y_m[length(y_m)] + 0.25,
                                 cex = 0.5, pch = 15,
                                 col = object@Class$STATUS_COLORS[pc$status[m]])
              }
              
              width = width + 1
            }
            
            # Pointage et affichage des items
            graphics::points(items_category[, c("x", "y")], col = item_colors, pch = 20)
            graphics::text(items_category$x, items_category$y, text_labels,
                           cex = 0.75, pos = 2, col = item_colors, xpd = TRUE)
          })



#### Association rule extraction methods ####

#' Rules extraction
#' 
#' Extract association rules from the observations. Can be used to find all rules, rules relating to
#'  patterns (or other specific item sets) or relating to specific items.
#' 
#' @details
#' Only creates rules with one item in the consequent. The reason is detailed on
#'  \href{https://borgelt.net/doc/apriori/apriori.html#conseq}{this web page} by the author of the
#'  algorithms used. Here are some extracts: \cr
#'  \emph{"There are usually already (many) more association rules than item sets if only a single
#'  item is allowed in the consequents."} \cr
#'  \emph{"Multiple items in the consequents of association rules therefore come at a considerable
#'  cost."} \cr
#'  \emph{"There is no true benefit."}
#' 
#' A rule is redundant if a more general rule with the same or higher confidence exists. A rule is more
#'  general if it has the same consequent but one or more items removed from the antecedent.
#' 
#' If \code{from = "observations"}, additional arguments are \code{parameter}, \code{appearance} and
#'  \code{control} of function \code{\link[arules:apriori]{apriori}} from the package \code{arules}.
#'  These arguments allow to specify minimum support (default \code{0.1}), minimum confidence (default
#'  \code{0.8}), minimum length (default \code{1}), maximum length (default \code{10}), specific items
#'  in antecedent or consequent, and some operating parameters of the rule extraction algorithm.
#' 
#' If \code{from} is \code{"patterns"} or a list, additional arguments are \code{confidence} and
#'  \code{control} of function \code{\link[arules:ruleInduction]{ruleInduction}} from the package
#'  \code{arules}. These arguments allow to specify minmum confidence (default \code{0.8}) and some
#'  operating parameters of the rule extraction algorithm.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @param from Character or list of item sets for which to extract the association rules.
#'  \itemize{
#'    \item{If \code{"observations"}, look for all rules within the observations saved in \code{object}
#'          or for rules with specific items.}
#'    \item{If \code{"patterns"}, look for rules whose union of the antecedent and the consequent form
#'          an entire pattern among those of \code{object["patterns"]$pattern}.}
#'    \item{Otherwise, a list of item sets defined the same way as \code{object["patterns"]$pattern}.
#'          The search is then done the same way as for \code{"patterns"}.}
#'  }
#' @param pruning If \code{TRUE}, remove redundant rules.
#' @param as_sets If \code{FALSE}, antecedents and consequents of the rules will be character vectors.
#'  If \code{TRUE}, they will be factors written in mathematical notation (i.e. set notation).
#' @param ... Additional arguments to configure the extraction. See Details.
#' @return Data frame containing the extracted rules and their characteristics.
#'  If \code{from} is not \code{"observations"}, the column \code{"itemset"} refers to the index of the
#'  item set from which the rule was generated, in the list of patterns (if \code{from = "patterns"})
#'  or the given list (otherwise).
#' 
#' @author Gauthier Magnin
#' 
#' @examples
#' ## Basic rule extraction
#' rules_1 <- extract_rules(SA_instance, from = "observations")
#' rules_2 <- extract_rules(SA_instance, from = "patterns")
#' rules_3 <- extract_rules(SA_instance, from = list(c("931", "3180"),
#'                                                   c("25", "192", "328")))
#' 
#' ## Rule extraction with conditions on the antecedent and the consequent
#' params <- list(supp = 0.001, conf = 0.5, maxlen = 2)
#' rules_4 <- extract_rules(SA_instance, from = "observations",
#'                          parameter = params,
#'                          appearance = list(rhs = "328"))
#' rules_5 <- extract_rules(SA_instance, from = "observations",
#'                          parameter = params,
#'                          appearance = list(lhs = "497"))
#' rules_6 <- extract_rules(SA_instance, from = "observations",
#'                          parameter = list(supp = 0.001, conf = 0,
#'                                           minlen = 2, maxlen = 2),
#'                          appearance = list(lhs = "328", rhs = "3180"))
#' 
#' @aliases extract_rules
#' @export
setMethod(f = "extract_rules",
          signature = "SpectralAnalyzer",
          definition = function(object, from, pruning = FALSE, as_sets = FALSE, ...) {
            
            # Validation du paramètre de choix des itemsets desquels extraire les règles
            if (is.character(from) && from != "observations" && from != "patterns")
              stop("from must be \"observations\", \"patterns\" or a list of item sets.")
            
            # Conversion des observations en transactions
            transact = turn_obs_into_transactions(object@observations, "CODE")
            
            if (is.character(from) && from == "observations") {
              
              # Vérification du bon choix du paramètre demandant l'extraction de règles
              args = list(...)
              if ("parameter" %in% names(args) && "target" %in% names(args$parameter)
                  && args$parameter$target != "rules") stop("target parameter must be \"rules\"")
              
              # Spécification du non-affichage de la progression
              if (!("control" %in% names(args))) args$control = list(verbose = FALSE)
              else if (!("verbose" %in% names(args$control))) args$control$verbose = FALSE
              
              # Extraction des règles d'association
              # rules = arules::apriori(transact, ...)
              rules = do.call(arules::apriori, c(data = transact, args))
              
            } else {
              if (is.character(from)) from = object@patterns$pattern
              
              # Conversion de la liste d'item sets en objet arules::itemMatrix puis arules::itemsets
              itemsets = methods::new("itemsets", items = arules::encode(from, object@items))
              
              # Extraction des règles d'association
              rules = arules::ruleInduction(itemsets, transact, ...)
            }
            
            # Recherche et retrait des règles redondantes
            if (pruning) rules = rules[!arules::is.redundant(rules)]
            
            # Si aucune règle ne correspond aux critères de recherche : NULL
            if (length(rules) == 0) return(NULL)
            
            # Conversion en data frame
            rules_df = arules::DATAFRAME(rules)
            
            # Changement de notation
            if (!as_sets) {
              rules_df[, c("LHS", "RHS")] = apply(rules_df[, c("LHS", "RHS")], 2, vector_notation)
            }
            
            # Renommage des colonnes "LHS" et "RHS" et replacement de la colonne "=>"
            colnames(rules_df)[c(1,2)] = c("antecedent", "consequent")
            rownames(rules_df) = NULL
            rules_df[, " "] = "=>"
            return(rules_df[, c(1, ncol(rules_df), seq(2, ncol(rules_df)-1))])
          })



#### Methods for search and save ####

#' Saving nodes, patterns or association rules
#' 
#' Save in CSV format a set of nodes, patterns or association rules as well as their characteristics.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @param characteristics Data frame of the characteristics of nodes, patterns or rules.
#' @param ... Further arguments to the function \code{\link[utils:write.table]{utils::write.csv2}}.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link[utils:write.table]{utils::write.csv2}}.
#' 
#' @examples
#' save_characteristics(SA_instance, SA_instance["nodes"],
#'                      file = "nodes.csv")
#' save_characteristics(SA_instance, SA_instance["patterns"][1:15, ],
#'                      file = "patterns.csv")
#' 
#' spectrosome <- spectrosome_chart(SA_instance, "patterns")
#' save_characteristics(SA_instance, spectrosome[["vertices"]],
#'                      file = "spectrosome_vertices.csv", row.names = FALSE)
#' 
#' rules <- extract_rules(SA_instance, from = "observations")
#' save_characteristics(SA_instance, rules,
#'                      file = "rules.csv", row.names = FALSE)
#' 
#' @aliases save_characteristics
#' @export
setMethod(f = "save_characteristics",
          signature = "SpectralAnalyzer",
          definition = function(object, characteristics, ...) {
            
            # Recherche du type d'entités fourni
            entities = which_entities(object, characteristics, object@Class$NODES_PATTERNS_OR_RULES)
            
            # Nom des colonnes dans lesquelles chercher les vecteurs à convertir
            if (entities == object@Class$NODES || entities == object@Class$PATTERNS) {
              columns = substr(entities, 1, nchar(entities) - 1)
            } else if (entities == object@Class$RULES) {
              columns = c("antecedent", "consequent")
            }
            
            # Conversion des itemsets en chaînes de caractères
            itemsets = apply(characteristics[columns], 2, turn_list_into_char)
            characteristics[, columns] = unlist(itemsets)
            
            # Enregistrement des données
            utils::write.csv2(x = characteristics, ...)
          })


#' Search for nodes by item, characteristic or category
#' 
#' Extract the nodes satisfying search criteria according to items, characteristics or categories.
#' 
#' @details
#' If `element = "items"` one or more items can be sought. The condition for a node to be extracted
#'  is the presence of the sought items (argument `value`). The argument `condition` must be `"all"`
#'  or `"any"` (default is `"all"`):
#'  * `"all"`: all the sought items must be part of the node.
#'  * `"any"`: at least one of the sought items must be part of the node.
#' 
#' If `element` refers to a characteristic (i.e. is `"length"` or `"weight"`), the condition for a node
#'  to be extracted is a comparison of the `value` according to one of the comparison operators (default
#'  is equality):
#'  * `"EQ"`, `"=="`: **EQ**ual. The value of the characteristic must be equal to that sought.
#'  * `"NE"`, `"!="`: **N**ot **E**qual. The value of the characteristic must be different from that
#'    sought.
#'  * `"LT"`, `"<"`: **L**ess **T**han. The value of the characteristic must be less than that sought.
#'  * `"GT"`, `">"`: **G**reater **T**han. The value of the characteristic must be greater than that
#'    sought.
#'  * `"LE"`, `"<="`: **L**ess than or **E**qual. The value of the caracteristic must be less than or
#'    equal to that sought.
#'  * `"GE"`, `">="`: **G**reater than or **E**qual. The value of the characteristic must be greater
#'    than or equal to that sought.
#' 
#' If `element` is the name or the number of a category, the condition for a node to be extracted is
#'  the correspondence to the sought category `value`. The argument `condition` must be one of `"items"`,
#'  `"links"`, `"vertices"`, `"edges"` (no default).
#'  * `"items"`, `"vertices"`: search for nodes containing an item associated with the sought category
#'    value.
#'  * `"links"`, `"edges"`: search for nodes generating links corresponding to the sought category value.
#' 
#' @param object `SpectralAnalyzer` class object.
#' @param nc Data frame of **n**odes and their **c**haracteristics. Any subset of `object["nodes"]`.\cr
#'  `"nodes"` and `"n"` are specific values for `object["nodes"]`.
#' @param element Type of element on which to search.
#'  One of `"items"`, `"length"`, `"weight"` or the name or number of a category on which to search
#'  (numbering according to the order of the columns of `object["items_categories"]`).
#' @param value Sought value(s) for the element specified by the argument `element`.
#' @param condition Search condition, depending on `element`. See 'Details' section.
#' @return Subset of the data frame of nodes that match the search criteria.
#' 
#' @author Gauthier Magnin
#' @seealso [`get_patterns`], [`get_complexes`], [`get_isolates`], [`get_non_isolates`].
#' 
#' @examples
#' ## Search on items
#' get_nodes(SA_instance, SA_instance["nodes"], element = "items", value = 3146)
#' get_nodes(SA_instance, SA_instance["nodes"],
#'           element = "items", value = c(3146, 3180), condition = "all")
#' get_nodes(SA_instance, SA_instance["nodes"],
#'           element = "items", value = c(3146, 3180), condition = "any")
#' 
#' ## Search on characteristics
#' get_nodes(SA_instance, SA_instance["nodes"], element = "weight", value = 2)
#' get_nodes(SA_instance, SA_instance["nodes"],
#'           element = "weight", value = 2, condition = ">=")
#' get_nodes(SA_instance, SA_instance["nodes"],
#'           element = "length", value = 5, condition = "LT")
#' 
#' ## Search on categories
#' get_nodes(SA_instance, SA_instance["nodes"],
#'           element = "family", value = "Chrome", condition = "items")
#' get_nodes(SA_instance, "nodes",
#'           element = 1, value = "Chrome", condition = "links")
#' 
#' @aliases get_nodes
#' @md
#' @export
setMethod(f = "get_nodes",
          signature = "SpectralAnalyzer",
          definition = function(object, nc,
                                element, value, condition = "default") {
            
            # Vérification du choix de l'élément sur lequel effectuer la recherche
            if (!(element %in% c("items", "length", "weight")) && !check_access_for_category(object, element, NA, stop = FALSE))
              stop("element must be one of \"items\", \"length\", \"weight\", or a category name or number.")
            
            # Appel à la fonction spécifique
            if (element == "items") {
              if (condition == "default")
                return(get_nodes_from_items(object, nc, value))
              else
                return(get_nodes_from_items(object, nc, value, condition))
            }
            if (element %in% c("length", "weight")) {
              if (condition == "default")
                return(get_nodes_from_characteristic(object, nc, element, value))
              else
                return(get_nodes_from_characteristic(object, nc, element, value, condition))
            }
            return(get_nodes_from_category(object, nc, element, value, condition))
          })


#' Search for nodes by item
#' 
#' Extract the nodes containing one or more sought items.
#' 
#' @param object `SpectralAnalyzer` class object.
#' @param nc Data frame of **n**odes and their **c**haracteristics. Any subset of `object["nodes"]`.\cr
#'  `"nodes"` and `"n"` are specific values for `object["nodes"]`.
#' @param items Sought items (one or more).
#' @param condition Item presence condition for a node to be extracted.
#'  One of `"all"`, `"any"`.
#'  \describe{
#'    \item{`"all"`}{All the sought items must be part of a node for this node to be extracted.}
#'    \item{`"any"}{At least one of the sought items must be part of a node for this node to be
#'                  extracted.}
#'  }
#' @return Subset of the data frame of nodes that match the search criteria.
#' 
#' @author Gauthier Magnin
#' @seealso [`get_nodes`], [`get_nodes_from_characteristic`], [`get_nodes_from_category`].
#' 
#' @examples
#' get_nodes_from_items(SA_instance, SA_instance["nodes"], items = 3146)
#' get_nodes_from_items(SA_instance, SA_instance["nodes"],
#'                      items = c(3146, 3180), condition = "all")
#' get_nodes_from_items(SA_instance, SA_instance["nodes"],
#'                      items = c(3146, 3180), condition = "any")
#' 
#' @aliases get_nodes_from_items
#' @md
#' @keywords internal
setMethod(f = "get_nodes_from_items",
          signature = "SpectralAnalyzer",
          definition = function(object, nc, items, condition = "all") {
            
            # Récupération des noeuds
            nc = get_nopc(object, nc, object@Class$NODES)
            
            if (!(condition %in% c("all", "any"))) stop("condition must be \"all\" or \"any\".")
            
            if (condition == "all") func = all
            else if (condition == "any") func = any
            
            return(subset(nc, sapply(nc$node, function(x) func(items %in% x))))
          })


#' Search for nodes by characteristic
#' 
#' Extract the nodes satisfying a search criterion according to one characteristic.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @param nc Data frame of \strong{n}odes and their \strong{c}haracteristics. Any subset of
#'  \code{object["nodes"]}.\cr
#'  \code{"nodes"} and \code{"n"} are specific values for \code{object["nodes"]}.
#' @param characteristic Name of the characteristic on which to do the search.
#'  One of \code{"length"}, \code{"weight"}.
#' @param value Sought value for the characteristic specified by the parameter \code{characteristic}.
#' @param condition Search condition.
#'  One of \code{"EQ"}, \code{"NE"}, \code{"LT"}, \code{"GT"}, \code{"LE"}, \code{"GE"},
#'  \code{"=="}, \code{"!="}, \code{"<"}, \code{">"}, \code{"<="}, \code{">="}.
#'  \describe{
#'    \item{\code{"EQ", "=="}}{\strong{EQ}ual: the value of the characteristic must be equal to that
#'                             sought.}
#'    \item{\code{"NE", "!="}}{\strong{N}ot \strong{E}qual: the value of the characteristic must be
#'                             different from that sought.}
#'    \item{\code{"LT", "<"}}{\strong{L}ess \strong{T}han: the value of the characteristic must be less
#'                            than that sought.}
#'    \item{\code{"GT", ">"}}{\strong{G}reater \strong{T}han: the value of the characteristic must be
#'                            greater than that sought.}
#'    \item{\code{"LE", "<="}}{\strong{L}ess than or \strong{E}qual: the value of the caracteristic must
#'                             be less than or equal to that sought.}
#'    \item{\code{"GE", ">="}}{\strong{G}reater than or \strong{E}qual: the value of the characteristic
#'                             must be greater than or equal to that sought.}
#'  }
#' @return Subset of the data frame of nodes that match the search criteria.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{get_nodes}}, \code{\link{get_nodes_from_items}},
#'          \code{\link{get_nodes_from_category}}.
#' 
#' @examples
#' get_nodes_from_characteristic(SA_instance, SA_instance["nodes"],
#'                               characteristic = "weight",
#'                               value = 2)
#' get_nodes_from_characteristic(SA_instance, SA_instance["nodes"],
#'                               characteristic = "length",
#'                               value = 2, condition = ">=")
#' get_nodes_from_characteristic(SA_instance, SA_instance["nodes"],
#'                               characteristic = "length",
#'                               value = 5, condition = "LT")
#' 
#' @aliases get_nodes_from_characteristic
#' @keywords internal
setMethod(f = "get_nodes_from_characteristic",
          signature = "SpectralAnalyzer",
          definition = function(object, nc, characteristic, value, condition = "EQ") {
            
            # Récupération des noeuds
            nc = get_nopc(object, nc, object@Class$NODES)
            
            if (!(characteristic %in% c("length", "weight")))
              stop("characteristic must be one of \"length\", \"weight\".")
            
            switch(EXPR = condition,
                   "EQ" = { return(nc[nc[characteristic] == value, ]) },
                   "==" = { return(nc[nc[characteristic] == value, ]) },
                   
                   "NE" = { return(nc[nc[characteristic] != value, ]) },
                   "!=" = { return(nc[nc[characteristic] != value, ]) },
                   
                   "LT" = { return(nc[nc[characteristic] <  value, ]) },
                   "<"  = { return(nc[nc[characteristic] <  value, ]) },
                   
                   "GT" = { return(nc[nc[characteristic] >  value, ]) },
                   ">"  = { return(nc[nc[characteristic] >  value, ]) },
                   
                   "LE" = { return(nc[nc[characteristic] <= value, ]) },
                   "<=" = { return(nc[nc[characteristic] <= value, ]) },
                   
                   "GE" = { return(nc[nc[characteristic] >= value, ]) },
                   ">=" = { return(nc[nc[characteristic] >= value, ]) },
                   
                   stop(paste("condition must be one of",
                              "\"EQ\", \"NE\", \"LT\", \"GT\", \"LE\", \"GE\",",
                              "\"==\", \"!=\", \"<\", \">\", \"<=\", \">=\".")))
          })


#' Search for nodes by category
#' 
#' Extract the nodes corresponding to a sought category value.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @param nc Data frame of \strong{n}odes and their \strong{c}haracteristics. Any subset of
#'  \code{object["nodes"]}.\cr
#'  \code{"nodes"} and \code{"n"} are specific values for \code{object["nodes"]}.
#' @param category Name or number of the category on which to search (numbering according to the order
#'  of the columns of \code{object["items_categories"]}).
#' @param value Sought value for the category specified by the parameter \code{category}.
#' @param condition Category value search condition for a node to be extracted.
#'  One of \code{"items"}, \code{"links"}, \code{"vertices"}, \code{"edges"}.
#'  \describe{
#'    \item{\code{"items"}, \code{"vertices"}}{Search for nodes containing an item associated with the
#'          sought category value.}
#'    \item{\code{"links"}, \code{"edges"}}{Search for nodes generating links corresponding to the
#'          sought category value.}
#'  }
#' @return Subset of the data frame of nodes that match the search criteria.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{get_nodes}}, \code{\link{get_nodes_from_items}},
#'          \code{\link{get_nodes_from_characteristic}}.
#' 
#' @examples
#' get_nodes_from_category(SA_instance, SA_instance["nodes"],
#'                         category = "family", value = "Chrome",
#'                         condition = "items")
#' 
#' get_nodes_from_category(SA_instance, SA_instance["nodes"],
#'                         category = 1, value = "Chrome",
#'                         condition = "links")
#' 
#' @aliases get_nodes_from_category
#' @keywords internal
setMethod(f = "get_nodes_from_category",
          signature = "SpectralAnalyzer",
          definition = function(object, nc, category, value, condition) {
            
            # Récupération des noeuds
            nc = get_nopc(object, nc, object@Class$NODES)
            
            # Validation des paramètres liés à une valeur de catégorie
            check_access_for_category(object, category, value)
            
            if (condition == "items" || condition == "vertices") {
              # Recherche des items correspondant à la catégorie recherchée
              items = rownames(subset(object@items_categories, object@items_categories[category] == value))
              # Extraction des noeuds contenant ces items
              return(get_nodes_from_items(object, nc, items, condition = "any"))
              
            } else if (condition == "links" || condition == "edges") {
              # Recherche de l'ensemble de liens correspondant aux motifs
              links = get_links(object, nc)
              # Valeurs associées à chaque lien pour le type de catégorie recherché
              categories_links = lapply(strsplit(links$items, "/"),
                                        function(x) sort(unique(as.character(object@items_categories[x, category]))))
              # Extraction des liens qui correspondent à la valeur de catégorie recherchée
              links = links[sapply(categories_links, function(x) value %in% x), ]
              # Récupération des noeuds associés
              return(nc[unique(unlist(links[, 1:2])), ])
            }
            stop("condition must be one of \"items\", \"links\", \"vertices\", \"edges\".")
          })


#' Search for patterns by item, characteristic or category
#' 
#' Extract the patterns satisfying search criteria according to items, characteristics or categories.
#' 
#' @details
#' If `element = "items"` one or more items can be sought. The condition for a pattern to be extracted
#'  is the presence of the sought items (argument `value`). The argument `condition` must be `"all"` or
#'  `"any"` (default is `"all"`):
#'  * `"all"`: all the sought items must be part of the pattern.
#'  * `"any"`: at least one of the sought items must be part of the pattern.
#' 
#' If `element` refers to a characteristic other than status (i.e. is one of `"year"`, `"frequency"`,
#'  `"weight"`, `"order"`, `"specificity"`), the condition for a pattern to be extracted is a comparaison
#'  of the `value` according to one of the comparison operators (default is equality):
#'  * `"EQ"`, `"=="`: **EQ**ual. The value of the characteristic must be equal to that sought.
#'  * `"NE"`, `"!="`: **N**ot **E**qual. The value of the characteristic must be different from that
#'    sought.
#'  * `"LT"`, `"<"`: **L**ess **T**han. The value of the characteristic must be less than that sought.
#'  * `"GT"`, `">"`: **G**reater **T**han. The value of the characteristic must be greater than that
#'    sought.
#'  * `"LE"`, `"<="`: **L**ess than or **E**qual. The value of the caracteristic must be less than or
#'    equal to that sought.
#'  * `"GE"`, `">="`: **G**reater than or **E**qual. The value of the characteristic must be greater
#'    than or equal to that sought.
#' 
#' If `element` refers to the specific characteristic `"status"`, one or more status can be sought.
#'  the condition for a pattern to be extracted is a comparison of the sought status (argument `value`)
#'  according to one of the basic comparison operators (default is equality):
#'  * `"EQ"`, `"=="`: **EQ**ual. The status of the pattern must be one of the sought values.
#'  * `"NE"`, `"!="`: **N**ot **E**qual. The status of the pattern must be different from the sought
#'    values.
#' 
#' If `element` is the name or the number of a category, the condition for a pattern to be extracted is
#'  the correspondence to the sought category `value`. The argument `condition` must be one of `"items"`,
#'  `"links"`, `"vertices"`, `"edges"` (no default).
#'  * `"items"`, `"vertices"`: search for patterns containing an item associated with the sought
#'    category value.
#'  * `"links"`, `"edges"`: search for patterns generating links corresponding to the sought category
#'    value.
#' 
#' @param object `SpectralAnalyzer` class object.
#' @param pc Data frame of **p**atterns and their **c**haracteristics. Any subset of
#'  `object["patterns"]`.\cr
#'  `"patterns"` and `"p"` are specific values for `object["patterns"]`.
#' @param element Type of element on which to search.
#'  One of `"items"`, `"year"`, `"frequency"`, `"weight"`, `"order"`, `"specificity"`, `"status"`
#'  or the name or number of a category on which to search (numbering according to the order of the
#'  columns of `object["items_categories"]`).
#' @param value Sought value(s) for the element specified by the argument `element`.
#' @param condition Search condition, depending on `element`. See 'Details' section.
#' @return Subset of the data frame of patterns that match the search criteria.
#' 
#' @author Gauthier Magnin
#' @seealso [`get_nodes`], [`get_complexes`], [`get_isolates`], [`get_non_isolates`].
#' 
#' @examples
#' ## Search on items
#' get_patterns(SA_instance, SA_instance["patterns"],
#'              element = "items", value = 3146)
#' get_patterns(SA_instance, SA_instance["patterns"],
#'              element = "items", value = c(3146, 3180), condition = "all")
#' get_patterns(SA_instance, SA_instance["patterns"],
#'              element = "items", value = c(3146, 3180), condition = "any")
#' 
#' ## Search on characteristics
#' get_patterns(SA_instance, SA_instance["patterns"],
#'              element = "weight", value = 3)
#' get_patterns(SA_instance, SA_instance["patterns"],
#'              element = "weight", value = 3, condition = ">=")
#' get_patterns(SA_instance, SA_instance["patterns"],
#'              element = "order", value = 3, condition = "LT")
#' 
#' get_patterns(SA_instance, SA_instance["patterns"], element = "status",
#'              value = SA_instance["Class"]$STATUS_PERSISTENT)
#' get_patterns(SA_instance, SA_instance["patterns"], element = "status",
#'              value = c("Persistent", "Declining"), condition = "!=")
#' 
#' ## Search on categories
#' get_patterns(SA_instance, SA_instance["patterns"],
#'              element = "family", value = "Chrome", condition = "items")
#' get_patterns(SA_instance, "patterns",
#'              element = 1, value = "Chrome", condition = "links")
#' 
#' @aliases get_patterns
#' @md
#' @export
setMethod(f = "get_patterns",
          signature = "SpectralAnalyzer",
          definition = function(object, pc,
                                element, value, condition = "default") {
            
            # Vérification du choix de l'élément sur lequel effectuer la recherche
            if (!(element %in% c("items", "year", "frequency", "weight", "order", "specificity", "status"))
                && !check_access_for_category(object, element, NA, stop = FALSE))
              stop(paste("element must be one of \"items\"",
                         "\"year\", \"frequency\", \"weight\", \"order\", \"specificity\", \"status\"",
                         "or a category name or number."))
            
            # Appel à la fonction spécifique
            if (element == "items") {
              if (condition == "default")
                return(get_patterns_from_items(object, pc, value))
              else
                return(get_patterns_from_items(object, pc, value, condition))
            }
            if (element %in% c("year", "frequency", "weight", "order", "specificity")) {
              if (condition == "default")
                return(get_patterns_from_characteristic(object, pc, element, value))
              else
                return(get_patterns_from_characteristic(object, pc, element, value, condition))
            }
            if (element == "status") {
              if (condition == "default")
                return(get_patterns_from_status(object, pc, value))
              else
                return(get_patterns_from_status(object, pc, value, condition))
            }
            return(get_patterns_from_category(object, pc, element, value, condition))
          })


#' Search for patterns by item
#' 
#' Extract the patterns containing one or more sought items.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @param pc Data frame of \strong{p}atterns and their \strong{c}haracteristics. Any subset of
#'  \code{object["patterns"]}.\cr
#'  \code{"patterns"} and \code{"p"} are specific values for \code{object["patterns"]}.
#' @param items Sought items (one or more).
#' @param condition Item presence condition for a pattern to be extracted.
#'  One of \code{"all"}, \code{"any"}.
#'  \describe{
#'    \item{\code{"all"}}{All the sought items must be part of a pattern for this pattern to be
#'                        extracted.}
#'    \item{\code{"any"}}{At least one of the sought items must be part of a pattern for this pattern
#'                        to be extracted.}
#'  }
#' @return Subset of the data frame of patterns that match the search criteria.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{get_patterns}}, \code{\link{get_patterns_from_characteristic}},
#'          \code{\link{get_patterns_from_status}}, \code{\link{get_patterns_from_category}}.
#' 
#' @examples
#' get_patterns_from_items(SA_instance, SA_instance["patterns"], items = 3146)
#' get_patterns_from_items(SA_instance, SA_instance["patterns"],
#'                         items = c(3146, 3180), condition = "all")
#' get_patterns_from_items(SA_instance, SA_instance["patterns"],
#'                         items = c(3146, 3180), condition = "any")
#' 
#' @aliases get_patterns_from_items
#' @keywords internal
setMethod(f = "get_patterns_from_items",
          signature = "SpectralAnalyzer",
          definition = function(object, pc, items, condition = "all") {
            
            # Récupération des patterns
            pc = get_nopc(object, pc, object@Class$PATTERNS)
            
            if (!(condition %in% c("all", "any"))) stop("condition must be \"all\" or \"any\".")
            
            if (condition == "all") func = all
            else if (condition == "any") func = any
            
            return(subset(pc, sapply(pc$pattern, function(x) func(items %in% x))))
          })


#' Search for patterns by characteristic
#' 
#' Extract the patterns satisfying a search criterion according to one characteristic.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @param pc Data frame of \strong{p}atterns and their \strong{c}haracteristics. Any subset of
#'  \code{object["patterns"]}.\cr
#'  \code{"patterns"} and \code{"p"} are specific values for \code{object["patterns"]}.
#' @param characteristic Name of the characteristic on which to do the search.
#'  One of \code{"year"}, \code{"frequency"}, \code{"weight"}, \code{"order"}, \code{"specificity"}
#'  See \code{\link{get_patterns_from_status}} to search by \code{"status"}.
#' @param value Sought value for the characteristic specified by the parameter \code{characteristic}.
#' @param condition Search condition.
#'  One of \code{"EQ"}, \code{"NE"}, \code{"LT"}, \code{"GT"}, \code{"LE"}, \code{"GE"},
#'  \code{"=="}, \code{"!="}, \code{"<"}, \code{">"}, \code{"<="}, \code{">="}.
#'  \describe{
#'    \item{\code{"EQ", "=="}}{\strong{EQ}ual: the value of the characteristic must be equal to that
#'                             sought.}
#'    \item{\code{"NE", "!="}}{\strong{N}ot \strong{E}qual: the value of the characteristic must be
#'                             different from that sought.}
#'    \item{\code{"LT", "<"}}{\strong{L}ess \strong{T}han: the value of the characteristic must be less
#'                            than that sought.}
#'    \item{\code{"GT", ">"}}{\strong{G}reater \strong{T}han: the value of the characteristic must be
#'                            greater than that sought.}
#'    \item{\code{"LE", "<="}}{\strong{L}ess than or \strong{E}qual: the value of the caracteristic must
#'                             be less than or equal to that sought.}
#'    \item{\code{"GE", ">="}}{\strong{G}reater than or \strong{E}qual: the value of the characteristic
#'                             must be greater than or equal to that sought.}
#'  }
#' @return Subset of the data frame of patterns that match the search criteria.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{get_patterns}}, \code{\link{get_patterns_from_items}},
#'          \code{\link{get_patterns_from_status}}, \code{\link{get_patterns_from_category}}.
#' 
#' @examples
#' get_patterns_from_characteristic(SA_instance, SA_instance["patterns"],
#'                                  characteristic = "weight",
#'                                  value = 3)
#' get_patterns_from_characteristic(SA_instance, SA_instance["patterns"],
#'                                  characteristic = "weight",
#'                                  value = 3, condition = ">=")
#' get_patterns_from_characteristic(SA_instance, SA_instance["patterns"],
#'                                  characteristic = "order",
#'                                  value = 3, condition = "LT")
#' 
#' @aliases get_patterns_from_characteristic
#' @keywords internal
setMethod(f = "get_patterns_from_characteristic",
          signature = "SpectralAnalyzer",
          definition = function(object, pc, characteristic, value, condition = "EQ") {
            
            # Récupération des patterns
            pc = get_nopc(object, pc, object@Class$PATTERNS)
            
            if (!(characteristic %in% c("year", "frequency", "weight", "order", "specificity")))
              stop("characteristic must be one of \"year\", \"frequency\", \"weight\", \"order\", \"specificity\".")
            
            switch(EXPR = condition,
                   "EQ" = { return(pc[pc[characteristic] == value, ]) },
                   "==" = { return(pc[pc[characteristic] == value, ]) },
                   
                   "NE" = { return(pc[pc[characteristic] != value, ]) },
                   "!=" = { return(pc[pc[characteristic] != value, ]) },
                   
                   "LT" = { return(pc[pc[characteristic] <  value, ]) },
                   "<"  = { return(pc[pc[characteristic] <  value, ]) },
                   
                   "GT" = { return(pc[pc[characteristic] >  value, ]) },
                   ">"  = { return(pc[pc[characteristic] >  value, ]) },
                   
                   "LE" = { return(pc[pc[characteristic] <= value, ]) },
                   "<=" = { return(pc[pc[characteristic] <= value, ]) },
                   
                   "GE" = { return(pc[pc[characteristic] >= value, ]) },
                   ">=" = { return(pc[pc[characteristic] >= value, ]) },
                   
                   stop(paste("condition must be one of",
                              "\"EQ\", \"NE\", \"LT\", \"GT\", \"LE\", \"GE\",",
                              "\"==\", \"!=\", \"<\", \">\", \"<=\", \">=\".")))
          })


#' Search for patterns by status
#' 
#' Extract the patterns whose status match one or more sought values.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @param pc Data frame of \strong{p}atterns and their \strong{c}haracteristics. Any subset of
#'  \code{object["patterns"]}.\cr
#'  \code{"patterns"} and \code{"p"} are specific values for \code{object["patterns"]}.
#' @param value Status value sought (one or more).
#' @param condition Search condition. One of \code{"EQ"}, \code{"NE"}, \code{"=="}, \code{"!="}.
#'  \describe{
#'    \item{\code{"EQ"}, \code{"=="}}{\strong{EQ}ual: the status of the pattern must be one of the
#'                                    sought values.}
#'    \item{\code{"NE"}, \code{"!="}}{\strong{N}ot \strong{E}qual: the status of the pattern must be
#'                                    different from the sought values.}
#'  }
#' @return Subset of the data frame of patterns that match the search criteria.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{get_patterns}}, \code{\link{get_patterns_from_items}},
#'          \code{\link{get_patterns_from_characteristic}}, \code{\link{get_patterns_from_category}}.
#' 
#' @examples
#' get_patterns_from_status(SA_instance, SA_instance["patterns"],
#'                          value = SA_instance["Class"]$STATUS_PERSISTENT,
#'                          condition = "EQ")
#' 
#' get_patterns_from_status(SA_instance, SA_instance["patterns"],
#'                          value = c("Persistent", "Declining"),
#'                          condition = "!=")
#' 
#' @aliases get_patterns_from_status
#' @keywords internal
setMethod(f = "get_patterns_from_status",
          signature = "SpectralAnalyzer",
          definition = function(object, pc, value, condition = "EQ") {
            
            # Récupération des patterns
            pc = get_nopc(object, pc, object@Class$PATTERNS)
            
            switch(EXPR = condition,
                   "EQ" = { return(pc[pc$status %in% value, ]) },
                   "==" = { return(pc[pc$status %in% value, ]) },
                   
                   "NE" = { return(pc[!(pc$status %in% value), ]) },
                   "!=" = { return(pc[!(pc$status %in% value), ]) },
                   
                   stop("condition must be one of \"EQ\", \"NE\", \"==\", \"!=\"."))
          })


#' Search for patterns by category
#' 
#' Extract the patterns corresponding to a sought category value.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @param pc Data frame of \strong{p}atterns and their \strong{c}haracteristics. Any subset of
#'  \code{object["patterns"]}.\cr
#'  \code{"patterns"} and \code{"p"} are specific values for \code{object["patterns"]}.
#' @param category Name or number of the category on which to search (numbering according to the order
#'  of the columns of \code{object["items_categories"]}).
#' @param value Sought value for the category specified by the argument \code{category}.
#' @param condition Category value search condition for a pattern to be extracted.
#'  One of \code{"items"}, \code{"links"}, \code{"vertices"}, \code{"edges"}.
#'  \describe{
#'    \item{\code{"items"}, \code{"vertices"}}{Search for patterns containing an item associated with
#'          the sought category value.}
#'    \item{\code{"links"}, \code{"edges"}}{Search for patterns generating links corresponding to the
#'          sought category value.}
#'  }
#' @return Subset of the data frame of patterns that match the search criteria.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{get_patterns}}, \code{\link{get_patterns_from_items}},
#'          \code{\link{get_patterns_from_characteristic}}, \code{\link{get_patterns_from_status}}.
#' 
#' @examples
#' get_patterns_from_category(SA_instance, SA_instance["patterns"],
#'                            category = "family", value = "Chrome",
#'                            condition = "items")
#' 
#' get_patterns_from_category(SA_instance, SA_instance["patterns"],
#'                            category = 1, value = "Chrome",
#'                            condition = "links")
#' 
#' @aliases get_patterns_from_category
#' @keywords internal
setMethod(f = "get_patterns_from_category",
          signature = "SpectralAnalyzer",
          definition = function(object, pc, category, value, condition) {
            
            # Récupération des patterns
            pc = get_nopc(object, pc, object@Class$PATTERNS)
            
            # Validation des paramètres liés à une valeur de catégorie
            check_access_for_category(object, category, value)
            
            if (condition == "items" || condition == "vertices") {
              # Recherche des items correspondant à la catégorie recherchée
              items = rownames(subset(object@items_categories, object@items_categories[category] == value))
              # Extraction des motifs contenant ces items
              return(get_patterns_from_items(object, pc, items, condition = "any"))
              
            } else if (condition == "links" || condition == "edges") {
              # Recherche de l'ensemble de liens correspondant aux motifs
              links = get_links(object, pc)
              # Valeurs associées à chaque lien pour le type de catégorie recherché
              categories_links = lapply(strsplit(links$items, "/"),
                                        function(x) sort(unique(as.character(object@items_categories[x, category]))))
              # Extraction des liens qui correspondent à la valeur de catégorie recherchée
              links = links[sapply(categories_links, function(x) value %in% x), ]
              # Récupération des motifs associés
              return(pc[unique(unlist(links[, 1:2])), ])
            }
            stop("condition must be one of \"items\", \"links\", \"vertices\", \"edges\".")
          })


#' Get links between nodes or patterns
#' 
#' Extract from the links those corresponding to the desired nodes or patterns.
#' 
#' @details
#' If among the nodes or patterns for which the links are sought, some become isolated because the other
#'  entities to which they are normally linked are not part of the subset \code{nopc}, these nodes or
#'  patterns are placed at the end of the return data frame.
#' These possible \code{n} additional lines are numbered \code{"A1"..."An"}.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @param nopc Data frame of \strong{n}odes \strong{o}r \strong{p}atterns and their
#'  \strong{c}haracteristics. Nodes or patterns whose links are be to sought. Any subset of
#'  \code{object["nodes"]} or \code{object["patterns"]}.\cr
#'  \code{"nodes"}, \code{"n"}, \code{"patterns"} and \code{"p"} are specific values for
#'  \code{object["nodes"]} and \code{object["patterns"]}.
#' @return Data frame associating the linked nodes or linked patterns.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{get_isolates}}, \code{\link{get_non_isolates}}, \code{\link{get_complexes}}.
#' 
#' @examples
#' get_links(SA_instance, "patterns")
#' get_links(SA_instance, SA_instance["patterns"][1:10, ])
#' 
#' @aliases get_links
#' @export
setMethod(f = "get_links",
          signature = "SpectralAnalyzer",
          definition = function(object, nopc) {
            
            # Récupération des noeuds/patterns et recherche du type d'entités fourni
            nopc = get_nopc(object, nopc)
            entities = which_entities(object, nopc)
            
            # Si les liens recherchés correspondent à l'intégralité des liens
            if (entities == "nodes" && identical(object@nodes, nopc)) {
              return(object@nodes_links)
            }
            if (entities == "patterns" && identical(object@patterns, nopc)) {
              return(object@patterns_links)
            }
            
            # Sinon...
            search_nodes = (entities == "nodes")
            all_links = if(search_nodes) object@nodes_links else object@patterns_links
            
            # Sous-ensemble des liens pour lesquels les deux sommets sont à afficher
            # (nop_links = nodes or patterns links)
            nop_links = all_links[all_links$endpoint.1 %in% rownames(nopc)
                                  & all_links$endpoint.2 %in% rownames(nopc), ]
            
            # Identification des nouveaux sommets isolés
            isolated = lapply(rownames(nopc),
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


#' Search for isolated nodes or patterns
#' 
#' Extract from the given nodes or patterns those which are isolated.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @param nopc Data frame of \strong{n}odes \strong{o}r \strong{p}atterns and their
#'  \strong{c}haracteristics. Nodes or patterns whose isolated are to be sought. Any subset of
#'  \code{object["nodes"]} or \code{object["patterns"]}.\cr
#'  \code{"nodes"}, \code{"n"}, \code{"patterns"} and \code{"p"} are specific values for
#'  \code{object["nodes"]} and \code{object["patterns"]}.
#' @return Subset of the data frame that corresponds to isolated entities.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{get_non_isolates}}, \code{\link{get_complexes}}, \code{\link{get_links}}.
#' 
#' @examples
#' get_isolates(SA_instance, "patterns")
#' get_isolates(SA_instance, SA_instance["patterns"][1:10, ])
#' 
#' @aliases get_isolates
#' @export
setMethod(f = "get_isolates",
          signature = "SpectralAnalyzer",
          definition = function(object, nopc) {
            
            # Récupération des noeuds/patterns
            nopc = get_nopc(object, nopc)
            
            links = get_links(object, nopc)
            row_id = as.character(links$endpoint.1[links$weight == 0])
            return(nopc[row_id, ])
          })


#' Search for non-isolated nodes or patterns
#' 
#' Extract from the given nodes or patterns those which are not isolated.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @param nopc Data frame of \strong{n}odes \strong{o}r \strong{p}atterns and their
#'  \strong{c}haracteristics. Nodes or patterns whose non-isolated are to be sought. Any subset of
#'  \code{object["nodes"]} or \code{object["patterns"]}.\cr
#'  \code{"nodes"}, \code{"n"}, \code{"patterns"} and \code{"p"} are specific values for
#'  \code{object["nodes"]} and \code{object["patterns"]}.
#' @return Subset of the data frame that corresponds to non-isolated entities.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{get_isolates}}, \code{\link{get_complexes}}, \code{\link{get_links}}.
#' 
#' @examples
#' get_non_isolates(SA_instance, "patterns")
#' get_non_isolates(SA_instance, SA_instance["patterns"][1:10, ])
#' 
#' @aliases get_non_isolates
#' @export
setMethod(f = "get_non_isolates",
          signature = "SpectralAnalyzer",
          definition = function(object, nopc) {
            
            # Récupération des noeuds/patterns
            nopc = get_nopc(object, nopc)
            
            links = get_links(object, nopc)
            row_id = as.character(sort(unique(unlist(links[links$weight != 0,
                                                           c("endpoint.1", "endpoint.2")]))))
            return(nopc[row_id, ])
          })


#' Search for complex nodes or patterns
#' 
#' Extract from the given nodes or patterns those which are complex by the number of items they
#'  contain or by the number of values with which they are associated, with regard to one category.
#' 
#' @details
#' If \code{category} and \code{condition} are \code{NULL}, entities with more than \code{min_nb_values}
#'  items are sought. Otherwise, the search is related to the \code{category} (see \code{condition}).
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @param nopc Data frame of \strong{n}odes \strong{o}r \strong{p}atterns and their
#'  \strong{c}haracteristics. Nodes or patterns whose complexes are to be sought. Any subset of
#'  \code{object["nodes"]} or \code{object["patterns"]}.\cr
#'  \code{"nodes"}, \code{"n"}, \code{"patterns"} and \code{"p"} are specific values for
#'  \code{object["nodes"]} and \code{object["patterns"]}.
#' @param category Name or number of the category on which to search (numbering according to the order
#'  of the columns of \code{object["items_categories"]}).
#' @param condition Condition for a node or a pattern to be extracted.
#'  One of \code{"items"}, \code{"links"}, \code{"vertices"}, \code{"edges"}.
#'  \describe{
#'    \item{\code{"items"}, \code{"vertices"}}{Search for nodes or patterns associated (via their items)
#'          with several values of the category \code{category}.}
#'    \item{\code{"links"}, \code{"edges"}}{Search for nodes or patterns generating links corresponding
#'          with several values of the category \code{category}.}
#'  }
#' @param min_nb_values Minimum number of different values of the category \code{category} a node, a
#'  pattern or a link must have to extract the related entity.
#' @return Subset of the data frame that corresponds to the complex entities sought.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{get_isolates}}, \code{\link{get_non_isolates}}, \code{\link{get_links}}.
#' 
#' @examples
#' get_complexes(SA_instance, "patterns")
#' get_complexes(SA_instance, SA_instance["patterns"][1:15, ],
#'               category = "family", condition = "items")
#' get_complexes(SA_instance, SA_instance["patterns"][1:15, ],
#'               category = 1, condition = "links")
#' 
#' @aliases get_complexes
#' @export
setMethod(f = "get_complexes",
          signature = "SpectralAnalyzer",
          definition = function(object, nopc, category = NULL, condition = NULL, min_nb_values = 2) {
            
            # Récupération des noeuds/patterns et recherche du type d'entités fourni
            nopc = get_nopc(object, nopc)
            entities = which_entities(object, nopc)
            
            if (is.null(category)) {
              # Entités possédant au moins min_nb_values items
              column = if (entities == "nodes") "length" else "order"
              return(nopc[nopc[, column] >= min_nb_values, ])
              
            } else {
              # Validation du paramètre d'accès à la catégorie
              check_access_for_category(object, category, NA)
              
              if (condition == "items" || condition == "vertices") {
                # Catégories associées à chaque noeud ou motif
                nop_category = lapply(nopc[[substr(entities, 1, nchar(entities) - 1)]],
                                      function(x) unique(as.character(object@items_categories[x, category])))
                
                # Entités associées à au moins min_nb_values valeurs différentes pour la catégorie
                return(nopc[lapply(nop_category, length) >= min_nb_values, ])
                
              } else if (condition == "links" || condition == "edges") {
                # Liens associés aux noeuds ou motifs
                nop_links = get_links(object, nopc)
                
                # Catégories associées à chaque lien
                links_category = lapply(strsplit(nop_links$items, "/"),
                                        function(x) sort(unique(as.character(object@items_categories[x, category]))))
                
                # Identifiants des entités dont au moins un lien est associé à au moins min_nb_values valeurs différentes pour la catégorie
                id = unique(unlist(nop_links[which(lapply(links_category, length) >= min_nb_values),
                                             c("endpoint.1", "endpoint.2")]))
                
                # Entités correspondantes
                return(nopc[as.character(id), ])
              }
              stop("condition must be one of \"items\", \"links\", \"vertices\", \"edges\".") 
            }
          })



#### Other specific methods ####


#' Validation of parameters for search by category
#' 
#' Check that the parameters provided match an existing category.
#' Print an error message if not.
#' 
#' @details
#' If \code{value = NA}, only the parameter \code{category} is checked.
#' 
#' @param object \code{SpectralAnalyzer} class object.
#' @param category Name or number of the category to access (numbering according to the order of the
#'  columns of \code{object["items_categories"]}).
#' @param value Sought value for the category specified by the argument \code{category}, or NA.
#' @param stop If \code{TRUE}, stop the execution and print an error message if the parameters do not
#'  allow access to a category. If \code{FALSE}, see 'Value' section.
#' @return \code{TRUE} or \code{FALSE} whether the parameters allow access to a category.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{get_patterns}}, \code{\link{get_nodes}}.
#'          \code{\link{get_patterns_from_category}}, \code{\link{get_nodes_from_category}}.
#' 
#' @aliases check_access_for_category
#' @keywords internal
setMethod(f = "check_access_for_category",
          signature = "SpectralAnalyzer",
          definition = function(object, category, value, stop = TRUE) {
            
            # Vérification que le type de catégorie recherché existe
            if (is.character(category) & !(category %in% colnames(object@items_categories))) {
              if (!stop) return(FALSE)
              stop("category must be one of ", paste0("\"", colnames(object@items_categories), "\"",
                                                      collapse = ", ") ,".")
            } else if (is.numeric(category) & (category < 1 | category > ncol(object@items_categories))) {
              if (!stop) return(FALSE)
              stop(paste0("category must be in range [1,", ncol(object@items_categories), "]."))
            }
            
            # Vérification que la valeur de la catégorie recherchée existe
            if (!is.na(value) && !(value %in% levels(object@items_categories[, category]))) {
              if (!stop) return(FALSE)
              stop("value must be one of ", paste0("\"", levels(object@items_categories[, category]), "\"",
                                                   collapse = ", ") ,".")
            }
            return(TRUE)
          })


#' Search for the nodes or patterns characteristics
#' 
#' Find and return the data frame corresponding to the nodes or the patterns of the `SpectralAnalyzer`
#'  object, or return the given data frame.
#' 
#' @details
#' If `nopc` is a data frame, it is returned.
#' 
#' If `nopc` is a character value equal to:
#'  * `"nodes"` or `"n"`: `object["nodes"]` is returned.
#'  * `"patterns"` or `"p"`: `object["patterns"]` is returned.
#' 
#' The argument `entities` is only used to adapt a possible error message.
#' 
#' @param object `SpectralAnalyzer` class object.
#' @param nopc Data frame of **n**odes **o**r **p**atterns and their **c**haracteristics or one of the
#'  following character values: `"nodes"`, `"n"`, `"patterns"`, `"p"`.
#' @param entities Type of the entities that the data frame can refer to (`NODES`, `PATTERNS` or
#'  `NODES_OR_PATTERNS`).
#' @return Data frame of nodes or patterns and their characteristics corresponding to the arguments.
#' 
#' @author Gauthier Magnin
#' @seealso [`which_entities`].
#' 
#' @aliases get_nopc
#' @md
#' @keywords internal
setMethod(f = "get_nopc",
          signature = "SpectralAnalyzer",
          definition = function(object, nopc, entities = object@Class$NODES_OR_PATTERNS) {
            
            if (is.character(nopc)) {
              if (nopc == object@Class$NODES || nopc == substr(object@Class$NODES, 1, 1))
                return(object@nodes)
              if (nopc == object@Class$PATTERNS || nopc == substr(object@Class$PATTERNS, 1, 1))
                return(object@patterns)
              
              var_name = deparse(substitute(nopc))
              
              if (entities == object@Class$NODES)
                msg = paste(var_name, "must be \"nodes\" or a data frame of nodes and their characteristics.")
              else if (entities == object@Class$PATTERNS)
                msg = paste(var_name, "must be \"patterns\" or a data frame of patterns and their characteristics.")
              else # object@Class$NODES_OR_PATTERNS
                msg = paste(var_name, "must be \"nodes\", \"patterns\" or a data frame of nodes or patterns and their characteristics.")
              
              stop(msg)
            }
            return(nopc)
          })


#' Detect the type of entities
#' 
#' Detect the type of entities contained in a data frame among nodes, patterns and association rules.
#' 
#' @details
#' The detection uses the column names of the data frame and search for `"node"`, `"pattern"` or
#'  `"antecedent"` for a data frame of nodes, patterns or rules, respectively.
#' 
#' The argument `entities` is only used to adapt a possible error message.
#' 
#' @param object `SpectralAnalyzer` class object.
#' @param npr Data frame of **n**odes, **p**atterns or association **r**ules and their characteristics.
#' @param entities Define if the data frame is either a data frame of nodes or a data frame of patterns
#'  (`NODES_OR_PATTERNS`), or if it can also be a data frame of rules (`NODES_PATTERNS_OR_RULES`).
#' @return Character corresponding to `NODES`, `PATTERNS` or `RULES`.
#' 
#' @author Gauthier Magnin
#' @seealso [`get_nopc`].
#' 
#' @aliases which_entities
#' @md
#' @keywords internal
setMethod(f = "which_entities",
          signature = "SpectralAnalyzer",
          definition = function(object, npr, entities = object@Class$NODES_OR_PATTERNS) {
            
            if ("node" %in% colnames(npr)) return(object@Class$NODES)
            if ("pattern" %in% colnames(npr)) return(object@Class$PATTERNS)
            if ("antecedent" %in% colnames(npr)) return(object@Class$RULES)
            
            var_name = deparse(substitute(nopc))
            
            if (entities == object@Class$NODES_OR_PATTERNS)
              stop(paste(var_name, "must be a data frame of nodes or patterns and their characteristics."))
            
            # entities = NODES_PATTERNS_OR_RULES
            stop(paste(var_name, "must be a data frame of nodes or patterns and their",
                       "characteristics or a data frame of association rules."))
          })


