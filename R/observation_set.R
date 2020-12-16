#' @include utils.R
NULL


#### Class definition and constructor ####

# Creation of a class corresponding to 2 types for allowing NA
setClassUnion("characterOrNA", c("character", "logical"))


#' Observation Set
#' 
#' S4 object class for representing observations.
#'  An observation is defined as an itemset associated with any other data.
#' 
#' @details
#' Since `year_key` is not required, observations do not need to contain temporal data. In such a case,
#'  `year_key` is `NA`.
#' 
#' @slot data List of observations containing items and any additional data.
#'  Each observation is itself a list. All observations share the same element names (corresponding to
#'  the names of the various data).
#' @slot names Names of the elements present in each observation (i.e. names of the elements contained in
#'  the list corresponding to an observation).
#' @slot item_key Name of the element containing the items of an observation.
#' @slot year_key Name of the element containing the year in which an observation was made.
#' 
#' @author Gauthier Magnin
#' @seealso
#' The `ObservationSet` constructor: [`observation.set`].
#' 
#' An example object of class `ObservationSet`: [`OS_instance`].
#' 
#' @aliases ObservationSet
#' @md
#' @export
setClass(Class = "ObservationSet",
         slots = c(
           data = "list",
           names = "character",
           item_key = "character",
           year_key = "characterOrNA"
         ))

# Validity
setValidity(Class = "ObservationSet",
            method = function(object) {
              
              if (length(object@data) != 0) {
                type_of_data = unique(unlist(lapply(object@data, typeof)))
                if (length(type_of_data) > 1 || type_of_data != "list")
                  return("All elements of data must be lists.")
                
                names = unique(lapply(object@data, names))
                if (length(names) != 1)
                  return("All sub-elements of data must share the same names.")
                
                if (any(unlist(names) != object@names))
                  return("names must be the names shared by the sub-elements of data.")
              }
              
              if (length(object@item_key) != 1 || !is.element(object@item_key, object@names))
                return("item_key must be one of the names of the sub-elements of data.")
              
              if (length(object@year_key) != 1 ||
                  (!is.element(object@year_key, object@names) && !is.na(object@year_key)))
                return("year_key must be one of the names of the sub-elements of data or NA.")
              
              return(TRUE)
            })

# Initializer
setMethod(f = "initialize",
          signature = "ObservationSet",
          definition = function(.Object, data, names, item_key, year_key) {
            
            .Object@data = data
            .Object@names = if (missing(names) || is.null(names)) names(data[[1]]) else names
            .Object@item_key = item_key
            .Object@year_key = year_key
            
            methods::validObject(.Object)
            return(.Object)
          })


#' Observation Set constructor
#' 
#' Create and initialize an S4 object of class `ObservationSet`.
#' 
#' @details
#' Observations do not necessarily require temporal data since `year_key` is not required.
#' 
#' If names are not specified using the argument `names`, they are automatically extracted from
#'  `data`. Specifying `names` allows to create `ObservationSet` (or subset of existing `ObservationSet`)
#'  without any observations.
#' 
#' @param data List of observations containing items and any additional data.
#'  Each observation is itself a list. All observations must share the same element names.
#' @param item_key Name of the element containing the items of an observation.
#' @param year_key Name of the element containing the year in which an observation was made.
#' @param names Names of the elements present in each observation (i.e. names of the elements contained in
#'  the list corresponding to an observation).
#' @return New object of class `ObservationSet`.
#' 
#' @author Gauthier Magnin
#' @seealso
#' The class: [`ObservationSet`].
#' 
#' @examples
#' ## Creating a list of observations then an ObservationSet
#' obs <- list(O1 = list(items = "A",
#'                       date = 2018,
#'                       labels = c("l1", "l2")),
#'             
#'             O2 = list(items = c("A", "B", "D"),
#'                       date = 2018,
#'                       labels = "l3"),
#'             
#'             O3 = list(items = c("B", "C", "D"),
#'                       date = 2019,
#'                       labels = c("l2", "l3")),
#'             
#'             O4 = list(items = c("A", "B", "C", "D"),
#'                       date = 2020,
#'                       labels = c("l1", "l2", "l3"))
#'             )
#' obs_object <- observation.set(obs, item_key = "items", year_key = "date")
#' print(obs_object)
#' 
#' @md
#' @export
observation.set = function(data, item_key, year_key = NA, names = NULL) {
  return(methods::new(Class = "ObservationSet",
                      data = data, names = names, item_key = item_key, year_key = year_key))
}



#### Methods print, show, plot, summary, length ####

# print: display in console
setMethod(f = "print",
          signature = "ObservationSet",
          definition = function(x, ...) {
            cat("ObservationSet\n",
                "  data:     ", length(x), pluralize(" observation", x), "\n",
                "  names:    ", paste0("\"", x@names, "\"", collapse = ", "), "\n",
                "  item_key: \"", x@item_key, "\"\n",
                "  year_key: ", if (is.na(x@year_key)) NA else paste0("\"", x@year_key, "\""),
                sep = "")
          })

# show: short display in console
setMethod(f = "show",
          signature = "ObservationSet",
          definition = function(object) {
            cat("ObservationSet\n ",
                length(object), pluralize("observation", object), "\n ",
                "Slots:", paste(methods::slotNames("ObservationSet"), collapse = ", "))
          })

# length: object length
setMethod(f = "length",
          signature = "ObservationSet",
          definition = function(x) {
            return(length(x@data))
          })



#### Selector, mutator and susbet ####

#' Extract or replace parts of an object of class ObservationSet
#' 
#' General selector and mutator to access the attributes of an object of class `ObservationSet`.
#' Extraction and replacement can be done by using an attribute name.
#' 
#' @details
#' Sub-elements of the attribute `data` can be accessed this way as well.
#' 
#' Numeric values can be used to access elements of the attribute `data`. Replacing one element of
#'  `data` will not replace its name.
#' 
#' @inheritParams base::Extract
#' 
#' @author Gauthier Magnin
#' 
#' @examples
#' OS_instance["data"]
#' 
#' OS_instance["item_key"]
#' OS_instance["CODE"]
#' 
#' OS_instance[3]
#' OS_instance[3:5]
#' 
#' @aliases [,ObservationSet-method
#' @md
#' @export
setMethod(f = "[",
          signature = "ObservationSet",
          definition = function(x, i, j, drop) {
            
            # Accès à un sous-ensemble d'observations
            if (is.numeric(i)) {
              if (length(i) == 1) return(x@data[[i]])
              return(x@data[i])
            }
            
            # Accès à un attribut
            if (i %in% slotNames("ObservationSet"))
              return(eval(parse(text = paste0("x@", i))))
            
            # Accès à un sous-élément de la liste d'observations
            if (i %in% x@names)
              return(lapply(x@data, "[[", i))
            
            stop("Unknown attribute.")
          })

#' @rdname sub-ObservationSet-ANY-ANY-ANY-method
#' 
#' @details
#' Replacing the attribute `names` renames the elements containing in each observation if `value` is the
#'  same length as `names`. In this case, the attributes `item_key` and `year_key` are updated (unless
#'  `year_key` is `NA`).
#'  
#' Replacing the attribute `names` removes missing elements of `value` in each observation if it is
#'  smaller than `names`.
#' 
#' @examples
#' OS_instance["year_key"] <- NA
#' OS_instance[3] <- list(CODE = 1,
#'                        YEAR = 2000,
#'                        JOB.TITLE = 45454545,
#'                        JOB.TASK = "A3000",
#'                        SAMPLE.ID = c(1, 3, 5, 8))
#' OS_instance["names"] <- c("CODE", "YEAR")
#' 
#' @aliases [<-,ObservationSet-method
#' @md
#' @export
setReplaceMethod(f = "[",
                 signature = "ObservationSet",
                 definition = function(x, i, j, value) {
                   
                   if (is.numeric(i)) {
                     if (length(i) == 1) x@data[[i]] = value
                     else x@data[i] = value
                   }
                   else {
                     if (!is.element(i, slotNames("ObservationSet")))
                       stop("Unknown attribute.")
                     
                     if (i == "names") {
                       if (length(value) > length(x@names))
                         stop("Replacing names need value to be the same length or smaller.")
                       
                       if (length(value) < length(x@names)) {
                         to_remove = x@names[which(!is.element(x@names, value))]
                         for (o in seq_along(x@data)) for (name in to_remove) x@data[[o]][name] = NULL
                       }
                       else { # length(value) = length(x@names)
                         to_rename = which(value != x@names)
                         for (o in seq_along(x@data)) for (j in to_rename) names(x@data[[o]])[j] = value[j]
                         
                         if (x@item_key %in% x@names[to_rename])
                           x@item_key = value[x@item_key == x@names]
                         
                         if (!is.na(x@year_key) && x@year_key %in% x@names[to_rename])
                           x@year_key = value[x@year_key == x@names]
                       }
                     }
                     eval(parse(text = paste0("x@", i, " = value")))
                   }
                   methods::validObject(x)
                   return(x)
                 })


#' Subsetting Observation Set
#' 
#' Return `ObservationSet` in which observations are a subset of the ones of another `ObservationSet`.
#' 
#' @details
#' If the observations from `x` are not named and `keep_names = TRUE`, the observations of the resulting
#'  object are named according to the indexes of the initial observations.
#' 
#' @param x Object of class `ObservationSet` to be subsetted.
#' @param indexes Numeric vector indicating which observations from `x["data"]` to keep,
#'  or logical vector indicating for each observation whether to keep it.
#' @param keep_names If `TRUE`, observations of the returned object keep the names from the initial object
#'  `x`. If `FALSE`, they are not named.
#' @return S4 object of class `ObservationSet` having a subset of observations from `x` (i.e. a subset of
#'  the attribute `data` from `x`).
#' 
#' @author Gauthier Magnin
#' @examples
#' subset(OS_instance, c(1, 7, 8, 9))
#' subset(OS_instance, c(TRUE, rep(FALSE, 12), TRUE))
#' 
#' @aliases subset
#' @md
#' @export
setMethod(f = "subset",
          signature = "ObservationSet",
          definition = function(x, indexes, keep_names = TRUE) {
            
            if (is.logical(indexes)) indexes = which(indexes)
            
            if (keep_names) {
              if (!is.null(names(x@data))) {
                return(observation.set(x@data[indexes],
                                       x@item_key, x@year_key, x@names))
              }
              return(observation.set(stats::setNames(x@data[indexes], indexes),
                                     x@item_key, x@year_key, x@names))
            }
            # if (!keep_names)
            return(observation.set(unname(x@data[indexes]),
                                   x@item_key, x@year_key, x@names))
          })



#### Type conversions ####

# Turn an ObservationSet into a set of [`transactions`][arules::transactions-class] from the
# package [`arules`].
# Only the items of the observations are considered. Other data are ignored.
setAs(from = "ObservationSet",
      to = "transactions",
      def = function(from) {
        
        # Liste des items retrouvés pour chaque observation et vecteurs des identifiants des items
        data = sapply(sapply(from@data, "[", from@item_key), as.character)
        labels = as.character(get_all_items(from))
        
        # Transformation en objet arules::itemMatrix puis en objet arules::transaction :
        # une ligne par observation, une colonne par item
        return(methods::as(arules::encode(data, labels), "transactions"))
      })



#### Declaration of the methods ####

# Methods for search in observations
setGeneric(name = "get_all_items",       def = function(object){ standardGeneric("get_all_items") })
setGeneric(name = "get_itemsets",        def = function(object){ standardGeneric("get_itemsets") })
setGeneric(name = "get_items_from_info", def = function(object, ...){ standardGeneric("get_items_from_info") })
setGeneric(name = "get_info_from_items", def = function(object, ...){ standardGeneric("get_info_from_items") })

# Methods for search for observations
setGeneric(name = "get_complex_obs",     def = function(object){ standardGeneric("get_complex_obs") })
setGeneric(name = "get_simple_obs",      def = function(object){ standardGeneric("get_simple_obs") })
setGeneric(name = "get_obs_from_items",  def = function(object, ...){ standardGeneric("get_obs_from_items") })
setGeneric(name = "get_obs_from_info",   def = function(object, ...){ standardGeneric("get_obs_from_info") })

# Methods for computations on observations
setGeneric(name = "complexity_ratio",     def = function(object, ...){ standardGeneric("complexity_ratio") })
setGeneric(name = "complexity_index",     def = function(object, ...){ standardGeneric("complexity_index") })
setGeneric(name = "co_occurrence_matrix", def = function(object, ...){ standardGeneric("co_occurrence_matrix") })

# Methods for plotting charts
setGeneric(name = "co_occurrence_chart", def = function(object, ...){ standardGeneric("co_occurrence_chart") })

# Other specific methods
setGeneric(name = "has_temporal_data",   def = function(object){ standardGeneric("has_temporal_data") })
setGeneric(name = "get_items",           def = function(object, ...){ standardGeneric("get_items") })



#### Methods for search in observations ####

#' Search all items
#' 
#' Extract all the items contained in the observations.
#' 
#' @param object S4 object of class `ObservationSet`.
#' @return Vector of all unique items.
#' 
#' @author Gauthier Magnin
#' @seealso [`get_itemsets`].
#' 
#' @examples
#' get_all_items(OS_instance)
#' 
#' @aliases get_all_items
#' @md
#' @export
setMethod(f = "get_all_items",
          signature = "ObservationSet",
          definition =
function(object) {
  return(sort(unique(unlist(sapply(object@data, "[", object@item_key)))))
})


#' Extract the item sets
#' 
#' Extract the item sets corresponding to the observations.
#' 
#' @param object S4 object of class `ObservationSet`.
#' @return List of the item sets of the observations.
#' 
#' @author Gauthier Magnin
#' @seealso [`get_all_items`].
#' 
#' @examples
#' get_itemsets(OS_instance)
#' 
#' @aliases get_itemsets
#' @md
#' @export
setMethod(f = "get_itemsets",
          signature = "ObservationSet",
          definition =
function(object) {
  return(lapply(object@data, "[[", object@item_key))
})


#' Search for items by specific information
#' 
#' Retrieve the items associated with observations whose information matches to one or more sought
#'  information.
#' 
#' @param object S4 object of class `ObservationSet`.
#' @param info Named list of sought information. Element names must refer to the names of variables
#'  contained in the observations and values must correspond to the sought values for these variables.
#' @param presence Information presence condition for an item to be extracted from an observation.
#'  One of `"all"`, `"any"`.
#'  \describe{
#'   \item{`"all"`}{All the sought information must be part of an observation for its items to be
#'                  extracted.}
#'   \item{`"any"`}{At least one of the sought information must be part of an observation for its items
#'                  to be extracted.}
#'  }
#' @param additional Names of additional information to extract during the search.
#' @return Vector of the item codes corresponding to the search if `additional` is equal to `NULL`.
#'  List of codes and information requested otherwise.
#' 
#' @author Gauthier Magnin
#' @seealso [`get_all_items`], [`get_info_from_items`].
#' 
#' @examples
#' get_items_from_info(OS_instance, info = list(JOB.TITLE = 44132017))
#' get_items_from_info(OS_instance, info = list(JOB.TITLE = 44132017),
#'                                  additional = "SAMPLE.ID")
#' 
#' @aliases get_items_from_info
#' @md
#' @export
setMethod(f = "get_items_from_info",
          signature = "ObservationSet",
          definition =
function(object, info, presence = "all", additional = NULL) {
  
  # Observations correspondant aux critères
  obs = get_obs_from_info(object, info, presence = presence)
  
  # Vecteur des items
  if (is.null(additional)) {
    items = unique(unlist(lapply(obs@data, "[", object@item_key)))
    return(sort(items))
  }
  
  # Liste des items et données demandées
  items = c(list(CODE = sort(unique(unlist(lapply(obs@data, "[[", object@item_key))))),
            lapply(additional, function(a) sort(unique(unlist(lapply(obs@data, "[[", a))))))
  return(stats::setNames(items, c(object@item_key, additional)))
})


#' Search for information by item
#' 
#' Retrieve information associated with observations that contain a set of sought items.
#' 
#' @param object S4 object of class `ObservationSet`.
#' @param items Sought items.
#' @param info_names Names of information to extract from observations.
#' @param presence Item presence condition for information to be extracted from an observation.
#' One of `"all"`, `"any"`, `"exact"`.
#'  \describe{
#'   \item{`"all"`}{All the sought items must be part of an observation for its information to be
#'                  extracted.}
#'   \item{`"any"`}{At least one of the sought items must be part of an observation for its information
#'                  to be extracted.}
#'    \item{`"exact"`}{The item set contained in an observation must be exactly the same as the sought
#'                     item set for this observation to be extracted.}
#'  }
#' @return Vector or list of information corresponding to the search.
#'  Vector if only one type of information is to be extracted. List otherwise.
#' 
#' @author Gauthier Magnin
#' @seealso [`get_items_from_info`], [`get_all_items`].
#' 
#' @examples
#' get_info_from_items(OS_instance, items = 3146,
#'                                  info_names = c("JOB.TITLE", "JOB.TASK"))
#' 
#' @aliases get_info_from_items
#' @md
#' @export
setMethod(f = "get_info_from_items",
          signature = "ObservationSet",
          definition =
function(object, items, info_names, presence = "all") {
  
  # Observations contenant le ou les items recherchés
  obs = get_obs_from_items(object, items, presence)
  
  # Vecteur de la variable demandée
  if (length(info_names) == 1) return(sort(unique(unlist(sapply(obs@data, "[", info_names)))))
  
  # Liste des variables demandées
  to_return = lapply(info_names, function(var) sort(unique(unlist(lapply(obs@data, "[[", var)))))
  return(stats::setNames(to_return, info_names))
})



#### Methods for search for observations ####

#' Search for complex observations
#' 
#' Extract the observations containing more than one item.
#' 
#' @param object S4 object of class `ObservationSet`.
#' @return S4 object of class `ObservationSet` containing the subset of observations that contain more
#'  than one item.
#' 
#' @author Gauthier Magnin
#' @seealso [`get_simple_obs`], [`get_obs_from_items`], [`get_obs_from_info`].
#' 
#' @examples
#' get_complex_obs(OS_instance)
#' 
#' @aliases get_complex_obs
#' @md
#' @export
setMethod(f = "get_complex_obs",
          signature = "ObservationSet",
          definition =
function(object) {
  index = sapply(get_itemsets(object),
                 function (x) length(x) > 1)
  return(subset(object, index))
})


#' Search for simple observations
#' 
#' Extract the observations containing exactly one item.
#' 
#' @param object S4 object of class `ObservationSet`.
#' @return S4 object of class `ObservationSet` containing the subset of observations that contain exactly
#'  one item.
#' 
#' @author Gauthier Magnin
#' @seealso [`get_complex_obs`], [`get_obs_from_items`], [`get_obs_from_info`].
#' 
#' @examples
#' get_simple_obs(OS_instance)
#' 
#' @aliases get_simple_obs
#' @md
#' @export
setMethod(f = "get_simple_obs",
          signature = "ObservationSet",
          definition =
function(object) {
  index = sapply(get_itemsets(object),
                 function (x) length(x) == 1)
  return(subset(object, index))
})


#' Search for observations by item
#' 
#' Extract the observations containing one or more sought items.
#' 
#' @param object S4 object of class `ObservationSet`.
#' @param items Sought items.
#' @param presence Item presence condition for an observation to be extracted.
#'  One of `"all"`, `"any"`, `"exact"`.
#'  \describe{
#'    \item{`"all"`}{All the sought items must be part of an observation for this observation to be
#'                   extracted.}
#'    \item{`"any"`}{At least one of the sought items must be part of an observation for this observation
#'                   to be extracted.}
#'    \item{`"exact"`}{The item set contained in an observation must be exactly the same as the sought
#'                     item set for this observation to be extracted.}
#'  }
#' @return S4 object of class `ObservationSet` containing the subset of observations that match the search
#'  criteria.
#' 
#' @author Gauthier Magnin
#' @seealso [`get_complex_obs`], [`get_simple_obs`], [`get_obs_from_info`].
#' 
#' @examples
#' get_obs_from_items(OS_instance, items = c(25, 192), presence = "all")
#' get_obs_from_items(OS_instance, items = c(25, 192), presence = "any")
#' get_obs_from_items(OS_instance, items = c(25, 192), presence = "exact")
#' 
#' @aliases get_obs_from_items
#' @md
#' @export
setMethod(f = "get_obs_from_items",
          signature = "ObservationSet",
          definition =
function(object, items, presence = "all") {
  
  check_param(presence, values = c("all", "any", "exact"))
  
  if (presence == "exact") {
    index = sapply(get_itemsets(object),
                   function(x) setequal(items, x))
  }
  else {
    func = if (presence == "all") all else any
    index = sapply(get_itemsets(object),
                   function(x) func(items %in% x))
  }
  
  return(subset(object, index))
})


#' Search for observations by specific information
#' 
#' Extract the observations whose information matches to one or more sought information.
#' 
#' @param object S4 object of class `ObservationSet`.
#' @param info Named list of sought information. Element names must refer to the names of variables
#'  contained in the observations and values must correspond to the sought values for these variables.
#' @param presence Information presence condition for an observation to be extracted.
#'  One of `"all"`, `"any"`.
#'  \describe{
#'   \item{`"all"`}{All the sought information must be part of an observation for this observation to
#'                  be extracted.}
#'   \item{`"any"`}{At least one of the sought information must be part of an observation for this
#'                  observation to be extracted.}
#'  }
#' @return `ObservationSet` containing the subset of observations that match the search criteria.
#' 
#' @author Gauthier Magnin
#' @seealso [`get_complex_obs`], [`get_simple_obs`], [`get_obs_from_items`].
#' 
#' @examples
#' get_obs_from_info(OS_instance, info = list(JOB.TITLE = 44132001,
#'                                            JOB.TASK = "A8310"))
#' 
#' @aliases get_obs_from_info
#' @md
#' @export
setMethod(f = "get_obs_from_info",
          signature = "ObservationSet",
          definition =
function(object, info, presence = "all") {
  
  check_param(presence, values = c("all", "any"))
  func = if (presence == "all") all else any
  
  if (length(info) == 0) return(object)
  
  # Vérification de la correspondance de chaque argument dans chaque observation
  correspondence = sapply(info,
                          function(arg) {
                            lapply(sapply(object@data, "[[", names(info)[parent.frame()$i[]]),
                                   function(o) arg %in% o)
                          })
  
  # Indices des observations correspondant aux critères
  if (length(info) == 1 && length(info[[1]]) == 1) {
    index = unlist(correspondence)
  } else {
    # Unlist indépendant pour chaque argument, nécessaire car certaines observations regroupent
    # plusieurs valeurs pour une même variable
    index = apply(t(apply(correspondence, 1, unlist)), 1, func)
  }
  
  return(subset(object, index))
})



#### Methods for computations on observations ####

#' Complexity Ratio, by item
#' 
#' For each item, compute the ratio between the number of complex observations containing the item and
#'  the total number of observations containing the item. In other words, compute the complement of the
#'  ratio between the number of times the item appears alone and the number of times the item appears.
#' 
#' @param object S4 object of class `ObservationSet`.
#' @param items Items for which to compute the complexity ratio. The default `NULL` means to compute
#'  it for each existing item.
#' @return Named vector of complexity ratios: for each item, the proportion of complex observations
#'  containing it among all observations containing it.
#' 
#' @author Gauthier Magnin
#' @seealso [`complexity_index`], [`co_occurrence_matrix`],
#'          [`get_all_items`], [`get_obs_from_items`], [`get_complex_obs`], [`get_simple_obs`].
#' 
#' @examples
#' complexity_ratio(OS_instance)
#' complexity_ratio(OS_instance, items = c(25, 148, 3146))
#' 
#' @aliases complexity_ratio
#' @md
#' @export
setMethod(f = "complexity_ratio",
          signature = "ObservationSet",
          definition =
function(object, items = NULL) {
  
  if (is.null(items)) items = get_all_items(object)
  
  # Pour chaque item, nombre d'observations complexes contenant l'item / nombre d'obs contenant l'item
  return(sapply(items,
                function(item) {
                  obs_item = get_obs_from_items(object, item)
                  return(stats::setNames(length(get_complex_obs(obs_item)) / length(obs_item), item))
                }))
})


#' Complexity Index, by item
#' 
#' For each item, count the number of complex observations containing the item.
#' 
#' @param object S4 object of class `ObservationSet`.
#' @param items Items for which to calculate the complexity index. The default `NULL` means to calculate
#'  it for each existing item.
#' @return Named vector of complexity indexes: for each item, the number of complex observations
#'  containing it.
#' 
#' @author Gauthier Magnin
#' @seealso [`complexity_ratio`], [`co_occurrence_matrix`],
#'          [`get_all_items`], [`get_obs_from_items`], [`get_complex_obs`], [`get_simple_obs`].
#' 
#' @examples
#' complexity_index(OS_instance)
#' complexity_index(OS_instance, items = c(19, 25, 148))
#' 
#' @aliases complexity_index
#' @md
#' @export
setMethod(f = "complexity_index",
          signature = "ObservationSet",
          definition =
function(object, items = NULL) {
  
  if (is.null(items)) items = get_all_items(object)
  
  # Pour chaque item, nombre d'observations complexes parmi les observations contenant l'item
  return(sapply(items,
                function(item)
                  stats::setNames(length(
                    get_complex_obs(get_obs_from_items(object, item))
                  ), item)))
})


#' Co-occurrence matrix
#' 
#' Build the co-occurrence matrix: the matrix of the number of observations containing a specific pair of
#'  items, for each possible pair. The diagonal is the number of observations containing each specific
#'  item.
#' 
#' @param object S4 object of class `ObservationSet`.
#' @param items Items for which to count co-occurrences between pairs. The default `NULL` means to count
#'  them considering each existing item.
#' @return Co-occurrence matrix between each pair of items.
#' 
#' @author Gauthier Magnin
#' @seealso [`complexity_ratio`], [`complexity_index`],
#'          [`get_all_items`], [`get_obs_from_items`], [`get_complex_obs`], [`get_simple_obs`].
#' 
#' @examples
#' co_occurrence_matrix(OS_instance)
#' co_occurrence_matrix(OS_instance, items = c(19, 25, 148, 3146))
#' 
#' @aliases co_occurrence_matrix
#' @md
#' @export
setMethod(f = "co_occurrence_matrix",
          signature = "ObservationSet",
          definition =
function(object, items = NULL) {
  
  if (is.null(items)) items = get_all_items(object)
  
  # Pour chaque pair d'items, recherche du nombre d'observations dans lesquelles la paire apparaît
  pairs = combn(items, 2)
  co = apply(pairs, 2, function(pair) length(get_obs_from_items(object, pair)))
  
  # Création d'une matrice qui correspondra à la table de contingence
  co_table = matrix(nrow = length(items), ncol = length(items))
  rownames(co_table) = colnames(co_table) = items
  
  # Conversion en caractère des identifiants des items pour accès à la matrice
  pairs = apply(pairs, c(1,2), as.character)
  
  # Remplissage de la matrice
  for (c in seq_len(ncol(pairs))) {
    co_table[pairs[1, c], pairs[2, c]] = co[c]
    co_table[pairs[2, c], pairs[1, c]] = co[c]
  }
  diag(co_table) = table_on_list(get_itemsets(object))[as.character(items)]
  
  return(co_table)
})



#### Methods for plotting charts ####

#' Co-occurrence chart
#' 
#' Plot a graph in which vertices are items and edges are their co-occurences in observations (i.e. for
#'  each pair of items, the number of observations containing it).
#' 
#' @details
#' The chart being plotted with the packages `ggraph` and `ggplot2`, it can be modified or completed
#'  afterwards using [`ggplot2::last_plot`] or the returned object.
#' 
#' @param object S4 object of class `ObservationSet`.
#' @param items Items for which to count co-occurrences between pairs and to plot on the graph.\cr
#'  `"items"` and `"i"` are special values specifying to use all existing items.
#' @param min_occ Minimum number of co-occurrences to consider to plot a link between two items.
#' @param max_occ Maximum number of co-occurrences to consider to plot a link between two items.
#' @inheritParams heb_chart
#' @return Graph created with the packages `ggraph` and `ggplot2`.
#' 
#' @author Gauthier Magnin
#' @seealso [`co_occurrence_matrix`].
#' 
#' Method for signature `SpectralAnalyzer`: 
#' [`co_occurrence_chart,SpectralAnalyzer`][co_occurrence_chart,SpectralAnalyzer-method].
#' 
#' @examples
#' co_occurrence_chart(OS_instance, get_all_items(OS_instance))
#' co_occurrence_chart(OS_instance, "items") +
#'   ggplot2::expand_limits(x = c(-1.2, 1.2), y = c(-1.2, 1.2))
#' co_occurrence_chart(OS_instance, "items",
#'                     min_occ = 2, palette = "OrRd")
#' co_occurrence_chart(OS_instance, items = c(25, 27, 49, 87, 148, 192, 252, 328))
#' 
#' @aliases co_occurrence_chart co_occurrence_chart,ObservationSet
#' @md
#' @export
setMethod(f = "co_occurrence_chart",
          signature = "ObservationSet",
          definition =
function(object, items, min_occ = 1, max_occ = Inf,
         vertex_size = 3, vertex_alpha = 1, vertex_margin = 0.05,
         label_size = 3, label_margin = 0.05,
         edge_tension = 0.8, edge_alpha = 1,
         palette = "Blues", palette_direction = 1) {
  
  # Validation des items fournis
  items = get_items(object, items)
  
  # Création de la hiérarchie (profondeurs de l'arbre et arêtes entre les sommets)
  hierarchy = data.frame(parent = "root", child = items, stringsAsFactors = FALSE)
  
  # Sommets du graphe
  vertices = data.frame(name = unique(unlist(hierarchy)), stringsAsFactors = FALSE)
  vertices$label = items[match(vertices$name, items)]
  
  # Liens à tracer entre les sommets (différent des arêtes de l'arbre)
  co_occ = as.data.frame(as.table(co_occurrence_matrix(object, items)), stringsAsFactors = FALSE)
  co_occ = co_occ[co_occ$Var1 != co_occ$Var2 & !duplicated(t(apply(co_occ[, c(1,2)], 1, sort))), ]
  connections = co_occ[co_occ$Freq >= min_occ & co_occ$Freq <= max_occ, ]
  
  return(heb_chart(hierarchy, vertices, connections, limits = c(1, max(co_occ$Freq)),
                   vertex_size = vertex_size, vertex_alpha = vertex_alpha, vertex_margin = vertex_margin,
                   label_size = label_size, label_margin = label_margin,
                   edge_tension = edge_tension, edge_alpha = edge_alpha,
                   palette = palette, palette_direction = palette_direction))
})



#### Other specific methods ####

#' Check if an ObservationSet contains temporal data
#' 
#' Equivalent to checking if the attribute `year_key` is not `NA`, but more explicit.
#' 
#' @param object S4 object of class `ObservationSet`.
#' @return `TRUE` or `FALSE` whether `object` contains temporal data.
#' 
#' @author Gauthier Magnin
#' 
#' @examples
#' has_temporal_data(OS_instance)
#' 
#' OS_instance["year_key"] <- NA
#' has_temporal_data(OS_instance)
#' 
#' @aliases has_temporal_data
#' @md
#' @keywords internal
setMethod(f = "has_temporal_data",
          signature = "ObservationSet",
          definition =
function(object) {
  return(!is.na(object@year_key))
})



#' Get items
#' 
#' Find and return the vector corresponding to the items of the object of class `ObservationSet`
#'  or return the given vector.
#' 
#' @details
#' If `items` is a vector corresponding to a subset of items contained in the observations,
#'  it is returned.
#' 
#' If `items` is a character value equal to `"items"` or `"i"`, the vector of unique items contained
#'  in `object` is returned.
#' 
#' @param object S4 object of class `ObservationSet`.
#' @param items Vector of items or one of the following character value: `"items"`, `"i"`.
#' @return Vector of items corresponding to the arguments.
#' 
#' @author Gauthier Magnin
#' @seealso
#' Method for signature `SpectralAnalyzer`: 
#' [`get_items,SpectralAnalyzer`][get_items,SpectralAnalyzer-method].
#' 
#' @aliases get_items get_items,ObservationSet
#' @md
#' @keywords internal
setMethod(f = "get_items",
          signature = "ObservationSet",
          definition =
function(object, items) {
  
  # Valeur spécifique faisant référence à l'intégralité des items
  if (length(items) == 1 && is.character(items) && (items == "items" || items == "i"))
    return(get_all_items(object))
  
  # Vecteur d'items (sous-ensemble de get_all_items(object))
  if (all(items %in% get_all_items(object)))
    return(items)
  
  stop("items must be \"items\" or a subset of items contained in object.")
})


