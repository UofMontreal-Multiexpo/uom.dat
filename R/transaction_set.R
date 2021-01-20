#' @include graphics_helper.R list_manager.R utils.R
NULL


#### Class definition and constructor ####

# Creation of a class corresponding to 2 types for allowing NA
setClassUnion("characterOrNA", c("character", "logical"))


#' Transaction Set
#' 
#' S4 object class for representing transactions.
#'  A transaction is defined as an itemset associated with any other data.
#' 
#' @details
#' Since `year_key` is not required, transactions do not need to contain temporal data. In such a case,
#'  `year_key` is `NA`.
#' 
#' @slot data List of transactions containing items and any additional data.
#'  Each transaction is itself a list. All transactions share the same element names (corresponding to
#'  the names of the various data).
#' @slot names Names of the elements present in each transaction (i.e. names of the elements contained in
#'  the list corresponding to a transaction).
#' @slot item_key Name of the element containing the items of a transaction.
#' @slot year_key Name of the element containing the year in which a transaction was made.
#' 
#' @author Gauthier Magnin
#' @seealso
#' The `TransactionSet` constructor: [`transaction.set`].
#' 
#' An example object of class `TransactionSet`: [`TS_instance`].
#' 
#' @aliases TransactionSet
#' @md
#' @export
setClass(Class = "TransactionSet",
         slots = c(
           data = "list",
           names = "character",
           item_key = "character",
           year_key = "characterOrNA"
         ))

# Validity
setValidity(Class = "TransactionSet",
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
          signature = "TransactionSet",
          definition = function(.Object, data, names, item_key, year_key) {
            
            .Object@data = data
            .Object@names = if (missing(names) || is.null(names)) names(data[[1]]) else names
            .Object@item_key = item_key
            .Object@year_key = year_key
            
            methods::validObject(.Object)
            return(.Object)
          })


#' Transaction Set constructor
#' 
#' Create and initialize an S4 object of class `TransactionSet`.
#' 
#' @details
#' Transactions do not necessarily require temporal data since `year_key` is not required.
#' 
#' If names are not specified using the argument `names`, they are automatically extracted from
#'  `data`. Specifying `names` allows to create `TransactionSet` (or subset of existing `TransactionSet`)
#'  without any transaction.
#' 
#' @param data List of transactions containing items and any additional data.
#'  Each transaction is itself a list. All transactions must share the same element names.
#' @param item_key Name of the element containing the items of a transaction.
#' @param year_key Name of the element containing the year in which a transaction was made.
#' @param names Names of the elements present in each transaction (i.e. names of the elements contained in
#'  the list corresponding to a transaction).
#' @return New object of class `TransactionSet`.
#' 
#' @author Gauthier Magnin
#' @seealso
#' The class: [`TransactionSet`].
#' 
#' @examples
#' ## Creating a list of transactions then a TransactionSet
#' trx <- list(T1 = list(items = "A",
#'                       date = 2018,
#'                       labels = c("l1", "l2")),
#'             
#'             T2 = list(items = c("A", "B", "D"),
#'                       date = 2018,
#'                       labels = "l3"),
#'             
#'             T3 = list(items = c("B", "C", "D"),
#'                       date = 2019,
#'                       labels = c("l2", "l3")),
#'             
#'             T4 = list(items = c("A", "B", "C", "D"),
#'                       date = 2020,
#'                       labels = c("l1", "l2", "l3"))
#'             )
#' trx_object <- transaction.set(trx, item_key = "items", year_key = "date")
#' print(trx_object)
#' 
#' @md
#' @export
transaction.set = function(data, item_key, year_key = NA, names = NULL) {
  return(methods::new(Class = "TransactionSet",
                      data = data, names = names, item_key = item_key, year_key = year_key))
}



#### Methods print, show, plot, summary, length ####

# print: display in console
setMethod(f = "print",
          signature = "TransactionSet",
          definition = function(x, ...) {
            cat("TransactionSet\n",
                "  data:     ", length(x), pluralize(" transaction", x), "\n",
                "  names:    ", paste0("\"", x@names, "\"", collapse = ", "), "\n",
                "  item_key: \"", x@item_key, "\"\n",
                "  year_key: ", if (is.na(x@year_key)) NA else paste0("\"", x@year_key, "\""),
                sep = "")
          })

# show: short display in console
setMethod(f = "show",
          signature = "TransactionSet",
          definition = function(object) {
            cat("TransactionSet\n ",
                length(object), pluralize("transaction", object), "\n ",
                "Slots:", paste(methods::slotNames("TransactionSet"), collapse = ", "))
          })

# length: object length
setMethod(f = "length",
          signature = "TransactionSet",
          definition = function(x) {
            return(length(x@data))
          })



#### Selector, mutator ####

#' Extract or replace parts of an object of class TransactionSet
#' 
#' General selector and mutator to access the attributes of an object of class `TransactionSet`.
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
#' TS_instance["data"]
#' 
#' TS_instance["item_key"]
#' TS_instance["CODE"]
#' 
#' TS_instance[3]
#' TS_instance[3:5]
#' 
#' @aliases [,TransactionSet-method
#' @md
#' @export
setMethod(f = "[",
          signature = "TransactionSet",
          definition = function(x, i, j, drop) {
            
            # Accès à un sous-ensemble de transactions
            if (is.numeric(i)) {
              if (length(i) == 1) return(x@data[[i]])
              return(x@data[i])
            }
            
            # Accès à un attribut
            if (i %in% slotNames("TransactionSet"))
              return(eval(parse(text = paste0("x@", i))))
            
            # Accès à un sous-élément de la liste de transactions
            if (i %in% x@names)
              return(lapply(x@data, "[[", i))
            
            stop("Unknown attribute.")
          })

#' @rdname sub-TransactionSet-ANY-ANY-ANY-method
#' 
#' @details
#' Replacing the attribute `names` renames the elements containing in each transaction if `value` is the
#'  same length as `names`. In this case, the attributes `item_key` and `year_key` are updated (unless
#'  `year_key` is `NA`).
#'  
#' Replacing the attribute `names` removes missing elements of `value` in each transaction if it is
#'  smaller than `names`.
#' 
#' @examples
#' TS_instance["year_key"] <- NA
#' TS_instance[3] <- list(CODE = 1,
#'                        YEAR = 2000,
#'                        JOB.TITLE = 45454545,
#'                        JOB.TASK = "A3000",
#'                        SAMPLE.ID = c(1, 3, 5, 8))
#' TS_instance["names"] <- c("CODE", "YEAR")
#' 
#' @aliases [<-,TransactionSet-method
#' @md
#' @export
setReplaceMethod(f = "[",
                 signature = "TransactionSet",
                 definition = function(x, i, j, value) {
                   
                   if (is.numeric(i)) {
                     if (length(i) == 1) x@data[[i]] = value
                     else x@data[i] = value
                   }
                   else {
                     if (!is.element(i, slotNames("TransactionSet")))
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



#### Type conversions ####

# Turn a TransactionSet into a data.frame.
# Parameter `stringAsFactors` is FALSE.
setAs(from = "TransactionSet",
      to = "data.frame",
      def = function(from) {
        
        # Définition d'une colonne temporaire pour pouvoir définir le nombre de lignes
        df = data.frame(tmp_col = numeric(length(from)), stringsAsFactors = FALSE)
        if (!is.null(names(from@data))) rownames(df) = names(from@data)
        df$tmp_col = NULL
        
        for (var_name in from@names) {
          # Vecteur si une seule valeur par transaction ; liste sinon
          if (all(sapply(from[var_name], length) == 1))
            df[[var_name]] = unname(unlist(from[var_name]))
          else
            df[[var_name]] = unname(lapply(from[var_name], c))
        }
        return(df)
      })


# Turn a TransactionSet into a set of [`transactions`][arules::transactions-class] from the
# package [`arules`].
# Only the items of the transactions are considered. Other data are ignored.
setAs(from = "TransactionSet",
      to = "transactions",
      def = function(from) {
        
        # Liste des items retrouvés pour chaque transaction et vecteurs des identifiants des items
        data = sapply(sapply(from@data, "[", from@item_key), as.character)
        labels = as.character(get_all_items(from))
        
        # Transformation en objet arules::itemMatrix puis en objet arules::transaction :
        # une ligne par transaction, une colonne par item
        return(methods::as(arules::encode(data, labels), "transactions"))
      })



#### Declaration of the methods ####

# Calls to setGeneric without the "def" argument correpond to generics already existing
# in R base packages (base, stats, etc.).

# Methods for processing on transactions
setGeneric(name = "subset")
setGeneric(name = "reorder")

# Methods for search in transactions
setGeneric(name = "get_all_items",       def = function(object){ standardGeneric("get_all_items") })
setGeneric(name = "get_itemsets",        def = function(object){ standardGeneric("get_itemsets") })
setGeneric(name = "get_items_from_info", def = function(object, ...){ standardGeneric("get_items_from_info") })
setGeneric(name = "get_info_from_items", def = function(object, ...){ standardGeneric("get_info_from_items") })

# Methods for search for transactions
setGeneric(name = "get_complex_trx",     def = function(object){ standardGeneric("get_complex_trx") })
setGeneric(name = "get_simple_trx",      def = function(object){ standardGeneric("get_simple_trx") })
setGeneric(name = "get_trx_from_items",  def = function(object, ...){ standardGeneric("get_trx_from_items") })
setGeneric(name = "get_trx_from_info",   def = function(object, ...){ standardGeneric("get_trx_from_info") })

# Methods for computations on transactions
setGeneric(name = "complexity_ratio",     def = function(object, ...){ standardGeneric("complexity_ratio") })
setGeneric(name = "complexity_index",     def = function(object, ...){ standardGeneric("complexity_index") })
setGeneric(name = "co_occurrence_matrix", def = function(object, ...){ standardGeneric("co_occurrence_matrix") })

# Methods for plotting charts
setGeneric(name = "itemset_chart",         def = function(object, ...){ standardGeneric("itemset_chart") })
setGeneric(name = "prepare_itemset_chart", def = function(object, ...){ standardGeneric("prepare_itemset_chart") })
setGeneric(name = "co_occurrence_chart",   def = function(object, ...){ standardGeneric("co_occurrence_chart") })

# Other specific methods
setGeneric(name = "has_temporal_data",   def = function(object){ standardGeneric("has_temporal_data") })
setGeneric(name = "get_items",           def = function(object, ...){ standardGeneric("get_items") })



#### Methods for processing on transactions ####

#' Subsetting Transaction Set
#' 
#' Return an `TransactionSet` in which transactions are a subset of the ones of another `TransactionSet`.
#' 
#' @details
#' If the transactions from `x` are not named and `keep_names = TRUE`, the transactions of the resulting
#'  object are named according to the indexes of the initial transactions.
#' 
#' @param x Object of class `TransactionSet` to be subsetted.
#' @param indexes Numeric vector indicating which transactions from `x["data"]` to keep,
#'  or logical vector indicating for each transaction whether to keep it.
#' @param keep_names If `TRUE`, transactions of the returned object keep the names from the initial
#'  object `x`. If `FALSE`, they are not named.
#' @return S4 object of class `TransactionSet` having a subset of transactions from `x` (i.e. a subset
#'  of the attribute `data` from `x`).
#' 
#' @author Gauthier Magnin
#' @examples
#' subset(TS_instance, c(1, 7, 8, 9))
#' subset(TS_instance, c(TRUE, rep(FALSE, 12), TRUE))
#' 
#' @aliases subset
#' @md
#' @export
setMethod(f = "subset",
          signature = "TransactionSet",
          definition =
function(x, indexes, keep_names = TRUE) {
  
  if (is.logical(indexes)) indexes = which(indexes)
  
  if (keep_names) {
    if (!is.null(names(x@data))) {
      return(transaction.set(x@data[indexes],
                             x@item_key, x@year_key, x@names))
    }
    return(transaction.set(stats::setNames(x@data[indexes], indexes),
                           x@item_key, x@year_key, x@names))
  }
  # if (!keep_names)
  return(transaction.set(unname(x@data[indexes]),
                         x@item_key, x@year_key, x@names))
})


#' Reorder Transaction Set
#' 
#' Return a copy of an `TransactionSet` in which the transactions are in another order.
#' 
#' @param x S4 object of class `TransactionSet`.
#' @param permutation Numeric or character vector. Permutation to use to rearrange the transactions.
#' @return S4 object of class `TransactionSet` containing the transactions of `x` in the order
#'  defined by `permutation`.
#' 
#' @author Gauthier Magnin
#' @examples
#' reorder(TS_instance, c(2, 1, 3:14))
#' reorder(TS_instance, names(TS_instance["data"])[c(2, 1, 3:14)])
#' reorder(TS_instance, 14:1)
#' 
#' @aliases reorder
#' @md
#' @export
setMethod(f = "reorder",
          signature = "TransactionSet",
          definition =
function(x, permutation) {
  return(subset(x, permutation, keep_names = is_named(x@data)[1]))
})



#### Methods for search in transactions ####

#' Search all items
#' 
#' Extract all the items contained in the transactions.
#' 
#' @param object S4 object of class `TransactionSet`.
#' @return Vector of all unique items.
#' 
#' @author Gauthier Magnin
#' @seealso [`get_itemsets`].
#' 
#' @examples
#' get_all_items(TS_instance)
#' 
#' @aliases get_all_items
#' @md
#' @export
setMethod(f = "get_all_items",
          signature = "TransactionSet",
          definition =
function(object) {
  return(sort(unique(unlist(sapply(object@data, "[", object@item_key)))))
})


#' Extract the item sets
#' 
#' Extract the item sets corresponding to the transactions.
#' 
#' @param object S4 object of class `TransactionSet`.
#' @return List of the item sets of the transactions.
#' 
#' @author Gauthier Magnin
#' @seealso [`get_all_items`].
#' 
#' @examples
#' get_itemsets(TS_instance)
#' 
#' @aliases get_itemsets
#' @md
#' @export
setMethod(f = "get_itemsets",
          signature = "TransactionSet",
          definition =
function(object) {
  return(lapply(object@data, "[[", object@item_key))
})


#' Search for items by specific information
#' 
#' Retrieve the items associated with transactions whose information matches to one or more sought
#'  information.
#' 
#' @param object S4 object of class `TransactionSet`.
#' @param info Named list of sought information. Element names must refer to the names of variables
#'  contained in the transactions and values must correspond to the sought values for these variables.
#' @param presence Information presence condition for an item to be extracted from a transaction.
#'  One of `"all"`, `"any"`.
#'  \describe{
#'   \item{`"all"`}{All the sought information must be part of a transaction for its items to be
#'                  extracted.}
#'   \item{`"any"`}{At least one of the sought information must be part of a transaction for its items
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
#' get_items_from_info(TS_instance, info = list(JOB.TITLE = 44132017))
#' get_items_from_info(TS_instance, info = list(JOB.TITLE = 44132017),
#'                                  additional = "SAMPLE.ID")
#' 
#' @aliases get_items_from_info
#' @md
#' @export
setMethod(f = "get_items_from_info",
          signature = "TransactionSet",
          definition =
function(object, info, presence = "all", additional = NULL) {
  
  # Transactions correspondant aux critères
  trx = get_trx_from_info(object, info, presence = presence)
  
  # Vecteur des items
  if (is.null(additional)) {
    items = unique(unlist(lapply(trx@data, "[", object@item_key)))
    return(sort(items))
  }
  
  # Liste des items et données demandées
  items = c(list(CODE = sort(unique(unlist(lapply(trx@data, "[[", object@item_key))))),
            lapply(additional, function(a) sort(unique(unlist(lapply(trx@data, "[[", a))))))
  return(stats::setNames(items, c(object@item_key, additional)))
})


#' Search for information by item
#' 
#' Retrieve information associated with transactions that contain a set of sought items.
#' 
#' @param object S4 object of class `TransactionSet`.
#' @param items Sought items.
#' @param info_names Names of information to extract from transactions.
#' @param presence Item presence condition for information to be extracted from a transaction.
#' One of `"all"`, `"any"`, `"exact"`.
#'  \describe{
#'   \item{`"all"`}{All the sought items must be part of a transaction for its information to be
#'                  extracted.}
#'   \item{`"any"`}{At least one of the sought items must be part of a transaction for its information
#'                  to be extracted.}
#'    \item{`"exact"`}{The item set contained in a transaction must be exactly the same as the sought
#'                     item set for this transaction to be extracted.}
#'  }
#' @return Vector or list of information corresponding to the search.
#'  Vector if only one type of information is to be extracted. List otherwise.
#' 
#' @author Gauthier Magnin
#' @seealso [`get_items_from_info`], [`get_all_items`].
#' 
#' @examples
#' get_info_from_items(TS_instance, items = 3146,
#'                                  info_names = c("JOB.TITLE", "JOB.TASK"))
#' 
#' @aliases get_info_from_items
#' @md
#' @export
setMethod(f = "get_info_from_items",
          signature = "TransactionSet",
          definition =
function(object, items, info_names, presence = "all") {
  
  # Transactions contenant le ou les items recherchés
  trx = get_trx_from_items(object, items, presence)
  
  # Vecteur de la variable demandée
  if (length(info_names) == 1) return(sort(unique(unlist(sapply(trx@data, "[", info_names)))))
  
  # Liste des variables demandées
  to_return = lapply(info_names, function(var) sort(unique(unlist(lapply(trx@data, "[[", var)))))
  return(stats::setNames(to_return, info_names))
})



#### Methods for search for transactions ####

#' Search for complex transactions
#' 
#' Extract the transactions containing more than one item.
#' 
#' @param object S4 object of class `TransactionSet`.
#' @return S4 object of class `TransactionSet` containing the subset of transactions that contain more
#'  than one item.
#' 
#' @author Gauthier Magnin
#' @seealso [`get_simple_trx`], [`get_trx_from_items`], [`get_trx_from_info`].
#' 
#' @examples
#' get_complex_trx(TS_instance)
#' 
#' @aliases get_complex_trx
#' @md
#' @export
setMethod(f = "get_complex_trx",
          signature = "TransactionSet",
          definition =
function(object) {
  index = sapply(get_itemsets(object),
                 function (x) length(x) > 1)
  return(subset(object, index))
})


#' Search for simple transactions
#' 
#' Extract the transactions containing exactly one item.
#' 
#' @param object S4 object of class `TransactionSet`.
#' @return S4 object of class `TransactionSet` containing the subset of transactions that contain exactly
#'  one item.
#' 
#' @author Gauthier Magnin
#' @seealso [`get_complex_trx`], [`get_trx_from_items`], [`get_trx_from_info`].
#' 
#' @examples
#' get_simple_trx(TS_instance)
#' 
#' @aliases get_simple_trx
#' @md
#' @export
setMethod(f = "get_simple_trx",
          signature = "TransactionSet",
          definition =
function(object) {
  index = sapply(get_itemsets(object),
                 function (x) length(x) == 1)
  return(subset(object, index))
})


#' Search for transactions by item
#' 
#' Extract the transactions containing one or more sought items.
#' 
#' @param object S4 object of class `TransactionSet`.
#' @param items Sought items.
#' @param presence Item presence condition for a transaction to be extracted.
#'  One of `"all"`, `"any"`, `"exact"`.
#'  \describe{
#'    \item{`"all"`}{All the sought items must be part of a transaction for this transaction to be
#'                   extracted.}
#'    \item{`"any"`}{At least one of the sought items must be part of a transaction for this transaction
#'                   to be extracted.}
#'    \item{`"exact"`}{The item set contained in a transaction must be exactly the same as the sought
#'                     item set for this transaction to be extracted.}
#'  }
#' @return S4 object of class `TransactionSet` containing the subset of transactions that match the search
#'  criteria.
#' 
#' @author Gauthier Magnin
#' @seealso [`get_complex_trx`], [`get_simple_trx`], [`get_trx_from_info`].
#' 
#' @examples
#' get_trx_from_items(TS_instance, items = c(25, 192), presence = "all")
#' get_trx_from_items(TS_instance, items = c(25, 192), presence = "any")
#' get_trx_from_items(TS_instance, items = c(25, 192), presence = "exact")
#' 
#' @aliases get_trx_from_items
#' @md
#' @export
setMethod(f = "get_trx_from_items",
          signature = "TransactionSet",
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


#' Search for transactions by specific information
#' 
#' Extract the transactions whose information matches to one or more sought information.
#' 
#' @param object S4 object of class `TransactionSet`.
#' @param info Named list of sought information. Element names must refer to the names of variables
#'  contained in the transactions and values must correspond to the sought values for these variables.
#' @param presence Information presence condition for a transaction to be extracted.
#'  One of `"all"`, `"any"`.
#'  \describe{
#'   \item{`"all"`}{All the sought information must be part of a transaction for this transaction to
#'                  be extracted.}
#'   \item{`"any"`}{At least one of the sought information must be part of a transaction for this
#'                  transaction to be extracted.}
#'  }
#' @return `TransactionSet` containing the subset of transactions that match the search criteria.
#' 
#' @author Gauthier Magnin
#' @seealso [`get_complex_trx`], [`get_simple_trx`], [`get_trx_from_items`].
#' 
#' @examples
#' get_trx_from_info(TS_instance, info = list(JOB.TITLE = 44132001,
#'                                            JOB.TASK = "A8310"))
#' 
#' @aliases get_trx_from_info
#' @md
#' @export
setMethod(f = "get_trx_from_info",
          signature = "TransactionSet",
          definition =
function(object, info, presence = "all") {
  
  check_param(presence, values = c("all", "any"))
  func = if (presence == "all") all else any
  
  if (length(info) == 0) return(object)
  
  # Vérification de la correspondance de chaque argument dans chaque transaction
  correspondence = sapply(info,
                          function(arg) {
                            lapply(sapply(object@data, "[[", names(info)[parent.frame()$i[]]),
                                   function(o) arg %in% o)
                          })
  
  # Indices des transactions correspondant aux critères
  if (length(info) == 1 && length(info[[1]]) == 1) {
    index = unlist(correspondence)
  } else {
    # Unlist indépendant pour chaque argument, nécessaire car certaines transactions regroupent
    # plusieurs valeurs pour une même variable
    index = apply(t(apply(correspondence, 1, unlist)), 1, func)
  }
  
  return(subset(object, index))
})



#### Methods for computations on transactions ####

#' Complexity Ratio, by item
#' 
#' For each item, compute the ratio between the number of complex transactions containing the item and
#'  the total number of transactions containing the item. In other words, compute the complement of the
#'  ratio between the number of times the item appears alone and the number of times the item appears.
#' 
#' @param object S4 object of class `TransactionSet`.
#' @param items Items for which to compute the complexity ratio. The default `NULL` means to compute
#'  it for each existing item.
#' @return Named vector of complexity ratios: for each item, the proportion of complex transactions
#'  containing it among all transactions containing it.
#' 
#' @author Gauthier Magnin
#' @seealso [`complexity_index`], [`co_occurrence_matrix`],
#'          [`get_all_items`], [`get_trx_from_items`], [`get_complex_trx`], [`get_simple_trx`].
#' 
#' @examples
#' complexity_ratio(TS_instance)
#' complexity_ratio(TS_instance, items = c(25, 148, 3146))
#' 
#' @aliases complexity_ratio
#' @md
#' @export
setMethod(f = "complexity_ratio",
          signature = "TransactionSet",
          definition =
function(object, items = NULL) {
  
  if (is.null(items)) items = get_all_items(object)
  
  # Pour chaque item, nombre de transactions complexes contenant l'item / nombre de trx contenant l'item
  return(sapply(items,
                function(item) {
                  trx_item = get_trx_from_items(object, item)
                  return(stats::setNames(length(get_complex_trx(trx_item)) / length(trx_item), item))
                }))
})


#' Complexity Index, by item
#' 
#' For each item, count the number of complex transactions containing the item.
#' 
#' @param object S4 object of class `TransactionSet`.
#' @param items Items for which to calculate the complexity index. The default `NULL` means to calculate
#'  it for each existing item.
#' @return Named vector of complexity indexes: for each item, the number of complex transactions
#'  containing it.
#' 
#' @author Gauthier Magnin
#' @seealso [`complexity_ratio`], [`co_occurrence_matrix`],
#'          [`get_all_items`], [`get_trx_from_items`], [`get_complex_trx`], [`get_simple_trx`].
#' 
#' @examples
#' complexity_index(TS_instance)
#' complexity_index(TS_instance, items = c(19, 25, 148))
#' 
#' @aliases complexity_index
#' @md
#' @export
setMethod(f = "complexity_index",
          signature = "TransactionSet",
          definition =
function(object, items = NULL) {
  
  if (is.null(items)) items = get_all_items(object)
  
  # Pour chaque item, nombre de transactions complexes parmi les transactions contenant l'item
  return(sapply(items,
                function(item)
                  stats::setNames(length(
                    get_complex_trx(get_trx_from_items(object, item))
                  ), item)))
})


#' Co-occurrence matrix
#' 
#' Build the co-occurrence matrix: the matrix of the number of transactions containing a specific pair of
#'  items, for each possible pair. The diagonal is the number of transactions containing each specific
#'  item.
#' 
#' @param object S4 object of class `TransactionSet`.
#' @param items Items for which to count co-occurrences between pairs. The default `NULL` means to count
#'  them considering each existing item.
#' @return Co-occurrence matrix between each pair of items.
#' 
#' @author Gauthier Magnin
#' @seealso [`complexity_ratio`], [`complexity_index`],
#'          [`get_all_items`], [`get_trx_from_items`], [`get_complex_trx`], [`get_simple_trx`].
#' 
#' @examples
#' co_occurrence_matrix(TS_instance)
#' co_occurrence_matrix(TS_instance, items = c(19, 25, 148, 3146))
#' 
#' @aliases co_occurrence_matrix
#' @md
#' @export
setMethod(f = "co_occurrence_matrix",
          signature = "TransactionSet",
          definition =
function(object, items = NULL) {
  
  if (is.null(items)) items = get_all_items(object)
  
  # Pour chaque pair d'items, recherche du nombre de transactions dans lesquelles la paire apparaît
  pairs = combn(items, 2)
  co = apply(pairs, 2, function(pair) length(get_trx_from_items(object, pair)))
  
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

#' Itemset chart, for TransactionSet
#' 
#' Plot a chart of the transaction itemsets. It can be automatically saved as a PDF file.
#' 
#' @details
#' Itemsets are sorted according to their lengths then (for the same length) are taken according to
#'  the initial order in `object`. Items are sorted alphanumerically.
#' 
#' If the argument `name` is not `NULL`, the chart is plotted in a PDF file of A4 landscape paper size.
#'  If it is `NULL`, the chart is plotted in the active device.
#' 
#' @inheritParams prepare_itemset_chart,TransactionSet-method
#' @param jitter If `FALSE`, non-equivalent itemsets of length \eqn{1} are aligned vertically.
#'  If `TRUE`, they are spread over several vertical lines to avoid overplotting while taking as little
#'  space as possible. If `NA`, they are plotted one after the other.
#'  Ignored if `length_one` is `FALSE`.
#' @param under,over Text to display on the chart under and over the itemsets.
#'  Identifiers (`"ID"`) or one of the elements of the transactions (i.e. one of the values of
#'  `object["names"]`).
#' @param title Chart title.
#' @param path Path of the directory in which to save the chart as a PDF file. Default is the working
#'  directory.
#' @param name Name of the PDF file in which to save the chart. To be ignored to plot the chart in the
#'  active device.
#' @return S4 object of class `TransactionSet` containing the transactions represented on the chart.
#' 
#' @author Delphine Bosson-Rieutort, Gauthier Magnin
#' @references Bosson-Rieutort D, Sarazin P, Bicout DJ, Ho V, Lavoué J (2020).
#'             Occupational Co-exposures to Multiple Chemical Agents from Workplace Measurements by the US Occupational Safety and Health Administration.
#'             *Annals of Work Exposures and Health*, Volume 64, Issue 4, May 2020, Pages 402–415.
#'             <https://doi.org/10.1093/annweh/wxaa008>.
#' @seealso [`get_itemsets`], [`get_all_items`].
#' 
#' Method for signature `SpectralAnalyzer`:
#' [`itemset_chart,SpectralAnalyzer`][itemset_chart,SpectralAnalyzer-method].
#' 
#' @examples
#' itemset_chart(TS_instance)
#' itemset_chart(TS_instance, identifiers = "new", under = "YEAR", over = "ID")
#' 
#' itemset_chart(TS_instance, path = getwd(), name = "trx_itemsets")
#' 
#' @aliases itemset_chart itemset_chart,TransactionSet
#' @md
#' @export
setMethod(f = "itemset_chart",
          signature = "TransactionSet",
          definition =
function(object, identifiers = "original",
         length_one = TRUE, jitter = TRUE,
         under = "ID", over = NULL,
         title = "Transaction itemsets", path = NULL, name = NULL) {
  
  # Validation des paramètres
  check_param(identifiers, values = c("original", "new"))
  
  # Préparation des variables pour la fonction de traçage du graphique
  vars = prepare_itemset_chart(object, identifiers, length_one, under, over)
  
  # Traçage du graphique (dans le device actif ou dans un fichier PDF)
  if (!is.null(name)) grDevices::pdf(paste0(turn_into_path(path), check_extension(name, "pdf")),
                                     14, 10, paper = "a4r", pointsize = 11)
  plot_itemset_chart(vars$itemsets, vars$items, category = NULL,
                     jitter, vars$under, vars$over, over_legend = NULL, title)
  if (!is.null(name)) grDevices::dev.off()
  
  # Transactions tracées
  return(vars$transactions)
})


#' Prepare data for plotting itemset chart
#' 
#' Prepare transactions, itemsets, items and text for itemset chart.
#' 
#' @param object S4 object of class `TransactionSet`.
#' @param identifiers Which IDs to use to identify the transactions on the chart and in the
#'  return object. One of `"original"`, `"new"`.
#'  \describe{
#'    \item{`"original"`}{Use of the original identifiers.}
#'    \item{`"new"`}{Use of new identifiers based on sorting (see 'Details' section to learn more
#'                   about the sort that is performed).}
#'  }
#' @param length_one If `FALSE`, itemsets of length \eqn{1} are not plotted. If `TRUE`, all itemsets
#'  are plotted.
#' @param under,over Text to display on the chart under and over the itemsets.
#'  Identifiers (`"ID"`) or one of the elements of the transactions (i.e. one of the values of
#'  `object["names"]`).
#' @return 
#'  \describe{
#'    \item{`transactions`}{Subset of `object` to plot.}
#'    \item{`itemsets`}{List of itemsets to plot.}
#'    \item{`items`}{Data frame containing the items contained in the itemsets to plot, duplicated in two
#'                   colonnes: item and label.}
#'    \item{`under`}{Text to display under the itemsets.}
#'    \item{`over`}{Text to display over the itemsets.}
#'  }
#' 
#' @seealso [`itemset_chart,TransactionSet`][itemset_chart,TransactionSet-method].
#' @author Gauthier Magnin
#' 
#' @aliases prepare_itemset_chart
#' @md
#' @keywords internal
setMethod(f = "prepare_itemset_chart",
          signature = "TransactionSet",
          definition =
function(object, identifiers, length_one, under, over) {
  
  # Itemsets de taille > 1 inclus ou exclus
  to_plot = if (length_one) object else get_complex_trx(object)
  
  # Si pas de nom et affichage des identifiants d'origine : attribution de noms correspondant aux index 
  # (nécessaire avant le tri par taille)
  if (identifiers == "original" && !is_named(to_plot@data)) names(to_plot@data) = seq_along(to_plot@data)
  to_plot = reorder(to_plot, order(sapply(get_itemsets(to_plot), length)))
  if (identifiers == "new") names(to_plot@data) = seq_along(to_plot@data)
  
  # Conversion en data frame
  trx_df = as(to_plot, "data.frame")
  trx_df$ID = rownames(trx_df)
  
  # Texte à afficher
  under_text = if (is.null(under)) NULL else trx_df[, under]
  over_text = if (is.null(over)) NULL else trx_df[, over]
  if (is.list(under_text)) under_text = turn_list_into_char(under_text)
  if (is.list(over_text)) over_text = turn_list_into_char(over_text)
  
  return(list(transactions = to_plot,
              itemsets = trx_df[, object@item_key],
              items = data.frame(item = get_all_items(to_plot),
                                 label = get_all_items(to_plot)),
              under = under_text,
              over = over_text))
})


#' Co-occurrence chart, for TransactionSet
#' 
#' Plot a graph in which vertices are items and edges are their co-occurences in transactions (i.e. for
#'  each pair of items, the number of transactions containing it).
#' 
#' @details
#' The chart being plotted with the packages `ggraph` and `ggplot2`, it can be modified or completed
#'  afterwards using [`ggplot2::last_plot`] or the returned object.
#' 
#' Items are ordered according to the order they are given. If one of the special values is given,
#'  they are ordered alphanumerically.
#' 
#' @param object S4 object of class `TransactionSet`.
#' @param items Items for which to count co-occurrences between pairs and to plot on the graph.\cr
#'  `"items"` and `"i"` are special values specifying to use all existing items.
#' @param min_occ Minimum number of co-occurrences to consider to plot a link between two items.
#' @param max_occ Maximum number of co-occurrences to consider to plot a link between two items.
#' @inheritParams plot_heb_chart
#' @return Graph created with the packages `ggraph` and `ggplot2`.
#' 
#' @author Gauthier Magnin
#' @seealso [`co_occurrence_matrix`].
#' 
#' Method for signature `SpectralAnalyzer`: 
#' [`co_occurrence_chart,SpectralAnalyzer`][co_occurrence_chart,SpectralAnalyzer-method].
#' 
#' @examples
#' co_occurrence_chart(TS_instance, get_all_items(TS_instance))
#' co_occurrence_chart(TS_instance, "items") +
#'   ggplot2::expand_limits(x = c(-1.2, 1.2), y = c(-1.2, 1.2))
#' co_occurrence_chart(TS_instance, "items",
#'                     min_occ = 2, palette = "OrRd")
#' co_occurrence_chart(TS_instance, items = c(25, 27, 49, 87, 148, 192, 252, 328))
#' 
#' @aliases co_occurrence_chart co_occurrence_chart,TransactionSet
#' @md
#' @export
setMethod(f = "co_occurrence_chart",
          signature = "TransactionSet",
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
  
  return(plot_heb_chart(hierarchy, vertices, connections, limits = c(1, max(co_occ$Freq)),
                        vertex_size = vertex_size, vertex_alpha = vertex_alpha, vertex_margin = vertex_margin,
                        label_size = label_size, label_margin = label_margin,
                        edge_tension = edge_tension, edge_alpha = edge_alpha,
                        palette = palette, palette_direction = palette_direction))
})



#### Other specific methods ####

#' Check if a TransactionSet contains temporal data
#' 
#' Equivalent to checking if the attribute `year_key` is not `NA`, but more explicit.
#' 
#' @param object S4 object of class `TransactionSet`.
#' @return `TRUE` or `FALSE` whether `object` contains temporal data.
#' 
#' @author Gauthier Magnin
#' 
#' @examples
#' has_temporal_data(TS_instance)
#' 
#' TS_instance["year_key"] <- NA
#' has_temporal_data(TS_instance)
#' 
#' @aliases has_temporal_data
#' @md
#' @keywords internal
setMethod(f = "has_temporal_data",
          signature = "TransactionSet",
          definition =
function(object) {
  return(!is.na(object@year_key))
})


#' Get items
#' 
#' Find and return the vector corresponding to the items of the object of class `TransactionSet`
#'  or return the given vector.
#' 
#' @details
#' If `items` is a vector corresponding to a subset of items contained in the transactions,
#'  it is returned.
#' 
#' If `items` is a character value equal to `"items"` or `"i"`, the vector of unique items contained
#'  in `object` is returned.
#' 
#' @param object S4 object of class `TransactionSet`.
#' @param items Vector of items or one of the following character value: `"items"`, `"i"`.
#' @return Vector of items corresponding to the arguments.
#' 
#' @author Gauthier Magnin
#' @seealso
#' Method for signature `SpectralAnalyzer`: 
#' [`get_items,SpectralAnalyzer`][get_items,SpectralAnalyzer-method].
#' 
#' @aliases get_items get_items,TransactionSet
#' @md
#' @keywords internal
setMethod(f = "get_items",
          signature = "TransactionSet",
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


