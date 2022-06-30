#' @include graphics_helper.R list_manager.R transaction_maker.R transaction_set.R utils.R
NULL


# Debug mode activation status
DEBUG_MODE_ = FALSE
# Defines the step up to which to perform the analysis before stopping the process
UP_TO_STEP_ = Inf



#### Class attributes ####

#' STATUS_PERSISTENT
#' @description Reference value for pattern dynamic status: persistent.
#' @keywords internal
STATUS_PERSISTENT = "Persistent"
#' STATUS_DECLINING
#' @description Reference value for pattern dynamic status: declining.
#' @keywords internal
STATUS_DECLINING = "Declining"
#' STATUS_EMERGENT
#' @description Reference value for pattern dynamic status: emergent.
#' @keywords internal
STATUS_EMERGENT = "Emergent"
#' STATUS_LATENT
#' @description Reference value for pattern dynamic status: latent.
#' @keywords internal
STATUS_LATENT = "Latent"

#' TRANSACTIONS
#' @description Reference value for naming entities: transactions.
#' @keywords internal
TRANSACTIONS = "transactions"
#' NODES
#' @description Reference value for naming entities: nodes.
#' @keywords internal
NODES = "nodes"
#' PATTERNS
#' @description Reference value for naming entities: patterns.
#' @keywords internal
PATTERNS = "patterns"
#' RULES
#' @description Reference value for naming entities: rules.
#' @keywords internal
RULES = "rules"

#' NODES_OR_PATTERNS
#' @description Reference value for defining possible entities: nodes or patterns.
#' @keywords internal
NODES_OR_PATTERNS = "np"
#' NODES_PATTERNS_OR_RULES
#' @description Reference value for defining possible entities: nodes, patterns or rules.
#' @keywords internal
NODES_PATTERNS_OR_RULES = "npr"
#' NODES_PATTERNS_OR_TRANSACTIONS
#' @description Reference value for defining possible entities: nodes, patterns or transactions.
#' @keywords internal
NODES_PATTERNS_OR_TRANSACTIONS = "npt"
#' ANY_ITEMSETS
#' @description Reference value for defining possible entities: any itemsets.
#' @keywords internal
ANY_ITEMSETS = "itemsets"

#' NODE_LINKS
#' @description Reference value for naming links between entities: links between nodes.
#' @keywords internal
NODE_LINKS = "node_links"
#' PATTERN_LINKS
#' @description Reference value for naming links between entities: links between patterns.
#' @keywords internal
PATTERN_LINKS = "pattern_links"



#### Class definition and constructor ####

#' Transaction Analyzer
#' 
#' S4 object class allowing transaction analysis.
#'  It consists of mining frequent itemsets from data, compute characteristics of these itemsets
#'  (specificity, dynamic status...), plot them on charts (spectrum, spectrosome...) and extract
#'  association rules.
#'  Items from itemsets can be associated with categories used in charts and computations.
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
#' @slot transactions S4 object of class \code{TransactionSet}: list of transactions containing the items
#'  corresponding to each one. It represents the dataset in which frequent itemsets are to be mined.
#' @slot items Set of codes identifying the items found in the transactions.
#' @slot items_categories Categories associated with the items. Each item is associated with one value
#'  for each category.
#' @slot categories_colors Colors associated with the values of the categories associated with the items.
#' @slot status_colors Colors associated with the values of the status the patterns can have.
#' @slot parameters List of parameters for pattern search and characterization:
#'  \describe{
#'    \item{\code{target}}{Type of patterns to mine during the analysis.}
#'    \item{\code{min_frequency}}{Minimum number of occurrences that a pattern must appear to be kept
#'                                when mining patterns.}
#'    \item{\code{min_length}}{Minimum number of items that a pattern must have to be kept when mining
#'                             patterns.}
#'    \item{\code{max_length}}{Maximum number of items that a pattern must have to be kept when mining
#'                             patterns.}
#'    \item{\code{status_limit}}{Time interval for which to characterize the status of the patterns in
#'                               relation to the total period of transactions (number of years).}
#'  }
#' @slot nodes Set of nodes (separate transactions considering only their items) and characteristics of
#'  these nodes. Data frame of 3 variables:
#'  \describe{
#'    \item{\code{node}}{Set of items composing the node.}
#'    \item{\code{length}}{Number of items composing the node.}
#'    \item{\code{frequency}}{Number of transactions for which the set of items matches exactly.}
#'  }
#' @slot nodes_per_year Number of occurrences of each node in the transactions, per year.
#' @slot n_links Set of weights of the links between the nodes. Adjacency matrix containing the number
#'  of items in common between each pair of nodes.
#' @slot node_links Set of links between the nodes and characteristics of these links.
#'  Isolated nodes (i.e. unrelated to any other node) appear at the bottom of the data structure.
#'  Data frame of 4 variables:
#'  \describe{
#'    \item{\code{endpoint.1}, \code{endpoint.2}}{Identifiers of two nodes from the attribute
#'                                                \code{nodes}.}
#'    \item{\code{items}}{Items in common between the two nodes.}
#'    \item{\code{weight}}{Number of items in common between the two nodes.}
#'  }
#' @slot nodes_patterns Set of associations between patterns and nodes determining whether a pattern
#'  is included in a node.
#' @slot patterns Set of mined patterns and characteristics of these patterns.
#'  Data frame of 8 variables:
#'  \describe{
#'    \item{\code{pattern}}{Set of items composing the pattern.}
#'    \item{\code{year}}{Year of appearance of the pattern among the transactions.}
#'    \item{\code{length}}{Number of items composing the pattern.}
#'    \item{\code{support}}{Proportion of transactions containing the set of items of the pattern.}
#'    \item{\code{frequency}}{Number of transactions containing the set of items of the pattern.}
#'    \item{\code{weight}}{Number of nodes containing the set of items of the pattern.}
#'    \item{\code{specificity}}{Specificity of the information conveyed by the pattern. It corresponds
#'      to the nature of a pattern of being specific of a particular combination or ubiquitous and
#'      allowing the formation of numerous combinations (with regard to the transactions).}
#'    \item{\code{status}}{Dynamic status of the pattern: persistent, declining, emergent or latent.}
#'  }
#' @slot patterns_per_year Number of occurrences of each pattern in the transactions, per year.
#' @slot p_links Set of weights of the links between the patterns. Adjacency matrix containing the number
#'  of items in common between each pair of patterns.
#' @slot pattern_links Set of links between the patterns and characteristics of these links.
#'  Isolated patterns (i.e. unrelated to any other pattern) appear at the bottom of the data structure.
#'  Data frame of 5 variables:
#'  \describe{
#'    \item{\code{endpoint.1}, \code{endpoint.2}}{Identifiers of two patterns from the attribute
#'                                                \code{patterns}.}
#'    \item{\code{items}}{Items in common between the two patterns.}
#'    \item{\code{weight}}{Number of items in common between the two patterns.}
#'    \item{\code{year}}{Year of appearance of the link between the two patterns.}
#'  }
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
#' @seealso
#' The \code{TransactionAnalyzer} constructor: \code{\link{transaction.analyzer}}.
#' 
#' An example object of class \code{TransactionAnalyzer}: \code{\link{TA_instance}}.
#' @aliases TransactionAnalyzer print,TransactionAnalyzer-method summary,TransactionAnalyzer-method
#' @export
setClass(Class = "TransactionAnalyzer",
         slots = c(
           transactions = "TransactionSet",
           items = "vector",
           items_categories = "data.frame",
           
           categories_colors = "list",
           status_colors = "vector",
           
           parameters = "list",
           
           nodes = "data.frame",
           nodes_per_year = "matrix",
           n_links = "matrix",
           node_links = "data.frame",
           
           nodes_patterns = "matrix",
           
           patterns = "data.frame",
           patterns_per_year = "matrix",
           p_links = "matrix",
           pattern_links = "data.frame"
         ))

# Validity
setValidity("TransactionAnalyzer", function(object) {
  
  # Values that will be used to check the validity
  parameters = c("target", "min_frequency", "min_length", "max_length", "status_limit")
  target = c("frequent itemsets", "closed frequent itemsets", "maximally frequent itemsets")
  status = c(STATUS_PERSISTENT, STATUS_DECLINING, STATUS_EMERGENT, STATUS_LATENT)
  
  # Errors that may be returned and booleans indicating which errors to actually return
  errors = c(
    E1  = "Item identification codes must not contain the characters \"/\" and \",\".",
    E2  = "Given item identification codes must contain all those existing in the given transactions.",
    E3  = "Transactions must contain itemsets.",
    E4  = "Transactions must contain temporal data.",
    E5  = paste0("parameters must contain elements ",
                 paste(parameters[1:4], collapse = ", "), " and ", parameters[5], "."),
    
    E6  = "target must be a character value.",
    E7  = "min_frequency must be a numeric value.",
    E8  = "min_length must be a numeric value.",
    E9  = "max_length must be a numeric value.",
    E10 = "status_limit must be a numeric value.",
    
    E11 = paste0("target must be one of \"", paste(target, collapse = "\", \""), "\"."),
    E12 = "min_frequency must be greater than zero.",
    E13 = "min_length must be greater than zero.",
    E14 = "max_length must be greater than or equal to min_length.",
    E15 = "status_limit must be greater than zero.",
    
    E16 = "The categories associated with the items must be factor type.",
    E17 = paste0("Names of status_colors must contain \"",
                 paste(status[1:3], collapse = "\", \""), "\" and \"", status[4], "\".")
  )
  is_relevant = rep(FALSE, length(errors))
  
  
  ## Validation
  
  # Validation of the item coding system
  if (any(grepl("/|,", object@items)))                                        is_relevant[1] = TRUE
  if (length(setdiff(get_all_items(object@transactions), object@items)) != 0) is_relevant[2] = TRUE
  
  # Validation of the transaction set
  if (length(object@transactions) == 0)        is_relevant[3] = TRUE
  if (!has_temporal_data(object@transactions)) is_relevant[4] = TRUE
  
  # Validation of the initialization parameters
  if (!all(parameters %in% names(object@parameters))) is_relevant[5] = TRUE
  
  if (!is.character(object@parameters$target))      is_relevant[6] = TRUE
  if (!is.numeric(object@parameters$min_frequency)) is_relevant[7] = TRUE
  if (!is.numeric(object@parameters$min_length))    is_relevant[8] = TRUE
  if (!is.numeric(object@parameters$max_length))    is_relevant[9] = TRUE
  if (!is.numeric(object@parameters$status_limit))  is_relevant[10] = TRUE
  
  if (!is.element(object@parameters$target, target))               is_relevant[11] = TRUE
  if (object@parameters$min_frequency < 1)                         is_relevant[12] = TRUE
  if (object@parameters$min_length < 1)                            is_relevant[13] = TRUE
  if (object@parameters$max_length < object@parameters$min_length) is_relevant[14] = TRUE
  if (object@parameters$status_limit < 1)                          is_relevant[15] = TRUE
  
  # Validation of the type of the categories associated with the items
  if (!all(sapply(seq_len(ncol(object@items_categories)),
                  function(c) is.factor(object@items_categories[, c])))) {
    is_relevant[16] = TRUE
  }
  
  # Validation of the status-color association
  if (!all(status %in% names(object@status_colors))) is_relevant[17] = TRUE
  
  
  # Return errors or TRUE
  if (any(is_relevant)) return(errors[is_relevant])
  return(TRUE)
})

# Initializer
setMethod(f = "initialize",
          signature = "TransactionAnalyzer",
          definition = function(.Object, transactions, items,
                                target, min_frequency, min_length, max_length, status_limit,
                                init, verbose) {
            
            .Object@transactions = transactions
            
            # All occurring items and associated categories
            if (missing(items) || is.null(items)) {
              .Object@items = get_all_items(transactions)
              names(.Object@items) = .Object@items
            } else {
              .Object@items = if (is.factor(items$item)) as.character(items$item) else items$item
              names(.Object@items) = if ("name" %in% colnames(items)) items$name else items$item
              .Object@items_categories = items[-which(colnames(items) %in% c("item", "name"))]
              rownames(.Object@items_categories) = items$item
              
              # Assigning colors to the values of each category
              if (length(.Object@items_categories) != 0) {
                .Object@categories_colors = lapply(.Object@items_categories, function(category) {
                  # Circular selection among the 20 colors of a D3 palette
                  colors = ggsci::pal_d3("category20")(20)[(seq_along(levels(category)) - 1) %% 20 + 1]
                  return(stats::setNames(colors, levels(category)))
                })
              }
            }
            
            # Assigning colors to the dynamic statuses
            .Object@status_colors = c("red", "royalblue", "orange", "gray")
            names(.Object@status_colors) = c(STATUS_PERSISTENT, STATUS_DECLINING,
                                             STATUS_EMERGENT, STATUS_LATENT)
            
            # Parameters for the search and characterization of patterns
            .Object@parameters = list(target = target,
                                      min_frequency = min_frequency,
                                      min_length = min_length,
                                      max_length = max_length,
                                      status_limit = status_limit)
            if (status_limit != 1 && length(unique(unlist(transactions[transactions@year_key]))) == 1) {
              .Object@parameters$status_limit = 1
              warning("The temporal data of the transactions are all equal. ",
                      "Parameter status_limit has been set to 1.")
            }
            
            # Validation of the first attributes
            methods::validObject(.Object)
            
            # Typecasting and initialization of the remaining attributes
            .Object@parameters$min_frequency = as.integer(min_frequency)
            .Object@parameters$min_length = as.integer(min_length)
            if (init) reset(.Object, from = 1, verbose = verbose)
            
            methods::validObject(.Object)
            return(.Object)
          })


#' Transaction Analyzer constructor
#' 
#' Create and initialize an S4 object of class \code{TransactionAnalyzer}.
#' 
#' @details
#' If items are not specified using the argument \code{items}, they are automatically listed from
#'  the \code{transactions} without any categorization or specific denomination.
#' 
#' The type of patterns mined can be:
#'  \itemize{
#'    \item{\code{"frequent itemsets"}: itemsets appearing in the transactions according to an
#'          occurrence threshold defined by the parameter \code{min_frequency}.}
#'    \item{\code{"closed frequent itemsets"}: maximal itemsets of equivalence classes. An equivalence
#'          class is defined as the set of itemsets appearing in the same transactions.}
#'    \item{\code{"maximally frequent itemsets"}: frequent itemsets which do not have any frequent
#'          superset. Also named as the maximals by inclusion of the frequent itemsets
#'          or the positive boundary of the frequent itemsets.}
#'  }
#' 
#' The \strong{frequent itemsets} are an exhaustive list of frequent itemsets.
#'  The \strong{closed frequent itemsets} synthesize the information so as to reduce the memory space
#'  required without real loss of information thanks to the equivalence classes.
#'  The \strong{maximal frequent itemsets} required even less memory since they are a subset of the
#'  closed frequent itemsets.
#'  
#' For an explanation with illustrated examples about the different types of itemsets, read the
#'  vignette titled "\emph{Itemset mining}".
#'  
#' The steps for initializing a transaction analyzer are:
#'  \enumerate{
#'    \item{Enumeration of the transactions per year.}
#'    \item{Enumeration of the nodes and calculation of the number of occurrences.}
#'    \item{Counting links between nodes.}
#'    \item{Elaboration of links between nodes.}
#'    \item{Mining for itemsets.}
#'    \item{Linking nodes to patterns.}
#'    \item{Enumeration of the patterns per year.}
#'    \item{Computation of pattern characteristics.}
#'    \item{Counting links between patterns.}
#'    \item{Elaboration of links between patterns.}
#'  }
#'  
#' The argument \code{init} and the method \code{\link{init}} allow to skip initialization steps.
#' 
#' @note
#' The following steps may be quite long (depending on the case):
#'  \itemize{
#'    \item{Step 4: elaboration of links between nodes (depending on the amount of data).}
#'    \item{Steps 5 and 6: mining for itemsets and linking nodes to patterns (depending on the
#'      mining parameters and the amount of data).}
#'    \item{Step 10: elaboration of links between patterns (depending on the amount of patterns,
#'      resulting from the mining parameters and the amount of data).}
#'  }
#' 
#' @param transactions S4 object of class \code{TransactionSet}: list of transactions containing the
#'  items corresponding to each one. Each transaction is itself a list containing at least two elements
#'  representing items and temporal data. It can contain any additional information but such data will
#'  be ignored.
#'  
#'  Items must be character or numeric values and must not contain the characters \code{"/"} and
#'  \code{","}.
#'  Temporal data must correspond to the years in which the transactions were made and must be numeric
#'  values.
#' @param items Data frame associating a name (column \code{name}) and possibly one or more categories
#'  (additional columns) to each item (column \code{item}). Each category must be of type \code{factor}.
#'  The column \code{item} must be of type \code{character} or \code{numeric}. The column \code{name}
#'  must be of type \code{character}. The default value (\code{NULL}) specifies that no name or
#'  category is defined.
#' @param target Type of patterns to mine. One of \code{"frequent itemsets"},
#'  \code{"closed frequent itemsets"}, \code{"maximally frequent itemsets"} (see 'Details').
#' @param min_frequency Minimum number of occurrences that a pattern must appear to be considered as
#'  frequent.
#' @param min_length Minimum number of items that a pattern must have to be kept when mining patterns.
#' @param max_length Maximum number of items that a pattern must have to be kept when mining patterns.
#'  The default \code{Inf} corresponds to a pattern search without maximum size limit.
#' @param status_limit Time interval for which to characterize the status of the patterns in relation
#'  to the total period of transactions (number of years).
#' @param init If \code{TRUE}, attributes relating to nodes, links between nodes, patterns and links
#'  between patterns are initialized.
#'  If \code{FALSE}, only attributes relating to transactions, items and categories are initialized.
#' @param verbose Logical value indicating whether to report progress.
#'  Ignored if \code{init} is \code{FALSE}.
#' @return New object of class \code{TransactionAnalyzer}.
#' 
#' @author Gauthier Magnin
#' @seealso
#' The class: \code{\link{TransactionAnalyzer}}.
#' 
#' Initialization: \code{\link{init}}, \code{\link{is_init}}, \code{\link{reset}}.
#' 
#' @examples
#' ## Creating a TransactionAnalyzer from a list of transactions
#' trx <- make_transactions(oedb_sample, by = "ID",
#'                          additional = c("CODE", "NAME", "YEAR"))
#' trx_object <- transaction.set(data = trx, item_key = "CODE", year_key = "YEAR")
#' 
#' ta_object_1 <- transaction.analyzer(trx_object)
#' 
#' ## Creating a TransactionAnalyzer after associating item identifiers with
#' ## names and one category
#' items_ids <- get_all_items(trx_object)
#' category_1 <- substances_information[match(items_ids,
#'                                            substances_information$CODE),
#'                                      "SUBFAMILY"]
#' category_1[is.na(category_1)] <- "Unknown"
#' names <- substances_information[match(items_ids,
#'                                       substances_information$CODE),
#'                                 "NAME"]
#' 
#' items <- data.frame(item = items_ids,
#'                     name = names,
#'                     family = as.factor(category_1),
#'                     stringsAsFactors = FALSE)
#' ta_object_2 <- transaction.analyzer(trx_object, items)
#' 
#' @export
transaction.analyzer = function(transactions, items = NULL, target = "closed frequent itemsets",
                                min_frequency = 1, min_length = 1, max_length = Inf, status_limit = 2,
                                init = TRUE, verbose = TRUE) {
  
  return(methods::new(Class = "TransactionAnalyzer",
                      transactions = transactions, items = items,
                      target = target, min_frequency = min_frequency, min_length = min_length,
                      max_length = max_length, status_limit = status_limit,
                      init = init, verbose = verbose))
}



#### Methods print, show, plot, summary, length ####

# Methods print and summary need to be exported explicitly.
# Methods show and length do not need.

# print: display in console
#' @export
setMethod(f = "print",
          signature = "TransactionAnalyzer",
          definition =
function(x, ...) {
  
  numbers = c(items = length(x@items),
              categories = ncol(x@items_categories),
              transactions = length(x@transactions))
  
  if (is_init_nodes(x)) {
    numbers = c(numbers, nodes = nrow(x@nodes))
    
    if (is_init_node_links(x))
      numbers = c(numbers, node_links = sum(x@node_links$weight != 0))
    
    if (is_init_patterns(x)) {
      numbers = c(numbers, patterns = nrow(x@patterns))
      
      if (is_init_pattern_links(x))
        numbers = c(numbers, pattern_links = sum(x@pattern_links$weight != 0))
    }
  }
  
  if (is_init_patterns(x)) {
    std_numbers = standardize_nchar(c(" ", numbers[c("items", "categories", "transactions", "nodes", "patterns")]))
    names(std_numbers) = c(" ", "items", "categories", "transactions", "nodes", "patterns")
  } else if (is_init_nodes(x)) {
    std_numbers = standardize_nchar(c(" ", numbers[c("items", "categories", "transactions", "nodes")]))
    names(std_numbers) = c(" ", "items", "categories", "transactions", "nodes")
  } else {
    std_numbers = standardize_nchar(c(" ", numbers))
    names(std_numbers) = c(" ", names(numbers))
  }
  
  if (numbers["categories"] == 0) to_show_categories = ""
  else {
    std_categories = standardize_nchar(paste0("\"", colnames(x@items_categories), "\":"), at_end = TRUE)
    categories_numbers = sapply(x@items_categories, function(c) length(levels(c)))
    std_category_numbers = standardize_nchar(categories_numbers)
    to_show_categories = character(length(std_categories))
    
    for (c in seq_along(std_categories)) {
      to_show_categories[c] = paste("\n ", std_numbers[" "],
                                    std_categories[c],
                                    std_category_numbers[c],
                                    pluralize("level", categories_numbers[c]))
    }
  }
  
  to_show = paste0("TransactionAnalyzer",
    "\n  ", std_numbers["items"], " ", pluralize("item", numbers["items"]),
    "\n  ", std_numbers["categories"], " ", pluralize("category", numbers["categories"]),
    paste(to_show_categories, collapse = ""),
    "\n  ", std_numbers["transactions"], pluralize(" transaction", numbers["transactions"]),
    "\n  ", std_numbers[" "], " names:    ", paste0("\"", x@transactions@names, "\"", collapse = ", "),
    "\n  ", std_numbers[" "], " item_key: \"", x@transactions@item_key, "\"",
    "\n  ", std_numbers[" "], " year_key: ", if (is.na(x@transactions@year_key)) NA else paste0("\"", x@transactions@year_key, "\"")
  )
  
  if (is_init_nodes(x)) {
    to_show = paste0(to_show,
                     "\n  ", std_numbers["nodes"], " ",
                     pluralize("node", numbers["nodes"]))
    
    if (is_init_node_links(x)) {
      to_show = paste(to_show, "with", numbers["node_links"],
                      pluralize("link", numbers["node_links"]))
    }
    if (is_init_patterns(x)) {
      to_show = paste0(to_show,
                       "\n  ", std_numbers["patterns"], " ",
                       pluralize("pattern", numbers["patterns"]))
      
      if (is_init_pattern_links(x)) {
        to_show = paste(to_show, "with", numbers["pattern_links"],
                        pluralize("link", numbers["pattern_links"]))
      }
      
      to_show = paste0(
        to_show,
        "\n  ", std_numbers[" "], " ", cap(x@parameters$target),
        "\n  ", std_numbers[" "], " Minimum frequency parameter: ", x@parameters$min_frequency,
        "\n  ", std_numbers[" "], " Length in ", interval(1, Inf),
        "\n  ", std_numbers[" "], " Status characterization over ",
        x@parameters$status_limit, pluralize(" year", x@parameters$status_limit)
      )
    }
  }
  
  cat(to_show)
})

# show: short display in console
setMethod(f = "show",
          signature = "TransactionAnalyzer",
          definition =
function(object) {
  
  numbers = c(item = length(object@items),
              category = ncol(object@items_categories),
              transaction = length(object@transactions))
  
  if (is_init_nodes(object)) numbers = c(numbers, node = nrow(object@nodes))
  if (is_init_patterns(object)) numbers = c(numbers, pattern = nrow(object@patterns))
  
  std_numbers = standardize_nchar(numbers)
  to_show = character(length(numbers))
  
  for (n in seq_along(numbers)) {
    to_show[n] = paste0("\n  ", std_numbers[n], " ", pluralize(names(numbers[n]), numbers[n]))
  }
  
  cat("TransactionAnalyzer",
      paste(to_show, collapse = ""),
      "\n  Slots:", paste(methods::slotNames("TransactionAnalyzer"), collapse = ", "), "\n")
})

# summary: object summary
#' @export
setMethod(f = "summary",
          signature = "TransactionAnalyzer",
          definition =
function(object, ...) {
  
  if (!is_init_patterns(object)) {
    # If the patterns have not been computed, only part of the summary
    return(c(items        = length(object@items),
             categories   = ncol(object@items_categories),
             transactions = length(object@transactions),
             nodes        = if (is_init_nodes(object)) nrow(object@nodes) else NA,
             patterns     = NA))
  }
  
  summaries = list()
  
  # Summary of pattern characteristics
  main = cbind(year        = summary(object@patterns$year),
               support     = summary(object@patterns$support),
               frequency   = summary(object@patterns$frequency),
               weight      = summary(object@patterns$weight),
               specificity = summary(object@patterns$specificity))
  
  summaries[["patterns"]] = list(main = main)
  summaries[["patterns"]][["length"]] = as.data.frame(table(object@patterns$length))
  summaries[["patterns"]][["status"]] = as.data.frame(table(object@patterns$status))
  colnames(summaries[["patterns"]][["length"]]) = c("length", "count")
  colnames(summaries[["patterns"]][["status"]]) = c("status", "count")
  
  # Length of the main attributes
  summaries[["count"]] = c(items        = length(object@items),
                           categories   = ncol(object@items_categories),
                           transactions = length(object@transactions),
                           nodes        = nrow(object@nodes),
                           patterns     = nrow(object@patterns))
  
  return(summaries)
})



#### Selector and mutator ####

#' Extract or replace parts of an object of class TransactionAnalyzer
#' 
#' General selector and mutator to access the attributes of an object of class
#'  \code{TransactionAnalyzer}.
#' 
#' @details
#' Character values can be used to access attributes as well as elements of the
#'  attribute \code{parameters}.
#' 
#' Numeric values can also be used to access attributes considering their order.
#' 
#' @param x Object from which to extract element(s) or in which to replace
#'  element(s).
#' @param i Numeric or character values. Indices specifying elements to extract
#'  or replace. See 'Details' section.
#' 
#' @author Gauthier Magnin
#' 
#' @examples
#' TA_instance["items"]
#' TA_instance["patterns"]
#' TA_instance[1]
#' TA_instance["target"]
#' 
#' @aliases [,TransactionAnalyzer-method
#' @export
setMethod(f = "[",
          signature = "TransactionAnalyzer",
          definition = function(x, i) {
            switch(EXPR = i,
                   "transactions"      = { return(x@transactions) },
                   "items"             = { return(x@items) },
                   "items_categories"  = { return(x@items_categories) },
                   "categories_colors" = { return(x@categories_colors) },
                   "status_colors"     = { return(x@status_colors) },
                   
                   "parameters"        = { return(x@parameters) },
                   "target"            = { return(x@parameters$target) },
                   "min_frequency"     = { return(x@parameters$min_frequency) },
                   "min_length"        = { return(x@parameters$min_length) },
                   "max_length"        = { return(x@parameters$max_length) },
                   "status_limit"      = { return(x@parameters$status_limit) },
                   
                   "nodes_per_year"    = { return(x@nodes_per_year) },
                   "nodes"             = { return(x@nodes) },
                   "n_links"           = { return(x@n_links) },
                   "node_links"        = { return(x@node_links) },
                   "nodes_patterns"    = { return(x@nodes_patterns) },
                   "patterns_per_year" = { return(x@patterns_per_year) },
                   "patterns"          = { return(x@patterns) },
                   "p_links"           = { return(x@p_links) },
                   "pattern_links"     = { return(x@pattern_links) },
                   
                   stop("Unknown attribute."))
          })

#' @rdname sub-TransactionAnalyzer-ANY-ANY-ANY-method
#' 
#' @param value Value of type similar to the element to be replaced.
#' 
#' @examples
#' TA_instance["target"] <- "maximally frequent itemsets"
#' TA_instance["min_frequency"] <- 2
#' TA_instance["max_length"] <- 3
#' 
#' @aliases [<-,TransactionAnalyzer-method
#' @export
setReplaceMethod(f = "[",
                 signature = "TransactionAnalyzer",
                 definition = function(x, i, value) {
                   switch(EXPR = i,
                          "transactions"      = { x@transactions = value },
                          "items"             = { x@items = value },
                          "items_categories"  = { x@items_categories = value },
                          "categories_colors" = { x@categories_colors = value },
                          "status_colors"     = { x@status_colors = value },
                          
                          "parameters"        = { x@parameters = value },
                          "target"            = { x@parameters$target = value },
                          "min_frequency"     = { x@parameters$min_frequency = value },
                          "min_length"        = { x@parameters$min_length = value },
                          "max_length"        = { x@parameters$max_length = value },
                          "status_limit"      = { x@parameters$status_limit = value },
                          
                          "nodes_per_year"    = { x@nodes_per_year = value },
                          "nodes"             = { x@nodes = value },
                          "n_links"           = { x@n_links = value },
                          "node_links"        = { x@node_links = value },
                          "nodes_patterns"    = { x@nodes_patterns = value },
                          "patterns_per_year" = { x@patterns_per_year = value },
                          "patterns"          = { x@patterns = value },
                          "p_links"           = { x@p_links = value },
                          "pattern_links"     = { x@pattern_links = value },
                          
                          stop("Unknown attribute."))
                   
                   methods::validObject(x)
                   return(x)
                 })



#### Declaration of the methods ####

# Instructions in comment correspond to generics already defined in transaction_set.R
# because some methods are defined for both signatures TransactionAnalyzer and TransactionSet.

# Initialization methods

setGeneric(name = "reset", def = function(object, from = 1, verbose = TRUE){ standardGeneric("reset") })

setGeneric(name = "init", def = function(object, part = NULL, verbose = TRUE){ standardGeneric("init") })

setGeneric(name = "init_nodes", def = function(object, verbose = TRUE){ standardGeneric("init_nodes") })

setGeneric(name = "init_node_links", def = function(object, verbose = TRUE){ standardGeneric("init_node_links") })

setGeneric(name = "init_patterns", def = function(object, verbose = TRUE){ standardGeneric("init_patterns") })

setGeneric(name = "init_pattern_links", def = function(object, verbose = TRUE){ standardGeneric("init_pattern_links") })

setGeneric(name = "is_init", def = function(object, part = NULL){ standardGeneric("is_init") })

setGeneric(name = "is_init_nodes", def = function(object){ standardGeneric("is_init_nodes") })

setGeneric(name = "is_init_node_links", def = function(object){ standardGeneric("is_init_node_links") })

setGeneric(name = "is_init_patterns", def = function(object){ standardGeneric("is_init_patterns") })

setGeneric(name = "is_init_pattern_links", def = function(object){ standardGeneric("is_init_pattern_links") })

setGeneric(name = "check_init", def = function(object, part = NULL, stop = TRUE, prefix = "", suffix = ""){ standardGeneric("check_init") })


# Computation methods used for the construction of the nodes

setGeneric(name = "list_trx_per_year", def = function(object){ standardGeneric("list_trx_per_year") })

setGeneric(name = "list_separate_trx", def = function(object){ standardGeneric("list_separate_trx") })


# Computation methods used for the construction of spectrosomes

setGeneric(name = "count_links", def = function(object, entities){ standardGeneric("count_links") })

setGeneric(name = "search_links", def = function(object, entities){ standardGeneric("search_links") })


# Computation methods used for the construction of the patterns

setGeneric(name = "list_separate_patterns", def = function(object, target, min_frequency = 1, min_length = 1, max_length = Inf, arules = FALSE){ standardGeneric("list_separate_patterns") })

setGeneric(name = "list_patterns_by_node", def = function(object){ standardGeneric("list_patterns_by_node") })

setGeneric(name = "list_patterns_per_year", def = function(object){ standardGeneric("list_patterns_per_year") })

setGeneric(name = "compute_patterns_characteristics", def = function(object){ standardGeneric("compute_patterns_characteristics") })

setGeneric(name = "compute_specificity", def = function(object, patterns, frequencies, weights){ standardGeneric("compute_specificity") })

setGeneric(name = "check_RI_params", def = function(object, end, period){ standardGeneric("check_RI_params") })

setGeneric(name = "compute_reporting_indexes", def = function(object, patterns, end = NULL, period = Inf){ standardGeneric("compute_reporting_indexes") })

setGeneric(name = "compute_reporting_indexes_limits", def = function(object, patterns, end = NULL, overall_period = Inf, recent_period = object["status_limit"]){ standardGeneric("compute_reporting_indexes_limits") })

setGeneric(name = "compute_xi_threshold", def = function(object, reporting_indexes){ standardGeneric("compute_xi_threshold") })

setGeneric(name = "compute_ri_threshold", def = function(object, reporting_indexes, xi = NULL){ standardGeneric("compute_ri_threshold") })

setGeneric(name = "dynamic_status", def = function(object, patterns, end = NULL, overall_period = Inf, recent_period = object["status_limit"]){ standardGeneric("dynamic_status") })


# Methods for creating spectrum charts

setGeneric(name = "spectrum_chart", def = function(object, pc, identifiers = "original", sort = TRUE, title = "Spectrum of patterns", path = NULL, name = NULL){ standardGeneric("spectrum_chart") })

setGeneric(name = "plot_spectrum_chart", def = function(object, pc, frequencies, title = "Spectrum of patterns"){ standardGeneric("plot_spectrum_chart") })

setGeneric(name = "pattern_node_characteristics", def = function(object, patterns){ standardGeneric("pattern_node_characteristics") })

setGeneric(name = "frequency_by_complexity", def = function(object, patterns){ standardGeneric("frequency_by_complexity") })


# Methods for creating spectrosome graphs and computing related indicators

setGeneric(name = "spectrosome_chart", def = function(object, nopc, identifiers = "original", nb_graphs = 1, min_link_weight = 1, vertex_size = "relative", size_range = c(0.5, 2.5), vertex_col = "status", clusters = Inf, highlight = 3, use_names = TRUE, n.cutoff = NULL, c.cutoff = NULL, display_mixt = TRUE, title = NULL, path = NULL, name = NULL, ...){ standardGeneric("spectrosome_chart") })

setGeneric(name = "cluster_text", def = function(object, graph, links, display = Inf, highlight = 3, use_names = TRUE, cutoff = NULL){ standardGeneric("cluster_text") })

setGeneric(name = "network_density", def = function(object, links){ standardGeneric("network_density") })

setGeneric(name = "degree", def = function(object, ID, links){ standardGeneric("degree") })


# Methods for creating itemset charts, category trees and co-occurrence graphs

# setGeneric(name = "itemset_chart", def = function(object, ...){ standardGeneric("itemset_chart") })

setGeneric(name = "category_tree_chart", def = function(object, category = NULL, items = object["items"], use_names = TRUE, n.cutoff = NULL, c.cutoff = NULL, vertex_size = 4, vertex_alpha = 1, leaf_size = 3, leaf_alpha = 1, leaf_margin = 0, label_size = 3, label_margin = 0.05){ standardGeneric("category_tree_chart") })

# setGeneric(name = "co_occurrence_chart", def = function(object, ...){ standardGeneric("co_occurrence_chart") })


# Methods for association rule extraction and visualization

setGeneric(name = "extract_rules", def = function(object, itemsets = NULL, pruning = FALSE, arules = FALSE, as_sets = FALSE, more = FALSE, ...){ standardGeneric("extract_rules") })

setGeneric(name = "compute_additional_rule_indicators", def = function(object, rules, transactions = NULL){ standardGeneric("compute_additional_rule_indicators") })

setGeneric(name = "rules_chart", def = function(object, rules = NULL, items = NULL, parameter = list(supp = 0.001, conf = 0), display = "highest confidence", threshold = NULL, direction = FALSE, use_names = TRUE, n.cutoff = NULL, category = NULL, c.cutoff = NULL, sort_by = "category", vertex_size = 3, vertex_alpha = 1, vertex_margin = 0.05, label_size = 3, label_margin = 0.05, edge_looseness = 0.8, edge_alpha = 1, palette = "default", palette_direction = 1, plot = FALSE){ standardGeneric("rules_chart") })


# Methods for search and save

# setGeneric(name = "export", def = function(object, ...){ standardGeneric("export") })

setGeneric(name = "get_trx_from_category", def = function(object, trx, category, value, as_indices = FALSE){ standardGeneric("get_trx_from_category") })

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

setGeneric(name = "get_item_names", def = function(object, items){ standardGeneric("get_item_names") })

setGeneric(name = "get_item_colors", def = function(object, category, items = object["items"]){ standardGeneric("get_item_colors") })

setGeneric(name = "category_values", def = function(object, itemsets, as_character = FALSE, unique = TRUE){ standardGeneric("category_values") })


# Other specific methods

setGeneric(name = "check_access_for_category", def = function(object, category, value, stop = TRUE){ standardGeneric("check_access_for_category") })

setGeneric(name = "has_item_names", def = function(object){ standardGeneric("has_item_names") })

# setGeneric(name = "get_items", def = function(object, ...){ standardGeneric("get_items") })

setGeneric(name = "get_items_from_category", def = function(object, category, value, force_character = FALSE){ standardGeneric("get_items_from_category") })

setGeneric(name = "get_tnp", def = function(object, tnp, entities = NODES_OR_PATTERNS){ standardGeneric("get_tnp") })

setGeneric(name = "get_tnp_itemsets", def = function(object, tnp, entities = NODES_OR_PATTERNS){ standardGeneric("get_tnp_itemsets") })

setGeneric(name = "which_entities", def = function(object, tnpr, entities = NODES_OR_PATTERNS){ standardGeneric("which_entities") })

setGeneric(name = "which_associated_links", def = function(object, entities){ standardGeneric("which_associated_links") })

setGeneric(name = "which_name", def = function(object, name){ standardGeneric("which_name") })



#### Initialization methods ####

#' Partial reset of a transaction analyzer
#' 
#' Reset the attributes of a transaction analyzer from a specific step.
#' 
#' @details
#' The steps for initializing a transaction analyzer are:
#' \enumerate{
#'   \item{Enumeration of the transactions per year.}
#'   \item{Enumeration of the nodes and calculation of the number of occurrences.}
#'   \item{Counting links between nodes.}
#'   \item{Elaboration of links between nodes.}
#'   \item{Mining for itemsets.}
#'   \item{Linking nodes to patterns.}
#'   \item{Enumeration of the patterns per year.}
#'   \item{Computation of pattern characteristics.}
#'   \item{Counting links between patterns.}
#'   \item{Elaboration of links between patterns.}
#' }
#' 
#' @note
#' The following steps may be quite long (depending on the case):
#'  \itemize{
#'    \item{Step 4: elaboration of links between nodes (depending on the amount of data).}
#'    \item{Steps 5 and 6: mining for itemsets and linking nodes to patterns (depending on the
#'      mining parameters and the amount of data).}
#'    \item{Step 10: elaboration of links between patterns (depending on the amount of patterns,
#'      resulting from the mining parameters and the amount of data).}
#'  }
#' 
#' @param object S4 object of class \code{TransactionAnalyzer}.
#' @param from Step from which to recompute the attributes.
#' @param verbose Logical value indicating whether to report progress.
#' 
#' @author Gauthier Magnin
#' @seealso
#' The \code{\link{TransactionAnalyzer}} constructor: \code{\link{transaction.analyzer}}.
#' 
#' Initialization: \code{\link{init}}, \code{\link{is_init}}.
#' 
#' @examples
#' ## Change one attribute (for instance, the pattern enumeration target) and
#' ## enumerate the patterns again for a new analysis
#' TA_instance["target"] <- "frequent itemsets"
#' reset(TA_instance, from = 5)
#' 
#' @aliases reset
#' @export
setMethod(f = "reset",
          signature = "TransactionAnalyzer",
          definition =
function(object, from = 1, verbose = TRUE) {
  
  # Name of the object for internal modification in the parent environment
  object_name = deparse(substitute(object))
  
  # Matrix of the instructions to perform and their descriptions, for each step
  steps = matrix(c(
    
    # Initialization of the attributes required for the creation of a spectrosome of nodes
    "\n*** Step 01/10: Enumeration of the transactions per year... ",
    expression(  list_trx_per_year(object)  ),
    "\n*** Step 02/10: Enumeration of the nodes and calculation of the number of occurrences... ",
    expression(  list_separate_trx(object)  ),
    "\n*** Step 03/10: Counting links between nodes... ",
    expression(  count_links(object, NODES)  ),
    "\n*** Step 04/10: Elaboration of links between nodes... ",
    expression(  search_links(object, NODES)  ),
    
    # Initialization of the attributes required for the creation of a spectrum
    "\n*** Step 05/10: Mining for itemsets... ",
    expression(  list_separate_patterns(object, object@parameters$target, object@parameters$min_frequency,
                                        object@parameters$min_length, object@parameters$max_length)  ),
    "\n*** Step 06/10: Linking nodes to patterns... ",
    expression(  list_patterns_by_node(object)  ),
    "\n*** Step 07/10: Enumeration of the patterns per year... ",
    expression(  list_patterns_per_year(object)  ),
    "\n*** Step 08/10: Computation of pattern characteristics... ",
    expression(  compute_patterns_characteristics(object)  ),
    
    # Initialization of the attributes required for the creation of a spectrosome of patterns
    "\n*** Step 09/10: Counting links between patterns... ",
    expression(  count_links(object, PATTERNS)  ),
    "\n*** Step 10/10: Elaboration of links between patterns... ",
    expression(  search_links(object, PATTERNS)  )
    
  ), ncol = 2, nrow = 10, byrow = TRUE)
  
  # Steps to actually perform
  steps_todo = (from <= seq(nrow(steps))) & (!DEBUG_MODE_ | seq(nrow(steps)) <= UP_TO_STEP_)
  # Removing the newline from the first of the steps to perform
  steps[[which(steps_todo)[1]]] = substring(steps[[which(steps_todo)[1]]], 2)
  
  # Execution of the steps
  for (i in seq(nrow(steps))) {
    if (steps_todo[i]) {
      if (verbose) {
        cat(steps[i, 1][[1]])
        eval(parse(text = paste("display_time(", steps[i, 2], ")")))
      } else {
        eval(parse(text = steps[i, 2]))
      }
    }
  }
  if (verbose) cat("\n")
  
  # Redefinition of the object
  assign(object_name, object, envir = parent.frame())
  return(invisible())
})


#' Initialization of attributes
#' 
#' Initialize the attributes related to nodes, links between nodes, patterns and/or links between
#'  patterns.
#' 
#' @details
#' The initialization of the nodes consists of the initialization of the attributes `nodes` and
#'  `nodes_per_year`:
#'  \enumerate{
#'    \item{Enumeration of the transactions per year.}
#'    \item{Enumeration of the nodes and calculation of the number of occurrences.}
#'  }
#' 
#' The initialization of the links between nodes consists of the initialization of the attributes
#'  `n_links` and `node_links`:
#'  \enumerate{
#'    \item{Counting links between nodes.}
#'    \item{Elaboration of links between nodes.}
#'  }
#' 
#' The initialization of the patterns consists of the initialization of the attributes `patterns`,
#'  `patterns_per_year` and `nodes_patterns`:
#'  \enumerate{
#'    \item{Mining for itemsets.}
#'    \item{Linking nodes to patterns.}
#'    \item{Enumeration of the patterns per year.}
#'    \item{Computation of pattern characteristics.}
#'  }
#' 
#' The initialization of the links between patterns consists of the initialization of the attributes
#'  `p_links` and `pattern_links`:
#'  \enumerate{
#'    \item{Counting links between patterns.}
#'    \item{Elaboration of links between patterns.}
#'  }
#' 
#' @param object S4 object of class `TransactionAnalyzer`.
#' @param part `NULL` or character value corresponding to the attributes to initialize.
#'  One of the following:
#'  \describe{
#'    \item{`"nodes"`, `"n"`}{Attributes related to nodes.}
#'    \item{`"node_links"`, `"nl"`}{Attributes related to links between nodes.}
#'    \item{`"patterns"`, `"p"`}{Attributes related to patterns.}
#'    \item{`"pattern_links"`, `"pl"`}{Attributes related to links between patterns.}
#'    \item{`NULL`}{All attributes.}
#'  }
#' @param verbose Logical value indicating whether to report progress.
#' @return Invisible. `NULL` if `part` does not refer to patterns. If it does refer to patterns,
#'  object of class [`itemsets`][arules::itemsets-class] (from the package `arules`).
#' 
#' @author Gauthier Magnin
#' @seealso [`is_init`], [`reset`], [`transaction.analyzer`],
#'  [`arules::itemsets`][arules::itemsets-class].
#' 
#' @examples
#' ## Creating a TransactionAnalyzer and initialize some parts of it
#' trx <- make_transactions(oedb_sample, by = "ID",
#'                          additional = c("CODE", "NAME", "YEAR"))
#' trx_object <- transaction.set(data = trx, item_key = "CODE", year_key = "YEAR")
#' ta_object <- transaction.analyzer(trx_object, init = FALSE)
#' init(ta_object, "nodes")
#' init(ta_object, "node_links")
#' 
#' ## Testing of its initialization
#' is_init(ta_object, "nodes")
#' is_init(ta_object, "patterns")
#' is_init(ta_object)
#' 
#' @aliases init
#' @md
#' @export
setMethod(f = "init",
          signature = "TransactionAnalyzer",
          definition =
function(object, part = NULL, verbose = TRUE) {
  
  # Name of the object for internal modification in the parent environment
  object_name = deparse(substitute(object))
  
  check_param(part, types = c("character", "NULL"))
  
  if (is.null(part)) {
    to_return = reset(object, from = 1, verbose = verbose)
  } else {
    check_param(part, values = c(NODES, NODE_LINKS, PATTERNS, PATTERN_LINKS,
                                 first_characters(c(NODES, NODE_LINKS, PATTERNS, PATTERN_LINKS))),
                suffix = " or NULL")
    
    if (part == NODES || part == first_characters(NODES)) {
      to_return = init_nodes(object, verbose)
    }
    else if (part == NODE_LINKS || part == first_characters(NODE_LINKS)) {
      check_init(object, NODES)
      to_return = init_node_links(object, verbose)
    }
    else if (part == PATTERNS || part == first_characters(PATTERNS)) {
      check_init(object, NODES)
      to_return = init_patterns(object, verbose)
    }
    else if (part == PATTERN_LINKS || part == first_characters(PATTERN_LINKS)) {
      check_init(object, PATTERNS)
      to_return = init_pattern_links(object, verbose)
    }
  }
  
  # Redefinition of the object
  assign(object_name, object, envir = parent.frame())
  return(invisible(to_return))
})


#' Initialization of specific attributes
#' 
#' @inherit init,TransactionAnalyzer-method description
#' @inherit init,TransactionAnalyzer-method details
#' 
#' @template methods_not_exported
#' 
#' @param object S4 object of class `TransactionAnalyzer`.
#' @param verbose Logical value indicating whether to report progress.
#' @return Invisible. Object of class [`itemsets`][arules::itemsets-class] (from the package `arules`)
#'  for the method `init_patterns`. `NULL` otherwise.
#' 
#' @author Gauthier Magnin
#' @seealso [`init`], [`is_init`].
#' @md
#' @name specific_init,TransactionAnalyzer-method
NULL

#' @rdname specific_init-TransactionAnalyzer-method
#' @aliases init_nodes
#' @keywords internal
setMethod(f = "init_nodes",
          signature = "TransactionAnalyzer",
          definition =
function(object, verbose = TRUE) {
  
  # Name of the object for internal modification in the parent environment
  object_name = deparse(substitute(object))
  
  if (verbose) {
    cat("*** Step N.1/2: Enumeration of the transactions per year... ")
    display_time(list_trx_per_year(object))
    
    cat("\n*** Step N.2/2: Enumeration of the nodes and calculation of the number of occurrences... ")
    display_time(list_separate_trx(object))
    cat("\n")
  } else {
    list_trx_per_year(object)
    list_separate_trx(object)
  }
  
  # Redefinition of the object
  assign(object_name, object, envir = parent.frame())
  return(invisible())
})

#' @rdname specific_init-TransactionAnalyzer-method
#' @aliases init_node_links
#' @keywords internal
setMethod(f = "init_node_links",
          signature = "TransactionAnalyzer",
          definition =
function(object, verbose = TRUE) {
  
  # Name of the object for internal modification in the parent environment
  object_name = deparse(substitute(object))
  
  if (verbose) {
    cat("*** Step NL.1/2: Counting links between nodes... ")
    display_time(count_links(object, NODES))
    
    cat("\n*** Step NL.2/2: Elaboration of links between nodes... ")
    display_time(search_links(object, NODES))
    cat("\n")
  } else {
    count_links(object, NODES)
    search_links(object, NODES)
  }
  
  # Redefinition of the object
  assign(object_name, object, envir = parent.frame())
  return(invisible())
})

#' @rdname specific_init-TransactionAnalyzer-method
#' @aliases init_patterns
#' @keywords internal
setMethod(f = "init_patterns",
          signature = "TransactionAnalyzer",
          definition =
function(object, verbose = TRUE) {
  
  # Name of the object for internal modification in the parent environment
  object_name = deparse(substitute(object))
  
  if (verbose) {
    cat("*** Step P.1/4: Mining for itemsets... ")
    display_time(arules_p <- list_separate_patterns(object,
                                                    object@parameters$target,
                                                    object@parameters$min_frequency,
                                                    object@parameters$min_length,
                                                    object@parameters$max_length,
                                                    arules = TRUE))
    
    cat("\n*** Step P.2/4: Linking nodes to patterns... ")
    display_time(list_patterns_by_node(object))
    
    cat("\n*** Step P.3/4: Enumeration of the patterns per year... ")
    display_time(list_patterns_per_year(object))
    
    cat("\n*** Step P.4/4: Computation of pattern characteristics... ")
    display_time(compute_patterns_characteristics(object))
    cat("\n")
  } else {
    arules_p = list_separate_patterns(object,
                                      object@parameters$target,
                                      object@parameters$min_frequency,
                                      object@parameters$min_length,
                                      object@parameters$max_length,
                                      arules = TRUE)
    list_patterns_by_node(object)
    list_patterns_per_year(object)
    compute_patterns_characteristics(object)
  }
  
  # Redefinition of the object
  assign(object_name, object, envir = parent.frame())
  return(invisible(arules_p))
})

#' @rdname specific_init-TransactionAnalyzer-method
#' @aliases init_pattern_links
#' @keywords internal
setMethod(f = "init_pattern_links",
          signature = "TransactionAnalyzer",
          definition =
function(object, verbose = TRUE) {
  
  # Name of the object for internal modification in the parent environment
  object_name = deparse(substitute(object))
  
  if (verbose) {
    cat("*** Step PL.1/2: Counting links between patterns... ")
    display_time(count_links(object, PATTERNS))
    
    cat("\n*** Step PL.2/2: Elaboration of links between patterns... ")
    display_time(search_links(object, PATTERNS))
    cat("\n")
  } else {
    count_links(object, PATTERNS)
    search_links(object, PATTERNS)
  }
  
  # Redefinition of the object
  assign(object_name, object, envir = parent.frame())
  return(invisible())
})


#' Test of attribute initialization
#' 
#' Test if the attributes related to nodes, links between nodes, patterns or links between patterns
#'  are initialized.
#' 
#' @details
#' The initialization of the nodes consists of the initialization of the attributes `n_links` and
#'  `node_links`.
#' 
#' The initialization of the links between nodes consists of the initialization of the attributes
#'  `nodes` and `nodes_per_year`.
#' 
#' The initialization of the patterns consists of the initialization of the attributes `patterns`,
#'  `patterns_per_year` and `nodes_patterns`.
#' 
#' The initialization of the links between patterns consists of the initialization of the attributes
#'  `p_links` and `pattern_links`.
#' 
#' @param object S4 object of class `TransactionAnalyzer`.
#' @param part `NULL` or character value corresponding to the attributes to test. One of the following:
#'  \describe{
#'    \item{`"nodes"`, `"n"`}{Attributes related to nodes.}
#'    \item{`"node_links"`, `"nl"`}{Attributes related to links between nodes.}
#'    \item{`"patterns"`, `"p"`}{Attributes related to patterns.}
#'    \item{`"pattern_links"`, `"pl"`}{Attributes related to links between patterns.}
#'  }
#' @return If `part` is not `NULL`, `TRUE` if the related attributes are initialized.
#'  If `part` is `NULL`, 4 logical values corresponding to the initialization related to `nodes`,
#'  `node_links`, `patterns` and `pattern_links` (in this order).
#' 
#' @author Gauthier Magnin
#' @seealso [`init`], [`reset`], [`transaction.analyzer`].
#' 
#' @examples
#' ## Creating a TransactionAnalyzer and initialize some parts of it
#' trx <- make_transactions(oedb_sample, by = "ID",
#'                          additional = c("CODE", "NAME", "YEAR"))
#' trx_object <- transaction.set(data = trx, item_key = "CODE", year_key = "YEAR")
#' ta_object <- transaction.analyzer(trx_object, init = FALSE)
#' init(ta_object, "nodes")
#' init(ta_object, "node_links")
#' 
#' ## Testing of its initialization
#' is_init(ta_object, "nodes")
#' is_init(ta_object, "patterns")
#' is_init(ta_object)
#' 
#' @aliases is_init
#' @md
#' @export
setMethod(f = "is_init",
          signature = "TransactionAnalyzer",
          definition =
function(object, part = NULL) {
  
  check_param(part, types = c("character", "NULL"))
  
  if (is.null(part)) {
    return(c("n" = is_init_nodes(object),    "nl" = is_init_node_links(object),
             "p" = is_init_patterns(object), "pl" = is_init_pattern_links(object)))
  }
  
  check_param(part, values = c(NODES, NODE_LINKS, PATTERNS, PATTERN_LINKS,
                               first_characters(c(NODES, NODE_LINKS, PATTERNS, PATTERN_LINKS))),
              suffix = " or NULL")
  
  if (part == NODES || part == first_characters(NODES))
    return(is_init_nodes(object))
  if (part == NODE_LINKS || part == first_characters(NODE_LINKS))
    return(is_init_node_links(object))
  if (part == PATTERNS || part == first_characters(PATTERNS))
    return(is_init_patterns(object))
  if (part == PATTERN_LINKS || part == first_characters(PATTERN_LINKS))
    return(is_init_pattern_links(object))
})


#' Test of specific attribute initialization
#' 
#' @inherit is_init,TransactionAnalyzer-method description
#' @inherit is_init,TransactionAnalyzer-method details
#' 
#' @template methods_not_exported
#' 
#' @param object S4 object of class `TransactionAnalyzer`.
#' @return `TRUE` if the related attributes are initialized. `FALSE` otherwise.
#' 
#' @author Gauthier Magnin
#' @seealso [`is_init`], [`init`].
#' @md
#' @name specific_is_init,TransactionAnalyzer-method
NULL

#' @rdname specific_is_init-TransactionAnalyzer-method
#' @aliases is_init_nodes
#' @keywords internal
setMethod(f = "is_init_nodes",
          signature = "TransactionAnalyzer",
          definition =
function(object) {
  return(nrow(object@nodes) != 0
         && nrow(object@nodes_per_year) != 0)
})

#' @rdname specific_is_init-TransactionAnalyzer-method
#' @aliases is_init_node_links
#' @keywords internal
setMethod(f = "is_init_node_links",
          signature = "TransactionAnalyzer",
          definition =
function(object) {
  return(nrow(object@n_links) != 0
         && nrow(object@node_links) != 0)
})

#' @rdname specific_is_init-TransactionAnalyzer-method
#' @aliases is_init_patterns
#' @keywords internal
setMethod(f = "is_init_patterns",
          signature = "TransactionAnalyzer",
          definition =
function(object) {
  return(nrow(object@nodes_patterns) != 0
         && ncol(object@patterns) != 0
         && ncol(object@patterns_per_year) != 0)
})

#' @rdname specific_is_init-TransactionAnalyzer-method
#' @aliases is_init_pattern_links
#' @keywords internal
setMethod(f = "is_init_pattern_links",
          signature = "TransactionAnalyzer",
          definition =
function(object) {
  return(!is.null(dimnames(object@p_links))
         && ncol(object@pattern_links) != 0)
})


#' Initialization validation
#' 
#' Check that a part of an object of class `TransactionAnalyzer` is initialized.
#' Stop the execution and print an error message if not.
#' 
#' @inherit is_init,TransactionAnalyzer-method details
#' 
#' @template method_not_exported
#' 
#' @param object S4 object of class `TransactionAnalyzer`.
#' @param part `NULL` or character value corresponding to the part of the attributes to test.
#'  One or more of the following:
#'  \describe{
#'    \item{`"nodes"`, `"n"`}{Attributes related to nodes.}
#'    \item{`"node_links"`, `"nl"`}{Attributes related to links between nodes.}
#'    \item{`"patterns"`, `"p"`}{Attributes related to patterns.}
#'    \item{`"pattern_links"`, `"pl"`}{Attributes related to links between patterns.}
#'  }
#' @param stop If `TRUE`, stop the execution and print an error message if the attributes related to
#'  `part` are note initialized. If `FALSE`, see 'Value' section.
#' @param prefix Text to be prefixed to the message.
#' @param suffix Text to be suffixed to the message.
#' @return If `part` is not `NULL`, as many values as there are in `part`:
#'  \itemize{
#'    \item{`TRUE` if the related attributes are initialized.}
#'    \item{`FALSE` if the related attributes are not initialized.}
#'  }
#'  
#'  If `part` is `NULL`, 4 logical values corresponding to the initialization related to `nodes`,
#'  `node_links`, `patterns`, `pattern_links` (in this order). 
#' 
#' @author Gauthier Magnin
#' @seealso [`is_init`], [`init`].
#' 
#' @aliases check_init
#' @md
#' @keywords internal
setMethod(f = "check_init",
          signature = "TransactionAnalyzer",
          definition =
function(object, part = NULL, stop = TRUE, prefix = "", suffix = "") {
  
  check_param(part, types = c("character", "NULL"))
  
  if (is.null(part) || length(part) == 1) {
    if (!is.null(part)) {
      check_param(part, values = c(NODES, NODE_LINKS, PATTERNS, PATTERN_LINKS,
                                   first_characters(c(NODES, NODE_LINKS, PATTERNS, PATTERN_LINKS))),
                  suffix = " or NULL")
    }
    
    condition = is_init(object, part)
    if (!stop || all(condition)) return(condition)
    
    beginning = if (prefix == "") "Attributes " else "attributes "
    
    if (is.null(part)) stop(prefix, beginning, "are not all initialized", suffix, ".")
    stop(prefix, beginning, "relating to ", which_name(object, part), " must first be initialized", suffix, ".")
  }
  else {
    for (i in seq_along(part)) {
      check_param(part[i], values = c(NODES, NODE_LINKS, PATTERNS, PATTERN_LINKS,
                                      first_characters(c(NODES, NODE_LINKS, PATTERNS, PATTERN_LINKS))))
    }
    
    conditions = sapply(part, is_init, object = object)
    if (!stop || all(conditions)) return(conditions)
    
    part = which_name(object, part)
    beginning = if (prefix == "") "Attributes " else "attributes "
    conditions = !conditions
    
    if (sum(conditions) == 1) {
      attributes = part[conditions]
    } else if (sum(conditions) == 2) {
      attributes = paste(part[conditions][1], "and", part[conditions][2])
    } else {
      attributes = paste(paste(part[conditions][-sum(conditions)], collapse = ", "), "and", part[conditions][sum(conditions)])
    }
    stop(prefix, beginning, "relating to ", attributes, " must first be initialized", suffix, ".")
  }
})



#### Computation methods used for the construction of the nodes ####

#' Enumeration of the transactions per year
#' 
#' Identify the separate transactions per year (considering only their respective items) and count their
#'  number of occurrences for each one.
#' The resulting matrix is assigned to the attribute \code{nodes_per_year} of \code{object}.
#' 
#' @template method_not_exported
#' 
#' @param object S4 object of class \code{TransactionAnalyzer}.
#' @return Invisible. Matrix of the number of occurrences of each separate transaction, per year.
#'  The rows correspond to the transactions. The columns correspond to the years.
#' 
#' @author Gauthier Magnin
#' @aliases list_trx_per_year
#' @keywords internal
setMethod(f = "list_trx_per_year",
          signature = "TransactionAnalyzer",
          definition =
function(object) {
  
  # Name of the object for internal modification in the parent environment
  object_name = deparse(substitute(object))
  
  
  # Conversion of the transaction list to a data.frame (and sorting the items of each transaction)
  trx_df = data.frame(year = sapply(object@transactions@data, "[[", object@transactions@year_key))
  trx_df$node = lapply(object@transactions[object@transactions@item_key], sort)
  
  # Concatenation of the item identifiers (needed for the function "table" and for faster sorting)
  trx_df$node = sapply(trx_df$node, paste0, collapse = "/")
  
  # Computation of the distribution of separate sets of items by year and conversion to a matrix
  nodes_df = as.data.frame(table(trx_df), stringsAsFactors = FALSE)
  nodes_mat = with(nodes_df, tapply(Freq, list(node, year), sum))
  
  # Splitting back the items composing each node to be able to calculate their lengths
  nodes = strsplit(rownames(nodes_mat), split = "/")
  
  # Sort by decreasing length and total frequency then by alphanumeric order
  the_order = order(sapply(nodes, length),
                    rowSums(nodes_mat),
                    order(order(rownames(nodes_mat), decreasing = TRUE)),
                    decreasing = TRUE)
  nodes_mat = nodes_mat[the_order, , drop = FALSE]
  rownames(nodes_mat) = nodes[the_order]
  
  # Setting the attribute and return
  object@nodes_per_year = nodes_mat
  assign(object_name, object, envir = parent.frame())
  return(invisible(nodes_mat))
})


#' Enumeration of nodes
#' 
#' Identify the separate transactions (considering only their respective items) and compute their number
#'  of items and number of occurrences.
#' The resulting data frame is assigned to the attribute \code{nodes} of \code{object}.
#' 
#' @template method_not_exported
#' 
#' @param object S4 object of class \code{TransactionAnalyzer}.
#' @return Invisible. Data frame of the separate transactions and their characteristics (length and
#'  weight).
#' 
#' @author Gauthier Magnin
#' @aliases list_separate_trx
#' @keywords internal
setMethod(f = "list_separate_trx",
          signature = "TransactionAnalyzer",
          definition =
function(object) {
  
  # Name of the object for internal modification in the parent environment
  object_name = deparse(substitute(object))
  
  # Weights of nodes per year
  nodes_per_year = object@nodes_per_year
  
  # Computation of total frequnecy for each node (= each separate transaction)
  nodes_df = data.frame(frequency = as.integer(unname(rowSums(nodes_per_year))))
  nodes_df$node = lapply(strsplit(rownames(nodes_per_year), 'c\\("|", "|")'),
                         function(node) {
                           if (length(node) > 1) { return(node[-1]) }
                           return(node)
                         })
  
  # Computation of the length of each node and reordering the columns
  nodes_df$length = sapply(nodes_df$node, length)
  nodes_df = nodes_df[, c("node", "length", "frequency")]
  
  # Sort by decreasing length and frequency then by alphanumeric order
  nodes_df = nodes_df[order(nodes_df$length,
                            nodes_df$frequency,
                            order(order(sapply(nodes_df$node, paste0, collapse = "/"), decreasing = TRUE)),
                            decreasing = TRUE), ]
  rownames(nodes_df) = NULL
  
  # Setting the attribute and return
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
#' @template method_not_exported
#' 
#' @param object S4 object of class \code{TransactionAnalyzer}.
#' @param entities Type of entities for which to count links (nodes or patterns).
#'  \code{NODES} or \code{PATTERNS}.
#' @return Invisible. Adjacency matrix: matrix of the number of links between each pair of nodes
#'  or patterns.
#' 
#' @author Gauthier Magnin
#' @aliases count_links
#' @keywords internal
setMethod(f = "count_links",
          signature = "TransactionAnalyzer",
          definition =
function(object, entities) {
  
  # Name of the object for internal modification in the parent environment
  object_name = deparse(substitute(object))
  
  # All nodes or patterns
  if (entities == NODES) to_link = object@nodes$node
  else if (entities == PATTERNS) to_link = object@patterns$pattern
  else stop("entities must be NODES or PATTERNS.")
  
  if (length(to_link) == 0) {
    n_intersections = matrix(NA_integer_, nrow = 0, ncol = 0,
                             dimnames = list(character(0), character(0)))
    
  } else {
    # Counts the number of items in common for each pair of entities to link
    names(to_link) = sapply(to_link, paste0, collapse = "/")
    n_intersections = crossprod(table(utils::stack(to_link)))
    
    # Naming of columns and rows by the corresponding itemsets
    dimnames(n_intersections) = NULL
    colnames(n_intersections) = rownames(n_intersections) = to_link
    
    # Memory size reduction
    class(n_intersections) = "integer"
  }
  
  # Setting the attribute and return
  if (entities == NODES) object@n_links = n_intersections
  else if (entities == PATTERNS) object@p_links = n_intersections
  assign(object_name, object, envir = parent.frame())
  return(invisible(n_intersections))
})


#' Elaboration of links
#' 
#' Identify the links according to items in common between nodes or patterns.
#' The resulting data frame is assigned respectively to the attribute \code{node_links} or
#'  \code{pattern_links} of \code{object}.
#' 
#' @template method_not_exported
#' 
#' @param object S4 object of class \code{TransactionAnalyzer}.
#' @param entities Type of entities for which to elaborate links (nodes or patterns).
#'  \code{NODES} or \code{PATTERNS}.
#' @return Invisible. Data frame detailing the links between pairs of nodes or patterns.
#'  Isolated nodes or patterns (i.e. unrelated to any other entity) appear at the bottom of the data
#'  frame.
#' 
#' @author Delphine Bosson-Rieutort, Gauthier Magnin
#' @aliases search_links
#' @keywords internal
setMethod(f = "search_links",
          signature = "TransactionAnalyzer",
          definition =
function(object, entities) {
  
  # Name of the object for internal modification in the parent environment
  object_name = deparse(substitute(object))
  
  # All nodes or patterns
  if (entities == NODES) {
    to_link = object@nodes$node
    entities_links = object@n_links
  } else if (entities == PATTERNS) {
    to_link = object@patterns$pattern
    entities_links = object@p_links
  } else stop("entities must be NODES or PATTERNS.")
  
  
  # Searching for the indices of the entities to link
  linked_indexes = which(apply(entities_links, 1, function(x) sum(x) != x[parent.frame()$i[]]))
  names(linked_indexes) = NULL
  
  # Matrix of pairs of linked entities
  if (length(linked_indexes) != 0) {
    
    # Using matrix symmetry property to count the number of links
    nb_links = (sum(entities_links != 0) - nrow(entities_links)) / 2
    links = matrix(NA, nrow = nb_links, ncol = ifelse(entities == PATTERNS, 5, 4))
    
    link_counter = 0
    loop_index = 0
    
    # Search for links between each pair of entities to be linked
    for(i in linked_indexes[1:(length(linked_indexes) - 1)]) {
      loop_index = loop_index + 1
      
      for(j in linked_indexes[(loop_index + 1):length(linked_indexes)]) {
        if (entities_links[i, j] != 0) {
          # New link identified
          link_counter = link_counter + 1
          intersection = to_link[[j]][to_link[[j]] %in% to_link[[i]]]
          
          # Entity i, entity j, items in common, nb items in common (, year of appearance of the link)
          if (entities == PATTERNS) {
            links[link_counter, ] = c(i, j,
                                      paste(intersection, collapse = "/"), entities_links[i, j],
                                      max(object@patterns[i, "year"], object@patterns[j, "year"]))
          } else {
            links[link_counter, ] = c(i, j, paste(intersection, collapse = "/"), entities_links[i, j])
          }
        }
      }
    }
  } else {
    # Empty matrix for the following merge (avoids having to test either one)
    links = matrix(NA, nrow = 0, ncol = ifelse(entities == PATTERNS, 5, 4))
  }
  
  
  # Search for isolated entities
  isolated_indexes = which(apply(entities_links, 1,
                                 function(x) sum(x) == x[parent.frame()$i[]]))
  names(isolated_indexes) = NULL
  
  # Matrix of isolated elements completing that of pairs of linked elements
  if (length(isolated_indexes) != 0) {
    no_links = t(sapply(isolated_indexes, entity = entities,
                        function(x, entity) {
                          if (entity == PATTERNS) {
                            return(c(x, x, "", 0, object@patterns[parent.frame()$i[], "year"]))
                          }
                          return(c(x, x, "", 0))
                        }))
  } else {
    # Empty matrix for the following merge (avoids having to test either one)
    no_links = matrix(NA, nrow = 0, ncol = ifelse(entities == PATTERNS, 5, 4))
  }
  
  
  # Merge the lists into a single data frame
  merged = as.data.frame(rbind(links, no_links), stringsAsFactors = FALSE)
  
  # Assigning column names and restoring types
  if (entities == PATTERNS) {
    colnames(merged) = c("endpoint.1", "endpoint.2", "items", "weight", "year")
    class(merged$year) = "integer"
  } else {
    colnames(merged) = c("endpoint.1", "endpoint.2", "items", "weight")
  }
  rownames(merged) = NULL
  class(merged$endpoint.1) = class(merged$endpoint.2) = class(merged$weight) = "integer"
  
  
  # Setting the attribute and return
  if (entities == NODES) object@node_links = merged
  else if (entities == PATTERNS) object@pattern_links = merged
  assign(object_name, object, envir = parent.frame())
  return(invisible(object@node_links))
})



#### Computation methods used for the construction of the patterns ####

#' Enumeration of patterns
#' 
#' Identify the patterns generated from the transactions.
#' The resulting data frame is assigned to the attribute \code{patterns} of \code{object}.
#' 
#' @template method_not_exported
#' 
#' @param object S4 object of class \code{TransactionAnalyzer}.
#' @param target Type of patterns to enumerate. One of \code{"frequent itemsets"},
#'  \code{"closed frequent itemsets"}, \code{"maximally frequent itemsets"}.
#' @param min_frequency Minimum number of occurrences of a pattern to be considered as frequent.
#' @param min_length Minimum number of items that a pattern must have to be kept when mining patterns.
#' @param max_length Maximum number of items that a pattern must have to be kept when mining patterns.
#'  The default \code{Inf} corresponds to a pattern search without maximum size limit.
#' @param arules If \code{TRUE}, patterns are returned as an object of class
#'  \code{\link[arules:itemsets-class]{itemsets}} from the package \code{arules}.
#' @return Invisible. Object of class \code{itemsets} or data frame in which a row is an association
#'  between a pattern and its support and frequency in the set of transactions (according to the
#'  argument \code{arules}).
#' 
#' @author Gauthier Magnin
#' @aliases list_separate_patterns
#' @keywords internal
setMethod(f = "list_separate_patterns",
          signature = "TransactionAnalyzer",
          definition =
function(object, target, min_frequency = 1, min_length = 1, max_length = Inf, arules = FALSE) {
  
  # Name of the object for internal modification in the parent environment
  object_name = deparse(substitute(object))
  
  
  # Conversion of transactions to arules transactions: one row per transaction, one column per item
  transact = methods::as(object@transactions, "transactions")
  
  # Preparing parameters for mining itemsets
  params = list(supp   = min_frequency / dim(transact)[1],
                minlen = min_length,
                maxlen = if (max_length == Inf) max(dim(transact)[2], min_length) else max_length,
                target = target)
  
  # Mining for itemsets
  if (params$supp <= 1) {
    result = arules::eclat(transact, parameter = params, control = list(verbose = FALSE))
    patterns_df = methods::as(result, "data.frame")
  } else {
    patterns_df = data.frame()
  }
  
  # Final data frame of patterns (may be empty if no patterns have been extracted)
  if (nrow(patterns_df) != 0) {
    
    # Conversion of the patterns to a list of vectors and renaming columns
    patterns_df$items = vector_notation(patterns_df$items)
    colnames(patterns_df) = c("pattern", "support", "frequency")
    
    # Sorting by frequency and renaming rows according to the new order
    patterns_df = patterns_df[order(patterns_df$frequency, decreasing = TRUE), ]
    rownames(patterns_df) = seq(nrow(patterns_df))
    
  } else {
    patterns_df = data.frame(pattern = character(0),
                             support = numeric(0),
                             frequency = integer(0))
    patterns_df$pattern = list()
  }
  
  # Setting the attribute and return
  object@patterns = patterns_df
  assign(object_name, object, envir = parent.frame())
  if (arules) return(invisible(result))
  return(invisible(patterns_df))
})


#' Linking nodes to patterns
#' 
#' Associate each separate transaction (i.e. each node) with the patterns included in it.
#' The resulting matrix is assigned to the attribute \code{nodes_patterns} of \code{object}.
#' 
#' @template method_not_exported
#' 
#' @param object S4 object of class \code{TransactionAnalyzer}.
#' @return Invisible. Logical matrix in which rows correspond to nodes and columns correspond to
#'  patterns. A value of \code{TRUE} means the pattern is included in the node.
#' 
#' @author Gauthier Magnin
#' @aliases list_patterns_by_node
#' @keywords internal
setMethod(f = "list_patterns_by_node",
          signature = "TransactionAnalyzer",
          definition =
function(object) {
  
  # Name of the object for internal modification in the parent environment
  object_name = deparse(substitute(object))
  
  
  # For each pattern and each node, finds if the pattern is included in the node
  associations = sapply(object@patterns$pattern,
                        function(pattern) {
                          sapply(object@nodes$node, pattern = pattern,
                                 function(node, pattern) {
                                   return(all(pattern %in% node))
                                 })
                        })
  if (!is.matrix(associations)) {
    associations = matrix(associations,
                          nrow = length(object@nodes$node),
                          ncol = length(object@patterns$pattern))
  }
  
  # Renaming rows and columns
  rownames(associations) = object@nodes$node
  colnames(associations) = object@patterns$pattern
  
  # Setting the attribute and return
  object@nodes_patterns = associations
  assign(object_name, object, envir = parent.frame())
  return(invisible(object@nodes_patterns))
})


#' Enumeration of patterns per year
#' 
#' Count the number of occurrences of each pattern per year.
#' The resulting matrix is assigned to the attribute \code{patterns_per_year} of \code{object}.
#' 
#' @template method_not_exported
#' 
#' @param object S4 object of class \code{TransactionAnalyzer}.
#' @return Invisible. Matrix of the number of occurrences of each pattern in the transactions, per year.
#'  The rows correspond to the patterns. The columns correspond to the years.
#' 
#' @author Gauthier Magnin
#' @aliases list_patterns_per_year
#' @keywords internal
setMethod(f = "list_patterns_per_year",
          signature = "TransactionAnalyzer",
          definition =
function(object) {
  
  # Name of the object for internal modification in the parent environment
  object_name = deparse(substitute(object))
  
  if (length(object@patterns$pattern) == 0) {
    # If no patterns, no rows
    ppy = matrix(NA_integer_, nrow = 0, ncol = ncol(object@nodes_per_year),
                 dimnames = list(character(0), colnames(object@nodes_per_year)))
  } else {
    # For each pattern, selection of the associated nodes them sum of their frequencies per year
    frequencies = lapply(seq_along(object@patterns$pattern), function(p) {
      nodes_names = rownames(object@nodes_patterns)[object@nodes_patterns[, p]]
      return(colSums(object@nodes_per_year[nodes_names, , drop = FALSE]))
    })
    
    # Matrix of pattern frequencies per year
    ppy = t(sapply(frequencies, as.integer))
    if (ncol(object@nodes_per_year) == 1) ppy = t(ppy)
    rownames(ppy) = object@patterns$pattern
    colnames(ppy) = colnames(object@nodes_per_year)
  }
  
  # Setting the attribute and return
  object@patterns_per_year = ppy
  assign(object_name, object, envir = parent.frame())
  return(invisible(ppy))
})


#' Computation of pattern characteristics
#' 
#' Compute the characteristics of the patterns (length, weight, specificity, dynamic status).
#' The resulting data frame is assigned to the attribute `patterns` of `object`.
#' 
#' @details
#' The length of a pattern is the number of items composing it.
#' 
#' The weight of a pattern is the number of nodes containing it.
#' 
#' The specificity of a pattern corresponds to the nature of the pattern of being specific of a
#'  particular combination or ubiquitous and allowing the formation of numerous combinations (with
#'  regard to the transactions).
#' 
#' The dynamic status is one of persistent, declining, emergent or latent.
#' 
#' \loadmathjax
#' 
#' @details ## Specificity
#' @template specificity_computation
#' 
#' @details ## Dynamic status
#' @template dynamic_status_classification_specific
#' 
#' @template method_not_exported
#' 
#' @param object S4 object of class `TransactionAnalyzer`.
#' @return Invisible. Data frame in which each row is an association between a pattern and its
#'  characteristics.
#' 
#' @author Gauthier Magnin
#' @references Bosson-Rieutort D, de Gaudemaris R, Bicout DJ (2018).
#'             The spectrosome of occupational health problems. *PLoS ONE* 13(1): e0190196.
#'             <https://doi.org/10.1371/journal.pone.0190196>.
#' @seealso [`compute_specificity`], [`dynamic_status`],
#' @aliases compute_patterns_characteristics
#' @md
#' @keywords internal
setMethod(f = "compute_patterns_characteristics",
          signature = "TransactionAnalyzer",
          definition =
function(object) {
  
  # Name of the object for internal modification in the parent environment
  object_name = deparse(substitute(object))
  
  # Association of new characteristics with the patterns
  object@patterns$weight = vapply(seq_along(object@patterns$pattern),
                                  function(p) sum(object@nodes_patterns[, p]),
                                  integer(1))
  object@patterns$length = lengths(object@patterns$pattern)
  object@patterns$year = vapply(seq_len(nrow(object@patterns_per_year)),
                                function(i) {
                                  # Year of appearance of the pattern
                                  x = object@patterns_per_year[i, ]
                                  return(as.integer(names(x[x > 0][1])))
                                },
                                integer(1))
  
  # Computation of the specificity and dynamic status of each pattern
  object@patterns$specificity = compute_specificity(object, object@patterns$pattern,
                                                    object@patterns$frequency, object@patterns$weight)
  object@patterns$status = dynamic_status(object, object@patterns$pattern)$res$status
  
  # Changing the column order
  object@patterns = object@patterns[, c("pattern", "year", "length", "support",
                                        "frequency", "weight", "specificity", "status")]
  
  # Setting the attribute and return
  assign(object_name, object, envir = parent.frame())
  return(invisible(object@patterns))
})


#' Specificity computation
#' 
#' Compute the specificity of the information conveyed by each pattern.
#' The specitificity corresponds to the nature of a pattern of being specific of a particular
#'  combination or ubiquitous and allowing the formation of numerous combinations (with regard to the
#'  transactions).
#' 
#' \loadmathjax
#' @template specificity_computation
#' 
#' @template method_not_exported
#' 
#' @param object S4 object of class `TransactionAnalyzer`.
#' @param patterns Patterns whose specificity is to be computed.
#' @param frequencies Vector of frequencies associated with the patterns contained in `patterns`.
#' @param weights Vector of weights associated with the patterns contained in `patterns`.
#' @return Vector containing the specificity of each pattern contained in `patterns`.
#' 
#' @author Gauthier Magnin
#' @references Bosson-Rieutort D, de Gaudemaris R, Bicout DJ (2018).
#'             The spectrosome of occupational health problems. *PLoS ONE* 13(1): e0190196.
#'             <https://doi.org/10.1371/journal.pone.0190196>.
#' @aliases compute_specificity
#' @md
#' @keywords internal
setMethod(f = "compute_specificity",
          signature = "TransactionAnalyzer",
          definition =
function(object, patterns, frequencies, weights) {
  
  # Renaming variables to match the formula
  w = weights
  f = frequencies
  h = numeric(length(patterns))
  specificity = rep(NA, length(patterns))
  
  # Indices of patterns in the matrix linking patterns to nodes
  p_indexes = match(as.character(patterns), colnames(object@nodes_patterns))
  
  
  # Presence of the pattern in a single node -> specificity of 1
  specificity[w == 1] = 1
  
  # For each other pattern
  for (p in seq_along(patterns)[w != 1]) {
    
    # Finding frequencies of nodes that containg the pattern
    a = object@nodes$frequency[object@nodes_patterns[, p_indexes[p]]]
    
    # Specificity of 0 if all the nodes have the same frequency; or computation according to the formula
    if (length(unique(a)) == 1) specificity[p] = 0
    else h[p] = -1 * sum(a / f[p] * log(a / f[p]))
  }
  
  # Computation of the specificity of the patterns for which it has not been determined yet
  to_compute = is.na(specificity)
  h_max = log(w[to_compute])
  h_min = log(f / (f - w + 1)) + (w - 1) / f * log(f - w + 1)
  specificity[is.na(specificity)] = (h_max - h[to_compute]) / (h_max - h_min[to_compute])
  
  return(specificity)
})


#' Validation of RI computation parameters
#' 
#' Check the validity of the values of the parameters given for the computation of reporting indexes.
#' Stop the execution and print an error message if they are not usable.
#' Adapt their values if they match the special values (\code{NULL} and \code{Inf} respectively).
#' 
#' @template method_not_exported
#' 
#' @inheritParams compute_reporting_indexes,TransactionAnalyzer-method
#' @param object S4 object of class \code{TransactionAnalyzer}.
#' @return Vector containing the final values of \code{end} and \code{period}.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{compute_reporting_indexes}}, \code{\link{compute_reporting_indexes_limits}}.
#' @aliases check_RI_params
#' @keywords internal
setMethod(f = "check_RI_params",
          signature = "TransactionAnalyzer",
          definition =
function(object, end, period) {
  
  # Maximum and minimum years
  max_year = as.integer(rev(colnames(object@nodes_per_year))[1])
  min_year = as.integer(colnames(object@nodes_per_year)[1])
  
  # Validation of the parameter defining the end of the period on which to perform the computations
  if (is.null(end)) end = max_year
  else if (end > max_year || end < min_year) {
    stop("end must not be less than the year of the oldest transaction (", min_year, ")",
         " nor greater than the year of the most recent one (", max_year, ").")
  }
  else end = as.integer(end)
  
  # Validation of the parameter defining the length of the computation period
  if (is.infinite(period)) period = end - min_year + 1L
  else if (period < 1 || period > end - min_year + 1) {
    stop("period must be greater than 0",
         " and end - period + 1 must not be less than the year of the oldest transaction.")
  }
  else period = as.integer(period)
  
  return(c(end = end, period = period))
})


#' Computation of the Reporting Index (RI)
#' 
#' Compute the reporting index of each pattern for a given period.
#' This index provides information on the proportion and importance of the occurrences of a pattern,
#'  taking into account the occurrences of the other patterns.
#'  
#' @details
#' \loadmathjax
#' The reporting index of the pattern \mjseqn{p} is given by:
#'  \mjdeqn{RI_p(t_1,t_0) = \frac{\sum_{t = t_0}^{t_1} F_{p,t}}{\sum_{q \in P} \sum_{t = t_0}^{t_1} F_{q,t}}}{RI_p(t_1,t_0) = sum F_pt from t = t_0 to t_1 / sum F_qt for q in P and from t = t_0 to t_1}
#' where \mjseqn{P} is the set of patterns, \mjeqn{F_{p,t}}{F_pt} is the frequency of the pattern
#'  \mjseqn{p} in the transactions of the year \mjseqn{t}, \mjseqn{t_0} and \mjseqn{t_1} are the first
#'  and last years defining the period on which to compute the reporting index.
#' 
#' @template method_not_exported
#'  
#' @param object S4 object of class \code{TransactionAnalyzer}.
#' @param patterns Patterns whose reporting indexes are to be computed.
#' @param end Year of end of the period, i.e. the date on which to characterize the patterns.
#'  \code{NULL} specifies that the characterization must be done in relation to the last year covered
#'  by the transactions.
#' @param period Time interval over which to compute the reporting indexes (number of years).
#'  For example, if \code{end = 2019} and \code{period = 9} then the computation is made over the
#'  period [2011 - 2019].
#'  
#'  \code{Inf} specifies that the period considered covers the interval starting in the year of the
#'  oldest transaction and ending in the year \code{end}.
#' @return Vector containing the reporting index of each pattern contained in \code{patterns}.
#' 
#' @author Gauthier Magnin
#' @references Bosson-Rieutort D, de Gaudemaris R, Bicout DJ (2018).
#'             The spectrosome of occupational health problems. \emph{PLoS ONE} 13(1): e0190196.
#'             \url{https://doi.org/10.1371/journal.pone.0190196}.
#' @seealso \code{\link{compute_reporting_indexes_limits}}, \code{\link{compute_xi_threshold}},
#'          \code{\link{compute_ri_threshold}}.
#' @aliases compute_reporting_indexes
#' @keywords internal
setMethod(f = "compute_reporting_indexes",
          signature = "TransactionAnalyzer",
          definition =
function(object, patterns, end = NULL, period = Inf) {
  
  # Adjusted parameter values
  params = check_RI_params(object, end, period)
  t1 = params["end"]
  
  # Period start year = end year - number of years considered + 1
  t0 = t1 - params["period"] + 1
  
  # Frequency of the pattern over the period / sum of the frequencies of all patterns over the period
  p_frequencies = unname(apply(
    object@patterns_per_year[
      match(as.character(patterns), rownames(object@patterns_per_year)),
      (as.numeric(colnames(object@patterns_per_year)) <= t1)
        & (as.numeric(colnames(object@patterns_per_year)) >= t0),
      drop = FALSE],
    1, sum))
  return(p_frequencies / sum(p_frequencies))
})


#' Computation of RI at temporal limits 
#' 
#' Compute the reporting indexes at the temporal limits used to characterize the patterns.
#' The first ones correspond to the reporting indexes computed over the period defined by the arguments
#'  \code{end} and \code{overall_period}.
#' The second ones correspond to the reporting indexes computed over the period defined by the arguments
#'  \code{end} and \code{recent_period}.
#' 
#' @details
#' \loadmathjax
#' The reporting index of the pattern \mjseqn{p} at the temporal limit \mjseqn{l} is given by:
#'  \mjdeqn{RI_{l,p} = RI_p(t_1, t_1 - l + 1)}{RI_lp = RI_p(t_1, t_1 - l + 1)}
#' where \mjseqn{t_1} is the year of end of the period to consider and \mjseqn{RI_p(t_1,t_0)}
#'  is the reporting index of the pattern \mjseqn{p} given by:
#'  \mjdeqn{RI_p(t_1,t_0) = \frac{\sum_{t = t_0}^{t_1} F_{p,t}}{\sum_{q \in P} \sum_{t = t_0}^{t_1} F_{q,t}}}{RI_p(t_1,t_0) = sum F_pt from t = t_0 to t_1 / sum F_qt for q in P and from t = t_0 to t_1}
#' where \mjseqn{P} is the set of patterns, \mjeqn{F_{p,t}}{F_pt} is the frequency of the pattern
#'  \mjseqn{p} in the transactions of the year \mjseqn{t}, \mjseqn{t_0} and \mjseqn{t_1} are the first and
#'  last years defining the period on which to compute the reporting index.
#' 
#' @template method_not_exported
#' 
#' @param object S4 object of class \code{TransactionAnalyzer}.
#' @param patterns Patterns whose limits are to be computed.
#' @param end Year of end of the periods, i.e. the date on which to characterize the patterns.
#'  \code{NULL} specifies that the characterization must be done in relation to the last year covered
#'  by the transactions.
#' @param overall_period Time interval over which to compute the overall reporting indexes (number of
#'  years). For example, if \code{end = 2019} and \code{overall_period = 9} then the computation is made
#'  over the period [2011 - 2019].
#'  
#'  \code{Inf} specifies that the period considered covers the interval starting in the year of the
#'  oldest transaction and ending in the year \code{end}.
#' @param recent_period Shorter time interval over which to compute the second reporting indexes
#'  (number of years). For example, if \code{end = 2019} and \code{recent_period = 2} then the
#'  computation is made over the period [2018 - 2019].
#' @return Matrix containing the two reporting indexes of each pattern contained in \code{patterns}.
#' 
#' @author Gauthier Magnin
#' @references Bosson-Rieutort D, de Gaudemaris R, Bicout DJ (2018).
#'             The spectrosome of occupational health problems. \emph{PLoS ONE} 13(1): e0190196.
#'             \url{https://doi.org/10.1371/journal.pone.0190196}.
#' @seealso \code{\link{compute_reporting_indexes}}, \code{\link{compute_xi_threshold}},
#'          \code{\link{compute_ri_threshold}}.
#' @aliases compute_reporting_indexes_limits
#' @keywords internal
setMethod(f = "compute_reporting_indexes_limits",
          signature = "TransactionAnalyzer",
          definition =
function(object, patterns, end = NULL, overall_period = Inf, recent_period = object["status_limit"]) {
            
  if (recent_period >= overall_period) stop("recent_period must be lower than overall_period.")
  
  ri_overall = compute_reporting_indexes(object, patterns, end, overall_period)
  ri_recent = compute_reporting_indexes(object, patterns, end, recent_period)
  
  return(matrix(c(ri_overall, ri_recent),
                ncol = 2, byrow = FALSE, dimnames = list(NULL, c("RI.overall", "RI.recent"))))
})


#' Computation of threshold xi
#' 
#' Compute the number of patterns allowing to explain the main part of the reporting indexes.
#' 
#' @details
#' \loadmathjax
#' The threshold \mjseqn{\xi} is given by:
#'  \mjdeqn{\xi = \left\lceil \frac{1}{\sum_{p \in P} RI_p(t_1,t_0)^2} \right\rceil}{xi = ceiling(1 / sum(RI_p(t_1,t_0)^2) for p in P)}
#' where \mjseqn{RI_p(t_1,t_0)} is the reporting index of the pattern \mjseqn{p} given by:
#'  \mjdeqn{RI_p(t_1,t_0) = \frac{\sum_{t = t_0}^{t_1} F_{p,t}}{\sum_{q \in P} \sum_{t = t_0}^{t_1} F_{q,t}}}{RI_p(t_1,t_0) = sum F_pt from t = t_0 to t_1 / sum F_qt for q in P and from t = t_0 to t_1}
#' where \mjseqn{P} is the set of patterns, \mjeqn{F_{p,t}}{F_pt} is the frequency of the pattern
#'  \mjseqn{p} in the transactions of the year \mjseqn{t}, \mjseqn{t_0} and \mjseqn{t_1} are the first and
#'  last years defining the period on which to compute the reporting index.
#' 
#' @template method_not_exported
#' 
#' @param object S4 object of class \code{TransactionAnalyzer}.
#' @param reporting_indexes Reporting indexes associated with the patterns.
#' @return Computed threshold.
#' 
#' @author Gauthier Magnin
#' @references Bosson-Rieutort D, de Gaudemaris R, Bicout DJ (2018).
#'             The spectrosome of occupational health problems. \emph{PLoS ONE} 13(1): e0190196.
#'             \url{https://doi.org/10.1371/journal.pone.0190196}.
#' @seealso \code{\link{compute_reporting_indexes}}, \code{\link{compute_reporting_indexes_limits}},
#'          \code{\link{compute_ri_threshold}}.
#' @aliases compute_xi_threshold
#' @keywords internal
setMethod(f = "compute_xi_threshold",
          signature = "TransactionAnalyzer",
          definition =
function(object, reporting_indexes) {
   return(as.integer(ceiling(1 / sum(reporting_indexes ^ 2))))
})


#' RI threshold computation
#' 
#' \loadmathjax
#' Compute the limit value separating two dynamic profiles with respect to the reporting indexes.
#' The patterns are ordered in descending order of their reporting index value and separated by
#'  the threshold \mjseqn{\xi}.
#'  
#' @inherit compute_xi_threshold,TransactionAnalyzer-method details
#' 
#' @template method_not_exported
#' 
#' @param object S4 object of class \code{TransactionAnalyzer}.
#' @param reporting_indexes Reporting indexes associated with the patterns.
#' @param xi Number of patterns to consider before setting the RI threshold.
#'  Is computed if \code{NULL}.
#' @return Computed threshold.
#' 
#' @author Gauthier Magnin
#' @references Bosson-Rieutort D, de Gaudemaris R, Bicout DJ (2018).
#'             The spectrosome of occupational health problems. \emph{PLoS ONE} 13(1): e0190196.
#'             \url{https://doi.org/10.1371/journal.pone.0190196}.
#' @seealso \code{\link{compute_reporting_indexes}}, \code{\link{compute_reporting_indexes_limits}},
#'          \code{\link{compute_xi_threshold}}.
#' @aliases compute_ri_threshold
#' @keywords internal
setMethod(f = "compute_ri_threshold",
          signature = "TransactionAnalyzer",
          definition =
function(object, reporting_indexes, xi = NULL) {
  
  # Computation of the treshold xi if not provided as a parameter
  if (is.null(xi)) xi = compute_xi_threshold(object, reporting_indexes)
  
  # Extraction of the value of RI of the xi_th element (ordered by RI)
  return(sort(reporting_indexes, decreasing = TRUE)[xi])
})


#' Dynamic status classification
#' 
#' Define the dynamic status of each pattern: persistent, declining, emergent or latent.
#'  Compute the reporting indexes of the patterns at two temporal limits and compare them to two
#'  thresholds.
#' 
#' \loadmathjax
#' @template dynamic_status_classification_generic
#' 
#' @param object S4 object of class `TransactionAnalyzer`.
#' @param patterns Patterns whose dynamic status are to be defined.
#'  Any subset of `object["patterns"]$pattern`.
#'  
#'  `"patterns"` and `"p"` are special values for `object["patterns"]$pattern`.
#' @param end Year of end of the periods, i.e. the date on which to characterize the patterns.
#'  `NULL` specifies that the characterization must be done in relation to the last year covered
#'  by the transactions.
#' @param overall_period Time interval over which to compute the overall reporting indexes (number of
#'  years). For example, if `end = 2019` and `overall_period = 9` then the computation is made over the
#'  period \[2011 - 2019\].
#'  
#'  `Inf` specifies that the period considered covers the interval starting in the year of the
#'  oldest transaction and ending in the year `end`.
#' @param recent_period Shorter time interval over which to compute the second reporting indexes
#'  (number of years). For example, if `end = 2019` and `recent_period = 2` then the computation is made
#'  over the period \[2018 - 2019\].
#' @return \describe{
#'  \item{`res`}{Data frame containing the dynamic status of each pattern contained in `patterns` and
#'               the results of intermediate computations.}
#'  \item{`thresholds`}{Matrix containing the \mjseqn{\xi} (xi) and RI thresholds used to classify the
#'                      patterns.}
#' }
#' 
#' @author Gauthier Magnin
#' @references Bosson-Rieutort D, de Gaudemaris R, Bicout DJ (2018).
#'             The spectrosome of occupational health problems. *PLoS ONE* 13(1): e0190196.
#'             <https://doi.org/10.1371/journal.pone.0190196>.
#' 
#' @examples
#' dynamic_status(TA_instance, "patterns")
#' dynamic_status(TA_instance, TA_instance["patterns"]$pattern[1:12])
#' 
#' @aliases dynamic_status
#' @md
#' @export
setMethod(f = "dynamic_status",
          signature = "TransactionAnalyzer",
          definition =
function(object, patterns, end = NULL, overall_period = Inf, recent_period = object["status_limit"]) {
  
  check_init(object, PATTERNS)
  patterns = get_tnp_itemsets(object, patterns, entities = PATTERNS)
  
  # If empty list
  if (length(patterns) == 0) {
    return(list(res = data.frame(RI.overall = numeric(0),
                                 is.above.threshold.1 = logical(0),
                                 RI.recent = numeric(0),
                                 is.above.threshold.2 = logical(0),
                                 status = character(0),
                                 stringsAsFactors = FALSE),
                thresholds = matrix(NA_real_,
                                    ncol = 2, nrow = 2,
                                    dimnames = list(c("xi", "RI"), c("threshold.1", "threshold.2")))))
  }
  
  # Compputation of the limits and associated thresholds
  ri_limits = compute_reporting_indexes_limits(object, patterns, end, overall_period, recent_period)
  
  ri_names = colnames(ri_limits)
  xi = ri_thresholds = numeric(2)
  for (i in seq_along(ri_names)) {
    xi[i] = compute_xi_threshold(object, ri_limits[, ri_names[i]])
    ri_thresholds[i] = compute_ri_threshold(object, ri_limits[, ri_names[i]], xi[i])
  }
  names(xi) = ri_names
  names(ri_thresholds) = ri_names
  
  # Highlighting RI values above the thresholds
  substantial_overall = ri_limits[, "RI.overall"] >= ri_thresholds["RI.overall"]
  substantial_recent = ri_limits[, "RI.recent"] >= ri_thresholds["RI.recent"]
  
  # Interpretation
  status = character(length(patterns))
  status[( substantial_overall &  substantial_recent)] = STATUS_PERSISTENT
  status[( substantial_overall & !substantial_recent)] = STATUS_DECLINING
  status[(!substantial_overall &  substantial_recent)] = STATUS_EMERGENT
  status[(!substantial_overall & !substantial_recent)] = STATUS_LATENT
  
  return(list(res = data.frame(RI.overall = ri_limits[, "RI.overall"],
                               is.above.threshold.1 = substantial_overall,
                               RI.recent = ri_limits[, "RI.recent"],
                               is.above.threshold.2 = substantial_recent,
                               status = status,
                               stringsAsFactors = FALSE),
              thresholds = matrix(c(xi, ri_thresholds),
                                  ncol = 2, nrow = 2, byrow = TRUE,
                                  dimnames = list(c("xi", "RI"), c("threshold.1", "threshold.2")))))
})



#### Methods for creating spectrum charts ####

#' Pattern spectrum
#' 
#' Plot a spectrum chart: bar chart of the given patterns showing their characteristics.
#'  It can be automatically saved as a PDF file.
#' 
#' @details
#' The patterns are sorted according to their specificities (desc.), status (in order of
#'  \code{object["status_colors"]}, default is  \code{"Persistent"}, \code{"Declining"},
#'  \code{"Emergent"}, \code{"Latent"}), frequencies (desc.) and lengths (asc.).
#'  If two patterns have the same characteristics concerning these ones, they are ordered relative to
#'  each other in the order they are given.
#' 
#' If the argument \code{name} is not \code{NULL}, the chart is plotted in a PDF file of A4 landscape
#'  paper size. If it is \code{NULL}, the chart is plotted in the active device.
#' 
#' @param object S4 object of class \code{TransactionAnalyzer}.
#' @param pc Data frame of \strong{p}atterns and their \strong{c}haracteristics. Patterns whose spectrum
#'  is to be plotted. Any subset of \code{object["patterns"]}.
#'  
#'  \code{"patterns"} and \code{"p"} are special values for \code{object["patterns"]}.
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
#' @param path Path of the directory in which to save the chart as a PDF file. Default is the working
#'  directory.
#' @param name Name of the PDF file in which to save the chart. To be ignored to plot the chart in the
#'  active device.
#' @return Data frame of the patterns and characteristics used, associated with the identifiers visible
#'  on the chart and with the distribution of pattern frequencies between complex and simple
#'  transactions.
#' 
#' @author Delphine Bosson-Rieutort, Gauthier Magnin
#' @references Bosson-Rieutort D, de Gaudemaris R, Bicout DJ (2018).
#'             The spectrosome of occupational health problems. \emph{PLoS ONE} 13(1): e0190196.
#'             \url{https://doi.org/10.1371/journal.pone.0190196}.
#' @seealso \code{\link{frequency_by_complexity}}.
#' 
#' @examples
#' spectrum_1 <- spectrum_chart(TA_instance, "patterns")
#' spectrum_2 <- spectrum_chart(TA_instance, TA_instance["patterns"][1:15, ])
#' 
#' spectrum_1 <- spectrum_chart(TA_instance, "patterns",
#'                              path = getwd(), name = "spectrum_of_patterns")
#' 
#' @aliases spectrum_chart
#' @export
setMethod(f = "spectrum_chart",
          signature = "TransactionAnalyzer",
          definition =
function(object, pc, identifiers = "original", sort = TRUE,
         title = "Spectrum of patterns", path = NULL, name = NULL) {
  
  # Getting patterns
  check_init(object, PATTERNS)
  pc = get_tnp(object, pc, PATTERNS)
  
  check_param(identifiers, values = c("original", "new"))
  
  # Break down of pattern frequencies accordign to the type of nodes (simple or complex)
  frequencies = frequency_by_complexity(object, pc$pattern)
  
  # Sorting patterns according to specificity, status, frequency, length
  if (sort) {
    sorting_vector = order(1 - pc$specificity,
                           match(pc$status, names(object@status_colors)),
                           max(pc$frequency) - pc$frequency,
                           pc$length)
    
    pc = pc[sorting_vector, ]
    frequencies = frequencies[sorting_vector, ]
  }
  
  # Assigning identifiers to patterns
  if (identifiers == "new") pc$ID = seq(nrow(pc))
  else pc$ID = as.integer(rownames(pc))
  
  # Chart plotting (in the active device or in a PDF file)
  if (!is.null(name)) {
    grDevices::pdf(paste0(turn_into_path(path), check_extension(name, "pdf")),
                   14, 10, paper = "a4r", pointsize = 11)
    plot_spectrum_chart(object, pc, frequencies, title)
    grDevices::dev.off()
  } else {
    plot_spectrum_chart(object, pc, frequencies, title)
  }
  
  # Patterns and characteristics (and break down of the frequency), ordered by ID (replaced in 1st column)
  pc = cbind(pc, f.complex = frequencies[, "complex"], f.simple = frequencies[, "simple"])
  return(pc[order(pc$ID), c("ID", "pattern", "year", "length", "support",
                            "frequency", "f.complex", "f.simple",
                            "weight", "specificity", "status")])
})


#' Pattern spectrum plotting
#' 
#' Plot a spectrum chart.
#' 
#' @template method_not_exported
#' 
#' @param object S4 object of class \code{TransactionAnalyzer}.
#' @param pc Data frame of \strong{p}atterns and their \strong{c}haracteristics. Patterns whose spectrum
#'  is to be plotted. Any subset of \code{object["patterns"]}.
#' @param frequencies Two-column matrix containing, for each pattern, its frequency related to complex
#'  nodes and its frequency related to simple nodes.
#' @param title Chart title.
#' 
#' @author Delphine Bosson-Rieutort, Gauthier Magnin
#' @references Bosson-Rieutort D, de Gaudemaris R, Bicout DJ (2018).
#'             The spectrosome of occupational health problems. \emph{PLoS ONE} 13(1): e0190196.
#'             \url{https://doi.org/10.1371/journal.pone.0190196}.
#' @seealso \code{\link{spectrum_chart}}, \code{\link{frequency_by_complexity}},
#'          \code{\link{pattern_node_characteristics}}.
#' 
#' @aliases plot_spectrum_chart
#' @keywords internal
setMethod(f = "plot_spectrum_chart",
          signature = "TransactionAnalyzer",
          definition =
function(object, pc, frequencies, title = "Spectrum of patterns") {
  
  graphics::par(mar = c(6.3, 5.0, 2.0+1.4, 5.0))
  
  cex_legend = 0.85
  cex_length = 0.75
  cex_axis = 1.05
  cex_lab = 1.05
  cex_id = 0.9
  
  ## Bar chart relating to the frequency
  
  # Setting colors of the bars
  bars_colors = object@status_colors[pc$status]
  
  # Margin between bars and axes on left and right
  x_margin = 0.03 * nrow(pc)
  
  # Bar chart according to pattern frequencies
  bar_plot = graphics::barplot(t(frequencies),
                               col = NA, space = 0, lwd = 2,
                               xlim = c(-x_margin, nrow(pc) + x_margin), xaxs = "i",
                               ylim = c(0, max(pc$frequency)), yaxt = "n",
                               xlab = "Pattern IDs", ylab = "Frequency",
                               names.arg = pc$ID, las = 3, font.axis = 2,
                               cex.lab = cex_lab, cex.names = cex_id)
  bar_width_2 = diff(bar_plot[1:2]) / 2
  
  # Axis on the left: removal of floating point numbers, orientation according to the number and
  # possible display of an additional tick to delimit the axis at the top of the chart
  ticks = unique(trunc(graphics::axTicks(2)))
  if (max(ticks) < max(pc$frequency)) {
     ticks = append(ticks, max(pc$frequency))
  }
  graphics::axis(2, lwd = 2, cex.axis = cex_axis,
                 at = ticks, las = if (any(ticks >= 10)) 3 else 1)
  
  # Bar coloring
  for (i in seq(nrow(frequencies))) {
    y = c(0, cumsum(c(frequencies[i, ])))
    graphics::rect(bar_plot[i] - bar_width_2, y[ - length(y)],
                   bar_plot[i] + bar_width_2, y[ - 1],
                   col = bars_colors[i], density = c(-1, 15), border = "black")
  }
  
  
  ## Line chart relating to the specificity
  graphics::par(new = TRUE)
  
  # Specificity line and threshold
  graphics::plot(x = seq(0.5, nrow(pc) - 0.5),
                 y = pc$specificity,
                 lwd = 3, type = "b", col = "black", pch = 20, xpd = TRUE,
                 bty = "n", xlab = "", ylab = "", main = "",
                 xlim = c(-x_margin, nrow(pc) + x_margin), xaxt = "n", xaxs = "i",
                 ylim = c(0, 1), yaxt = "n", yaxs = "i")
  graphics::segments(x0 = 0, y0 = 0.5,
                     x1 = nrow(pc) * (1 + x_margin),
                     lwd = 0.5, lty = "dotted")
  
  # Axis on the right
  graphics::axis(4, yaxp = c(0, 1, 5), lwd = 2, cex.axis = cex_axis)
  graphics::mtext("Specificity", side = 4, line = 3, cex = cex_lab, at = 0.5)
  
  
  ## Text relating to pattern lengths (above the line)
  # Change of coordinate system due to the change of graph (bar -> line)
  new_y = pc$frequency * 1 / max(pc$frequency)
  TeachingDemos::shadowtext(bar_plot, new_y, utils::as.roman(pc$length),
                            col = "black", bg = "white", cex = cex_length,
                            pos = 3, offset = cex_length, xpd = TRUE)
  
  
  ## Legends and titles
  
  # Side, bottom and top margins
  w_margin = convert_gunits(graphics::par("mai")[4]/10, "inches", "user", "w")  # = 0.5 mar (line)
  b_margin = convert_gunits(w_margin, "user", dim = "w", rotation = TRUE)       # = 0.5 mar
  t_margin = convert_gunits(graphics::par("mai")[3]/1.7, "inches", "user", "h") # = 2.0 mar
  
  # Status legend
  status_legend = graphics::legend("top", plot = FALSE,
                                   horiz = TRUE, pch = 15, cex = cex_legend,
                                   col = object@status_colors, legend = names(object@status_colors))
  
  graphics::legend(x = fig_in_usr_coords(2) - w_margin - status_legend$rect$w +
                     (graphics::strwidth(STATUS_PERSISTENT) - graphics::strwidth(STATUS_LATENT)),
                   y = fig_in_usr_coords(4) - t_margin / 2 + status_legend$rect$h / 2,
                   bty = "n", horiz = TRUE, xpd = TRUE,
                   pch = 15, cex = cex_legend,
                   col = object@status_colors,
                   legend = names(object@status_colors))
  
  # Frequency legend
  freq_legend = graphics::legend("bottom", plot = FALSE,
                                   cex = cex_legend, fill = "red", density = c(-1, 15),
                                   legend = c("Frequency in complex transactions", "Frequency in simple transactions"))
  
  graphics::legend(x = fig_in_usr_coords(1) + w_margin,
                   y = fig_in_usr_coords(3) + freq_legend$rect$h + b_margin,
                   bty = "n", xpd = TRUE,
                   cex = cex_legend, fill = "red", density = c(-1, 15),
                   legend = c("Frequency in complex transactions", "Frequency in simple transactions"))
  
  # Legend of specificity and length
  so_legend = graphics::legend("bottom", plot = FALSE,
                               pch = c(20, 86), lty = c("dotted", NA), cex = cex_legend,
                               legend = c("Specificity", "Length"))
  
  graphics::legend(x = fig_in_usr_coords(2) - so_legend$rect$w - w_margin,
                   y = fig_in_usr_coords(3) + so_legend$rect$h + b_margin,
                   bty = "n", xpd = TRUE,
                   pch = c(19, 86), lty = c("dotted", NA), cex = cex_legend,
                   legend = c("Specificity", "Length"))
  
  # Chart title (function text instead of title for a precise placement with coordinates)
  graphics::text(x = fig_in_usr_coords(1) + w_margin,
                 y = fig_in_usr_coords(4) - t_margin / 2,
                 title, cex = 1.3, font = 2, adj = c(0, 0.5), xpd = TRUE)
})


#' Pattern node characteristics
#' 
#' For each pattern, extract the frequencies and lengths of the nodes in which it is included.
#' 
#' @template method_not_exported
#' 
#' @param object S4 object of class \code{TransactionAnalyzer}.
#' @param patterns Patterns whose characteristics of the nodes that contain them are to be found.
#'  Any subset of \code{object["patterns"]$pattern}.
#' @return
#'  \describe{
#'    \item{\code{frequencies}}{For each pattern, the frequencies of the nodes in which it is included.}
#'    \item{\code{lengths}}{For each pattern, the lengths of the nodes in which it is included.}
#'  }
#'  
#' @author Gauthier Magnin
#' @seealso \code{\link{frequency_by_complexity}}.
#' 
#' @aliases pattern_node_characteristics
#' @keywords internal
setMethod(f = "pattern_node_characteristics",
          signature = "TransactionAnalyzer",
          definition =
function(object, patterns) {
  
  check_init(object, PATTERNS)
  
  # All frequencies and lengths of the nodes containing the patterns
  frequencies = list()
  lengths = list()
  
  # For each pattern
  for (i in seq_along(patterns)) {
    # All nodes containing the pattern
    pat = as.character(patterns[i])
    nodes = object@nodes[object@nodes_patterns[, pat], ]
    
    frequencies[[i]] = nodes$frequency
    lengths[[i]] = nodes$length
  }
  
  return(list(frequencies = frequencies, lengths = lengths))
})


#' Pattern frequency by transaction complexity
#' 
#' For each pattern, compute its frequency related to complex transactions (i.e. the number of
#'  transactions containing more than one item and containing the pattern) and its frequency related to
#'  simple transactions (i.e. the number of transactions containing only one item and containing the
#'  pattern).
#' 
#' @param object S4 object of class `TransactionAnalyzer`.
#' @param patterns Patterns whose frequencies according to the complexity of the transactions containing
#'  them are to be computed. Any subset of `object["patterns"]$pattern`.
#'  
#'  `"patterns"` and `"p"` are special values for `object["patterns"]$pattern`.
#' @return Two-column matrix containing, for each pattern, its frequency related to complex transactions
#'  and its frequency related to simple transactions.
#' 
#' @author Gauthier Magnin
#' @seealso [`get_complex_trx`], [`get_simple_trx`], [`get_complexes`]
#' 
#' @examples
#' frequency_by_complexity(TA_instance, "patterns")
#' frequency_by_complexity(TA_instance, TA_instance["patterns"]$pattern[1:15])
#' 
#' @aliases frequency_by_complexity
#' @md
#' @export
setMethod(f = "frequency_by_complexity",
          signature = "TransactionAnalyzer",
          definition =
function(object, patterns) {
  
  patterns = get_tnp_itemsets(object, patterns, entities = PATTERNS)
  
  if (length(patterns) == 0) frequencies = matrix(NA_integer_, nrow = 0, ncol = 2)
  else {
    pnc = pattern_node_characteristics(object, patterns)
    
    frequencies = t(sapply(seq_along(patterns), function(i) {
      c(sum(pnc$frequencies[[i]][pnc$lengths[[i]] > 1]),
        sum(pnc$frequencies[[i]][pnc$lengths[[i]] == 1]))
    }))
  }
  
  colnames(frequencies) = c("complex", "simple")
  return(frequencies)
})



#### Methods for creating spectrosome graphs and computing related indicators ####

#' Spectrosome
#' 
#' Plot one or more spectrosome charts: graph in which vertices are nodes or patterns and edges are the
#'  items they have in common. Clusters corresponding to nodes or patterns sharing one item are
#'  mentioned by displaying the name or code identifying the item between these nodes or patterns.
#'  It can be automatically saved as a PNG file.
#' 
#' @details
#' If the argument \code{name} is not \code{NULL}, charts are plotted in PNG files of size 950 x 700
#'  pixels. If it is \code{NULL}, charts are plotted in the active device, one behind the other.
#' 
#' If \code{nb_graphs} is greater than \code{1} and \code{name} is not \code{NULL} a number is
#'  automatically added to the end of the file names to distinguish the different files.
#'  
#' If categories are associated with the items, each category generates a spectrosome. If there is more
#'  than one category and \code{name} is not \code{NULL}, the category name is appended to the end of
#'  the file name (after the possible addition of the number of the graph).
#' 
#' If mixed links are relative to category values that are not represented by single links, these
#'  values are present in the legend below "Mixt", with no associated color.
#' The same is true for mixed vertices if the argument \code{vertex_col} is \code{"categories"}.
#' 
#' If \code{min_link_weight} is greater than 1 or the chart is to be plotted from a subset of
#'  all nodes or patterns, some of them may become isolated because their links to the other
#'  entities may no longer be considered.
#' These new isolated vertices are moved to the end of the return data frame \code{edges}.
#' The \code{n} related rows are numbered \code{"A1"..."An"}.
#' 
#' @template default_category_values_colors
#' 
#' @details
#' The names of clusters confused because all of their links are mixed links, are not displayed.
#'  The identifiers of the items forming the clusters are sorted in alphanumeric order to define those
#'  which are displayed and those highlighted. Therefore, if several clusters are of the same size but
#'  the value given to the related argument (\code{clusters} or \code{highlight}) does not allow
#'  all of them to be considered, only the first ones will be, using this order.
#' 
#' Additional arguments can be supplied to the function in charge of plotting the graph.
#'  See the list of parameters: \code{\link[sna:gplot]{sna::gplot}}.
#' Among them, the following parameters are already defined and cannot be modified: \code{dat},
#'  \code{gmode}, \code{vertex.sides}, \code{vertex.cex}, \code{vertex.col}, \code{edge.col}.
#' The following parameters, which are among those that can be redefined, have the following values:
#'  \itemize{
#'    \item{\code{mode = "fruchtermanreingold"}}
#'    \item{\code{displaylabels = TRUE}}
#'    \item{\code{label.pos = 0}}
#'    \item{\code{boxed.labels = TRUE}}
#'    \item{\code{displayisolates = TRUE}}
#'  }
#' 
#' @param object S4 object of class \code{TransactionAnalyzer}.
#' @param nopc Data frame of \strong{n}odes \strong{o}r \strong{p}atterns and their
#'  \strong{c}haracteristics. Nodes or patterns whose spectrosome is to be plotted. Any subset of
#'  \code{object["nodes"]} or \code{object["patterns"]}.
#'  
#'  \code{"nodes"}, \code{"n"}, \code{"patterns"} and \code{"p"} are special values for
#'  \code{object["nodes"]} and \code{object["patterns"]}.
#' @param identifiers Which IDs to use to identify the nodes or patterns on the chart and in the
#'  return data frames. One of \code{"original"}, \code{"new"}.
#'  \describe{
#'    \item{\code{"original"}}{Use of the original identifiers.}
#'    \item{\code{"new"}}{Use of new identifiers ordered according to the subset corresponding to
#'                        \code{nopc}.}
#'  }
#' @param nb_graphs Number of graphs to generate. The position of the vertices differs between each copy.
#' @param min_link_weight Minimum number of items in common between two entities to plot their link on
#'  the chart.
#' @param vertex_size Way how the sizes of the vertices of the graph should be defined.
#'  One of \code{"relative"}, \code{"grouped"}, \code{"absolute"} or a numeric vector.
#'  \describe{
#'    \item{\code{"relative"}}{The sizes are defined by a linear interpolation of the frequencies of the
#'                             entities in \code{size_range}.}
#'    \item{\code{"grouped"}}{The frequencies of the entities are grouped according to 5 intervals
#'                            defined by quantiles. The 5 corresponding size values are taken in a
#'                            regular sequence bounded by \code{size_range}.}
#'    \item{\code{"absolute"}}{The size of a vertex is defined directly according to the frequency of
#'                             the entity.}
#'    \item{A single numeric value}{The vertices are all the size defined by this value.}
#'    \item{A longer numeric vector}{The sizes defined in this vector are directly assigned to the
#'                                   nodes of patterns to plot. Is recycled if smaller than the number
#'                                   of entities.}
#'  }
#' @param size_range If \code{vertex_size} is \code{"relative"} or \code{"grouped"},
#'  range of vertex size values (given as expansion factors). Ignored otherwise.
#' @param vertex_col Way how the colors of the vertices of the graph should be defined.
#'  One of \code{"status"}, \code{"categories"}, \code{"none"} or a character vector corresponding to
#'  R predefined color names or hexadecimal values.
#'  
#'  \describe{
#'   \item{\code{"status"}}{If \code{nopc} refers to patterns, coloring according to the status of the
#'         patterns. If it refers to nodes, consider the value \code{"none"}.}
#'   \item{\code{"categories"}}{Coloring according to the categories associated with the items of
#'         the entities represented.}
#'   \item{\code{"none"}}{Vertices are colored gray.}
#'   \item{A single character value}{All vertices are colored with this color.}
#'   \item{A longer character vector}{The colors are directly assigned to nodes or patterns to plot.
#'         If the length of this vector is not equal to the number of entities to plot, it is recycled.}
#'  }
#' @param clusters Maximum number of clusters to name on the graph.
#'  If the actual number of clusters is greater, the names of the smaller ones are not displayed.
#' @param highlight Number of clusters to highlight among those named on the graph.
#'  The names of the largest clusters are displayed in bold.
#' @param use_names If \code{TRUE}, display item names if they are defined. Display their identification
#'  codes otherwise.
#' @param n.cutoff If \code{use_names = TRUE}, limit number of characters to display concerning the names
#'  of the represented items.
#' @param c.cutoff Limit number of characters to display in the legend for the categories represented.
#' @param display_mixt If \code{TRUE}, also display in the legend the category values included only in
#'  mixed links (or in mixed vertices, if \code{vertex_col = "categories"}).
#' @param title Chart title. Default title depends on the type of entities contained in \code{nopc}.
#'  Example of default title: \code{"Spectrosome of nodes"} if \code{nopc} contains nodes.
#' @param path Path of the directory in which to save the charts as PNG files. Default is the working
#'  directory.
#' @param name Single name of the PNG files in which to save the charts. To be ignored to plot the charts
#'  in the active device.
#' @param ... Additional arguments to the function \code{\link[sna:gplot]{gplot}} from the package
#'  \code{sna} for plotting the graph. See Details section.
#' @return
#'  \describe{
#'    \item{\code{vertices}}{Data frame of the nodes or patterns and characteristics used,
#'                           associated with the identifiers of the vertices of the graph and their
#'                           degrees in the graph.}
#'    \item{\code{edges}}{Data frame of information relating to the edges of the graph.}
#'    \item{\code{coords}}{List containing the coordinate matrices of the vertices of the graph.
#'                         As many matrices as there are graphs (argument \code{nb_graphs}), which can
#'                         be reused via the argument \code{coord} (see \code{...}).}
#'  }
#' 
#' @author Delphine Bosson-Rieutort, Gauthier Magnin
#' @references Bosson-Rieutort D, de Gaudemaris R, Bicout DJ (2018).
#'             The spectrosome of occupational health problems. \emph{PLoS ONE} 13(1): e0190196.
#'             \url{https://doi.org/10.1371/journal.pone.0190196}.
#' @seealso \code{\link{degree}}, \code{\link{network_density}}, \code{\link[sna:gplot]{sna::gplot}}.
#' 
#' @examples
#' spectrosome_1 <- spectrosome_chart(TA_instance, "nodes")
#' spectrosome_2 <- spectrosome_chart(TA_instance, TA_instance["patterns"])
#' spectrosome_3 <- spectrosome_chart(TA_instance, TA_instance["patterns"][1:15, ])
#' 
#' spectrosome_2 <- spectrosome_chart(TA_instance, "patterns",
#'                                    path = getwd(),
#'                                    name = "spectrosome_of_patterns")
#' 
#' @aliases spectrosome_chart
#' @export
setMethod(f = "spectrosome_chart",
          signature = "TransactionAnalyzer",
          definition =
function(object, nopc, identifiers = "original",
         nb_graphs = 1, min_link_weight = 1,
         vertex_size = "relative", size_range = c(0.5, 2.5), vertex_col = "status",
         clusters = Inf, highlight = 3,
         use_names = TRUE, n.cutoff = NULL, c.cutoff = NULL, display_mixt = TRUE,
         title = NULL, path = NULL, name = NULL, ...) {
  
  # Getting nodes/patterns and finding the type of entities provided
  entities = which_entities(object, nopc)
  nopc = get_tnp(object, nopc)
  check_init(object, c(entities, which_associated_links(object, entities)))
  
  # Parameter validation
  if (nrow(nopc) < 2)
    stop("There must be at least 2 nodes or patterns to draw a spectrosome.")
  
  if (identifiers != "original" && identifiers != "new")
    stop("identifiers must be \"original\" or \"new\".")
  
  if (!is.character(vertex_col)) {
    stop(paste("vertex_col must be \"status\", \"categories\", \"none\" or",
               "a character vector of R predefined color names or hexadecimal values."))
  }
  if (!(vertex_size %in% c("relative", "grouped", "absolute")) && !is.numeric(vertex_size)) {
    stop(paste("vertex_size must be one of \"relative\", \"grouped\", \"absolute\"",
               "or a numeric vector."))
  }
  
  
  # Extraction of the links for the entities to visualize (nop_links = node or pattern links)
  nop_links = get_links(object, nopc)
  
  if (all(nop_links$weight == 0) && "displayisolates" %in% names(list(...)) && !(list(...)$displayisolates)) {
    warning("There is no graph to plot: displayisolates = FALSE and all entities are isolated.")
    return(NULL)
  }
  
  if (entities == NODES) {
    # Text displayed on the chart
    nop_subtitle_1 = "Nodes: %d (%d isolate"
    nop_subtitle_2 = "); Links: %d"
    
    not_identical = !identical(object@nodes, nopc)
    
    # Renaming columns for later simplification (cf. vertices_colors and vertices_shapes)
    colnames(nopc)[colnames(nopc) == "node"] = "pattern"
    
  } else if (entities == PATTERNS) {
    # Text displayed on the chart
    nop_subtitle_1 = "Patterns: %d (%d isolate"
    nop_subtitle_2 = "); Links: %d"
    
    not_identical = !identical(object@patterns, nopc)
  }
  
  # Graph vertex identifiers
  vertices_id = seq(nrow(nopc))
  
  if (not_identical) {
    # New numbering of kept entities
    names(vertices_id) = rownames(nopc)
    nop_links$endpoint.1 = vertices_id[as.character(nop_links$endpoint.1)]
    nop_links$endpoint.2 = vertices_id[as.character(nop_links$endpoint.2)]
  }
  
  # Removal of links between vertices that have less than min_link_weight items in common
  if (min_link_weight > 1) {
    all_vertices = unique(c(t(nop_links[, 1:2]))) # unlist horizontally
    nop_links = nop_links[nop_links$weight >= min_link_weight | nop_links$weight == 0, ]
    
    # Redifining vertices that are now without links
    missing_vertices = as.data.frame(t(
      sapply(setdiff(all_vertices, unique(unlist(nop_links[, 1:2]))),
             function(x){
               if (entities == NODES) return(c(x, x, "", 0))
               return(c(x, x, "", 0, object@patterns[x, "year"]))
             })), stringsAsFactors = FALSE)
    
    # Reassignement the column names and classes before concatenation to the data frame of links
    colnames(missing_vertices) = colnames(nop_links)
    for (c_name in colnames(missing_vertices)) class(missing_vertices[c_name]) = class(nop_links[c_name])
    class(missing_vertices$endpoint.1) = class(missing_vertices$endpoint.2) = class(missing_vertices$weight) = "integer"
    if(entities == PATTERNS) class(missing_vertices$year) = "integer"
    nop_links = rbind(nop_links, missing_vertices)
    
    # Assignment of names to the new rows, different from those of the general data frame (the attribute)
    rownames(nop_links) = c(rownames(nop_links)[1:(nrow(nop_links) - nrow(missing_vertices))],
                            paste0("A", seq_len(nrow(missing_vertices))))
  }
  # Assignment of identifiers to the links
  nop_links$ID = seq_len(nrow(nop_links))
  if (entities == NODES) nop_links = nop_links[, c("ID", "endpoint.1", "endpoint.2", "items", "weight")]
  else nop_links = nop_links[, c("ID", "endpoint.1", "endpoint.2", "items", "weight", "year")]
  
  
  # Colors and legends for each existing category
  categories_colors = list()
  links_colors = list()
  
  # Setting link colors by searching for the categories associated with the items forming the links
  if (length(object@items_categories) == 0) {
    # If no category is associated with the entities, none is associated with the links
    categories_colors[[1]] = character(0)
    links_colors[[1]] = 1
    
  } else {
    # For each type of category
    for (category in seq_len(ncol(object@items_categories))) {
      
      # If there is no link
      if (sum(nop_links$weight != 0) == 0) {
        categories_colors[[category]] = character(0)
        links_colors[[category]] = rep("white", nrow(nop_links))
        
      } else if (length(levels(object@items_categories[, category])) > 1) {
        
        # Categories associated with the links
        links_categories = lapply(strsplit(nop_links$items, "/"),
                                  function(x) sort(unique(as.character(object@items_categories[x, category]))))
        category_values = unique(unlist(links_categories))
        links_categories = unlist(lapply(links_categories, function(x) {
          if (length(x) == 1) return(x)
          if (length(x) > 1) return("Mixt")
          return("Isolated")
        }))
        
        # Separation of category values that are only included in mixed links
        category_mixed = sort(setdiff(category_values, unique(links_categories)))
        category_not_mixed = sort(setdiff(category_values, category_mixed))
        
        # Selection of the related colors
        categories_colors[[category]] = c(object@categories_colors[[category]][category_not_mixed],
                                          "black", "white")
        names(categories_colors[[category]]) = c(category_not_mixed, "Mixt", "Isolated")
        
        # Colors of the links drawn on the chart
        links_colors[[category]] = categories_colors[[category]][links_categories]
        
        # Removal of the black associated with mixed links if there is none and removal of the white
        # associated with isolated vertices, so as not to display them later in the legend
        if (length(category_mixed) == 0 && !("Mixt" %in% links_categories)) {
          categories_colors[[category]] = categories_colors[[category]][seq(length(categories_colors[[category]])-2)]
        } else {
          categories_colors[[category]] = categories_colors[[category]][seq(length(categories_colors[[category]])-1)]
          
          # Addition of the the category values included only in mixed links
          if (display_mixt) {
            new_names = c(names(categories_colors[[category]]), category_mixed)
            categories_colors[[category]] = append(categories_colors[[category]], rep("white", length(category_mixed)))
            names(categories_colors[[category]]) = new_names
          }
        }
        
      } else if(length(levels(object@items_categories[, category])) == 1) {
        # A single category
        categories_colors[[category]] = c("black", "white")
        names(categories_colors[[category]]) = c(levels(object@items_categories[, category]), "Isolated")
        
        links_categories = ifelse(nop_links$weight == 0, "Isolated", levels(object@items_categories[, category]))
        
        # Colors of the links drawn on the chart
        links_colors[[category]] = categories_colors[[category]][links_categories]
        
        # Removal of the white associated with the isolated vertices so as not to display it later
        # in the legend
        categories_colors[[category]] = categories_colors[[category]][seq(length(categories_colors[[category]])-1)]
        
      } else {
        stop("The categories associated with the items must be factor type and it must have at least one factor.")
      }
    }
  }
  
  
  # Defining vertex colors according to status and number for each status
  if (entities == PATTERNS && vertex_col[1] == "status") {
    vertices_colors = object@status_colors[nopc$status]
    count_status = sapply(names(object@status_colors), function(status) sum(nopc$status == status))
    
    # Related legend
    vertex_legend_legend = c(names(object@status_colors), "", "Single items", "Multiple items")
    vertex_legend_col = c(object@status_colors, "white", "black", "black")
    
  } else if (vertex_col[1] == "categories") {
    # Colors according to the category
    v.categories_colors = list()
    vertices_colors = list()
    vertex_legend_legend = list()
    vertex_legend_col = list()
    
    if (length(object@items_categories) == 0) {
      v.categories_colors[[1]] = character(0)
      vertices_colors[[1]] = rep("grey", nrow(nopc))
      vertex_legend_legend[[1]] = c("Single items", "Multiple items")
      vertex_legend_col[[1]] = c("black", "black")
      
    } else {
      # For each category
      for (category in seq_len(ncol(object@items_categories))) {
        
        # Category values associated with vertices
        vertices_categories = lapply(nopc$pattern,
                                     function(x) sort(unique(as.character(object@items_categories[x, category]))))
        category_values = unique(unlist(vertices_categories))
        vertices_categories = unlist(lapply(vertices_categories, function(x) {
          if (length(x) == 1) return(x)
          if (length(x) > 1) return("Mixt")
        }))
        
        # Separation of category values that are only included in mixed vertices
        category_mixed = sort(setdiff(category_values, unique(vertices_categories)))
        category_not_mixed = sort(setdiff(category_values, category_mixed))
        
        # Selection of the related colors
        v.categories_colors[[category]] = c(object@categories_colors[[category]][category_not_mixed],
                                            "black")
        names(v.categories_colors[[category]]) = c(category_not_mixed, "Mixt")
        
        # Colors of vertices plotted on the graph
        vertices_colors[[category]] = v.categories_colors[[category]][vertices_categories]
        
        # Removal of the black associated with the mixed vertices if there are none, so as not to
        # display it later in the legend
        if (length(category_mixed) == 0) {
          v.categories_colors[[category]] = v.categories_colors[[category]][seq(length(v.categories_colors[[category]])-1)]
        } else {
          # Addition of the category values included only in mixed vertices
          if (display_mixt) {
            new_names = c(names(v.categories_colors[[category]]), category_mixed)
            v.categories_colors[[category]] = append(v.categories_colors[[category]], rep("white", length(category_mixed)))
            names(v.categories_colors[[category]]) = new_names
          }
        }
        
        # Related legend
        vertex_legend_legend[[category]] = c(names(v.categories_colors[[category]]),
                                             "", "Single items", "Multiple items")
        vertex_legend_col[[category]] = c(v.categories_colors[[category]], "white", "black", "black")
      }
    }
  } else {
    if (vertex_col[1] == "none" || vertex_col[1] == "status") { # ("none") or ("status" and nodes)
      vertices_colors = "grey"
    } else {
      vertices_colors = vertex_col
    }
    
    # Related legend
    vertex_legend_legend = c("Single items", "Multiple items")
    vertex_legend_col = c("black", "black")
  }
  
  # Vertices with several items as circles; triangles otherwise
  vertices_shapes = rep(100 , nrow(nopc))
  vertices_shapes[nopc$length == 1] = 3
  
  # Defining vertex sizes
  if (is.numeric(vertex_size)) vertices_sizes = vertex_size
  else {
    switch(EXPR = vertex_size,
           "relative" = {
             # Linear interpolation from frequencies to values [size_range[1], size_range[2]]
             if (min(nopc$frequency) != max(nopc$frequency)) {
               func = stats::approxfun(x = c(min(nopc$frequency), max(nopc$frequency)),
                                       y = size_range)
               vertices_sizes = func(nopc$frequency)
             } else {
               vertices_sizes = rep(mean(size_range), length(nopc$frequency))
             }
           },
           "grouped" = {
             # Groupings of frequency values according to 5 quantiles
             breaks = round(stats::quantile(nopc$frequency, prob = seq(0, 1, 0.2)))
             intervals = cut(nopc$frequency, breaks = unique(breaks), include.lowest = TRUE)
             sizes = seq(size_range[1], size_range[2], length.out = length(levels(intervals)))
             vertices_sizes = sizes[intervals]
           },
           "absolute" = {
             vertices_sizes = nopc$frequency
           })
  }
  
  
  # Network generated with the package network
  links = as.matrix(nop_links[, c("endpoint.1", "endpoint.2")], ncol = 2)
  network_data = network::network(links, directed = FALSE, matrix.type = "edgelist")
  # vertices_names = network::network.vertex.names(network_data)
  
  # Getting the additional arguments and determining default values for sna::gplot
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
  
  # Duplication of the function used by the argument "mode" of the function sna::gplot to work
  # without having to load the package
  eval(parse(text = paste0("gplot.layout.", args$mode, " <- sna::gplot.layout.", args$mode)))
  
  # Number of chart variations
  nb_categories = ifelse(length(object@items_categories) == 0, 1, ncol(object@items_categories))
  
  # Setting title default value and checking file name
  if (is.null(title)) title = paste0("Spectrosome of ", entities)
  if (!is.null(name)) name = check_extension(name, "png")
  path = turn_into_path(path)
  
  # Reuse or not of coordinates
  if ("coord" %in% names(args)) {
    coord = args$coord
    args$coord = NULL
    is.missing = is.null(coord)
    
  } else { is.missing = TRUE }
  coords_list = list()
  
  # Plotting of the charts
  for (i in seq(nb_graphs)) {
    # Coordinates that will be reused
    if (is.missing) coord = NULL
    
    for (j in seq(nb_categories)) {
      
      # Opening a PNG file if specified
      if (!is.null(name)) {
        # Chart name according to the number
        file_name = ifelse(nb_graphs == 1, name, sub(".png", paste0("-", i, ".png"), name))
        file_name = ifelse(nb_categories == 1,
                           file_name,
                           sub(".png", paste0("-", colnames(object@items_categories)[j], ".png"), file_name))
        
        # Plotting the charts in PNG files
        grDevices::png(paste0(path, file_name), 950, 700)
      }
      
      graphics::par(mar = c(0.5, 0.5, 3.5, 0.5))
      graphics::plot.new()
      w_margin_inches = graphics::par("mai")[4]
      
      # Titles of the chart
      title(main = title, cex.main = 1.3, line = 2)
      nb_isolates = length(sna::isolates(network_data))
      title(main = paste0(sprintf(nop_subtitle_1, nrow(nopc), nb_isolates),
                          if (nb_isolates < 2) "" else "s",
                          sprintf(nop_subtitle_2, sum(nop_links$weight != 0))),
            font.main = 3, cex.main = 1.1, line = 0.7)
      
      # Preparation of the shapes of the vertex legend
      if (entities == PATTERNS && vertex_col[1] == "status") {
        vertex_legend_pt.cex = c(rep(2, length(object@status_colors)), 0, 1.6, 2)
        vertex_legend_pch = c(rep(15, length(object@status_colors)), 0, 2, 1)
      } else if (vertex_col[1] != "categories" || length(object@items_categories) == 0) {
        vertex_legend_pt.cex = c(1.6, 2)
        vertex_legend_pch = c(2, 1)
      } else {
        vertex_legend_pt.cex = c(rep(2, length(v.categories_colors[[j]])), 0, 1.6, 2)
        vertex_legend_pch = c(rep(15, length(v.categories_colors[[j]])), 0, 2, 1)
      }
      
      # Preparation of the text and color of the vertex legend
      if (vertex_col[1] != "categories") {
        legend_legend = vertex_legend_legend
      } else {
        if (length(object@items_categories) != 0) {
          # Application of the cutoff on the legend of the vertex colors if depending on the category
          nb_vertices_leg = length(vertex_legend_legend[[j]])
          legend_legend = c(substr2(vertex_legend_legend[[j]][1:(nb_vertices_leg-3)], stop = c.cutoff),
                            vertex_legend_legend[[j]][(nb_vertices_leg-2):nb_vertices_leg])
        } else {
          legend_legend = vertex_legend_legend[[1]]
        }
      }
      legend_col = if (vertex_col[1] == "categories") vertex_legend_col[[j]] else vertex_legend_col
      
      # Display of the legend
      cex_legend = 1
      vertex_legend_output = graphics::legend("topleft", bty = "n",
                                              title = cap(entities), title.adj = 0,
                                              pt.cex = vertex_legend_pt.cex, pch = vertex_legend_pch,
                                              lwd = -1, cex = cex_legend,
                                              col = legend_col, legend = legend_legend)
      
      if (length(object@items_categories) != 0 && length(categories_colors[[j]]) != 0) {
        edge_legend_legend = substr2(names(categories_colors[[j]]), stop = c.cutoff)
        
        edge_legend_output = graphics::legend(x = vertex_legend_output$rect$left,
                                              y = vertex_legend_output$rect$top - vertex_legend_output$rect$h,
                                              bty = "n", title = "Links", title.adj = 0,
                                              pch = NA_integer_, lwd = 3, cex = cex_legend,
                                              col = categories_colors[[j]], legend = edge_legend_legend)
        
        legend_width = max(vertex_legend_output$rect$w, edge_legend_output$rect$w)
      } else {
        legend_width = vertex_legend_output$rect$w
      }
      
      # Additional legend regarding the distribution of statuses
      if (entities == PATTERNS && vertex_col[1] == "status") {
        status_legend = paste0("(", count_status, ")")
        
        status_legend_output = graphics::legend(x = vertex_legend_output$text$x[1] + 
                                                  graphics::strwidth(paste0(STATUS_PERSISTENT), cex = cex_legend),
                                                y = vertex_legend_output$rect$top,
                                                bty = "n", title = "",
                                                legend = status_legend, cex = cex_legend)
        
        if (status_legend_output$rect$w + status_legend_output$rect$left > legend_width + vertex_legend_output$rect$left) {
          legend_width = status_legend_output$rect$left - vertex_legend_output$rect$left + status_legend_output$rect$w
        }
      }
      
      
      # Reset graphics area margins to separate legend and plot
      graphics::par(new = TRUE, mai = graphics::par("mai") +
                      c(0, convert_gunits(legend_width, "user", "inches", "w") + w_margin_inches, 0, 0))
      
      tryCatch({
        # Plotting the graph: call of sna::gplot with the modified "..." arguments (variable args)
        coord = do.call(sna::gplot, c(list(
          dat = network_data, gmode = "graph",
          coord = coord,
          vertex.sides = vertices_shapes,
          vertex.cex = vertices_sizes,
          vertex.col = if (vertex_col[1] == "categories") vertices_colors[[j]] else vertices_colors,
          edge.col = links_colors[[j]][links_colors[[j]] != "white"]
          # [links_colors[[j]] != "white"] is necessary since an update of sna and/or network
          # => To be simplified if this does not lead to an error elsewhere
        ), args))
      },
      error = function(e) {
        # Closing and deleting the graphic file before displaying the error message
        if (!is.null(name)) {
          grDevices::dev.off()
          file.remove(paste0(path, file_name))
        }
        stop(e)
        # Example of possible error: vertex_col contains incorrect color names
      })
      
      # If there are links, identification and display of cluster names
      if (sum(nop_links$weight != 0)) {
        cluster_text(object, coord, nop_links, clusters, highlight, use_names, n.cutoff)
      }
      
      # Closing the PNG file
      if (!is.null(name)) grDevices::dev.off()
    }
    
    # Keeping the coordinates of the graph vertices
    coords_list[[i]] = coord
  }
  
  # Computation of the degree of each vertex in the graph
  degrees = sapply(vertices_id, function(ID) degree(object, ID, nop_links))
  # Renaming columns to their initial names before return
  if (entities == NODES) colnames(nopc)[colnames(nopc) == "pattern"] = "node"
  
  # Reassignment of initial IDs (not compatible with sna::gplot)
  if (identifiers == "original" && not_identical) {
    nop_links$endpoint.1 = as.integer(names(vertices_id[nop_links$endpoint.1]))
    nop_links$endpoint.2 = as.integer(names(vertices_id[nop_links$endpoint.2]))
    
    vertices_id = as.numeric(rownames(nopc))
  }
  
  # Nodes or patterns, characteristics, identifiers on the graph and degrees in the graph
  return(list(vertices = data.frame(ID = vertices_id, nopc, degree = degrees, stringsAsFactors = FALSE),
              edges = nop_links,
              coords = coords_list))
})


#' Display of cluster names
#' 
#' Identify and display the names of the clusters of the graph provided, corresponding to nodes or
#'  patterns sharing one item.
#' The names of clusters confused because all of their links are mixed links, are not displayed.
#' Texts are written on the active graphics device.
#' 
#' @details
#' The identifiers of the items forming the clusters are sorted in alphanumeric order to define those
#'  which are displayed and those highlighted. Therefore, if several clusters are of the same size but
#'  the value given to the related argument (\code{display} or \code{highlight}) does not allow
#'  all of them to be considered, only the first ones will be, using this order.
#' 
#' @template method_not_exported
#' 
#' @param object S4 object of class \code{TransactionAnalyzer}.
#' @param graph Graph generated by the function \code{\link[sna:gplot]{gplot}} from the package
#'  \code{sna}: "A two-column matrix containing the vertex positions as x,y coordinates".
#' @param links Links of nodes or patterns used to generate \code{graph}.
#' @param display Maximum number of clusters to name on the graph.
#'  If the actual number of clusters is greater, the names of the smaller ones are not displayed.
#' @param highlight Number of clusters to highlight among those named on the graph.
#'  The names of the largest clusters are displayed in bold.
#' @param use_names If \code{TRUE}, display item names if they are defined. Display their identification
#'  codes otherwise.
#' @param cutoff If \code{use_names = TRUE}, limit number of characters to display concerning the names
#'  of the represented items.
#' 
#' @author Delphine Bosson-Rieutort, Gauthier Magnin
#' @seealso \code{\link{spectrosome_chart}}, \code{\link[sna:gplot]{sna::gplot}}.
#' @aliases cluster_text
#' @keywords internal
setMethod(f = "cluster_text",
          signature = "TransactionAnalyzer",
          definition =
function(object, graph, links, display = Inf, highlight = 3, use_names = TRUE, cutoff = NULL) {
  
  # Computation of the coordinates of the midpoints of the links
  coord_e1 = graph[links$endpoint.1, ] # Coordinates of the first vertices of the links
  coord_e2 = graph[links$endpoint.2, ] # Coordinates of the second vertices of the links
  # If there are multiple links
  if (!is.vector(coord_e1)) {
    coord_L = data.frame(X = rowMeans(cbind(coord_e1[, "x"], coord_e2[, "x"])), 
                         Y = rowMeans(cbind(coord_e1[, "y"], coord_e2[, "y"])),
                         LABEL = links$items,
                         stringsAsFactors = FALSE)
  } else {
    # If there is only one link and only two vertices
    coord_L = data.frame(X = mean(c(coord_e1["x"], coord_e2["x"])), 
                         Y = mean(c(coord_e1["y"], coord_e2["y"])),
                         LABEL = links$items,
                         stringsAsFactors = FALSE)
  }
  # Grouping according to the type of link ("LABEL")
  coord_L = coord_L[order(coord_L$LABEL), ]
  
  # Break down of multiple links and computation of the number of actual links of each item
  clusters = sort(table(unlist(strsplit(as.character(coord_L$LABEL[coord_L$LABEL != ""]), "/"))), decreasing = TRUE)
  clusters = names(clusters)
  
  # Average link coordinates for each type of link ("LABEL")
  coords = stats::aggregate(data.frame(MOY.X = coord_L$X),
                            by = list(LABEL = coord_L$LABEL), mean)
  coords$MOY.Y = tapply(coord_L$Y, coord_L$LABEL, mean)
  
  # Association of the average coordinates of the exact links (not multiple and not broken down) to the
  # names of the items having generated the most links
  coords = coords[match(clusters, as.character(coords$LABEL)), ]
  #! The coordinates are therefore not the average of all the links corresponding to the item but only
  #! of those which correspond exactly to this item (no combinations) although the "cluster" variable
  #! has sought the number of links corresponding to the item, whether there is a combination or not.
  #! => Allows a kind of attraction of the label towards the vertices sharing only the item.
  
  # Extraction of the names of the items having generated the most links
  coords = coords[stats::complete.cases(coords), ]
  if (nrow(coords) >= display) coords = coords[seq_len(display), ]
  
  # If there actually are clusters to name (this is not the case if there are only mixed links)
  if (nrow(coords) > 0) {
    
    # Display of the names of the selected "clusters"
    if (use_names) {
      coords$LABEL = get_item_names(object, coords$LABEL)
      if (!is.null(cutoff)) coords$LABEL = substr(coords$LABEL, 1, cutoff)
    }
    TeachingDemos::shadowtext(coords$MOY.X, coords$MOY.Y, coords$LABEL, r = 0.3,
                              col = "black", bg = "white", cex = 0.9,
                              font = ifelse(coords$LABEL %in% coords$LABEL[seq_len(highlight)], 2, 1))
  }
})


#' Network density
#' 
#' Compute the density of a graph as the ratio between the number of links identified and the
#'  number of links there would be if it was a complete graph (i.e. if all the vertices of the graph
#'  were connected to each other).
#' 
#' @param object S4 object of class `TransactionAnalyzer`.
#' @param links Data frame of the links (or edges) of a spectrosome graph.
#' @return Density of the network.
#' 
#' @author Gauthier Magnin
#' @seealso [`spectrosome_chart`], [`get_links`], [`degree`].
#' 
#' @examples
#' spectrosome <- spectrosome_chart(TA_instance, TA_instance["patterns"][1:15, ])
#' network_density(TA_instance, spectrosome[["edges"]])
#' 
#' @aliases network_density
#' @md
#' @export
setMethod(f = "network_density",
          signature = "TransactionAnalyzer",
          definition =
function(object, links) {
  
  # Number of edges and vertices
  nb_edges = nrow(links) - sum(links$weight == 0)
  nb_vertices = length(unique(c(links$endpoint.1, links$endpoint.2)))
  
  # Maximum possible number of edges (1 between each pair of vertices, without loop)
  nb_edges_max = nb_vertices * (nb_vertices - 1) / 2
  
  return(nb_edges / nb_edges_max)
})


#' Degree of a vertex
#' 
#' Compute the degree of a vertex in a graph, i.e. the number of vertices to which it is adjacent.
#' 
#' @param object S4 object of class `TransactionAnalyzer`.
#' @param ID Identifier of the vertex (node or pattern) whose degree is to be computed.
#' @param links Data frame of the links (or edges) of a spectrosome graph.
#' @return Degree of the vertex.
#' 
#' @author Gauthier Magnin
#' @seealso [`spectrosome_chart`], [`get_links`], [`network_density`].
#' 
#' @examples
#' spectrosome <- spectrosome_chart(TA_instance, TA_instance["patterns"][1:15, ])
#' degree(TA_instance, 7, spectrosome[["edges"]])
#' 
#' @aliases degree
#' @md
#' @export
setMethod(f = "degree",
          signature = "TransactionAnalyzer",
          definition =
function(object, ID, links) {
  return(sum(xor(links$endpoint.1 == ID, links$endpoint.2 == ID)))
})



#### Methods for creating itemset charts, category trees and co-occurrence graphs ####

#' Itemset chart, for TransactionAnalyzer
#' 
#' Plot a chart of the transaction, node or pattern itemsets. It can be automatically saved as a PDF file.
#' 
#' @details
#' If they are from nodes or patterns, itemsets are sorted according to their lengths (increasing) then
#'  to their frequencies (decreasing). If they are from transactions, they are sorted according to their
#'  lengths only. When there is equality of these characteristics, itemsets are then taken according to
#'  the initial order in `tnpc`.
#' 
#' If `category` is `NULL` or `sort_by = "item"`, items are sorted alphanumerically.
#' If `category` is not `NULL` and `sort_by = "category"`, items are sorted according to the values of
#'  the category then alphanumerically.
#' 
#' If the argument `name` is not `NULL`, the chart is plotted in a PDF file of A4 landscape paper size.
#'  If it is `NULL`, the chart is plotted in the active device.
#' 
#' @template default_category_values_colors
#' 
#' @param object S4 object of class `TransactionAnalyzer`.
#' @param tnpc Object of class `TransactionSet` (**t**) or data frame of **n**odes or **p**atterns and
#'  their **c**haracteristics. Itemsets whose chart is to be plotted. Any subset of
#'  `object["transactions"]`, `object["nodes"]` or `object["patterns"]`.
#'  
#'  `"transactions"`, `"t"`, `"nodes"`, `"n"`, `"patterns"` and `"p"` are special values for
#'  `object["transactions"]`, `object["nodes"]` and `object["patterns"]`.
#' @param identifiers Which IDs to use to identify the itemsets on the chart and in the
#'  return. One of `"original"`, `"new"`.
#'  \describe{
#'    \item{`"original"`}{Use of the original identifiers from `tnpc`.}
#'    \item{`"new"`}{Use of new identifiers based on sorting (see 'Details' section to learn more
#'                   about the sort that is performed).}
#'  }
#' @param length_one If `FALSE`, itemsets of length \eqn{1} are not plotted. If `TRUE`, all itemsets
#'  are plotted.
#' @param jitter If `FALSE`, non-equivalent itemsets of length \eqn{1} are aligned vertically.
#'  If `TRUE`, they are spread over several vertical lines to avoid overplotting while taking as little
#'  space as possible. If `NA`, they are plotted one after the other.
#'  Ignored if `length_one` is `FALSE`.
#' @param under,over Data to display on the chart under and over the itemsets.
#'  Can be:
#'  * Identifiers: `"ID"`.
#'  * One of the elements of the transactions (i.e. one of the values of `tnpc["names"]`), if `tnpc` is
#'    a `TransactionSet`.
#'  * One of the characteristics of the nodes or the patterns (`"support"`, `"frequency"`, `"weight"`,
#'    `"specificity"`, `"year"`, `"status"`), if `tnpc` is a data frame of nodes or patterns and their
#'    characteristics.
#'  
#'  `"status"` can only be used for the argument `"over"`.
#'  `NULL` value specifies to display no text.
#' @param use_names If `TRUE`, display item names if they are defined. Display their identification
#'  codes otherwise.
#' @param n.cutoff If `use_names = TRUE`, limit number of characters to display concerning the names
#'  of the represented items.
#' @param category Name or number of the category to represent on the chart (numbering according to
#'  the order of the columns of `object["items_categories"]`).
#' @param c.cutoff Limit number of characters to display in the legend for the category represented.
#' @param sort_by Sorting method of displayed items. One of `"category"`, `"item"`.
#' @param title Chart title. Default title depends on the type of entities contained in `tnpc`.
#'  Example of default title: `"Node itemsets"` if `tnpc` contains nodes.
#' @param path Path of the directory in which to save the chart as a PDF file. Default is the working
#'  directory.
#' @param name Name of the PDF file in which to save the chart. To be ignored to plot the chart in the
#'  active device.
#' @return Object of class `TransactionSet` or data frame of the characteristics of the nodes or patterns
#'  represented on the chart, associated with their identifiers (visible on the chart if one of `under`
#'  or `over` is `"ID"`).
#' 
#' @author Delphine Bosson-Rieutort, Gauthier Magnin
#' @references Bosson-Rieutort D, Sarazin P, Bicout DJ, Ho V, Lavoué J (2020).
#'             Occupational Co-exposures to Multiple Chemical Agents from Workplace Measurements by the US Occupational Safety and Health Administration.
#'             *Annals of Work Exposures and Health*, Volume 64, Issue 4, May 2020, Pages 402–415.
#'             <https://doi.org/10.1093/annweh/wxaa008>.
#' @seealso
#' Method for signature `TransactionSet`:
#' [`itemset_chart,TransactionSet`][itemset_chart,TransactionSet-method].
#' 
#' @examples
#' trx <- itemset_chart(TA_instance, "transactions", length_one = TRUE,
#'                      category = "family", c.cutoff = 7, n.cutoff = 20)
#' nodes <- itemset_chart(TA_instance, "nodes",
#'                        category = "family", c.cutoff = 10, n.cutoff = 20)
#' patterns_1 <- itemset_chart(TA_instance, "patterns",
#'                             category = 1, c.cutoff = 10, n.cutoff = 20)
#' patterns_2 <- itemset_chart(TA_instance, TA_instance["patterns"][1:15, ])
#' 
#' ## Plotting in a PDF file
#' patterns_1 <- itemset_chart(TA_instance, "patterns", category = "family",
#'                             c.cutoff = 17, n.cutoff = 20,
#'                             path = getwd(), name = "pattern_itemsets")
#' 
#' @aliases itemset_chart itemset_chart,TransactionAnalyzer
#' @md
#' @export
setMethod(f = "itemset_chart",
          signature = "TransactionAnalyzer",
          definition =
function(object, tnpc, identifiers = "original",
         length_one = FALSE, jitter = TRUE,
         under = "ID", over = "status",
         use_names = TRUE, n.cutoff = NULL,
         category = NULL, c.cutoff = NULL, sort_by = "category",
         title = NULL, path = NULL, name = NULL) {
  
  # Getting nodes/patterns and finding the type of entities provided
  entities = which_entities(object, tnpc, NODES_PATTERNS_OR_TRANSACTIONS)
  if (entities != TRANSACTIONS) check_init(object, entities)
  tnpc = get_tnp(object, tnpc, NODES_PATTERNS_OR_TRANSACTIONS)
  
  # Parameter validation
  check_access_for_category(object, category, NA)
  check_param(sort_by, values = c("category", "item"))
  if (is.null(category) && sort_by == "category") sort_by = "item"
  check_param(identifiers, values = c("original", "new"))
  
  if (entities != PATTERNS && !is.null(over) && over == "status") over = NULL
  if (!is.null(under) && under == "status") under = NULL
  category = if (is.numeric(category)) colnames(object@items_categories)[category] else category
  
  
  # Preparation of the variables for the chart plotting function
  if (entities == TRANSACTIONS) {
    vars = prepare_itemset_chart(tnpc, identifiers, length_one, under, over)
    
    itemsets = vars$itemsets
    items = data.frame(item = as.character(vars$items$item), stringsAsFactors = FALSE)
    under_text = vars$under
    over_text = vars$over
    over_legend = NULL
  }
  else{
    # Renaming columns for simplification
    colnames(tnpc)[colnames(tnpc) == "node" | colnames(tnpc) == "pattern"] = "itemset"
    
    # Itemset of length > 1, sorted by increasing length then by decreasing frequency
    tnpc = if (length_one) tnpc else tnpc[tnpc$length != 1, ]
    tnpc = tnpc[order(tnpc$length,
                      max(tnpc$frequency) - tnpc$frequency), ]
    
    # Assigning identifiers to the itemsets
    tnpc$ID = if (identifiers == "new") seq(nrow(tnpc)) else as.integer(rownames(tnpc))
    
    # Itemsets and separate items within the itemsets
    itemsets = tnpc$itemset
    items = data.frame(item = unique(unlist(tnpc$itemset)), stringsAsFactors = FALSE)
    
    # Text to display
    under_text = if (is.null(under)) NULL else tnpc[, under]
    if (is.null(over)) over_text = over_legend = NULL
    else if (over == "status") {
      over_text = object@status_colors[tnpc[, over]]
      over_legend = object@status_colors
    } else {
      over_text = tnpc[, over]
      over_legend = NULL
    }
  }
  
  # Labels and values of a category associated with the items
  if (!is.null(category)) items[, category] = object@items_categories[items$item, category]
  if (use_names) items$label = substr2(get_item_names(object, items$item), stop = n.cutoff)
  else items$label = items$item
  
  # Sorting items
  if (sort_by == "item") { # By name or by code
    if (has_item_names(object) && use_names) {
      items = items[order(get_item_names(object, items$item)), ]
    } else {
      items = items[order(match(items$item, object@items)), ]
    }
  } else {                 # According to the category (then name or code)
    items = items[order(items[[category]],
                        if (has_item_names(object) && use_names) get_item_names(object, items$item)
                        else match(items$item, object@items)), ]
  }
  rownames(items) = NULL
  
  # Categoryes values, colors and associated labels
  if (!is.null(category)) {
    category_name = category
    
    category = data.frame(value = unique(items[, category_name]), stringsAsFactors = FALSE)
    category$col = object@categories_colors[[category_name]][category$value]
    category$label = substr2(category$value, stop = c.cutoff)
    category = category[order(category$value), ]
  }
  
  
  # Setting title default value
  if (is.null(title)) title = paste(cap(substr(entities, 1, nchar(entities) - 1)), "itemsets")
  
  # Plotting of the chart (in the active device or in a PDF file)
  if (!is.null(name)) grDevices::pdf(paste0(turn_into_path(path), check_extension(name, "pdf")),
                                     14, 10, paper = "a4r", pointsize = 11)
  plot_itemset_chart(itemsets, items, category, jitter, under_text, over_text, over_legend, title)
  if (!is.null(name)) grDevices::dev.off()
  
  
  # Return (if transactions)
  if (entities == TRANSACTIONS) return(vars$transactions)
  
  # Renaming columns to their initial names before return (if nodes or patterns)
  colnames(tnpc)[colnames(tnpc) == "itemset"] = substr(entities, 1, nchar(entities)-1)
  # Nodes/patterns and characteristics, ordered according to ID (moved in 1st column)
  return(tnpc[order(tnpc$ID),
              c(ncol(tnpc), seq(ncol(tnpc)-1))])
})


#' Category tree
#' 
#' Plot a tree representing the values of one category and the related items. Vertices at depth 1
#'  represent the values of the chosen category and vertices at depth 2 represent items related to each
#'  specific category value.
#' 
#' @details
#' If `category` is `NULL`, items are sorted alphanumerically.
#' If `category` is not `NULL`, items are sorted according to the values of the category then in the
#'  order they are given.
#' 
#' The chart being plotted with the packages `ggraph` and `ggplot2`, it can be modified or completed
#'  afterwards using [`ggplot2::last_plot`] or the returned object.
#' 
#' @template default_category_values_colors
#'
#' @param object S4 object of class `TransactionAnalyzer`.
#' @param category Name or number of the category to represent on the tree (numbering according to
#'  the order of the columns of `object["items_categories"]`).
#' @param items Items to represent on the tree. Any subset of `object["items"]`.
#'  
#'  `"items"` and `"i"` are special values for `object["items"]`.
#' @param use_names  If `TRUE`, display item names if they are defined. Display their identification
#'  codes otherwise.
#' @param n.cutoff If `use_names` is `TRUE`, limit number of characters to display concerning the names
#'  of the represented items.
#' @param c.cutoff Limit number of characters to display in the legend for the category represented.
#' @param vertex_size Size of vertices at depth 1, representing the category values. One of the
#'  following.
#'  \describe{
#'    \item{A single numeric value}{All vertices have the size defined by this value.}
#'    \item{A named numeric vector}{The sizes are assigned to the category values identified by
#'          the names of the vector.}
#'    \item{An unnamed numeric vector}{Must be of length equal to the number of category values
#'          represented on the tree. The sizes are directly assigned to these category values
#'          in alphanumeric order.}
#'  }
#' @param vertex_alpha Opacity of vertices at depth 1 (from 0 to 1).
#' @param leaf_size Size of the leaves (i.e., vertices at depth 2), representing the items.
#'  One of the following.
#'  \describe{
#'    \item{A single numeric value}{All leaves have the size defined by this value.}
#'    \item{A named numeric vector}{The sizes are assigned to the items identified by the names of
#'          the vector.}
#'    \item{An unnamed numeric vector}{Must be of length equal to the number of items specified
#'          by the argument `items`. The sizes are directly assigned to these items in the order
#'          they are given.}
#'  }
#' @param leaf_alpha Opacity of the leaves (from 0 to 1).
#' @param leaf_margin Margin before the leaves (i.e. distance between the ends of the edges and the
#'  centers of the leaves).
#' @param label_size Size of the labels associated with the leaves.
#' @param label_margin Margin before the labels (i.e. distance between the centers of the leaves and
#'  the labels).
#' @return Graph created with the packages `ggraph` and `ggplot2`.
#' 
#' @author Gauthier Magnin
#' @seealso [`co_occurrence_chart`].
#'
#' @examples
#' category_tree_chart(TA_instance, "family")
#' category_tree_chart(TA_instance, 1,
#'                     items = c(19, 25, 27, 77, 87, 163, 192, 1603, 3146, 3350))
#' category_tree_chart(TA_instance, 1, items = TA_instance["items"][2:11]) +
#'   ggplot2::expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))
#' 
#' ## Use of an indicator as leaf size
#' ratio <- complexity_ratio(TA_instance["transactions"])
#' category_tree_chart(TA_instance, "family",
#'                     items = names(ratio),
#'                     use_names = FALSE,
#'                     leaf_size = ratio * 4)
#' 
#' @aliases category_tree_chart
#' @md
#' @export
setMethod(f = "category_tree_chart",
          signature = "TransactionAnalyzer",
          definition =
function(object, category = NULL, items = object["items"],
         use_names = TRUE, n.cutoff = NULL, c.cutoff = NULL,
         vertex_size = 4, vertex_alpha = 1,
         leaf_size = 3, leaf_alpha = 1, leaf_margin = 0,
         label_size = 3, label_margin = 0.05) {
  
  # Validation of the category access parameter and the items provided
  check_access_for_category(object, category, NA)
  items = get_items(object, items)
  
  
  # Creation of the hierarchy (tree depths and edges between vertices)
  if (!is.null(category)) {
    depth_1 = data.frame(parent = "root",
                         child = as.character(sort(unique(object@items_categories[as.character(items),
                                                                                  category]))),
                         stringsAsFactors = FALSE)
    depth_2 = data.frame(parent = as.character(object@items_categories[as.character(items), category]),
                         child = items,
                         stringsAsFactors = FALSE)
    hierarchy = rbind(depth_1, depth_2[order(depth_2$parent), ]) # Ordered by category
  } else {
    hierarchy = data.frame(parent = "root",
                           child = items[order(if (has_item_names(object) && use_names) names(items)
                                               else items)],
                           stringsAsFactors = FALSE)
  }
  
  # Graph vertices
  vertices = data.frame(name = unique(unlist(hierarchy)), stringsAsFactors = FALSE)
  if (use_names) {
    vertices$label = substr2(names(items[match(vertices$name, items)]), stop = n.cutoff)
  } else {
    vertices$label = items[match(vertices$name, items)]
  }
  
  # Positions and opacities of the vertices according to internal vertex or leaf
  vertices$is.leaf = is.na(match(vertices$name, hierarchy$parent))
  vertices$leaf_coord_multiplier = ifelse(vertices$is.leaf, 1 + leaf_margin, 1)
  vertices$label_coord_multiplier = ifelse(vertices$is.leaf, 1 + leaf_margin + label_margin, 1)
  vertices$size = NA_real_
  vertices$alpha = ifelse(vertices$is.leaf, leaf_alpha, vertex_alpha)
  
  # Validaion of the parameters defining the sizes of the vertices
  if (length(leaf_size) != 1 && !is_named(leaf_size) && length(leaf_size) != length(items))
    stop("If leaf_size has multiple values, it must be named or have the same length as items.")
  if (length(vertex_size) != 1 && !is_named(vertex_size) && length(vertex_size) != length(depth_1$child))
    stop("If vertex_size has multiple values, it must be named or have the same",
         " length as the number of represented category values.")
  
  # Sizes of leaf vertices
  if (length(leaf_size) == 1) vertices$size[vertices$is.leaf] = leaf_size
  else {
    vertices$size[match(if (is_named(leaf_size)) names(leaf_size) else items,
                        vertices$name)] = leaf_size
    vertices$size[vertices$is.leaf & is.na(vertices$size)] = 0
  }
  
  # Sizes of internal vertices
  if (length(vertex_size) == 1) vertices$size[!vertices$is.leaf] = vertex_size
  else {
    vertices$size[match(if (is_named(vertex_size)) names(vertex_size) else depth_1$child,
                        vertices$name)] = vertex_size
    vertices$size[!vertices$is.leaf & is.na(vertices$size)] = 0
  }
  
  # Processing of the category and its legend
  if (!is.null(category)) {
    vertices$group = object@items_categories[vertices$name, category]
    vertices$group[is.na(vertices$group)][-1] = vertices$name[is.na(vertices$group)][-1]
    category_legend = object@categories_colors[[category]][unique(vertices$group)][-1] # 1st = NA/root
    
    names(category_legend) = substr2(names(category_legend), stop = c.cutoff)
    vertices$group = substr2(vertices$group, stop = c.cutoff)
    
    edge_col = ifelse(vertices$is.leaf[-1], category_legend[as.character(vertices$group[-1])], "black")
  }
  
  # Graph
  tree = igraph::graph_from_data_frame(hierarchy, vertices = vertices)
  
  graph = ggraph::ggraph(tree, layout = "dendrogram", circular = TRUE) + 
    ggraph::geom_edge_diagonal(ggplot2::aes(color = if (!is.null(category)) edge_col)) +
    ggraph::geom_node_point(ggplot2::aes(x = x * leaf_coord_multiplier,
                                         y = y * leaf_coord_multiplier,
                                         filter = (name != "root"),
                                         color = if (!is.null(category)) group),
                            size = vertices$size[-1],
                            alpha = vertices$alpha[-1]) + 
    ggraph::geom_node_text(ggplot2::aes(x = x * label_coord_multiplier,
                                        y = y * label_coord_multiplier,
                                        filter = is.leaf,
                                        label = label,
                                        angle = atan(y / x) * 180 / pi, # Angle in degrees
                                        hjust = ifelse(x < 0, 1, 0),
                                        color = if (!is.null(category)) group), 
                           size = label_size, show.legend = FALSE) +
    ggplot2::theme_void() +
    ggplot2::coord_fixed()
  
  if (!is.null(category)) {
    category_name = if (is.numeric(category)) colnames(object@items_categories)[category] else category
    return(graph + ggplot2::scale_color_manual(cap(category_name),
                                               values = category_legend,
                                               guide = ggplot2::guide_legend(override.aes = list(size = 1.5))))
  }
  return(graph)
})


#' Co-occurrence chart, for TransactionAnalyzer
#' 
#' Plot a graph in which vertices are items and edges are their co-occurences in transactions (i.e. for
#'  each pair of items, the number of transactions containing it). Edges can also represent the
#'  proportions of these co-occurrences (i.e., the ratio between the number of transactions containing a
#'  pair of items and the number of transactions containing at least one of them).
#' 
#' @details
#' The chart being plotted with the packages `ggraph` and `ggplot2`, it can be modified or completed
#'  afterwards using [`ggplot2::last_plot`] or the returned object.
#' 
#' If `category` is `NULL` or `sort_by = "item"`, items are sorted alphanumerically.
#' If `category` is not `NULL` and `sort_by = "category"`, items are sorted according to the values of
#'  the category then alphanumerically.
#' 
#' @template default_category_values_colors
#' 
#' @note
#' If using the RStudio IDE and the value of the argument `edge_alpha` is not \eqn{1}, edges may not
#'  be displayed in the RStudio "Plots" pane. However, they will be actually displayed in the "Plot Zoom"
#'  window; while exporting the plot; or by using another graphics device.
#' 
#' @param object S4 object of class `TransactionAnalyzer`.
#' @param items Items for which to plot co-occurrences between pairs.
#'  Any subset of `object["items"]` containing at least two items.
#'  
#'  `"items"` and `"i"` are special values for `object["items"]`.
#' @param category Name or number of the category to represent on the graph (numbering according to
#'  the order of the columns of `object["items_categories"]`).
#' @param co_occ Matrix containing the co-occurrences (or their proportions) for at least the items
#'  specified by the argument `items`. Is computed if `NULL`.
#' @param proportions `TRUE` if the proportions of co-occurrences are to be plotted (and computed, if
#'  `co_occ` is `NULL`) instead of the co-occurrences themselves.
#' @param min_occ Minimum number of co-occurrences (or minimum proportion) to consider to plot a link
#'  between two items.
#'  Default value depends on the argument `proportions` and allows not to plot links between items
#'  that never co-occur.
#' @param max_occ Maximum number of co-occurrences (or maximum proportion) to consider to plot a link
#'  between two items.
#' @param use_names If `TRUE`, display item names if they are defined. Display their identification
#'  codes otherwise.
#' @param n.cutoff If `use_names = TRUE`, limit number of characters to display concerning the names
#'  of the represented items.
#' @param c.cutoff Limit number of characters to display in the legend for the category represented.
#' @param sort_by Sorting method of displayed items. One of `"category"`, `"item"`.
#' @inheritParams plot_heb_chart
#' @return Graph created with the packages `ggraph` and `ggplot2`.
#' 
#' @author Gauthier Magnin
#' @seealso [`co_occurrence_matrix`], [`category_tree_chart`].
#' 
#' Method for signature `TransactionSet`:
#' [`co_occurrence_chart,TransactionSet`][co_occurrence_chart,TransactionSet-method].
#' 
#' @examples
#' co_occurrence_chart(TA_instance, category = "family")
#' co_occurrence_chart(TA_instance, category = 1, n.cutoff = 20) +
#'   ggplot2::expand_limits(x = c(-1.5, 1.5), y = c(-1.5, 1.5))
#' co_occurrence_chart(TA_instance, category = "family",
#'                     min_occ = 2, palette = "OrRd")
#' 
#' co_occurrence_chart(TA_instance, TA_instance["items"][2:13], "family")
#' co_occurrence_chart(TA_instance, TA_instance["items"][2:13], "family",
#'                     proportions = TRUE)
#' 
#' @aliases co_occurrence_chart co_occurrence_chart,TransactionAnalyzer
#' @md
#' @export
setMethod(f = "co_occurrence_chart",
          signature = "TransactionAnalyzer",
          definition =
function(object, items = object["items"], category = NULL,
         co_occ = NULL, proportions = FALSE,
         min_occ = if (proportions) .Machine$double.xmin else 1, max_occ = Inf,
         use_names = TRUE, n.cutoff = NULL, c.cutoff = NULL, sort_by = "category",
         vertex_size = 3, vertex_alpha = 1, vertex_margin = 0.05,
         label_size = 3, label_margin = 0.05,
         edge_looseness = 0.8, edge_alpha = 1,
         palette = "Blues", palette_direction = 1) {
  
  # Parameter validation
  check_access_for_category(object, category, NA)
  check_param(sort_by, values = c("category", "item"))
  if (is.null(category) && sort_by == "category") sort_by = "item"
  items = get_items(object, items)
  if (length(items) < 2 )
    stop("items must be a subset of the items contained in object and contain at least two items.")
  
  # Creation of the hierarchy (tree depths and edges between vertices) 
  hierarchy = data.frame(parent = "root", child = items, stringsAsFactors = FALSE)
  
  # Sort by name, by identifer or according to the values of the category (then name or code)
  if (sort_by == "item") {
    if (has_item_names(object) && use_names) hierarchy = hierarchy[order(names(items)), ]
    else hierarchy = hierarchy[order(items), ]
  } else {
    hierarchy = hierarchy[order(object@items_categories[as.character(items), category],
                                if (has_item_names(object) && use_names) names(items) else items), ]
  }
  
  # Vertices of the graph
  vertices = data.frame(name = unique(unlist(hierarchy)), stringsAsFactors = FALSE)
  if (use_names) {
    vertices$label = substr2(names(items[match(vertices$name, items)]), stop = n.cutoff)
  } else {
    vertices$label = items[match(vertices$name, items)]
  }
  
  # Processing of the category and its legend
  if (!is.null(category)) {
    vertices$group = object@items_categories[vertices$name, category]
    category_legend = object@categories_colors[[category]][unique(vertices$group)][-1] # 1st is NA
    category_legend = category_legend[order(names(category_legend))]
    
    names(category_legend) = substr2(names(category_legend), stop = c.cutoff)
    vertices$group = substr2(vertices$group, stop = c.cutoff)
  }
  
  # Compute or subset the co-occurrence matrix
  if (is.null(co_occ)) co_occ = co_occurrence_matrix(object@transactions, items, proportions)
  else co_occ = co_occ[as.character(items), as.character(items)]
  
  # Links to be drawn between the vertices (different from the edges of the tree) 
  co_occ = as.data.frame(as.table(co_occ), stringsAsFactors = FALSE)
  co_occ = co_occ[co_occ$Var1 != co_occ$Var2 & !duplicated(t(apply(co_occ[, c(1,2)], 1, sort))), ]
  connections = co_occ[co_occ$Freq >= min_occ & co_occ$Freq <= max_occ, ]
  
  
  # Preparation of the list of arguments
  args = list(hierarchy = hierarchy, vertices = vertices, edges = connections,
              vertex_size = vertex_size, vertex_alpha = vertex_alpha, vertex_margin = vertex_margin,
              label_size = label_size, label_margin = label_margin,
              edge_looseness = edge_looseness, edge_alpha = edge_alpha,
              palette = palette, palette_direction = palette_direction)
  
  # Scale limits, breakpoints and name
  if (proportions) {
    args$scale_name = "Co-occurrence proportions"
    args$limits = c(0, 1)
    args$breaks = "default"
  } else {
    args$scale_name = "Co-occurrences"
    args$limits = c(1, max(co_occ$Freq))
    args$breaks = unique(floor(pretty(seq(args$limits[1], args$limits[2]))))
  }
  
  # Name and values of the legend relating to the category
  if (!is.null(category)) {
    category_name = if (is.numeric(category)) colnames(object@items_categories)[category] else category
    args$legend_name = cap(category_name)
    args$legend_values = category_legend
  }
  
  # Call of the plot function
  return(do.call(plot_heb_chart, args))
})



#### Methods for association rule extraction and visualization ####

#' Rules extraction
#' 
#' Extract association rules from the transactions (i.e. presence implication between two disjoint
#'  itemsets). Can be used to find all rules, rules relating to patterns (or other specific itemsets)
#'  or relating to specific items.
#' 
#' @details
#' Only creates rules with one item in the consequent. The reason is detailed on
#'  \href{https://borgelt.net/doc/apriori/apriori.html#conseq}{this web page} by the author of the
#'  algorithms used. Here are some extracts: \cr
#'  "\emph{There are usually already (many) more association rules than item sets if only a single
#'  item is allowed in the consequents.}" \cr
#'  "\emph{Multiple items in the consequents of association rules therefore come at a considerable
#'  cost.}" \cr
#'  "\emph{There is no true benefit.}"
#' 
#' \subsection{Main characteristics and redundancy}{
#' 
#' \loadmathjax
#' The main characteristics of an association rule of the form \mjeqn{X \rightarrow Y}{X -> Y} are
#'  the following.
#'  \itemize{
#'    \item{The \strong{support}: support of the itemset \mjeqn{X \cup Y}{X union Y}, i.e. the
#'          proportion of transactions containing \mjeqn{X \cup Y}{X union Y} among all transactions.}
#'    \item{The \strong{confidence}: quotient of the support of \mjeqn{X \cup Y}{X union Y} and the
#'          support of \eqn{X}, i.e. the number of transactions in which the rule is correct
#'          relative to the number of transactions containing the antecedent \eqn{X}.}
#'    \item{The \strong{lift}: quotient of the confidence of \mjeqn{X \rightarrow Y}{X -> Y} and the
#'          support of the consequent \eqn{Y}.}
#'  }
#' 
#' Support and confidence indices measure the strength of a rule.
#'  A rule can be said to be \strong{valid} if its confidence and its support are greater than two
#'  chosen thresholds. Defining minimum support \eqn{s} and confidence \eqn{c} means that the
#'  union of items in the antecedent and consequent of rules must be present in a minimum of \eqn{s}\%
#'  of transactions and at least \eqn{c}\% of transactions must satisfy the antecedent. A rule is said
#'  to be \strong{exact} if its confidence is \eqn{1}, otherwise the rule is \strong{partial}.
#' 
#' The lift measures the importance of a rule. A lift greater than \eqn{1} reflects a positive
#'  correlation between the presences of \eqn{X} and \eqn{Y}, and therefore the significant nature of
#'  the association.
#' 
#' The direction of the rule (i.e. \mjeqn{X \rightarrow Y}{X -> Y} or \mjeqn{Y \rightarrow X}{Y -> X})
#'  does not impact the support and the lift but does impact the confidence.
#' 
#' A rule is redundant if a more general rule with the same or higher confidence exists. A rule is more
#'  general if it has the same consequent but one or more items removed from the antecedent. In other
#'  words, having inferred a dependency \mjeqn{X \rightarrow Y}{X -> Y}, any other dependency of the
#'  form \mjeqn{X \cup A \rightarrow Y}{X union A -> Y} is considered redundant.
#' }
#' 
#' \subsection{Additional indicators}{
#' 
#' Considering the notation \mjeqn{\overline{X}}{not X} denoting any itemset not containing \eqn{X},
#'  the computed additional indicators associated with an association rule of the form
#'  \mjeqn{X \rightarrow Y}{X -> Y} are the following.
#'  \itemize{
#'    \item{The \strong{specificity}: confidence of the rule
#'          \mjeqn{\overline{X} \rightarrow \overline{Y}}{not X -> not Y}.}
#'    \item{The \strong{accuracy}: sum of the support of the itemset \mjeqn{X \cup Y}{X union Y} and
#'          the support of \mjeqn{\overline{X \cup Y}}{not(X union Y)}.}
#'    \item{The \strong{added value}: difference between the confidence of the rule
#'          \mjeqn{X \rightarrow Y}{X -> Y} and the support of the consequent \eqn{Y}.}
#'  }
#' 
#' If the support of the antecedent \eqn{X} is equal to \eqn{1}, the specificity of the rule cannot be
#'  computed and is \code{NA}.
#' 
#' The direction of the rule does not impact the accuracy but does impact the specificity and the
#'  added value.
#' }
#' 
#' \subsection{Additional arguments}{
#' 
#' If \code{itemsets = NULL}, additional arguments are \code{parameter}, \code{appearance} and
#'  \code{control} of function \code{\link[arules:apriori]{apriori}} from the package \code{arules}.
#'  These arguments allow to specify minimum support (default \code{0.1}), minimum confidence (default
#'  \code{0.8}), minimum length (default \code{1}), maximum length (default \code{10}), specific items
#'  in antecedent or consequent, and some operating parameters of the rule extraction algorithm.
#' 
#' If \code{itemsets} is \code{"patterns"} or a list, additional arguments are \code{confidence},
#'  \code{method}, \code{reduce} and \code{verbose} of function
#'  \code{\link[arules:ruleInduction]{ruleInduction}} from the package \code{arules}.
#'  These arguments allow to specify minimum confidence (default \code{0.8}) and some
#'  operating parameters of the rule extraction algorithm.
#' }
#' 
#' @param object S4 object of class \code{TransactionAnalyzer}.
#' @param itemsets If not \code{NULL}, character or list of itemsets for which to extract the
#'  association rules.
#'  \itemize{
#'    \item{If \code{NULL}, look for all rules within the transactions saved in \code{object}
#'          or for rules with specific items.}
#'    \item{If \code{"patterns"}, look for rules whose union of the antecedent and the consequent form
#'          an entire pattern among those contained in \code{object} (more precisely, in
#'          \code{object["patterns"]$pattern}).}
#'    \item{Otherwise, a list of itemsets defined the same way as \code{object["patterns"]$pattern}.
#'          Look for rules whose union of the antecedent and the consequent form an entire itemset.}
#'  }
#' @param pruning If \code{TRUE}, remove redundant rules (see 'Details' to know how redundant rules
#'  are defined).
#' @param arules If \code{TRUE}, rules are returned as an object of class
#'  \code{\link[arules:rules-class]{rules}} from the package \code{arules}. If \code{FALSE}, rules are
#'  returned as a data frame.
#' @param as_sets If \code{FALSE}, antecedents and consequents of the returned rules will be character
#'  vectors. If \code{TRUE}, they will be factors written in mathematical notation (i.e. set notation).
#'  Ignored if \code{arules} is \code{TRUE}.
#' @param more \code{TRUE} or \code{FALSE} whether to compute additional indicators associated with the
#'  rules. Ignored if \code{arules} is \code{TRUE}. See 'Details'.
#' @param ... Additional arguments to configure the extraction. See 'Details'.
#' @return Data frame or object of class \code{rules} (according to the argument \code{arules})
#'  containing the extracted rules and their characteristics.
#'  
#'  If \code{itemsets} is not \code{NULL}, the additional column \code{"itemset"} refers to the index
#'  of the one from which the rule was generated in the list of patterns (if \code{from = "patterns"})
#'  or in the given list (otherwise).
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{rules_chart}}, \code{\link[arules:rules-class]{arules::rules}}.
#' 
#' @examples
#' ## Basic rule extraction
#' rules_1 <- extract_rules(TA_instance, itemsets = NULL)
#' rules_2 <- extract_rules(TA_instance, itemsets = "patterns")
#' rules_3 <- extract_rules(TA_instance, itemsets = list(c("931", "3180"),
#'                                                       c("25", "192", "328")))
#' 
#' ## Rule extraction with conditions on the antecedent and the consequent
#' params <- list(supp = 0.001, conf = 0.5, maxlen = 2)
#' rules_4 <- extract_rules(TA_instance,
#'                          parameter = params,
#'                          appearance = list(rhs = "328"))
#' rules_5 <- extract_rules(TA_instance,
#'                          parameter = params,
#'                          appearance = list(lhs = "497"))
#' rules_6 <- extract_rules(TA_instance,
#'                          parameter = list(supp = 0.001, conf = 0,
#'                                           minlen = 2, maxlen = 2),
#'                          appearance = list(lhs = "328", rhs = "3180"))
#' 
#' ## Getting rules as an object of class rules from the package arules
#' rules_7 <- extract_rules(TA_instance, arules = TRUE)
#' arules::inspect(rules_7)
#' 
#' @aliases extract_rules
#' @export
setMethod(f = "extract_rules",
          signature = "TransactionAnalyzer",
          definition =
function(object, itemsets = NULL, pruning = FALSE, arules = FALSE,
         as_sets = FALSE, more = FALSE, ...) {
  
  # Validation of the parameter for choosing the itemsets from which to extract the rules
  if ((!is.null(itemsets) && !is.character(itemsets) && !is.list(itemsets))
      || (is.character(itemsets) && itemsets != "patterns"))
    stop("itemsets must be NULL, \"patterns\" or a list of item sets.")
  
  # Conversion of the TransactionSet object to arules transactions
  transact = methods::as(object@transactions, "transactions")
  
  if (is.null(itemsets)) {
    
    # Verification of the correct choice of the parameter requesting the extraction of rules
    args = list(...)
    if ("parameter" %in% names(args) && "target" %in% names(args$parameter)
        && args$parameter$target != "rules") stop("target parameter must be \"rules\"")
    
    # Prevent arules error if support parameter is greater than 1
    if ("parameter" %in% names(args) &&
        any(grepl("su*", names(args$parameter))) && args$parameter$su > 1) return(NULL)
    
    # Specifying not to display progress
    if (!("control" %in% names(args))) args$control = list(verbose = FALSE)
    else if (!("verbose" %in% names(args$control))) args$control$verbose = FALSE
    
    # Extraction of association rules
    # rules = arules::apriori(transact, ...)
    rules = do.call(arules::apriori, c(data = transact, args))
    
  } else {
    if (is.character(itemsets)) {
      check_init(object, PATTERNS, prefix = "If itemsets = \"patterns\", ")
      itemsets = object@patterns$pattern
    }
    
    # Conversion of the list of item sets into an arules::itemMatrix then arules::itemsets object
    arules_itemsets = methods::new("itemsets", items = arules::encode(itemsets, object@items))
    
    # Extraction of association rules
    rules = arules::ruleInduction(arules_itemsets, transact, ...)
  }
  
  # Find and remove redundant rules
  if (pruning) rules = rules[!arules::is.redundant(rules)]
  
  
  # If no rule matches the search criteria: NULL
  if (length(rules) == 0) return(NULL)
  
  # If requested, return rules as a rules class from the arules package
  if (arules) return(rules)
  
  
  # Conversion to data frame (and removal of a column specific to arules::apriori)
  rules_df = arules::DATAFRAME(rules)
  rules_df = rules_df[, colnames(rules_df) != "coverage"]
  
  # Changing notation
  if (!as_sets) {
    rules_df[, c("LHS", "RHS")] = apply(rules_df[, c("LHS", "RHS")], 2, vector_notation)
  }
  
  # Renaming columns "LHS" and "RHS" and put the column "=>" back
  colnames(rules_df)[c(1,2)] = c("antecedent", "consequent")
  rownames(rules_df) = NULL
  
  # Renaming the column "count" to "frequency" if exists or compute frequencies; and reorder columns
  if ("count" %in% colnames(rules_df)) {
    colnames(rules_df)[colnames(rules_df) == "count"] = "frequency"
  } else {
    rules_df$frequency = as.integer(round(rules_df$support * dim(transact)[1]))
  }
  rules_df = rules_df[, c(1, 2, ncol(rules_df), seq(3, ncol(rules_df) - 1))]
  
  # Compute additional indicators
  if (more) {
    rules_df = cbind(rules_df, compute_additional_rule_indicators(object, rules, transact))
    itemset_col = which(colnames(rules_df) == "itemset")
    rules_df = rules_df[, c(setdiff(seq_len(ncol(rules_df)), itemset_col), itemset_col)]
  }
  
  return(rules_df)
})


#' Computation of indicators relating to association rules
#' 
#' Compute indicators relating to association rules excluding the main ones (which are support,
#'  confidence and lift). Compute the specificity, accuracy and added value of each given rule.
#' 
#' @details
#' \loadmathjax
#' Considering the notation \mjeqn{\overline{X}}{not X} denoting any itemset not containing \eqn{X},
#'  the computed indicators of an association rule of the form \mjeqn{X \rightarrow Y}{X -> Y} are:
#'  * The **specificity**: confidence of the rule
#'    \mjeqn{\overline{X} \rightarrow \overline{Y}}{not X -> not Y}.
#'  * The **accuracy**: sum of the support of the itemset \mjeqn{X \cup Y}{X union Y} and the
#'    support of \mjeqn{\overline{X \cup Y}}{not(X union Y)}.
#'  * The **added value**: difference between the confidence of the rule \mjeqn{X \rightarrow Y}{X -> Y}
#'    and the support of the consequent \eqn{Y}.
#'    
#' If the support of the antecedent \eqn{X} is equal to \eqn{1}, the specificity of the rule cannot be
#'  computed and is `NA`.
#' 
#' As a reminder, the support and the confidence of \mjeqn{X \rightarrow Y}{X -> Y} are defined as
#'  follows.
#'  * The **support** of the rule is the support of the itemset \mjeqn{X \cup Y}{X union Y}, i.e. the
#'    proportion of transactions containing \mjeqn{X \cup Y}{X union Y} among all transactions.
#'  * The **confidence** of the rule is the quotient of the support of \mjeqn{X \cup Y}{X union Y} and
#'    the support of \eqn{X}, i.e. the number of transactions in which the rule is correct
#'    relative to the number of transactions containing the antecedent \eqn{X}.
#' 
#' @template method_not_exported
#' 
#' @param object S4 object of class `TransactionAnalyzer`.
#' @param rules Object of class `rules` containing the rules whose indicators are to be computed.
#' @param transactions Object of class `transactions` used to generate `rules`. Needed if the `rules`
#'  were generated using the `arules` function `ruleInduction` and not `apriori`.
#' @return Matrix in which each column is one indicator and rows match the association rules.
#' 
#' @author Gauthier Magnin
#' @seealso [`extract_rules`], [`arules::rules`][arules::rules-class],
#'  [`arules::transactions`][arules::transactions-class].
#' @aliases compute_additional_rule_indicators
#' @md
#' @keywords internal
setMethod(f = "compute_additional_rule_indicators",
          signature = "TransactionAnalyzer",
          definition =
function(object, rules, transactions = NULL) {
  
  measures = arules::interestMeasure(rules, c("table", "addedValue"), transactions)
  
  indicators = matrix(
    c(
      measures$table.n00 / (measures$table.n00 + measures$table.n01),   # Specificity
      (measures$table.n11 + measures$table.n00) / dim(transactions)[1], # Accuracy
      measures$addedValue                                               # Added value
    ),
    ncol = 3, byrow = FALSE, dimnames = list(NULL, c("specificity", "accuracy", "added.value"))
  )
  
  # Computed specificity is NaN if support(antecedent) = 1. Change NaN to NA
  indicators[is.nan(indicators[, "specificity"]), "specificity"] = NA_real_
  
  return(indicators)
})


#' Length-two association rule visualization
#' 
#' Plot a graph in which vertices are items and edges are association rules in which the antecedent is
#'  an item and the consequent is another item.
#' 
#' @details
#' \loadmathjax
#' For an association rule \mjeqn{X \rightarrow Y}{X -> Y}, the reciprocal rule is the rule
#'  \mjeqn{Y \rightarrow X}{Y -> X}. These two rules have the same support, lift, frequency and
#'  accuracy but may have two different confidence, specificity and added values.
#' 
#' If `display` refers to the selection of rules having the highest or lowest specified characteristic
#'  and if an association rule and its reciprocal have the same value for this characteristic, both
#'  rules are represented.
#' 
#' Rules whose value of the desired characteristic is `NA` are not represented.
#' 
#' The chart being plotted with the packages `ggraph` and `ggplot2`, it can be modified or completed
#'  afterwards using [`ggplot2::last_plot`] or the returned object.
#' 
#' If `category` is `NULL` or `sort_by = "item"`, items are sorted alphanumerically.
#' If `category` is not `NULL` and `sort_by = "category"`, items are sorted according to the values of
#'  the category then alphanumerically.
#' 
#' Since other values are returned besides the graph and a graph is automatically plotted if
#'  the return is not assigned to a variable but is not plotted if the return is assigned,
#'  the argument `plot` allows to counter this natural effect. The graph can thus be plotted despite
#'  an assignment thanks to this argument. However, if `plot = TRUE` and the return is not assigned
#'  to a variable, the graph will be plotted twice.
#' 
#' @template default_category_values_colors
#' 
#' @note
#' If using the RStudio IDE and if the argument `display` refers to the confidence, the specificity or
#'  the added value and `direction` is `TRUE`, edges may not be displayed in the RStudio "Plots" pane.
#'  However, they will be actually displayed in the "Plot Zoom" window; while exporting the plot; or by
#'  using another graphics device.
#' Moreover, such plotting may take a while.
#' 
#' @param object S4 object of class `TransactionAnalyzer`.
#' @param rules Data frame of association rules to plot (given by the [`extract_rules`] method).
#'  Only those of length 2 are considered. If `NULL`, rules of length 2 are extracted from
#'  `object["transactions"]` using the mining parameters given as `parameter`.
#' @param items Items to consider in the given or extracted rules. If `NULL`, items considered are all
#'  those present in the given or extracted rules. Any subset of `object["items"]`.
#'  
#'  `"items"` and `"i"` are special values for `object["items"]`.
#' @param parameter List of mining parameters specifying minimum support and minimum confidence of
#'  association rules to extract. Ignored if `rules` is not `NULL`. See
#'  [APparameter][arules::APparameter] for more.
#' @param display Rule characteristic to visualize. One of the following.
#'  \describe{
#'    \item{`"support"` (or `"supp"`)}{The support of the two rules existing between two items is
#'          represented.}
#'    \item{`"confidence"` or (`"conf"`)}{The two confidence values of the two rules existing between two
#'          items are both represented.}
#'    \item{`"lift"`}{The lift of the two rules existing between two items is represented.}
#'    \item{`"frequency"` (or `"freq"`)}{The frequency of the two rules existing between two items is
#'          represented.}
#'    \item{`"specificity"` (or `"spec"`)}{The two specifity values of the two rules existing between two
#'          items are both represented.}
#'    \item{`"accuracy"` (or `"accu"`)}{The accuracy of the two rules existing between two items is
#'          represented.}
#'    \item{`"added.value"` (or `"adva"`)}{The two added values of the two rules existing between two
#'          items are both represented.}
#'  }
#'  For the confidence, the specificy and the added value, this can be preceded by `"highest "` or by
#'  `"lowest "` to choose between visualizing a rule or its reciprocal instead of visualizing both
#'  rules.
#'  \describe{
#'    \item{`"highest "` (or `"hi."`)}{Only the rule having the highest value of the desired
#'          characteristic is represented among the two rules existing between two items.}
#'    \item{`"lowest "` (or `"lo."`)}{Only the rule having the lowest value of the desired
#'          characteristic is represented among the two rules existing between two items.}
#'  }
#' @param threshold Threshold from which the characteristic referred by `display` must be for a rule to
#'  be considered.
#' @param direction Ignored if `display` does not refer to highest or lowest values.
#'  If `FALSE`, the opacity of the edges representing the rules is set by the argument `edge_alpha`.
#'  If `TRUE`, the opacity increases gradually according to the direction of the rule represented.
#'  Always `FALSE` if `display` refers to the support, the lift, the frequency or the accuracy.
#'  Always `TRUE` if `display` refers to the confidence, the specificity or the added value without
#'  referring to highest or lowest values.
#' @param use_names If `TRUE`, display item names if they are defined. Display their identification
#'  codes otherwise.
#' @param n.cutoff If `use_names = TRUE`, limit number of characters to display concerning the names
#'  of the represented items.
#' @param category Name or number of the category to represent on the graph (numbering according to
#'  the order of the columns of `object["items_categories"]`).
#' @param c.cutoff Limit number of characters to display in the legend for the category represented.
#' @param sort_by Sorting method of displayed items. One of `"category"`, `"item"`.
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
#'  Ignored if `display` refers to the confidence, the specificity or the added value and `direction`
#'  is `TRUE`.
#' @param palette
#'  Name of the palette to use for coloring the edges.
#'  
#'  If `display` refers to the added value, one of `"Spectral"`, `"RdYlGn"`, `"RdYlBu"`, `"RdGy"`,
#'  `"RdBu"`, `"PuOr"`, `"PRGn"`, `"PiYG"`, `"BrBG"`. Default is `"RdBu"`.
#'  
#'  If `display` does not refer to the added value, one of `"Blues"`, `"BuGn"`, `"BuPu"`, `"GnBu"`,
#'  `"Greens"`, `"Greys"`, `"Oranges"`, `"OrRd"`, `"PuBu"`, `"PuBuGn"`, `"PuRd"`, `"Purples"`,
#'  `"RdPu"`, `"Reds"`, `"YlGn"`, `"YlGnBu"`, `"YlOrBr"`, `"YlOrRd"`. Default is `"Blues"`.
#' @param palette_direction Direction in which to use the color palette.
#'  If `1`, colors are in original order (e.g. from the lightest to the darkest).
#'  If `-1`, color order is reversed (e.g. from the darkest to the lightest).
#' @param plot If `TRUE`, the chart is plotted in the active graphics device before the return.
#' @return 
#'  \describe{
#'    \item{`graph`}{Graph created with the packages `ggraph` and `ggplot2`.}
#'    \item{`rules`}{Association rules represented on the graph (i.e. of length 2 and considering
#'                   the arguments `items`, `parameter` and `threshold`).}
#'  }
#' 
#' @author Gauthier Magnin
#' @seealso [`extract_rules`].
#' 
#' @examples
#' ## All rules of length 2
#' result <- rules_chart(TA_instance, category = "family")
#' plot(result$graph)
#' result$rules
#' 
#' ## Rules from a data frame
#' rules_chart(TA_instance, rules = result$rules[11:20, ], category = 1)
#' rules_chart(TA_instance, rules = result$rules[11:20, ], items = "items")
#' 
#' ## Rules relating to specific items
#' ## Display of confidence or display rules of highest or of lowest confidence
#' rules_chart(TA_instance, items = c(497, 930, 402), category = 1,
#'             display = "confidence")
#' rules_chart(TA_instance, items = c(497, 930, 402), category = 1,
#'             display = "confidence", palette = "category10")
#' rules_chart(TA_instance, items = c(497, 930, 402), category = 1,
#'             display = "highest confidence", palette_direction = 1)
#' rules_chart(TA_instance, items = c(497, 930, 402), category = 1,
#'             display = "lowest confidence", palette_direction = -1)
#' 
#' ## Display of support or lift
#' rules_chart(TA_instance, category = 1, display = "support")
#' rules_chart(TA_instance, category = 1, display = "lift")
#' rules_chart(TA_instance, category = 1, display = "lift",
#'             threshold = 5, n.cutoff = 20)$graph +
#'   ggplot2::expand_limits(x = c(-1.5, 1.5), y = c(-1.5, 1.5))
#' 
#' @aliases rules_chart
#' @md
#' @export
setMethod(f = "rules_chart",
          signature = "TransactionAnalyzer",
          definition =
function(object, rules = NULL, items = NULL,
         parameter = list(supp = 0.001, conf = 0),
         display = "highest confidence", threshold = NULL, direction = FALSE,
         use_names = TRUE, n.cutoff = NULL,
         category = NULL, c.cutoff = NULL,
         sort_by = "category",
         vertex_size = 3, vertex_alpha = 1, vertex_margin = 0.05,
         label_size = 3, label_margin = 0.05,
         edge_looseness = 0.8, edge_alpha = 1,
         palette = "default", palette_direction = 1,
         plot = FALSE) {
  
  # Conversion of factor values in rules to character values
  a_factor = FALSE
  c_factor = FALSE
  if (!is.null(rules)) {
    if (is.factor(rules$antecedent)) {
      rules$antecedent = vector_notation(rules$antecedent)
      a_factor = TRUE
    }
    if (is.factor(rules$consequent)) {
      rules$consequent = vector_notation(rules$consequent)
      c_factor = TRUE
    }
  }
  
  
  ## Validation of the parameters
  
  # Validation of the provided items
  if (!is.null(items)) items = get_items(object, items)
  
  # Validation of the provided rule search parameters
  if (is.null(rules)) {
    if (is.null(parameter)) parameter = list(minlen = 2, maxlen = 2)
    else parameter$minlen = parameter$maxlen = 2
  }
  
  # Validation of the category access parameter
  check_access_for_category(object, category, NA)
  check_param(sort_by, values = c("category", "item"))
  if (is.null(category) && sort_by == "category") sort_by = "item"
  
  # Validate the parameter for choosing the characteristic to display and compute related variables
  col_to_display = (function(x) {
    if (grepl("supp", x)) return("support")
    if (grepl("conf", x)) return("confidence")
    if (grepl("lift", x)) return("lift")
    if (grepl("freq", x)) return("frequency")
    if (grepl("spec", x)) return("specificity")
    if (grepl("accu", x)) return("accuracy")
    if (grepl("adva|added", x)) return("added.value")
    return(x)
  })(display)
  
  check_param(col_to_display, var_name = "display",
              values = c("support", "confidence", "lift", "frequency",
                         "specificity", "accuracy", "added.value"),
              suffix = ", optionally preceded by \"highest\" or \"lowest\"; or one of their abbreviations")
  
  if      (grepl("hi", display)) operator = ">"
  else if (grepl("lo", display)) operator = "<"
  else                           operator = NULL
  
  is_an_additional = (col_to_display %in% c("specificity", "accuracy", "added.value"))
  has_direction = (col_to_display %in% c("confidence", "specificity", "added.value"))
  want_direction = (is.null(operator) || direction)
  is_divergent = col_to_display == "added.value"
  
  # Validation of the given rules
  if (!is.null(rules) && !is.element(col_to_display, colnames(rules))) {
    stop("rules does not contain the characteristic to be displayed (", col_to_display, ").")
  }
  
  # Validation of the parameters for choosing the palette
  check_param(palette_direction, values = c(1, -1), quotes = FALSE)
  
  if (palette == "default") {
    palette = if (is_divergent) "RdBu" else "Blues"
  }
  else if (is_divergent) {
      check_param(palette,
                  values = c("Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn",
                             "PiYG", "BrBG"),
                  prefix = "If display refers to the added value, ")
  } else {
    check_param(palette,
                values = c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd",
                           "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu",
                           "YlOrBr", "YlOrRd"),
                prefix = "If display does not refer to the added value, ")
  }
  
  
  ## Extraction of rules and items to consider
  
  # Computation of rules or removal of irrelevant rules
  if (is.null(rules)) {
    
    args = list(object    = object,
                parameter = parameter,
                more      = is_an_additional)
    
    # Computation of rules without or with specification of items
    if (!is.null(items) && !identical(items, object@items)) {
      args = c(args, appearance = list(list(both = items)))
    }
    rules = do.call(extract_rules, args)
    
    # Create an empty data frame if no rules could be extracted
    if (is.null(rules)) {
      rules = data.frame(character(0), character(0), integer(0), numeric(0), numeric(0), numeric(0))
      colnames(rules) = c("antecedent", "consequent", "frequency", "support", "confidence", "lift")
      
      if (args$more) {
        rules = cbind(rules, specificity = numeric(0), accuracy = numeric(0), added.value = numeric(0))
      }
    }
    
  } else {
    # Items present in the rules given before removal of those that are not of size 2
    if (is.null(items)) {
      items_tmp = get_items(object, unique(unlist(rules[, c("antecedent", "consequent")])))
    }
    
    # Removal of rules that are not of size 2
    rules = rules[lengths(rules$antecedent) == 1
                  & lengths(rules$consequent) == 1, ]
    
    if (!is.null(items)) {
      # Removal of rules relating to items that are not searched
      rules = rules[rules$antecedent %in% items
                    & rules$consequent %in% items, ]
    }
  }
  
  # Retrieval of items corresponding to the extracted or given rules
  if (is.null(items)) {
    if (nrow(rules) != 0) {
      items = get_items(object, unique(unlist(rules[, c("antecedent", "consequent")])))
    } else {
      if (exists("items_tmp")) items = items_tmp
      else items = get_items(object, "items")
    }
  }
  
  # Remove rules having NA values for the characteristic
  rules = rules[!is.na(rules[, col_to_display]), ]
  
  # Application of the threshold on the characteristic to be displayed
  if (!is.null(threshold)) rules = rules[rules[, col_to_display] >= threshold, ]
  
  
  ## Simplification of the rules to consider
  
  if (nrow(rules) != 0) {
    
    # Simplification of the structure (lists -> vectors)
    rules$antecedent = unlist(rules$antecedent)
    rules$consequent = unlist(rules$consequent)
    
    # Finding reciprocal rules (A -> B; B -> A)
    to_keep = rep(TRUE, nrow(rules))
    dup_from_first = duplicated(t(apply(rules[, c("antecedent", "consequent")], 1, sort)))
    
    # Keeping the reciprocal rules having a lower or higher directed indicator (and equivalent)
    if (!is.null(operator)) {
      dup_from_last = duplicated(t(apply(rules[, c("antecedent", "consequent")], 1, sort)),
                                 fromLast = TRUE)
      
      # For each duplicated rule, find its duplicate and compare the desired indicator
      for (i1 in which(dup_from_first)) {
        
        for (i2 in which(dup_from_last)) {
          if (all(rules[i2, c("antecedent", "consequent")] == rules[i1, c("consequent", "antecedent")]))
            break
        }
        
        if_2.0(rules[i1, col_to_display], operator, rules[i2, col_to_display],
               expression(to_keep[i2] <- FALSE))
        
        if_2.0(rules[i2, col_to_display], operator, rules[i1, col_to_display],
               expression(to_keep[i1] <- FALSE))
      }
    } else if (!has_direction) {
      # Ignoring the reciprocal rules because they correspond to duplicates
      to_keep[dup_from_first] = FALSE
    }
  } else {
    rules$antecedent = character(0)
    rules$consequent = character(0)
    to_keep = rep(TRUE, nrow(rules))
  }
  
  
  ## Preparation of the chart
  
  # Creation of the hierarchy (tree depths and edges between vertices)
  hierarchy = data.frame(parent = "root", child = items, stringsAsFactors = FALSE)
  
  # Sort by name, by identifier or according to the values of the category (then name or code)
  if (sort_by == "item") {
    if (has_item_names(object) && use_names) hierarchy = hierarchy[order(names(items)), ]
    else hierarchy = hierarchy[order(items), ]
  } else {
    hierarchy = hierarchy[order(object@items_categories[as.character(items), category],
                                if (has_item_names(object) && use_names) names(items) else items), ]
  }
  
  # Graph vertices
  vertices = data.frame(name = unique(unlist(hierarchy)), stringsAsFactors = FALSE)
  if (use_names) {
    vertices$label = substr2(names(items[match(vertices$name, items)]), stop = n.cutoff)
  } else {
    vertices$label = items[match(vertices$name, items)]
  }
  vertices$vertex_coord_multiplier = 1 + vertex_margin
  vertices$label_coord_multiplier = 1 + vertex_margin + label_margin
  
  # Processing of the category and its legend
  if (!is.null(category)) {
    vertices$group = object@items_categories[vertices$name, category]
    category_legend = object@categories_colors[[category]][unique(vertices$group)][-1] # 1st is NA
    category_legend = category_legend[order(names(category_legend))]
    
    names(category_legend) = substr2(names(category_legend), stop = c.cutoff)
    vertices$group = substr2(vertices$group, stop = c.cutoff)
  }
  
  # Finding the numbers of vertices to link
  from = match(rules[to_keep, "antecedent"], vertices$name)
  to = match(rules[to_keep, "consequent"], vertices$name)
  
  # Sorting the links so that the darker ones are above the lighter ones
  if (palette_direction == 1) the_order = order( rules[to_keep, col_to_display], from, to)
  else                        the_order = order(-rules[to_keep, col_to_display], from, to)
  from = from[the_order]
  to = to[the_order]
  rules_to_plot = rules[to_keep, ][the_order, ]
  
  # Discretization of the directed indicator to better distinguish possible double coloring
  if (has_direction) {
    rules_to_plot[[col_to_display]] = cut(rules_to_plot[[col_to_display]],
                                          breaks = if (is_divergent) seq(-1, 1, 0.2) else seq(0, 1, 0.1),
                                          include.lowest = TRUE)
  }
  
  
  ## Plotting of the graph
  
  tree = igraph::graph_from_data_frame(hierarchy, vertices = vertices)
  graph = ggraph::ggraph(tree, layout = "dendrogram", circular = TRUE)
  
  if (nrow(rules_to_plot) != 0) {
    graph = graph +
      ggraph::geom_conn_bundle(
        data = ggraph::get_con(from = from, to = to, colors = rules_to_plot[, col_to_display]),
        ggplot2::aes(
          color = colors,
          alpha = if (has_direction && want_direction) ggplot2::after_stat(index) else edge_alpha
        ),
        tension = edge_looseness)
  }
  
  graph = graph +
    ggraph::geom_node_point(ggplot2::aes(x = x * vertex_coord_multiplier,
                                         y = y * vertex_coord_multiplier,
                                         filter = leaf,
                                         color = if (!is.null(category)) group),
                            size = vertex_size, alpha = vertex_alpha) +
    ggraph::geom_node_text(ggplot2::aes(x = x * label_coord_multiplier,
                                        y = y * label_coord_multiplier,
                                        filter = leaf,
                                        label = label,
                                        angle = atan(y / x) * 180 / pi, # Angle in degrees
                                        hjust = ifelse(x < 0, 1, 0),
                                        color = if (!is.null(category)) group),
                           size = label_size, show.legend = FALSE) +
    ggplot2::theme_void() +
    ggplot2::coord_fixed()
  
  if (has_direction) {
    if (want_direction) {
      graph = graph + ggraph::scale_edge_alpha("Rule direction",
                                               guide = ggraph::guide_edge_direction(order = 2))
    }
    
    if (is_divergent) {
      graph = graph + ggraph::scale_edge_color_brewer(
        gsub(".", " ", cap(col_to_display), fixed = TRUE),
        palette = palette,
        direction = palette_direction,
        breaks = sort(unique((rules_to_plot[[col_to_display]]))),
        limits = levels(rules_to_plot[[col_to_display]]),
        guide = ggplot2::guide_legend(order = 1)
      )
    } else {
      # Create a 10 color palette from the specified 9 color palette
      # (by creating a 11 color palette to get a less light first color.
      # Otherwise, the first level of the discrete scale can hardly be seen)
      pal = grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(11)[2:11]
      
      graph = graph + ggraph::scale_edge_color_manual(
        cap(col_to_display),
        values = stats::setNames(
          if (palette_direction == 1) pal else rev(pal),
          levels(rules_to_plot[[col_to_display]])),
        guide = ggplot2::guide_legend(order = 1)
      )
    }
  } else {
    graph = graph + 
      ggraph::scale_edge_color_distiller(cap(col_to_display),
                                         palette = palette, direction = palette_direction,
                                         limits = c(0, max(rules_to_plot[, col_to_display])),
                                         # Parameter necessary if ggraph is not loaded
                                         guide = ggraph::guide_edge_colorbar(order = 1))
  }
  
  if (!is.null(category)) {
    category_name = if (is.numeric(category)) colnames(object@items_categories)[category] else category
    graph = graph + ggplot2::scale_color_manual(cap(category_name),
                                                values = category_legend,
                                                guide = ggplot2::guide_legend(
                                                  order = 3,
                                                  override.aes = list(size = 1.5, alpha = 1)))
  }
  
  
  # Conversion of rules back to factor if they were given as such
  if (a_factor) rules$antecedent = set_notation(rules$antecedent)
  if (c_factor) rules$consequent = set_notation(rules$consequent)
  
  # Since assigning the return prevents plotting the graph, use of a parameter
  if (plot) graphics::plot(graph)
  
  # If debug mode, additional return of the selection of rules actually plotted
  if (DEBUG_MODE_) return(list(graph = graph, rules = rules, plotted_rules = rules_to_plot))
  return(list(graph = graph, rules = rules))
})



#### Methods for search and save ####

#' @rdname export
#' @aliases export,TransactionAnalyzer
#' @export
setMethod(f = "export",
          signature = "TransactionAnalyzer",
          definition =
function(object, nporc, ...) {
  
  # Finding the type of entities provided
  entities = which_entities(object, nporc, NODES_PATTERNS_OR_RULES)
  
  # Names of the columns in which to search for vectors to convert
  if (entities == NODES || entities == PATTERNS) {
    columns = substr(entities, 1, nchar(entities) - 1)
  } else if (entities == RULES) {
    columns = c("antecedent", "consequent")
  }
  
  # Conversion of itemsets to character strings
  itemsets = apply(nporc[columns], 2, turn_list_into_char)
  nporc[, columns] = unlist(itemsets)
  
  # Writing data
  utils::write.csv2(x = nporc, ...)
})


#' Search for transactions by category
#' 
#' Extract the transactions corresponding to a sought category value.
#' 
#' @param object S4 object of class `TransactionAnalyzer`.
#' @param trx S4 object of class `TransactionSet`. Any subset of `object["transactions"]`.
#' 
#' `"transactions"` and `"t"` are special values for `object["transactions"]`.
#' @param category Name or number of the category on which to search (numbering according to the order
#'  of the columns of `object["items_categories"]`.
#' @param value Sought value(s) for the category specified by the argument `category`. If several values
#'  are given, transactions related to any one of them are extracted.
#' @param as_indices `TRUE` or `FALSE` whether to return transactions or only
#'  their indices.
#' @return S4 object of class `TransactionSet` containing the subset of
#'  transactions that match the search criteria, or indices of these
#'  transactions (according to the argument `as_indices`). If the given
#'  transactions are named (and `as_indices` is `TRUE`), the returned indices
#'  are named as well.
#' 
#' @author Gauthier Magnin
#' @seealso
#' About transactions: [`get_trx_from_items`], [`get_trx_from_info`], [`get_complex_trx`],
#'  [`get_simple_trx`].
#' 
#' About nodes and patterns: [`get_nodes`], [`get_patterns`], [`get_complexes`], [`get_isolates`],
#'  [`get_non_isolates`].
#' 
#' @examples
#' get_trx_from_category(TA_instance, TA_instance["transactions"],
#'                       category = "family", value = "Chrome")
#' get_trx_from_category(TA_instance, TA_instance["transactions"],
#'                       category = "family", value = "Chrome", as_indices = TRUE)
#' 
#' @aliases get_trx_from_category
#' @md
#' @export
setMethod(f = "get_trx_from_category",
          signature = "TransactionAnalyzer",
          definition =
function(object, trx, category, value, as_indices = FALSE) {
  # Items corresponding to the sought category value then transactions containing these items
  return(get_trx_from_items(get_tnp(object, trx, TRANSACTIONS),
                            get_items_from_category(object, category, value),
                            presence = "any",
                            as_indices = as_indices))
})


#' Search for nodes by item, characteristic or category
#' 
#' Extract the nodes satisfying search criteria according to items, characteristics or categories.
#' 
#' @details
#' If `element = "items"` one or more items can be sought. The condition for a node to be extracted
#'  is the presence of the sought items (argument `value`). The argument `condition` must be `"all"`,
#'  `"any"`, `"exactly"` or `"only"` (default is `"all"`):
#'  * `"all"`: all the sought items must be part of the node.
#'  * `"any"`: at least one of the sought items must be part of the node.
#'  * `"exactly"`: the item set contained in the node must be exactly the same as the sought item set.
#'  * `"only"`: the node must contain only the sought items (any of them).
#' 
#' If `element` refers to a characteristic (i.e. is `"length"` or `"frequency"`), the condition for a
#'  node to be extracted is a comparison of the `value` according to one of the comparison operators
#'  (default is equality). If the condition refers to equality or non-equality, several values can be
#'  given. If it does not, only one value must be given. The argument `condition` must be one of the
#'  following.
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
#'  the correspondence to the sought category value (argument `value`). If several values are given,
#'  nodes related to any one of them are extracted. The argument `condition` must be one of `"items"`,
#'  `"links"`, `"vertices"`, `"edges"` (no default).
#'  * `"items"`, `"vertices"`: search for nodes containing an item associated with the sought category
#'    value.
#'  * `"links"`, `"edges"`: search for nodes generating links corresponding to the sought category value.
#' 
#' @param object S4 object of class `TransactionAnalyzer`.
#' @param nc Data frame of **n**odes and their **c**haracteristics. Any subset of `object["nodes"]`.
#' 
#'  `"nodes"` and `"n"` are special values for `object["nodes"]`.
#' @param element Type of element on which to search.
#'  One of `"items"`, `"length"`, `"frequency"` or the name or number of a category on which to search
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
#' get_nodes(TA_instance, TA_instance["nodes"], element = "items", value = 3146)
#' get_nodes(TA_instance, TA_instance["nodes"],
#'           element = "items", value = c(3146, 3180), condition = "all")
#' get_nodes(TA_instance, "nodes",
#'           element = "items", value = c(3146, 3180), condition = "any")
#' get_nodes(TA_instance, "nodes",
#'           element = "items", value = c(3146, 3180), condition = "exactly")
#' get_nodes(TA_instance, "nodes",
#'           element = "items", value = c(3146, 3180), condition = "only")
#' 
#' ## Search on characteristics
#' get_nodes(TA_instance, TA_instance["nodes"], element = "frequency", value = 2)
#' get_nodes(TA_instance, "nodes",
#'           element = "frequency", value = 2, condition = ">=")
#' get_nodes(TA_instance, "nodes",
#'           element = "length", value = 5, condition = "LT")
#' 
#' ## Search on categories
#' get_nodes(TA_instance, TA_instance["nodes"],
#'           element = "family", value = "Chrome", condition = "items")
#' get_nodes(TA_instance, "nodes",
#'           element = 1, value = "Chrome", condition = "links")
#' 
#' @aliases get_nodes
#' @md
#' @export
setMethod(f = "get_nodes",
          signature = "TransactionAnalyzer",
          definition =
function(object, nc, element, value, condition = "default") {
  
  # Verification of the choice of the element for which to perform the search
  if (!(element %in% c("items", "length", "frequency"))
      && !check_access_for_category(object, element, NA, stop = FALSE)) {
    
    if (ncol(object@items_categories) == 0)
      stop("There is no category associated with the items. element must be one of \"items\", \"length\", \"frequency\".")
    stop("element must be one of \"items\", \"length\", \"frequency\", or a category name or number.")
  }
  
  # Call to the specific function
  if (element == "items") {
    if (condition == "default")
      return(get_nodes_from_items(object, nc, value))
    else
      return(get_nodes_from_items(object, nc, value, condition))
  }
  if (element %in% c("length", "frequency")) {
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
#' @template method_not_exported
#' 
#' @param object S4 object of class `TransactionAnalyzer`.
#' @param nc Data frame of **n**odes and their **c**haracteristics. Any subset of `object["nodes"]`.
#' 
#'  `"nodes"` and `"n"` are special values for `object["nodes"]`.
#' @param items Sought items (one or more).
#' @param condition Item presence condition for a node to be extracted.
#'  One of `"all"`, `"any"`, `"exactly"`, `"only"`.
#'  \describe{
#'    \item{`"all"`}{All the sought items must be part of a node for this node to be extracted.}
#'    \item{`"any"`}{At least one of the sought items must be part of a node for this node to be
#'                  extracted.}
#'    \item{`"exactly"`}{The item set contained in a node must be exactly the same as the
#'                       sought item set for this node to be extracted.}
#'    \item{`"only"`}{A node must contain only the sought items (any of them) for this node
#'                    to be extracted.}
#'  }
#' @return Subset of the data frame of nodes that match the search criteria.
#' 
#' @author Gauthier Magnin
#' @seealso [`get_nodes`], [`get_nodes_from_characteristic`], [`get_nodes_from_category`].
#' 
#' @examples
#' get_nodes_from_items(TA_instance, TA_instance["nodes"], items = 3146)
#' get_nodes_from_items(TA_instance, TA_instance["nodes"],
#'                      items = c(3146, 3180), condition = "all")
#' get_nodes_from_items(TA_instance, TA_instance["nodes"],
#'                      items = c(3146, 3180), condition = "any")
#' get_nodes_from_items(TA_instance, TA_instance["nodes"],
#'                      items = c(3146, 3180), condition = "exactly")
#' get_nodes_from_items(TA_instance, TA_instance["nodes"],
#'                      items = c(3146, 3180), condition = "only")
#' 
#' @aliases get_nodes_from_items
#' @md
#' @keywords internal
setMethod(f = "get_nodes_from_items",
          signature = "TransactionAnalyzer",
          definition =
function(object, nc, items, condition = "all") {
  
  # Getting nodes
  check_init(object, NODES)
  nc = get_tnp(object, nc, NODES)
  
  check_param(condition, values = c("all", "any", "exactly", "only"))
  
  func = switch(condition,
                all     = { function(x) all(items %in% x) },
                any     = { function(x) any(items %in% x) },
                exactly = { function(x) setequal(items, x) },
                only    = { function(x) all(x %in% items) })
  
  return(subset(nc, sapply(nc$node, func)))
})


#' Search for nodes by characteristic
#' 
#' Extract the nodes satisfying a search criterion according to one characteristic.
#' 
#' @template method_not_exported
#' 
#' @param object S4 object of class \code{TransactionAnalyzer}.
#' @param nc Data frame of \strong{n}odes and their \strong{c}haracteristics. Any subset of
#'  \code{object["nodes"]}.
#'  
#'  \code{"nodes"} and \code{"n"} are special values for \code{object["nodes"]}.
#' @param characteristic Name of the characteristic on which to do the search.
#'  One of \code{"length"}, \code{"frequency"}.
#' @param value Sought value for the characteristic specified by the parameter \code{characteristic}.
#'  Several values can be given if \code{condition} refers to equality or non-equality.
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
#' get_nodes_from_characteristic(TA_instance, TA_instance["nodes"],
#'                               characteristic = "frequency",
#'                               value = 2)
#' get_nodes_from_characteristic(TA_instance, TA_instance["nodes"],
#'                               characteristic = "length",
#'                               value = 2, condition = ">=")
#' get_nodes_from_characteristic(TA_instance, TA_instance["nodes"],
#'                               characteristic = "length",
#'                               value = 5, condition = "LT")
#' 
#' @aliases get_nodes_from_characteristic
#' @keywords internal
setMethod(f = "get_nodes_from_characteristic",
          signature = "TransactionAnalyzer",
          definition =
function(object, nc, characteristic, value, condition = "EQ") {
  
  # Getting nodes
  check_init(object, NODES)
  nc = get_tnp(object, nc, NODES)
  
  if (!(characteristic %in% c("length", "frequency")))
    stop("characteristic must be one of \"length\", \"frequency\".")
  
  operators = c("EQ" = "==", "==" = "==",    "NE" = "!=", "!=" = "!=",
                "LT" = "<", "<" = "<",       "GT" = ">", ">" = ">",
                "LE" = "<=", "<=" = "<=",    "GE" = ">=", ">=" = ">=")
  
  if (!(condition %in% names(operators))) {
    stop(paste("condition must be one of",
               "\"EQ\", \"NE\", \"LT\", \"GT\", \"LE\", \"GE\",",
               "\"==\", \"!=\", \"<\", \">\", \"<=\", \">=\"."))
  }
  
  # Case of exact search (equal or different): possibility to search for several values
  if (operators[condition] %in% c("==", "!=")){
    return(nc[eval(parse(text = paste0(ifelse(operators[condition] == "!=", "!", ""),
                                       "is.element(nc[[characteristic]], value)"))), ])
  }
  
  # Rows in "nc" where "characteristic" is "condition" (greater, etc.) than "value"
  return(nc[eval(parse(text = paste("nc[[characteristic]]", operators[condition], "value"))), ])
})


#' Search for nodes by category
#' 
#' Extract the nodes corresponding to a sought category value.
#' 
#' @template method_not_exported
#' 
#' @param object S4 object of class \code{TransactionAnalyzer}.
#' @param nc Data frame of \strong{n}odes and their \strong{c}haracteristics. Any subset of
#'  \code{object["nodes"]}.
#'  
#'  \code{"nodes"} and \code{"n"} are special values for \code{object["nodes"]}.
#' @param category Name or number of the category on which to search (numbering according to the order
#'  of the columns of \code{object["items_categories"]}).
#' @param value Sought value(s) for the category specified by the argument \code{category}.
#'  If several values are given, nodes related to any one of them are extracted.
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
#' get_nodes_from_category(TA_instance, TA_instance["nodes"],
#'                         category = "family", value = "Chrome",
#'                         condition = "items")
#' 
#' get_nodes_from_category(TA_instance, TA_instance["nodes"],
#'                         category = 1, value = "Chrome",
#'                         condition = "links")
#' 
#' @aliases get_nodes_from_category
#' @keywords internal
setMethod(f = "get_nodes_from_category",
          signature = "TransactionAnalyzer",
          definition =
function(object, nc, category, value, condition) {
  
  # Getting nodes
  nc = get_tnp(object, nc, NODES)
  
  # Validation of parameters related to a category value
  check_access_for_category(object, category, value)
  
  if (condition == "items" || condition == "vertices") {
    check_init(object, NODES)
    
    # Items corresponding to the sought category value then nodes containing these items
    return(get_nodes_from_items(object, nc, get_items_from_category(object, category, value, TRUE),
                                condition = "any"))
    
  } else if (condition == "links" || condition == "edges") {
    check_init(object, c(NODES, NODE_LINKS))
    
    # Finding the set of links corresponding to the nodes
    links = get_links(object, nc)
    # Values associated with each link for the type of category sought
    categories_links = lapply(strsplit(links$items, "/"),
                              function(x) sort(unique(as.character(object@items_categories[x, category]))))
    # Extraction of links that correspond to the searched category value
    links = links[sapply(categories_links, function(x) value %in% x), ]
    # Getting associated nodes
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
#'  is the presence of the sought items (argument `value`). The argument `condition` must be `"all"`,
#'  `"any"`, `"exactly"` or `"only"` (default is `"all"`):
#'  * `"all"`: all the sought items must be part of the pattern.
#'  * `"any"`: at least one of the sought items must be part of the pattern.
#'  * `"exactly"`: the item set contained in the pattern must be exactly the same as the sought item set.
#'  * `"only"`: the pattern must contain only the sought items (any of them).
#' 
#' If `element` refers to a characteristic other than status (i.e. is one of `"year"`, `"length"`,
#'  `"support"`, `"frequency"`, `"weight"`, `"specificity"`), the condition for a pattern to be extracted
#'  is a comparison of the `value` according to one of the comparison operators (default is equality).
#'  If the condition refers to equality or non-equality, several values can be given. If it does not,
#'  only one value must be given. The argument `condition` must be one of the following.
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
#' If `element` refers to the characteristic `"status"`, one or more status can be sought.
#'  the condition for a pattern to be extracted is a comparison of the sought status (argument `value`)
#'  according to one of the basic comparison operators (default is equality):
#'  * `"EQ"`, `"=="`: **EQ**ual. The status of the pattern must be one of the sought values.
#'  * `"NE"`, `"!="`: **N**ot **E**qual. The status of the pattern must be different from the sought
#'    values.
#' 
#' If `element` is the name or the number of a category, the condition for a pattern to be extracted is
#'  the correspondence to the sought category value (argument `value`). If several values are given,
#'  patterns related to any one of them are extracted. The argument `condition` must be one of `"items"`,
#'  `"links"`, `"vertices"`, `"edges"` (no default).
#'  * `"items"`, `"vertices"`: search for patterns containing an item associated with the sought
#'    category value.
#'  * `"links"`, `"edges"`: search for patterns generating links corresponding to the sought category
#'    value.
#' 
#' @param object S4 object of class `TransactionAnalyzer`.
#' @param pc Data frame of **p**atterns and their **c**haracteristics. Any subset of
#'  `object["patterns"]`.
#'  
#'  `"patterns"` and `"p"` are special values for `object["patterns"]`.
#' @param element Type of element on which to search.
#'  One of `"items"`, `"year"`, `"length"`, `"support"`, `"frequency"`, `"weight"`, `"specificity"`,
#'  `"status"` or the name or number of a category on which to search (numbering according to the order
#'  of the columns of `object["items_categories"]`).
#' @param value Sought value(s) for the element specified by the argument `element`.
#' @param condition Search condition, depending on `element`. See 'Details' section.
#' @return Subset of the data frame of patterns that match the search criteria.
#' 
#' @author Gauthier Magnin
#' @seealso [`get_nodes`], [`get_complexes`], [`get_isolates`], [`get_non_isolates`].
#' 
#' @examples
#' ## Search on items
#' get_patterns(TA_instance, TA_instance["patterns"],
#'              element = "items", value = 3146)
#' get_patterns(TA_instance, TA_instance["patterns"],
#'              element = "items", value = c(3146, 3180), condition = "all")
#' get_patterns(TA_instance, "patterns",
#'              element = "items", value = c(3146, 3180), condition = "any")
#' get_patterns(TA_instance, "patterns",
#'              element = "items", value = c(3146, 3180), condition = "exactly")
#' get_patterns(TA_instance, "patterns",
#'              element = "items", value = c(3146, 3180), condition = "only")
#' 
#' ## Search on characteristics
#' get_patterns(TA_instance, TA_instance["patterns"],
#'              element = "frequency", value = 3)
#' get_patterns(TA_instance, "patterns",
#'              element = "frequency", value = 3, condition = ">=")
#' get_patterns(TA_instance, "patterns",
#'              element = "length", value = 3, condition = "LT")
#' 
#' get_patterns(TA_instance, TA_instance["patterns"], element = "status",
#'              value = "Persistent")
#' get_patterns(TA_instance, "patterns", element = "status",
#'              value = c("Persistent", "Declining"), condition = "!=")
#' 
#' ## Search on categories
#' get_patterns(TA_instance, TA_instance["patterns"],
#'              element = "family", value = "Chrome", condition = "items")
#' get_patterns(TA_instance, "patterns",
#'              element = 1, value = "Chrome", condition = "links")
#' 
#' @aliases get_patterns
#' @md
#' @export
setMethod(f = "get_patterns",
          signature = "TransactionAnalyzer",
          definition =
function(object, pc, element, value, condition = "default") {
  
  # Verification of the choice of the element on which to perform the search
  if (!(element %in% c("items", "year", "length", "support",
                       "frequency", "weight", "specificity", "status"))
      && !check_access_for_category(object, element, NA, stop = FALSE)) {
    
    if (ncol(object@items_categories) == 0)
      stop(paste("There is no category associated with the items. element must be one of \"items\",",
                 "\"year\", \"length\", \"support\", \"frequency\", \"weight\", \"specificity\",",
                 "\"status\"."))
    stop(paste("element must be one of \"items\", \"year\", \"length\", \"support\", \"frequency\",",
               "\"weight\", \"specificity\", \"status\" or a category name or number."))
  }
  
  # Call to the specific function
  if (element == "items") {
    if (condition == "default")
      return(get_patterns_from_items(object, pc, value))
    else
      return(get_patterns_from_items(object, pc, value, condition))
  }
  if (element %in% c("year", "length", "support", "frequency", "weight", "specificity")) {
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
#' @template method_not_exported
#' 
#' @param object S4 object of class \code{TransactionAnalyzer}.
#' @param pc Data frame of \strong{p}atterns and their \strong{c}haracteristics. Any subset of
#'  \code{object["patterns"]}.
#'  
#'  \code{"patterns"} and \code{"p"} are special values for \code{object["patterns"]}.
#' @param items Sought items (one or more).
#' @param condition Item presence condition for a pattern to be extracted.
#'  One of \code{"all"}, \code{"any"}, \code{"exactly"}, \code{"only"}.
#'  \describe{
#'    \item{\code{"all"}}{All the sought items must be part of a pattern for this pattern to be
#'                        extracted.}
#'    \item{\code{"any"}}{At least one of the sought items must be part of a pattern for this pattern
#'                        to be extracted.}
#'    \item{\code{"exactly"}}{The item set contained in a pattern must be exactly the same as the
#'                            sought item set for this pattern to be extracted.}
#'    \item{\code{"only"}}{A pattern must contain only the sought items (any of them) for this pattern
#'                         to be extracted.}
#'  }
#' @return Subset of the data frame of patterns that match the search criteria.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{get_patterns}}, \code{\link{get_patterns_from_characteristic}},
#'          \code{\link{get_patterns_from_status}}, \code{\link{get_patterns_from_category}}.
#' 
#' @examples
#' get_patterns_from_items(TA_instance, TA_instance["patterns"], items = 3146)
#' get_patterns_from_items(TA_instance, TA_instance["patterns"],
#'                         items = c(3146, 3180), condition = "all")
#' get_patterns_from_items(TA_instance, TA_instance["patterns"],
#'                         items = c(3146, 3180), condition = "any")
#' get_patterns_from_items(TA_instance, TA_instance["patterns"],
#'                         items = c(3146, 3180), condition = "exactly")
#' get_patterns_from_items(TA_instance, TA_instance["patterns"],
#'                         items = c(3146, 3180), condition = "only")
#' 
#' @aliases get_patterns_from_items
#' @keywords internal
setMethod(f = "get_patterns_from_items",
          signature = "TransactionAnalyzer",
          definition =
function(object, pc, items, condition = "all") {
  
  # Getting patterns
  check_init(object, PATTERNS)
  pc = get_tnp(object, pc, PATTERNS)
  
  check_param(condition, values = c("all", "any", "exactly", "only"))
  
  func = switch(condition,
                all     = { function(x) all(items %in% x) },
                any     = { function(x) any(items %in% x) },
                exactly = { function(x) setequal(items, x) },
                only    = { function(x) all(x %in% items) })
  
  return(subset(pc, sapply(pc$pattern, func)))
})


#' Search for patterns by characteristic
#' 
#' Extract the patterns satisfying a search criterion according to one characteristic.
#' 
#' @template method_not_exported
#' 
#' @param object S4 object of class \code{TransactionAnalyzer}.
#' @param pc Data frame of \strong{p}atterns and their \strong{c}haracteristics. Any subset of
#'  \code{object["patterns"]}.
#'  
#'  \code{"patterns"} and \code{"p"} are special values for \code{object["patterns"]}.
#' @param characteristic Name of the characteristic on which to do the search.
#'  One of \code{"year"}, \code{"length"}, \code{"support"}, \code{"frequency"}, \code{"weight"} or
#'  \code{"specificity"}. See \code{\link{get_patterns_from_status}} to search by \code{"status"}.
#' @param value Sought value for the characteristic specified by the parameter \code{characteristic}.
#'  Several values can be given if \code{condition} refers to equality or non-equality.
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
#' get_patterns_from_characteristic(TA_instance, TA_instance["patterns"],
#'                                  characteristic = "frequency",
#'                                  value = 3)
#' get_patterns_from_characteristic(TA_instance, TA_instance["patterns"],
#'                                  characteristic = "frequency",
#'                                  value = 3, condition = ">=")
#' get_patterns_from_characteristic(TA_instance, TA_instance["patterns"],
#'                                  characteristic = "length",
#'                                  value = 3, condition = "LT")
#' 
#' @aliases get_patterns_from_characteristic
#' @keywords internal
setMethod(f = "get_patterns_from_characteristic",
          signature = "TransactionAnalyzer",
          definition =
function(object, pc, characteristic, value, condition = "EQ") {
  
  # Getting patterns
  check_init(object, PATTERNS)
  pc = get_tnp(object, pc, PATTERNS)
  
  if (!(characteristic %in% c("year", "length", "support", "frequency", "weight", "specificity")))
    stop("characteristic must be one of \"year\", \"length\", \"support\", \"frequency\", ",
         "\"weight\", \"specificity\".")
  
  operators = c("EQ" = "==", "==" = "==",    "NE" = "!=", "!=" = "!=",
                "LT" = "<", "<" = "<",       "GT" = ">", ">" = ">",
                "LE" = "<=", "<=" = "<=",    "GE" = ">=", ">=" = ">=")
  
  if (!(condition %in% names(operators))) {
    stop(paste("condition must be one of",
               "\"EQ\", \"NE\", \"LT\", \"GT\", \"LE\", \"GE\",",
               "\"==\", \"!=\", \"<\", \">\", \"<=\", \">=\"."))
  }
  
  # Case of exact search (equal or different): possibility to search for several values
  if (operators[condition] %in% c("==", "!=")){
    return(pc[eval(parse(text = paste0(ifelse(operators[condition] == "!=", "!", ""),
                                       "is.element(pc[[characteristic]], value)"))), ])
  }
  
  # Rows in "pc" where "characteristic" is "condition" (greater, etc.) than "value"
  return(pc[eval(parse(text = paste("pc[characteristic]", operators[condition], "value"))), ])
})


#' Search for patterns by status
#' 
#' Extract the patterns whose status match one or more sought values.
#' 
#' @template method_not_exported
#' 
#' @param object S4 object of class \code{TransactionAnalyzer}.
#' @param pc Data frame of \strong{p}atterns and their \strong{c}haracteristics. Any subset of
#'  \code{object["patterns"]}.
#'  
#'  \code{"patterns"} and \code{"p"} are special values for \code{object["patterns"]}.
#' @param value Status value sought (one or more).
#' @param condition Search condition. One of \code{"EQ"}, \code{"NE"}, \code{"=="}, \code{"!="}.
#'  \describe{
#'    \item{\code{"EQ"}, \code{"=="}}{\strong{EQ}ual: the status of a pattern must be one of the
#'                                    sought values.}
#'    \item{\code{"NE"}, \code{"!="}}{\strong{N}ot \strong{E}qual: the status of a pattern must be
#'                                    different from the sought values.}
#'  }
#' @return Subset of the data frame of patterns that match the search criteria.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{get_patterns}}, \code{\link{get_patterns_from_items}},
#'          \code{\link{get_patterns_from_characteristic}}, \code{\link{get_patterns_from_category}}.
#' 
#' @examples
#' get_patterns_from_status(TA_instance, TA_instance["patterns"],
#'                          value = "Persistent",
#'                          condition = "EQ")
#' 
#' get_patterns_from_status(TA_instance, TA_instance["patterns"],
#'                          value = c("Persistent", "Declining"),
#'                          condition = "!=")
#' 
#' @aliases get_patterns_from_status
#' @keywords internal
setMethod(f = "get_patterns_from_status",
          signature = "TransactionAnalyzer",
          definition =
function(object, pc, value, condition = "EQ") {
  
  # Getting patterns
  check_init(object, PATTERNS)
  pc = get_tnp(object, pc, PATTERNS)
  
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
#' @template method_not_exported
#' 
#' @param object S4 object of class \code{TransactionAnalyzer}.
#' @param pc Data frame of \strong{p}atterns and their \strong{c}haracteristics. Any subset of
#'  \code{object["patterns"]}.
#'  
#'  \code{"patterns"} and \code{"p"} are special values for \code{object["patterns"]}.
#' @param category Name or number of the category on which to search (numbering according to the order
#'  of the columns of \code{object["items_categories"]}).
#' @param value Sought value(s) for the category specified by the argument \code{category}.
#'  If several values are given, patterns related to any one of them are extracted.
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
#' get_patterns_from_category(TA_instance, TA_instance["patterns"],
#'                            category = "family", value = "Chrome",
#'                            condition = "items")
#' 
#' get_patterns_from_category(TA_instance, TA_instance["patterns"],
#'                            category = 1, value = "Chrome",
#'                            condition = "links")
#' 
#' @aliases get_patterns_from_category
#' @keywords internal
setMethod(f = "get_patterns_from_category",
          signature = "TransactionAnalyzer",
          definition =
function(object, pc, category, value, condition) {
  
  # Getting patterns
  pc = get_tnp(object, pc, PATTERNS)
  
  # Validation of the parameters related to a category value
  check_access_for_category(object, category, value)
  
  if (condition == "items" || condition == "vertices") {
    check_init(object, PATTERNS)
    
    # Items corresponding to the category value sought then patterns containing these items
    return(get_patterns_from_items(object, pc, get_items_from_category(object, category, value, TRUE),
                                   condition = "any"))
    
  } else if (condition == "links" || condition == "edges") {
    check_init(object, c(PATTERNS, PATTERN_LINKS))
    
    # Finding the set of links corresponding to the patterns
    links = get_links(object, pc)
    # Values associated with each link for the type of category sought
    categories_links = lapply(strsplit(links$items, "/"),
                              function(x) sort(unique(as.character(object@items_categories[x, category]))))
    # Extraction of the links that correspond to the searched category value
    links = links[sapply(categories_links, function(x) value %in% x), ]
    # Getting associated patterns
    return(pc[unique(unlist(links[, 1:2])), ])
  }
  stop("condition must be one of \"items\", \"links\", \"vertices\", \"edges\".")
})


#' Get links between nodes or patterns
#' 
#' Extract from the data frame of links between nodes or patterns those corresponding to the given
#'  nodes or patterns.
#' 
#' @details
#' If among the nodes or patterns for which the links are sought, some become isolated because the other
#'  entities to which they are normally linked are not part of the subset \code{nopc}, these nodes or
#'  patterns are placed at the end of the return data frame.
#' These possible \code{n} additional rows are numbered \code{"A1"..."An"}.
#' 
#' @param object S4 object of class \code{TransactionAnalyzer}.
#' @param nopc Data frame of \strong{n}odes \strong{o}r \strong{p}atterns and their
#'  \strong{c}haracteristics. Nodes or patterns whose links are be to sought. Any subset of
#'  \code{object["nodes"]} or \code{object["patterns"]}.
#'  
#'  \code{"nodes"}, \code{"n"}, \code{"patterns"} and \code{"p"} are special values for
#'  \code{object["nodes"]} and \code{object["patterns"]}.
#' @return Data frame associating the linked nodes or linked patterns.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{get_isolates}}, \code{\link{get_non_isolates}}, \code{\link{get_complexes}}.
#' 
#' @examples
#' get_links(TA_instance, "patterns")
#' get_links(TA_instance, TA_instance["patterns"][1:10, ])
#' 
#' @aliases get_links
#' @export
setMethod(f = "get_links",
          signature = "TransactionAnalyzer",
          definition =
function(object, nopc) {
  
  # Getting nodes/patterns and finding the type of entities provided
  entities = which_entities(object, nopc)
  nopc = get_tnp(object, nopc)
  check_init(object, c(entities, which_associated_links(object, entities)))
  
  # If the links searched correspond to all the links
  if (entities == NODES && identical(object@nodes, nopc)) {
    return(object@node_links)
  }
  if (entities == PATTERNS && identical(object@patterns, nopc)) {
    return(object@pattern_links)
  }
  
  # If not...
  search_nodes = (entities == NODES)
  all_links = if(search_nodes) object@node_links else object@pattern_links
  
  # Subset of links for which both vertices are to be displayed
  # (nop_links = node or pattern links)
  nop_links = all_links[all_links$endpoint.1 %in% rownames(nopc)
                        & all_links$endpoint.2 %in% rownames(nopc), ]
  
  # Identification of new isolated vertices
  isolated = lapply(rownames(nopc),
                    function(x) {
                      if (!(x %in% unlist(nop_links[, 1:2]))) {
                        if (search_nodes) return(c(x, x, "", 0))
                        return(c(x, x, "", 0, object@patterns[x, "year"]))
                      }
                      return(NULL)
                    })
  
  # If there are new isolated vertices
  if (any(sapply(isolated, function(x) !is.null(x)))) {
    
    # Addition to the set of links/vertices
    no_links = do.call(rbind, isolated)
    colnames(no_links) = colnames(nop_links)
    nop_links = rbind(nop_links, no_links, stringsAsFactors = FALSE)
    class(nop_links$endpoint.1) = class(nop_links$endpoint.2) = class(nop_links$weight) = "integer"
    if(!search_nodes) class(nop_links$year) = "integer"
    
    # Assigning names to the new rows, different from those of the general data frame (the attribute)
    if (nrow(no_links) == nrow(nop_links)) {
      # If all rows are just new isolated vertices
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
#' Extract from the given nodes or patterns those which are isolated (i.e. those which are not linked
#'  to any other entity).
#' 
#' @param object S4 object of class \code{TransactionAnalyzer}.
#' @param nopc Data frame of \strong{n}odes \strong{o}r \strong{p}atterns and their
#'  \strong{c}haracteristics. Nodes or patterns whose isolated are to be sought. Any subset of
#'  \code{object["nodes"]} or \code{object["patterns"]}.
#'  
#'  \code{"nodes"}, \code{"n"}, \code{"patterns"} and \code{"p"} are special values for
#'  \code{object["nodes"]} and \code{object["patterns"]}.
#' @return Subset of the data frame that corresponds to isolated entities.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{get_non_isolates}}, \code{\link{get_complexes}}, \code{\link{get_links}}.
#' 
#' @examples
#' get_isolates(TA_instance, "patterns")
#' get_isolates(TA_instance, TA_instance["patterns"][1:10, ])
#' 
#' @aliases get_isolates
#' @export
setMethod(f = "get_isolates",
          signature = "TransactionAnalyzer",
          definition =
function(object, nopc) {
  
  links = get_links(object, nopc)
  row_id = as.character(links$endpoint.1[links$weight == 0])
  return(get_tnp(object, nopc)[row_id, ])
})


#' Search for non-isolated nodes or patterns
#' 
#' Extract from the given nodes or patterns those which are not isolated (i.e. those which are linked
#'  to any other entity).
#' 
#' @param object S4 object of class \code{TransactionAnalyzer}.
#' @param nopc Data frame of \strong{n}odes \strong{o}r \strong{p}atterns and their
#'  \strong{c}haracteristics. Nodes or patterns whose non-isolated are to be sought. Any subset of
#'  \code{object["nodes"]} or \code{object["patterns"]}.
#'  
#'  \code{"nodes"}, \code{"n"}, \code{"patterns"} and \code{"p"} are special values for
#'  \code{object["nodes"]} and \code{object["patterns"]}.
#' @return Subset of the data frame that corresponds to non-isolated entities.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{get_isolates}}, \code{\link{get_complexes}}, \code{\link{get_links}}.
#' 
#' @examples
#' get_non_isolates(TA_instance, "patterns")
#' get_non_isolates(TA_instance, TA_instance["patterns"][1:10, ])
#' 
#' @aliases get_non_isolates
#' @export
setMethod(f = "get_non_isolates",
          signature = "TransactionAnalyzer",
          definition =
function(object, nopc) {
  
  links = get_links(object, nopc)
  row_id = as.character(sort(unique(unlist(links[links$weight != 0, c("endpoint.1", "endpoint.2")]))))
  return(get_tnp(object, nopc)[row_id, ])
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
#' @param object S4 object of class \code{TransactionAnalyzer}.
#' @param nopc Data frame of \strong{n}odes \strong{o}r \strong{p}atterns and their
#'  \strong{c}haracteristics. Nodes or patterns whose complexes are to be sought. Any subset of
#'  \code{object["nodes"]} or \code{object["patterns"]}.
#'  
#'  \code{"nodes"}, \code{"n"}, \code{"patterns"} and \code{"p"} are special values for
#'  \code{object["nodes"]} and \code{object["patterns"]}.
#' @param category Name or number of the category on which to search (numbering according to the order
#'  of the columns of \code{object["items_categories"]}).
#' @param condition Condition for a node or a pattern to be extracted.
#'  One of \code{"items"}, \code{"links"}, \code{"vertices"}, \code{"edges"}.
#'  \describe{
#'    \item{\code{"items"}, \code{"vertices"}}{Search for nodes or patterns associated (via their items)
#'          with several values of the category \code{category}.}
#'    \item{\code{"links"}, \code{"edges"}}{Search for nodes or patterns generating links corresponding
#'          to several values of the category \code{category}.}
#'  }
#' @param min_nb_values Minimum number of different values of the category \code{category} a node, a
#'  pattern or a link must have to extract the related entity.
#' @return Subset of the data frame that corresponds to the complex entities sought.
#' 
#' @author Gauthier Magnin
#' @seealso 
#' About nodes and patterns: \code{\link{get_isolates}}, \code{\link{get_non_isolates}},
#'  \code{\link{get_links}}, \code{\link{frequency_by_complexity}}.
#' 
#' About transactions: \code{\link{get_complex_trx}}, \code{\link{get_simple_trx}}.
#' 
#' @examples
#' get_complexes(TA_instance, "patterns")
#' get_complexes(TA_instance, TA_instance["patterns"][1:15, ],
#'               category = "family", condition = "items")
#' get_complexes(TA_instance, TA_instance["patterns"][1:15, ],
#'               category = 1, condition = "links")
#' 
#' @aliases get_complexes
#' @export
setMethod(f = "get_complexes",
          signature = "TransactionAnalyzer",
          definition =
function(object, nopc, category = NULL, condition = NULL, min_nb_values = 2) {
  
  # Getting nodes/patterns and finding the type of entities provided
  entities = which_entities(object, nopc)
  nopc = get_tnp(object, nopc)
  
  if (is.null(category)) {
    check_init(object, entities)
    
    # Entities having at least min_nb_values items
    return(nopc[nopc[, "length"] >= min_nb_values, ])
    
  } else {
    # Validation of the category access parameter
    check_access_for_category(object, category, NA)
    
    if (condition == "items" || condition == "vertices") {
      check_init(object, entities)
      
      # Categories associated with each node or pattern
      nop_category = lapply(nopc[[substr(entities, 1, nchar(entities) - 1)]],
                            function(x) unique(as.character(object@items_categories[x, category])))
      
      # Entities associated with at least min_nb_values different values for the category
      return(nopc[lapply(nop_category, length) >= min_nb_values, ])
      
    } else if (condition == "links" || condition == "edges") {
      check_init(object, c(entities, which_associated_links(object, entities)))
      
      # Links associated with the nodes or patterns
      nop_links = get_links(object, nopc)
      
      # Categories associated with each link
      links_category = lapply(strsplit(nop_links$items, "/"),
                              function(x) sort(unique(as.character(object@items_categories[x, category]))))
      
      # Identifiers of the entities having at least one link associated with at least
      # min_nb_values different values for the category
      id = unique(unlist(nop_links[which(lapply(links_category, length) >= min_nb_values),
                                   c("endpoint.1", "endpoint.2")]))
      
      # Corresponding entities
      return(nopc[as.character(id), ])
    }
    stop("condition must be one of \"items\", \"links\", \"vertices\", \"edges\".") 
  }
})


#' Get item names
#' 
#' Find the names associated with items given their identification codes.
#' 
#' @param object S4 object of class `TransactionAnalyzer`.
#' @param items Vector of items: unnamed subset of `object["items"]`.
#' @return Names of the `items` in `object["items"]`.
#' 
#' @author Gauthier Magnin
#' @examples
#' get_item_names(TA_instance, items = c(19, 163, 1603))
#' 
#' @aliases get_item_names
#' @md
#' @export
setMethod(f = "get_item_names",
          signature = "TransactionAnalyzer",
          definition =
function(object, items) {
  return(names(object@items)[match(items, object@items)])
})


#' Get item colors
#' 
#' Find the colors associated with the values of a category that are associated with items.
#' 
#' @param object S4 object of class `TransactionAnalyzer`.
#' @param category Name or number of the category whose associated colors are to be found
#'  (numbering according to the order of the columns of `object["items_categories"]`).
#' @param items Items for which to find the colors associated with their values in the given category.
#'  Any subset of `object["items"]`.
#'  
#'  `"items"` and `"i"` are special values for `object["items"]`.
#' @return Colors associated with the values of the category `category` that are
#'  associated with the `items`.
#' 
#' @author Gauthier Magnin
#' @examples
#' get_item_colors(TA_instance, category = "family", items = c(19, 25, 27))
#' 
#' @aliases get_item_colors
#' @md
#' @export
setMethod(f = "get_item_colors",
          signature = "TransactionAnalyzer",
          definition =
function(object, category, items = object["items"]) {
  
  # Validation of the parameter
  check_access_for_category(object, category, NA)
  
  return(stats::setNames(
    object["categories_colors"][[category]][object["items_categories"][as.character(items), 1]],
    get_item_names(object, items)
  ))
})


#' Itemset category values
#' 
#' Give the category values associated with specific itemsets.
#' 
#' @param object S4 object of class `TransactionAnalyzer`.
#' @param itemsets List of itemsets whose category values are to be found. Can be any itemsets.
#'  
#'  `"transactions"`, `"t"`, `"nodes"`, `"n"`, `"patterns"` and `"p"` are special values for the itemsets
#'  corresponding to the transactions, the nodes or the patterns contained in `object`.
#' @param as_character If `FALSE`, category values are returned as factor, in a list.
#'  If `TRUE`, they are returned as character, in a data frame.
#' @param unique If `TRUE`, sorted unique values are returned for each itemset. If `FALSE`,
#'  duplicated values are not removed and there is correspondence between the return values and the
#'  items forming the given itemsets.
#' @return List or data frame (depending on the value of `as_character`) contaning for each category
#'  (associated with the items in `object["items_categories"]`) the values regarding each itemset.
#' 
#' @author Gauthier Magnin
#' @seealso [`get_nodes`], [`get_patterns`], [`get_complexes`], [`get_trx_from_category`]
#' 
#' @examples
#' ## Values associated with specific itemsets
#' itemset_list <- list(c("19", "25"),
#'                      c("3156", "3157", "3345"),
#'                      c("19", "163", "929"))
#' 
#' category_values(TA_instance, itemset_list, unique = TRUE)
#' category_values(TA_instance, itemset_list, unique = FALSE)
#' 
#' category_values(TA_instance, itemset_list, as_character = FALSE)
#' category_values(TA_instance, itemset_list, as_character = TRUE)
#' 
#' ## Values associated with itemsets from transactions or patterns
#' category_values(TA_instance, "transactions")
#' category_values(TA_instance, TA_instance["patterns"]$pattern[17:18])
#' 
#' ## Values associated with itemsets from association rules
#' rules <- extract_rules(TA_instance)
#' category_values(TA_instance, rules$antecedent)
#' category_values(TA_instance, rules$consequent)
#' 
#' @aliases category_values
#' @md
#' @export
setMethod(f = "category_values",
          signature = "TransactionAnalyzer",
          definition =
function(object, itemsets, as_character = FALSE, unique = TRUE) {
  
  # Verification of the existence of at least one category
  check_access_for_category(object, NA, NA)
  
  # Optionally get specific itemsets, and veritification of the type of itemsets
  itemsets = get_tnp_itemsets(object, itemsets, entities = ANY_ITEMSETS)
  if (is.numeric(itemsets[[1]])) itemsets = lapply(itemsets, as.character)
  
  # List of factors containing the correspondences of each itemset for each category
  the_list = stats::setNames(
    lapply(names(object@items_categories), function(category)
      lapply(itemsets, function(itemset) object@items_categories[itemset, category])),
    names(object@items_categories))
  if (unique) the_list = lapply(the_list, function(l) lapply(l, function(values) sort(unique(values))))
  
  # Return the list of factors or a data frame of character
  if (!as_character) return(the_list)
  if (length(the_list[[1]]) != 1) {
    return(as.data.frame(sapply(the_list, function(l) lapply(l, as.character)), stringsAsFactors = FALSE))
  }
  
  # Spacial case of creation of a data frame if there is only one itemset
  # Use of a temporary column to be able to place the vector(s) in a single row
  df = data.frame(tmp_col = character(1), stringsAsFactors = FALSE)
  char_list = lapply(the_list, function(l) lapply(l, as.character))
  for (name in names(char_list)) df[[name]] = char_list[[name]]
  df$tmp_col = NULL
  return(df)
})



#### Other specific methods ####

#' Validation of parameters for search by category
#' 
#' Check that the parameters provided match an existing category and an existing value within this
#'  category. Stop the execution and print an error message if not.
#' 
#' @details
#' If \code{value} is \code{NA}, it is not checked; only the parameter \code{category} is.
#' 
#' If \code{category} is \code{NA}, only existence of any category is checked.
#' 
#' @template method_not_exported
#' 
#' @param object S4 object of class \code{TransactionAnalyzer}.
#' @param category Name or number of the category to access (numbering according to the order of the
#'  columns of \code{object["items_categories"]}).
#' @param value Sought value(s) for the category specified by the argument \code{category}, or \code{NA}.
#' @param stop If \code{TRUE}, stop the execution and print an error message if the parameters do not
#'  allow access to a category and a category value. If \code{FALSE}, see 'Value' section.
#' @return \code{TRUE} or \code{FALSE} whether the parameters allow access to a category and a category
#'  value.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{get_patterns}}, \code{\link{get_nodes}}.
#'          \code{\link{get_patterns_from_category}}, \code{\link{get_nodes_from_category}}.
#' 
#' @aliases check_access_for_category
#' @keywords internal
setMethod(f = "check_access_for_category",
          signature = "TransactionAnalyzer",
          definition =
function(object, category, value, stop = TRUE) {
  
  # If there is no category, the value must be NULL
  if (is.null(category)) return(TRUE)
  if (ncol(object@items_categories) == 0) {
    if (!stop) return(FALSE)
    stop("There is no category associated with the items.")
  }
  
  # Checking that the type of category sought exists
  if (is.character(category) && !(category %in% colnames(object@items_categories))
      || is.numeric(category) && (category < 1 || category > ncol(object@items_categories))) {
    if (!stop) return(FALSE)
    stop("category must be in range [1,", ncol(object@items_categories), "] or one of the following: ",
         paste0("\"", colnames(object@items_categories), "\"", collapse = ", "), ".")
  }
  
  # Checking that the sought values of the category exist
  if (length(value) == 1 && is.na(value)) return(TRUE)
  
  if (any(!(value %in% levels(object@items_categories[, category])))) {
    if (!stop) return(FALSE)
    stop("value must be one or more of the levels of the given category (",
         if (is.numeric(category)) category else paste0("\"", category,  "\""), ").")
  }
  return(TRUE)
})


#' Check if the items have names
#' 
#' Check if the `TransactionAnalyzer` has item names, i.e. if the current names associated with the items
#'  are different from their identification codes.
#' 
#' @template method_not_exported
#' 
#' @param object S4 object of class `TransactionAnalyzer`.
#' @return `TRUE` if at least one of the names of `object["items"]` is different from the related item.
#'  `FALSE` if all names of `object["items"]` are equal to the values of `object["items"]`.
#' 
#' @author Gauthier Magnin
#' @seealso [`get_item_names`], [`get_items`][get_items,TransactionAnalyzer-method].
#' 
#' @aliases has_item_names
#' @md
#' @keywords internal
setMethod(f = "has_item_names",
          signature = "TransactionAnalyzer",
          definition =
function(object) {
  return(!all(names(object@items) == object@items))
})


#' Get items
#' 
#' Find and return the vector or subset of the vector corresponding to the items of the object of class
#'  `TransactionAnalyzer`, or return the given vector.
#' 
#' @details
#' If `items` is a named vector corresponding to a subset of `object["items"]`, it is returned.
#' 
#' If `items` is a vector corresponding to an unnamed subset of `object["items"]`, the corresponding
#'  named subset of `object["items"]` is returned.
#' 
#' If `items` is a character value equal to `"items"` or `"i"`, `object["items"]` is returned.
#' 
#' @template method_not_exported
#' 
#' @param object S4 object of class `TransactionAnalyzer`.
#' @param items Vector of items or one of the following character value: `"items"`, `"i"`.
#' @return Named vector of items corresponding to the arguments.
#' 
#' @author Gauthier Magnin
#' @seealso [`get_item_names`], [`has_item_names`], [`get_tnp`], [`which_entities`].
#' 
#' Method for signature `TransactionSet`: [`get_items,TransactionSet`][get_items,TransactionSet-method].
#' 
#' @aliases get_items get_items,TransactionAnalyzer
#' @md
#' @keywords internal
setMethod(f = "get_items",
          signature = "TransactionAnalyzer",
          definition =
function(object, items) {
  
  # Specific value referring to all items
  if (length(items) == 1 && is.character(items) && (items == "items" || items == "i"))
    return(object@items)
  
  # Vector of items (subset of object@items)
  if (all(items %in% object@items)) {
    # With or without the associated names
    if (is_named(items)) return(items)
    return(object@items[match(items, object@items)])
  }
  
  stop("items must be \"items\" or a subset of object[\"items\"].")
})


#' Search for items by category
#' 
#' Extract the items corresponding to a sought category value.
#' 
#' @template method_not_exported
#' 
#' @param object S4 object of class `TransactionAnalyzer`.
#' @param category Name or number of the category on which to search (numbering according to the order
#'  of the columns of `object["items_categories"]`).
#' @param value Sought value(s) for the category specified by the argument `category`.
#' @param force_character If `TRUE`, items are returned as character values.
#'  If `FALSE`, they are the same type as in `object["items"]` (numeric or character).
#' @return Items that match the search criteria.
#' 
#' @author Gauthier Magnin
#' @aliases get_items_from_category
#' @md
#' @keywords internal
setMethod(f = "get_items_from_category",
          signature = "TransactionAnalyzer",
          definition =
function(object, category, value, force_character = FALSE) {
  
  # Checking parameters of access to a category and search for corresponding items
  check_access_for_category(object, category, value)
  items = rownames(object@items_categories)[object@items_categories[[category]] %in% value]
  
  # Type corresponding to the attribute items or character
  if (is.numeric(object@items) && !force_character) return(as.numeric(items))
  else return(items)
})


#' Get transactions or nodes or patterns and their characteristics
#' 
#' Find and return the `TransactionSet` corresponding to the transactions or the data frame corresponding
#'  to the nodes or the patterns of the object of class `TransactionAnalyzer`, or return the given value.
#' 
#' @details
#' If `tnp` is an object of class `TransactionSet` or a data frame, it is returned.
#' 
#' If `tnp` is a character value equal to:
#'  * `"transactions"` or `"t"`: `object["transactions"]` is returned.
#'  * `"nodes"` or `"n"`: `object["nodes"]` is returned.
#'  * `"patterns"` or `"p"`: `object["patterns"]` is returned.
#' 
#' The argument `entities` is only used to adapt a possible error message.
#' 
#' @template method_not_exported
#' 
#' @param object S4 object of class `TransactionAnalyzer`.
#' @param tnp Object of class `TransactionSet` (**t**) or data frame of **n**odes or **p**atterns and
#'  their characteristics or one of the following character values: `"transactions"`, `"t"`, `"nodes"`,
#'  `"n"`, `"patterns"`, `"p"`.
#' @param entities Type of the entities that `tnp` may refer to (`TRANSACTIONS`, `NODES`, `PATTERNS`,
#'  `NODES_OR_PATTERNS` or `NODES_PATTERNS_OR_TRANSACTIONS`).
#' @return Object of class `TransactionSet` or data frame of nodes or patterns and their characteristics,
#'  corresponding to the arguments.
#' 
#' @author Gauthier Magnin
#' @seealso [`get_tnp_itemsets`], [`which_entities`], [`get_items`][get_items,TransactionAnalyzer-method].
#' 
#' @aliases get_tnp
#' @md
#' @keywords internal
setMethod(f = "get_tnp",
          signature = "TransactionAnalyzer",
          definition =
function(object, tnp, entities = NODES_OR_PATTERNS) {
  
  if (is.character(tnp)) {
    if (tnp == TRANSACTIONS || tnp == first_characters(TRANSACTIONS)) return(object@transactions)
    if (tnp == NODES        || tnp == first_characters(NODES))        return(object@nodes)
    if (tnp == PATTERNS     || tnp == first_characters(PATTERNS))     return(object@patterns)
    
    var_name = deparse(substitute(tnp))
    
    if (entities == TRANSACTIONS)
      msg = paste(var_name, "must be \"transactions\" or an object of class TransactionSet.")
    else if (entities == NODES)
      msg = paste(var_name, "must be \"nodes\" or a data frame of nodes and their characteristics.")
    else if (entities == PATTERNS)
      msg = paste(var_name, "must be \"patterns\" or a data frame of patterns and their characteristics.")
    else if (entities == NODES_OR_PATTERNS)
      msg = paste(var_name, "must be \"nodes\", \"patterns\" or a data frame of nodes or patterns and their characteristics.")
    else # NODES_PATTERNS_OR_TRANSACTIONS
      msg = paste(var_name, "must be \"transactions\", \"nodes\", \"patterns\", an object of class TransactionSet or a data frame of nodes or patterns and their characteristics.")
    
    stop(msg)
  }
  return(tnp)
})


#' Get transaction, node or pattern itemsets
#' 
#' Find and return the list of itemsets corresponding to the transactions, the nodes or the patterns of
#'  the object of class `TransactionAnalyzer`, or return the given list.
#' 
#' @details
#' If `tnp` is a list, it is returned.
#' 
#' If `tnp` is a character value equal to:
#'  * `"transactions"` or `"t"`: `object["transactions"][object["transactions"]["item_key"]]` is returned.
#'  * `"nodes"` or `"n"`: `object["nodes"]$node` is returned.
#'  * `"patterns"` or `"p"`: `object["patterns"]$pattern` is returned.
#' 
#' The argument `entities` is only used to adapt a possible error message.
#' 
#' @template method_not_exported
#' 
#' @param object S4 object of class `TransactionAnalyzer`.
#' @param tnp List of **t**ransaction, **n**ode or **p**attern itemsets (or of any itemsets) or one of
#'  the following character values: `"transactions"`, `"t"`, `"nodes"`, `"n"`, `"patterns"`, `"p"`.
#' @param entities Type of the entities that the list may refer to (`TRANSACTIONS`, `NODES`, `PATTERNS`,
#'  `NODES_OR_PATTERNS`, `NODES_PATTERNS_OR_TRANSACTIONS`, `ANY_ITEMSETS`).
#' @return List of transaction, node or pattern itemsets corresponding to the arguments.
#' 
#' @author Gauthier Magnin
#' @seealso [`get_tnp`], [`which_entities`], [`get_items`][get_items,TransactionAnalyzer-method].
#' 
#' @aliases get_tnp_itemsets
#' @md
#' @keywords internal
setMethod(f = "get_tnp_itemsets",
          signature = "TransactionAnalyzer",
          definition =
function(object, tnp, entities = NODES_OR_PATTERNS) {
  
  if (is.character(tnp)) {
    if (tnp == TRANSACTIONS || tnp == first_characters(TRANSACTIONS)) {
      return(object@transactions[object@transactions@item_key])
    }
    if (tnp == NODES    || tnp == first_characters(NODES))    return(object@nodes$node)
    if (tnp == PATTERNS || tnp == first_characters(PATTERNS)) return(object@patterns$pattern)
    
    var_name = deparse(substitute(tnp))
    
    if (entities == TRANSACTIONS)
      msg = paste(var_name, "must be \"transactions\" or a list of transactions.")
    else if (entities == NODES)
      msg = paste(var_name, "must be \"nodes\" or a list of nodes.")
    else if (entities == PATTERNS)
      msg = paste(var_name, "must be \"patterns\" or a list of patterns.")
    else if (entities == NODES_OR_PATTERNS)
      msg = paste(var_name, "must be \"nodes\", \"patterns\" or a list of nodes or patterns.")
    else if (entities == NODES_PATTERNS_OR_TRANSACTIONS)
      msg = paste(var_name, "must be \"transactions\", \"nodes\", \"patterns\" or a list of transactions, nodes or patterns.")
    else # entities == ANY_ITEMSETS
      msg = paste(var_name, "must be \"transactions\", \"nodes\", \"patterns\" or a list of any itemsets.")
    
    stop(msg)
  }
  return(tnp)
})


#' Detect the type of entities
#' 
#' Detect the type of entities contained in a data frame or an object among transactions, nodes,
#'  patterns and association rules.
#' 
#' @details
#' The detection uses the class of the argument `tnpr` or the column names of the given data frame.
#' If the class is `TransactionSet`, entities are transactions. If it is a data frame, it searches for
#'  column names `"node"`, `"pattern"` or `"antecedent"` for nodes, patterns or rules, respectively.
#' 
#' The argument `entities` is only used to adapt a possible error message.
#' 
#' @template method_not_exported
#' 
#' @param object S4 object of class `TransactionAnalyzer`.
#' @param tnpr Object of class `TransactionSet` (**t**) or data frame of **n**odes, **p**atterns or
#'  association **r**ules and their characteristics.
#'  
#'  `"transactions"`, `"t"`, `"nodes"`, `"n"`, `"patterns"`, `"p"`, `"rules"` and `"r"` are special
#'  values.
#' @param entities Define if `tnpr` is either a data frame of nodes or a data frame of patterns
#'  (`NODES_OR_PATTERNS`), or if it can also be a data frame of rules (`NODES_PATTERNS_OR_RULES`) or
#'  a set of transactions (`NODES_PATTERNS_OR_TRANSACTIONS`).
#' @return Character corresponding to `TRANSACTIONS`, `NODES`, `PATTERNS` or `RULES`.
#' 
#' @author Gauthier Magnin
#' @seealso [`get_tnp`], [`which_associated_links`], [`which_name`].
#' 
#' @aliases which_entities
#' @md
#' @keywords internal
setMethod(f = "which_entities",
          signature = "TransactionAnalyzer",
          definition =
function(object, tnpr, entities = NODES_OR_PATTERNS) {
  
  if (is.character(tnpr)) {
    if (tnpr == TRANSACTIONS || tnpr == first_characters(TRANSACTIONS)) return(TRANSACTIONS)
    if (tnpr == NODES        || tnpr == first_characters(NODES))        return(NODES)
    if (tnpr == PATTERNS     || tnpr == first_characters(PATTERNS))     return(PATTERNS)
    if (tnpr == RULES        || tnpr == first_characters(RULES))        return(RULES)
    
    var_name = deparse(substitute(tnpr))
    
    if (entities == NODES_OR_PATTERNS)
      msg = paste("If", var_name, "is character, it must be \"nodes\" or \"patterns\".")
    else if (entities == NODES_PATTERNS_OR_RULES)
      msg = paste("If", var_name, "is character, it must be \"nodes\", \"patterns\" or \"rules\".")
    else # NODES_PATTERNS_OR_TRANSACTIONS
      msg = paste("If", var_name, "is character, it must be \"transactions\", \"nodes\" or \"patterns\".")
    
    stop(msg)
  }
  
  if (class(tnpr) == "TransactionSet") return(TRANSACTIONS)
  if ("node" %in% colnames(tnpr)) return(NODES)
  if ("pattern" %in% colnames(tnpr)) return(PATTERNS)
  if ("antecedent" %in% colnames(tnpr)) return(RULES)
  
  var_name = deparse(substitute(tnpr))
  
  if (entities == NODES_OR_PATTERNS) {
    stop(paste(var_name, "must be \"nodes\", \"patterns\" or a data frame of nodes or patterns and their characteristics."))
  }
  else if (entities == NODES_PATTERNS_OR_RULES) {
    stop(paste(var_name, "must be a data frame of nodes or patterns and their",
               "characteristics, or a data frame of association rules."))
  }
  # entities = NODES_PATTERNS_OR_TRANSACTIONS
  stop(paste(var_name, "must be \"transactions\", \"nodes\", \"patterns\", an",
             "object of class TransactionSet or a data frame of nodes or patterns and",
             "their characteristics."))
})


#' Type of links relating to a type of entities
#' 
#' Give a type of links given a type of entities (nodes or patterns).
#' 
#' @template method_not_exported
#' 
#' @param object S4 object of class `TransactionAnalyzer`.
#' @param entities Type of entities for which to give the type of links.
#'  Character corresponding to `NODES`, `PATTERNS` or their simplifications (see [`first_characters`]).
#' @return Character corresponding to `NODE_LINKS` or `PATTERN_LINKS`.
#' 
#' @author Gauthier Magnin
#' @seealso [`which_entities`], [`which_name`].
#' 
#' @aliases which_associated_links
#' @md
#' @keywords internal
setMethod(f = "which_associated_links",
          signature = "TransactionAnalyzer",
          definition =
function(object, entities) {
  
  if (entities == NODES    || entities == first_characters(NODES))    return(NODE_LINKS)
  if (entities == PATTERNS || entities == first_characters(PATTERNS)) return(PATTERN_LINKS)
  
  stop("entities must refer to nodes or patterns.")
})


#' Type of entities or links corresponding to a name
#' 
#' Give the type of entities or links given a name referring to it.
#' 
#' @template method_not_exported
#' 
#' @param object S4 object of class `TransactionAnalyzer`.
#' @param name Type of entities or links.
#'  Character corresponding to `TRANSACTIONS`, `NODES`, `PATTERNS`, `RULES`, `NODE_LINKS`, `PATTERN_LINKS`
#'   or their simplifications (see [`first_characters`]). One or more.
#' @return Vector of characters corresponding to `TRANSACTIONS`, `NODES`, `PATTERNS`, `RULES`,
#'  `NODE_LINKS` or `PATTERN_LINKS`. Same size as the argument `name`.
#' 
#' @author Gauthier Magnin
#' @seealso [`which_entities`], [`which_associated_links`].
#' 
#' @aliases which_name
#' @md
#' @keywords internal
setMethod(f = "which_name",
          signature = "TransactionAnalyzer",
          definition =
function(object, name) {
  
  if (length(name) > 1) return(sapply(name, which_name, object = object))
  
  if (name == TRANSACTIONS  || name == first_characters(TRANSACTIONS))  return(TRANSACTIONS)
  if (name == NODES         || name == first_characters(NODES))         return(NODES)
  if (name == PATTERNS      || name == first_characters(PATTERNS))      return(PATTERNS)
  if (name == RULES         || name == first_characters(RULES))         return(RULES)
  if (name == NODE_LINKS    || name == first_characters(NODE_LINKS))    return(NODE_LINKS)
  if (name == PATTERN_LINKS || name == first_characters(PATTERN_LINKS)) return(PATTERN_LINKS)
})


