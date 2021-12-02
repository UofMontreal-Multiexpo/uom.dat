## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(fig.align = "center",
                      results = "hold")

## ----message=FALSE------------------------------------------------------------
library(uom.dat)

## ----eval=FALSE---------------------------------------------------------------
#  View(TA_instance)

## -----------------------------------------------------------------------------
print(TA_instance)

## -----------------------------------------------------------------------------
# Creation of a TransactionSet object by grouping rows of 'oedb_sample' having
# the same 'ID'.
trx_object <- make_transactions(oedb_sample,
                                by = "ID",
                                additional = c("CODE", "NAME", "YEAR"),
                                item_key = "CODE",
                                year_key = "YEAR")

# Creation of a data.frame associating item identifiers with item names
# and with one category
items_ids <- get_all_items(trx_object)
names <- substances_information[match(items_ids,
                                      substances_information$CODE),
                                "NAME"]
category_1 <- substances_information[match(items_ids,
                                           substances_information$CODE),
                                     "SUBFAMILY"]
items <- data.frame(item = items_ids,
                    name = names,
                    family = as.factor(category_1),
                    stringsAsFactors = FALSE)

# Creation of a TransactionAnalyzer object
ta_object_1 <- transaction.analyzer(trx_object,
                                    items,
                                    target = "closed frequent itemsets",
                                    count = 1,
                                    min_length = 1,
                                    max_length = Inf,
                                    status_limit = 2)

## -----------------------------------------------------------------------------
# Print the new object
print(ta_object_1)

## -----------------------------------------------------------------------------
# Creation of a non-initialized TransactionAnalyzer
ta_object_2 <- transaction.analyzer(trx_object,
                                    init = FALSE)
print(ta_object_2)

## ----warning=FALSE------------------------------------------------------------
# Initalize nodes then patterns
init(ta_object_2, "nodes")
init(ta_object_2, "patterns")

## -----------------------------------------------------------------------------
# Initialization tests
is_init(ta_object_2, "nodes")
is_init(ta_object_2, "node_links")
is_init(ta_object_2)

## -----------------------------------------------------------------------------
# Access the transactions
ta_object_1["transactions"]

## -----------------------------------------------------------------------------
# Access the patterns
ta_object_1["patterns"]

## -----------------------------------------------------------------------------
# Access a mining parameter
ta_object_1["target"]

## ----warning=FALSE------------------------------------------------------------
# Change some parameters
ta_object_1["target"] <- "maximally frequent itemsets"
ta_object_1["min_length"] <- 2

# Mine again patterns, compute their characteristics and search their links
reset(ta_object_1, 5)

# Take a look at the new patterns
ta_object_1["patterns"]

## -----------------------------------------------------------------------------
# Existing categories
cat("Categories associated with the items:\n")
print(colnames(TA_instance["items_categories"]))

# Existing levels of one category
cat("\nLevels of the category named 'family' of the object 'TA_instance':\n")
print(levels(TA_instance["items_categories"]$family))

## -----------------------------------------------------------------------------
# Logical vector indicating which items have the value 'Esters'
# in the category 'family'
value_is_esters <- TA_instance["items_categories"]$family == "Esters"

# Use of the rownames to get the item identifiers
items_1 <- rownames(TA_instance["items_categories"])[value_is_esters]

cat("Items relating to the family 'Esters':\n")
items_1

## -----------------------------------------------------------------------------
cat("Items relating to the family 'Esters':\n")
get_item_names(TA_instance, items_1)

## -----------------------------------------------------------------------------
# Category values associated with some transaction itemsets of 'TA_instance'
category_values(TA_instance,
                get_itemsets(TA_instance["transactions"])[1:3])

## -----------------------------------------------------------------------------
# Category values associated with some pattern itemsets of 'TA_instance'
category_values(TA_instance,
                TA_instance["patterns"]$pattern[17:18])

## -----------------------------------------------------------------------------
# Category values associated with 3 specific itemsets
itemset_list <- list(c("19", "25"),
                     c("3156", "3157", "3345"),
                     c("19", "163", "929"))

category_values(TA_instance,
                itemset_list)

## -----------------------------------------------------------------------------
# Category values associated with the previously defined list of itemsets
itemset_category_values <- category_values(TA_instance,
                                           itemset_list,
                                           as_character = TRUE,
                                           unique = FALSE)

# Add one column for the itemsets in the returned data frame of category values
itemset_category_values$itemset <- itemset_list
itemset_category_values[, c("itemset", "family")]

## -----------------------------------------------------------------------------
# Search for the names associated to the items of each itemset
itemset_category_values$names <- lapply(itemset_list,
                                        get_item_names, object = TA_instance)
# Show the result for two of them
itemset_category_values[c(1,3),
                        c("itemset", "names", "family")]

## -----------------------------------------------------------------------------
# Transactions corresponding to a specific category value
cat("Transactions with items related to the value 'Chrome' from the category 'family':\n")
trx_subset <- get_trx_from_category(TA_instance,
                                    trx = TA_instance["transactions"],
                                    category = "family",
                                    value = "Chrome")
print(trx_subset)

## -----------------------------------------------------------------------------
TA_instance["nodes"]

## -----------------------------------------------------------------------------
TA_instance["patterns"]

## -----------------------------------------------------------------------------
# Subset of nodes having the items '3146' and '3180'
get_nodes(TA_instance,
          TA_instance["nodes"],
          element = "items",
          value = c("3146", "3180"),
          condition = "all")

## -----------------------------------------------------------------------------
# Subset of nodes having the item '3146' or the item '3180'
get_nodes(TA_instance,
          TA_instance["nodes"],
          element = "items",
          value = c("3146", "3180"),
          condition = "any")

## ----warning=FALSE------------------------------------------------------------
# Subset of nodes having exactly the itemset composed of '3146' and '3180'
get_nodes(TA_instance,
          TA_instance["nodes"],
          element = "items",
          value = c("3146", "3180"),
          condition = "exactly")

## -----------------------------------------------------------------------------
# Subset of nodes having only the items '3146' and '3180'
get_nodes(TA_instance,
          TA_instance["nodes"],
          element = "items",
          value = c("3146", "3180"),
          condition = "only")

## -----------------------------------------------------------------------------
cat("Possibles values for the argument `element` if referring to a node characteristic:\n")
print(colnames(TA_instance["nodes"])[-1])

cat("\nPossibles values for the argument `element` if referring to a pattern characteristic:\n")
print(colnames(TA_instance["patterns"])[-1])

## -----------------------------------------------------------------------------
# Subset of patterns having a length greater than 3
get_patterns(TA_instance,
             TA_instance["patterns"],
             element = "length",
             value = 3,
             condition = ">")

## -----------------------------------------------------------------------------
# Subset of patterns having a frequency equals to 3
p_subset_1 <- get_patterns(TA_instance,
                           TA_instance["patterns"],
                           element = "frequency",
                           value = 3,
                           condition = "==")
p_subset_1

## -----------------------------------------------------------------------------
# Subset of patterns having a status different from "Declining"
# among those having a frequency equals to 3
get_patterns(TA_instance,
             p_subset_1,
             element = "status",
             value = "Declining",
             condition = "!=")

## -----------------------------------------------------------------------------
cat("Items relating to the family 'Chrome':\n")
value_is_chrome <- TA_instance["items_categories"][["family"]] == "Chrome"
rownames(TA_instance["items_categories"])[value_is_chrome]

## -----------------------------------------------------------------------------
# Subset of patterns containing any item relating to the value "Chrome"
# of the category "family"
get_patterns(TA_instance,
             TA_instance["patterns"],
             element = "family",
             value = "Chrome",
             condition = "items")

## -----------------------------------------------------------------------------
# Subset of patterns which generate links in which any item is related to the value "Chrome"
# of the category "family"
get_patterns(TA_instance,
             TA_instance["patterns"],
             element = 1,
             value = "Chrome",
             condition = "links")

## -----------------------------------------------------------------------------
# Nodes having at least 2 items
get_complexes(TA_instance,
              TA_instance["nodes"],
              min_nb_values = 2)

## -----------------------------------------------------------------------------
# Nodes whose itemsets are associated with at least 2 different values
# of the category 'family'
get_complexes(TA_instance,
              TA_instance["nodes"],
              category = "family",
              condition = "items",
              min_nb_values = 2)

## -----------------------------------------------------------------------------
# Nodes having links associated with at least 2 different values
# of the category 'family'
get_complexes(TA_instance,
              TA_instance["nodes"],
              category = 1,
              condition = "links",
              min_nb_values = 2)

## -----------------------------------------------------------------------------
# Isolated nodes
get_isolates(TA_instance,
             TA_instance["nodes"])

## -----------------------------------------------------------------------------
# Non-isolated nodes
get_non_isolates(TA_instance,
                 TA_instance["nodes"])

## -----------------------------------------------------------------------------
# Attribute 'node_links' of the object 'TA_instance'
TA_instance["node_links"]

## -----------------------------------------------------------------------------
# Links relating to the first 5 nodes
get_links(TA_instance,
          TA_instance["nodes"][1:5, ])

## -----------------------------------------------------------------------------
# Checking isolated nodes if only conderting the first 5 nodes
get_isolates(TA_instance,
             TA_instance["nodes"][1:5, ])

## -----------------------------------------------------------------------------
# All rules from the transactions, with default parameters
extract_rules(TA_instance,
              itemsets = NULL)

## -----------------------------------------------------------------------------
# Rules having the item '328' as consequent and only one item as antecedent
rules_1 <- extract_rules(TA_instance,
                         itemsets = NULL,
                         parameter = list(support = 0.001,
                                          confidence = 0.5,
                                          maxlen = 2),
                         appearance = list(rhs = "328"))
rules_1

## -----------------------------------------------------------------------------
# Rules in which the items '328' and '3180' can be each one either in the
# antecedent or consequent (without thresholds of support and confidence)
extract_rules(TA_instance,
              itemsets = NULL,
              parameter = list(support = 0,
                               confidence = 0,
                               minlen = 2,
                               maxlen = 2),
              appearance = list(both = c("328", "3180")))

## -----------------------------------------------------------------------------
# Rules forming the patterns
extract_rules(TA_instance,
              itemsets = "patterns")

## -----------------------------------------------------------------------------
# Rules forming 2 specific itemsets
rules_2a <- extract_rules(TA_instance,
                          itemsets = list(c("931", "3180"),
                                          c("25", "192", "328")))
rules_2a

## -----------------------------------------------------------------------------
# Rules forming one specific itemset containing 2 items
extract_rules(TA_instance,
              itemsets = list(c("328", "3180")))

## -----------------------------------------------------------------------------
cat("Confidence of the rule '192 => 328':", rules_1[1, "confidence"])

## -----------------------------------------------------------------------------
cat("Confidence of the rule '25 => 328':    ",   rules_1[2, "confidence"])
cat("\nConfidence of the rule '25 => not 328':", 1 - rules_1[2, "confidence"])

## ----eval=FALSE---------------------------------------------------------------
#  # Identifiers, names and values of the category 'family' associated with the
#  # antecedents of previous association rules
#  cbind(identifiers = rules_2a$antecedent,
#        names = lapply(rules_2a$antecedent,
#                       get_item_names, object = TA_instance),
#        family = category_values(TA_instance,
#                                 rules_2a$antecedent,
#                                 as_character = TRUE)$family)

## ----echo=FALSE---------------------------------------------------------------
# Beautify the display by using a data.frame instead of a matrix
as.data.frame(
  cbind(identifiers = rules_2a$antecedent,
        names = lapply(rules_2a$antecedent,
                       get_item_names, object = TA_instance),
        family = category_values(TA_instance,
                                 rules_2a$antecedent,
                                 as_character = TRUE)$family
        )
  )

## -----------------------------------------------------------------------------
# Get association rules in mathematical notation
rules_2b <- extract_rules(TA_instance,
                          itemsets = list(c("931", "3180"),
                                          c("25", "192", "328")),
                          as_sets = TRUE)
rules_2b

## -----------------------------------------------------------------------------
# Switching between mathematical notation and vector notation
cat("Vector notation:\n")
c(vector_notation(rules_2b$antecedent),
  vector_notation(rules_2b$consequent))

# Switching between vector notation and mathematical notation
cat("\nMathematical notation as character:\n")
set_notation(c(rules_2a$antecedent, rules_2a$consequent),
             type = "character")

cat("\nMathematical notation as factor:\n")
set_notation(c(rules_2a$antecedent, rules_2a$consequent),
             type = "factor")

## -----------------------------------------------------------------------------
# Tree chart of the category 'family' from the TransactionAnalyzer 'TA_instance'
category_tree_chart(TA_instance,
                    category = "family")

## -----------------------------------------------------------------------------
category_tree_chart(TA_instance,
                    category = "family",
                    use_names = FALSE)

## -----------------------------------------------------------------------------
category_tree_chart(TA_instance,
                    category = "family",
                    n.cutoff = 10,
                    c.cutoff = 20)

## ----warning=FALSE, message=FALSE---------------------------------------------
# Loading the package
library(ggplot2)

# Make changes to the last plot
last_plot() + expand_limits(x = c(-1.3, 1.3),
                            y = c(-1.3, 1.3))

## -----------------------------------------------------------------------------
category_tree_chart(TA_instance,
                    category = "family",
                    use_names = FALSE,
                    items = c(19, 25, 27, 77, 87, 163, 192, 1603, 3146, 3350))

## -----------------------------------------------------------------------------
category_tree_chart(TA_instance,
                    category = "family",
                    vertex_size = 4,
                    vertex_alpha = 0.5,
                    leaf_size = 5,
                    leaf_alpha = 0.8,
                    leaf_margin = 0.08)

## -----------------------------------------------------------------------------
# Use of the complexity ratio as leaf size
ratio <- complexity_ratio(TA_instance["transactions"])
print(ratio)

category_tree_chart(TA_instance,
                    category = "family",
                    use_names = FALSE,
                    leaf_size = ratio * 4)

## -----------------------------------------------------------------------------
category_tree_chart(TA_instance,
                    category = "family",
                    use_names = FALSE,
                    label_size = 5,
                    label_margin = 0.1) + 
  expand_limits(x = c(-1.3, 1.3),
                y = c(-1.3, 1.3))

## ----fig.height=6-------------------------------------------------------------
# Co-occurrence chart of 'TA_instance' using the category 'family'
co_occurrence_chart(TA_instance,
                    category = "family")

## ----fig.height=6-------------------------------------------------------------
# Chart of the co-occurrence proportions of 'TA_instance' using the category 'family'
co_occurrence_chart(TA_instance,
                    category = "family",
                    proportions = TRUE)

## ----fig.height=6-------------------------------------------------------------
co_occurrence_chart(TA_instance,
                    category = "family",
                    use_names = FALSE)

## ----fig.height=6-------------------------------------------------------------
co_occurrence_chart(TA_instance,
                    category = "family",
                    n.cutoff = 10,
                    c.cutoff = 17)

## ----fig.height=6, warning=FALSE, message=FALSE-------------------------------
# Loading the package
library(ggplot2)

# Make changes to the last plot
last_plot() + expand_limits(x = c(-1.3, 1.3),
                            y = c(-1.3, 1.3))

## ----fig.height=6-------------------------------------------------------------
co_occurrence_chart(TA_instance,
                    category = "family",
                    c.cutoff = 17,
                    min_occ = 2,
                    max_occ = 2)

## -----------------------------------------------------------------------------
co_occurrence_chart(TA_instance,
                    category = "family",
                    items = c(25, 27, 49, 87, 148, 192, 252, 328))

## ----fig.height=6-------------------------------------------------------------
co_occurrence_chart(TA_instance,
                    category = "family",
                    c.cutoff = 17,
                    use_names = FALSE,
                    sort_by = "item")

## ----fig.height=6-------------------------------------------------------------
co_occurrence_chart(TA_instance,
                    category = "family",
                    c.cutoff = 17,
                    vertex_size = 5,
                    vertex_alpha = 0.4,
                    vertex_margin = 0.15)

## ----fig.height=6-------------------------------------------------------------
co_occurrence_chart(TA_instance,
                    category = "family",
                    c.cutoff = 17,
                    label_size = 5,
                    label_margin = 0.15)

## ----fig.height=6-------------------------------------------------------------
co_occurrence_chart(TA_instance,
                    category = "family",
                    c.cutoff = 17,
                    edge_looseness = 0,
                    edge_alpha = 0.5)

## ----echo=FALSE, out.extra="class='border_none'"------------------------------
old_par <- graphics::par(mar = c(0, 4.1, 0, 2.1))
RColorBrewer::display.brewer.all(type = "seq")
graphics::par(old_par)

## ----fig.height=6-------------------------------------------------------------
co_occurrence_chart(TA_instance,
                    category = "family",
                    c.cutoff = 17,
                    palette = "OrRd",
                    palette_direction = -1)

## -----------------------------------------------------------------------------
# Itemset chart of the nodes of 'TA_instance' using the category 'family'
itemset_chart(TA_instance,
              TA_instance["nodes"],
              category = "family",
              n.cutoff = 50,
              title = "Node itemsets from TA_instance")

## -----------------------------------------------------------------------------
itemset_chart(TA_instance,
              TA_instance["nodes"],
              category = "family",
              use_names = FALSE)

## -----------------------------------------------------------------------------
itemsets_1 <- itemset_chart(TA_instance,
                            TA_instance["nodes"],
                            category = "family",
                            n.cutoff = 20,
                            c.cutoff = 8)

## -----------------------------------------------------------------------------
itemset_chart(TA_instance,
              TA_instance["nodes"],
              category = "family",
              n.cutoff = 20,
              c.cutoff = 7,
              length_one = TRUE,
              jitter = FALSE)

## -----------------------------------------------------------------------------
itemsets_2 <- itemset_chart(TA_instance,
                            TA_instance["nodes"],
                            category = "family",
                            n.cutoff = 20,
                            c.cutoff = 7,
                            length_one = TRUE,
                            jitter = NA)

## -----------------------------------------------------------------------------
cat("Possible data to display on itemset charts using transactions:\n")
TA_instance["transactions"]["names"]

cat("\nPossible data to display on itemset charts using nodes:\n")
colnames(TA_instance["nodes"][-1])

cat("\nPossible data to display on itemset charts using patterns:\n")
colnames(TA_instance["patterns"][-1])

## -----------------------------------------------------------------------------
# Plot the itemset chart of the patterns of 'TA_instance' and display the years
# and status associated to the patterns
itemset_chart(TA_instance,
              TA_instance["patterns"],
              category = "family",
              n.cutoff = 20,
              c.cutoff = 8,
              under = "year",
              over = "status")

## -----------------------------------------------------------------------------
itemset_chart(TA_instance,
              TA_instance["nodes"],
              category = "family",
              n.cutoff = 20,
              c.cutoff = 7,
              length_one = TRUE,
              jitter = NA,
              identifiers = "new")

## -----------------------------------------------------------------------------
itemsets_3 <- itemset_chart(TA_instance,
                            TA_instance["nodes"],
                            category = "family",
                            c.cutoff = 8,
                            use_names = FALSE,
                            sort_by = "item")

## -----------------------------------------------------------------------------
# Spectrum chart of the patterns of the TransactionAnalyzer 'TA_instance'
spectrum_chart(TA_instance,
               TA_instance["patterns"],
               title = "Spectrum of all patterns")

## -----------------------------------------------------------------------------
spectrum_chart(TA_instance,
               TA_instance["patterns"],
               identifiers = "new")

## -----------------------------------------------------------------------------
spectrum_chart(TA_instance,
               TA_instance["patterns"],
               sort = FALSE)

## ----echo=FALSE---------------------------------------------------------------
# Set a seed to have the same graph at each vignette generation
set.seed(2456201)

## ----fig.height=5.5-----------------------------------------------------------
# Spectrosome chart of the patterns of 'TA_instance'
spectrosome_1 <- 
  spectrosome_chart(TA_instance,
                    TA_instance["patterns"],
                    title = "Spectrosome of all patterns of TA_instance")

## -----------------------------------------------------------------------------
# Vertices of the previous graph
spectrosome_1$vertices

## -----------------------------------------------------------------------------
# Edges of the previous graph
 spectrosome_1$edges

## -----------------------------------------------------------------------------
# Coordinates of the vertices of the previous graph
spectrosome_1$coord

## ----fig.height=5.5-----------------------------------------------------------
spectrosome_1 <- spectrosome_chart(TA_instance,
                                   TA_instance["patterns"],
                                   coord = spectrosome_1$coords[[1]],
                                   use_names = FALSE)

## ----fig.height=5.5-----------------------------------------------------------
spectrosome_1 <- spectrosome_chart(TA_instance,
                                   TA_instance["patterns"],
                                   coord = spectrosome_1$coords[[1]],
                                   n.cutoff = 10,
                                   c.cutoff = 17)

## ----fig.height=5.5, eval=FALSE-----------------------------------------------
#  spectrosome_2 <- spectrosome_chart(TA_instance,
#                                     TA_instance["patterns"],
#                                     use_names = FALSE,
#                                     c.cutoff = 17,
#                                     nb_graphs = 3)

## ----fig.height=5.5, echo=FALSE-----------------------------------------------
# Set seeds to have the same graphs at each vignette generation
set.seed(123456)
s <- spectrosome_chart(TA_instance, TA_instance["patterns"], use_names = FALSE, c.cutoff = 17)

set.seed(2456102)
s <- spectrosome_chart(TA_instance, TA_instance["patterns"], use_names = FALSE, c.cutoff = 17)

set.seed(2456188)
s <- spectrosome_chart(TA_instance, TA_instance["patterns"], use_names = FALSE, c.cutoff = 17)

## ----fig.height=5.5-----------------------------------------------------------
spectrosome_3 <- spectrosome_chart(TA_instance,
                                   TA_instance["patterns"],
                                   coord = spectrosome_1$coords[[1]],
                                   use_names = FALSE,
                                   c.cutoff = 17,
                                   min_link_weight = 3)
spectrosome_3$edges

## ----fig.height=5.5-----------------------------------------------------------
spectrosome_1 <- spectrosome_chart(TA_instance,
                                   TA_instance["patterns"],
                                   coord = spectrosome_1$coords[[1]],
                                   use_names = FALSE,
                                   c.cutoff = 17,
                                   vertex_col = "categories",
                                   display_mixt = FALSE)

## ----fig.height=5.5-----------------------------------------------------------
spectrosome_1 <- spectrosome_chart(TA_instance,
                                   TA_instance["patterns"],
                                   coord = spectrosome_1$coords[[1]],
                                   use_names = FALSE,
                                   c.cutoff = 17,
                                   vertex_col = c("yellow", "pink", "#000060"))

## ----fig.height=5.5-----------------------------------------------------------
spectrosome_1 = spectrosome_chart(TA_instance,
                                  TA_instance["patterns"],
                                  coord = spectrosome_1$coords[[1]],
                                  use_names = FALSE,
                                  c.cutoff = 17,
                                  vertex_size = "absolute")

## ----fig.height=5.5-----------------------------------------------------------
spectrosome_1 = spectrosome_chart(TA_instance,
                                  TA_instance["patterns"],
                                  coord = spectrosome_1$coords[[1]],
                                  use_names = FALSE,
                                  c.cutoff = 17,
                                  vertex_size = 1.2)

## ----fig.height=5.5-----------------------------------------------------------
spectrosome_1 = spectrosome_chart(TA_instance,
                                  TA_instance["patterns"],
                                  coord = spectrosome_1$coords[[1]],
                                  use_names = FALSE,
                                  c.cutoff = 17,
                                  clusters = 3,
                                  highlight = 1)

## ----echo=FALSE---------------------------------------------------------------
# Set a seed to have the same graph at each vignette generation
set.seed(123457)

## -----------------------------------------------------------------------------
# Plot a spectrosome about the patterns 7 to 10 using original identifiers
spectrosome_4a <- spectrosome_chart(TA_instance,
                                    TA_instance["patterns"][7:10, ],
                                    identifiers = "original")

## ----echo=FALSE---------------------------------------------------------------
# Set a seed to have the same graph at each vignette generation
set.seed(123457)

## -----------------------------------------------------------------------------
# Plot a spectrosome about the patterns 7 to 10 using new identifiers
spectrosome_4b <- spectrosome_chart(TA_instance,
                                    TA_instance["patterns"][7:10,],
                                    coord = spectrosome_4a$coords[[1]],
                                    identifiers = "new")

## -----------------------------------------------------------------------------
spectrosome_5 <- spectrosome_chart(TA_instance,
                                   TA_instance["nodes"],
                                   clusters = 0,
                                   displaylabels = FALSE,
                                   displayisolates = FALSE,
                                   mode = "circle")

## ----echo=FALSE---------------------------------------------------------------
# Set a seed to have the same graph at each vignette generation
set.seed(2456208)

## -----------------------------------------------------------------------------
# Extract patterns relating to the item '3146'
patterns_1 <- get_patterns(TA_instance,
                           TA_instance["patterns"],
                           element = "items",
                           value = "3146")

# Plot the cluster of patterns relating to the item '3146'
spectrosome_6 <- spectrosome_chart(TA_instance,
                                   patterns_1,
                                   title = "Pattern cluster - item 3146")
spectrosome_6$vertices

## ----echo=FALSE---------------------------------------------------------------
# Set a seed to have the same graph at each vignette generation
set.seed(123460)

## ----fig.height=6-------------------------------------------------------------
# Extract patterns having items related to the value 'Chrome' of the category
# 'family'
patterns_2 <- get_patterns(TA_instance,
                           TA_instance["patterns"],
                           element = "family",
                           value = "Chrome",
                           condition = "items")

# Plot the spectrosome of the extracted patterns
spectrosome_7 <- spectrosome_chart(TA_instance,
                                   patterns_2,
                                   vertex_col = "categories",
                                   title = "Patterns - family Chrome")
spectrosome_7$vertices

## ----echo=FALSE---------------------------------------------------------------
# Use of a non-exported dataset: another TransactionAnalyzer object
another_ta_object <- uom.dat:::another_ta_object

# Set a seed to have the same graph at each vignette generation
set.seed(2456188)

## ----fig.height=6.5-----------------------------------------------------------
# Spectrosome chart of the patterns of 'another_ta_object'
spectrosome_8 <- spectrosome_chart(
  another_ta_object,
  another_ta_object["patterns"],
  vertex_size = "grouped",
  size_range = c(0.5, 1.5),
  displayisolates = FALSE,
  displaylabels = FALSE,
  title = "Another spectrosome graph about other data"
)

## ----echo=FALSE---------------------------------------------------------------
# Set a seed to have the same graph at each vignette generation
set.seed(123457)

## ----fig.height=6.5-----------------------------------------------------------
# Focus on the patterns that make the junction between two or more families of
# substances (i.e. two values of one specific category)
patterns_3 <- get_complexes(another_ta_object,
                            another_ta_object["patterns"],
                            category = "family",
                            condition = "vertices",
                            min_nb_values = 2)

# Plot these specific patterns
spectrosome_9 <- spectrosome_chart(
  another_ta_object,
  patterns_3,
  vertex_col = "categories",
  title = "Patterns complex by their number of associated families")

## ----eval=FALSE---------------------------------------------------------------
#  # Extract and create a graph of all rules of length 2, representing support
#  rules_3 <- rules_chart(TA_instance,
#                         category = "family",
#                         display = "support")
#  
#  # Plot the graph and print the characteristics of some rules
#  plot(rules_3$graph)
#  rules_3$rules[95:104, ]

## ----echo=FALSE, fig.height=6-------------------------------------------------
## Same chunk as the precedent because option results="hold" doesn't work with
## plots

# Extract and create a graph of all rules of length 2, representing support
rules_3 <- rules_chart(TA_instance,
                       category = "family",
                       display = "support")

# Plot the graph and print the characteristics of some rules
plot(rules_3$graph)
rules_3$rules[95:104, ]

## ----fig.height=5-------------------------------------------------------------
# Plot of specific rules, representing highest confidence
rules_chart(TA_instance,
            category = "family",
            display = "highest confidence",
            rules = rules_3$rules[95:104, ])

## ----eval=FALSE---------------------------------------------------------------
#  # Create a graph about rules of length 2 using parameter
#  rules_4a <- rules_chart(TA_instance,
#                          category = "family",
#                          display = "lift",
#                          parameter = list(support = 0.1,
#                                           confidence = 0.5))
#  
#  # Plot the graph and print the characteristics of the rules
#  plot(rules_4a$graph)
#  rules_4a$rules

## ----echo=FALSE---------------------------------------------------------------
## Same chunk as the precedent because option results="hold" doesn't work with
## plots

# Create a graph about rules of length 2 using parameter
rules_4a <- rules_chart(TA_instance,
                        category = "family",
                        display = "lift",
                        parameter = list(support = 0.1,
                                         confidence = 0.5))

# Plot the graph and print the characteristics of the rules
plot(rules_4a$graph)
rules_4a$rules

## ----eval=FALSE---------------------------------------------------------------
#  # Plot the previous rules having a lift greater than 4
#  rules_4b <- rules_chart(TA_instance,
#                          category = "family",
#                          rules = rules_4a$rules,
#                          display = "lift",
#                          threshold = 4)
#  
#  # Plot the graph and print the characacteristics of the rules
#  plot(rules_4b$graph)
#  rules_4b$rules

## ----echo=FALSE---------------------------------------------------------------
## Same chunk as the precedent because option results="hold" doesn't work with
## plots

# Plot the previous rules having a lift greater than 4
rules_4b <- rules_chart(TA_instance,
                        category = "family",
                        rules = rules_4a$rules,
                        display = "lift",
                        threshold = 4)

# Plot the graph and print the characacteristics of the rules
plot(rules_4b$graph)
rules_4b$rules

## -----------------------------------------------------------------------------
rules_chart(TA_instance,
            category = "family",
            rules = rules_4a$rules,
            display = "lift",
            use_names = FALSE
            )$graph

## -----------------------------------------------------------------------------
rules_chart(TA_instance,
            category = "family",
            rules = rules_4a$rules,
            display = "lift",
            n.cutoff = 10,
            c.cutoff = 20
            )$graph

## ----warning=FALSE, message=FALSE---------------------------------------------
# Loading the package
library(ggplot2)

# Make changes to the last plot
last_plot() + expand_limits(x = c(-1.3, 1.3),
                            y = c(-1.3, 1.3))

## ----fig.height=6-------------------------------------------------------------
# Plot the previous rules and all items
rules_chart(TA_instance,
            category = "family",
            rules = rules_4a$rules,
            display = "lift",
            c.cutoff = 17,
            items = TA_instance["items"]
            )$graph

## -----------------------------------------------------------------------------
# Plot the part of previous rules relating to 3 specific items
rules_chart(TA_instance,
            category = "family",
            rules = rules_4a$rules,
            display = "lift",
            items = c("25", "192", "328")
            )$graph +
  expand_limits(x = c(-1.1, 1.65),
                y = c(-1.0, 1.2))

## ----fig.height=6-------------------------------------------------------------
rules_chart(TA_instance,
            category = "family",
            rules = rules_4a$rules,
            display = "lift",
            c.cutoff = 17,
            use_names = FALSE,
            items = TA_instance["items"],
            sort_by = "item"
            )$graph

## -----------------------------------------------------------------------------
rules_chart(TA_instance,
            category = "family",
            rules = rules_4a$rules,
            display = "lift",
            vertex_size = 5,
            vertex_alpha = 0.4,
            vertex_margin = 0.15
            )$graph

## -----------------------------------------------------------------------------
rules_chart(TA_instance,
            category = "family",
            rules = rules_4a$rules,
            display = "lift",
            label_size = 5,
            label_margin = 0.15
            )$graph

## -----------------------------------------------------------------------------
rules_chart(TA_instance,
            category = "family",
            rules = rules_4a$rules,
            display = "lift",
            edge_looseness = 0,
            edge_alpha = 0.5
            )$graph

## ----echo=FALSE, out.extra="class='border_none'"------------------------------
old_par_1 <- graphics::par(mar = c(0, 4.1, 0, 2.1))

## Creation of the palettes

palettes <- matrix(character(200), ncol = 10, nrow = 20)
rownames(palettes) <- c("category10", "red", "pink", "purple", "deep-purple", "indigo", "blue", "light-blue", "cyan", "teal", "green", "light-green", "lime", "yellow", "amber", "orange", "deep-orange", "brown", "grey", "blue-grey")

palettes["category10", ] <- ggsci::pal_d3("category10")(10)
for (palette in rownames(palettes)[-1]) {
  palettes[palette, ] <- ggsci::pal_material(palette)(10)
}


## Display of the palettes

old_par_2 <- graphics::par(mgp = c(2, 0.25, 0))
graphics::plot(1, 1,
               xlim = c(0, ncol(palettes)),
               ylim = c(0, nrow(palettes)),
               type = "n", axes = FALSE, bty = "n", xlab = "", ylab = "")
for (i in seq_len(nrow(palettes))) {
  graphics::rect(xleft = seq_len(ncol(palettes)) - 1,
                 ybottom = i - 1,
                 xright = seq_len(ncol(palettes)),
                 ytop = i - 0.2,
                 col = palettes[i, ],
                 border = "light grey")
}
graphics::text(x = rep(-0.1, nrow(palettes)),
               y = seq_len(nrow(palettes)) - 0.6,
               labels = rownames(palettes), xpd = TRUE, adj = 1)

graphics::par(old_par_1)
graphics::par(old_par_2)

## ----echo=FALSE, out.extra="class='border_none'"------------------------------
old_par <- graphics::par(mar = c(0, 4.1, 0, 2.1))
RColorBrewer::display.brewer.all(type = "seq")
graphics::par(old_par)

## -----------------------------------------------------------------------------
rules_chart(TA_instance,
            category = "family",
            rules = rules_4a$rules,
            display = "lift",
            palette = "OrRd",
            palette_direction = -1
            )$graph

## -----------------------------------------------------------------------------
# Decomposed frequencies of all patterns of 'TA_instance'
frequency_by_complexity(TA_instance,
                        patterns = TA_instance["patterns"]$pattern)

# Frequencies of all patterns of 'TA_instance'
TA_instance["patterns"][, c("pattern", "frequency")]

## -----------------------------------------------------------------------------
# Look for the first and last year covered by the transactions of 'TA_instance'
cat("Year of the oldest transaction:     ",
    colnames(TA_instance["nodes_per_year"])[1])

cat("\nYear of the most recent transaction:",
    rev(colnames(TA_instance["nodes_per_year"]))[1])

## -----------------------------------------------------------------------------
# Classify the patterns
status_1 <- dynamic_status(TA_instance,
                           patterns = TA_instance["patterns"]$pattern,
                           end = 2018,
                           overall_period = 8,
                           recent_period = 2)

status_1$res
status_1$thresholds

## ----echo=FALSE---------------------------------------------------------------
# Set a seed to have the same graph at each vignette generation
set.seed(123461)

## -----------------------------------------------------------------------------
# Spectrosome from a subset of patterns made previously
spectrosome_10 <- spectrosome_chart(TA_instance,
                                    patterns_2,
                                    use_names = FALSE)

cat("Network density of the graph:",
    network_density(TA_instance,
                    links = spectrosome_10$edges))

## -----------------------------------------------------------------------------
# Density of the network of nodes of 'TA_instance'
cat("Network density of the graph formed by all nodes:",
    network_density(TA_instance,
                    links = TA_instance["node_links"]))

## -----------------------------------------------------------------------------
cat("Degree of the pattern identified 17 in the previous graph:",
    degree(TA_instance,
           ID = 17,
           links = spectrosome_10[["edges"]]))

cat("\nDegree of the first node in the graph of all nodes:",
    degree(TA_instance,
           ID = 1,
           links = TA_instance["node_links"]))

## -----------------------------------------------------------------------------
# Write in CSV the data relating to the patterns 3 to 7
export(TA_instance,
       TA_instance["patterns"][3:7, ])

## ----eval=FALSE---------------------------------------------------------------
#  # Write in a CSV file the data frame of nodes
#  export(TA_instance,
#         TA_instance["nodes"],
#         file = "nodes.csv")
#  
#  # Write in a CSV file vertices from a spectrosome graph made previously
#  export(TA_instance,
#         spectrosome_10[["vertices"]],
#         file = "vertices.csv")
#  
#  # Write in a CSV file association rules
#  export(TA_instance,
#         rules_2a,
#         file = "rules.csv")

## ----warning=FALSE------------------------------------------------------------
# Creation of a non-initialized TransactionAnalyzer from the transactions
# 'trx_object' created previously
ta_object_3 <- transaction.analyzer(trx_object,
                                    init = FALSE)

# Initalize nodes
init(ta_object_3,
     "nodes",
     verbose = FALSE)

# Initialize patterns and assign the returned object of class itemsets
arules_itemsets <- init(ta_object_3,
                        "patterns",
                        verbose = FALSE)
arules_itemsets

## -----------------------------------------------------------------------------
# Extract all rules from the transactions using default mining parameters, as
# rules object
arules_rules <- extract_rules(TA_instance,
                              arules = TRUE)
arules_rules

## ----eval=FALSE---------------------------------------------------------------
#  # Loading the package
#  library(arulesViz)
#  
#  # Plot a basic chart about rules
#  plot(arules_rules)

## ----warning=FALSE, message=FALSE, echo=FALSE---------------------------------
## Previous chunk: not hidden chunk containing the two instructions in only one chunk

## Hidden chunk: load library without warning messages
library(arulesViz)

## ----echo=FALSE---------------------------------------------------------------
## Hidden chunk: plot the chart with warning messages
# Set a seed to have the same graph at each vignette generation
set.seed(123468)
plot(arules_rules)

