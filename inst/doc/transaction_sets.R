## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(fig.align = "center",
                      results = "hold")

## ----message=FALSE------------------------------------------------------------
library(uom.dat)

## -----------------------------------------------------------------------------
print(TS_instance)

## ----eval=FALSE---------------------------------------------------------------
#  View(TS_instance)

## -----------------------------------------------------------------------------
# Creation of a list of transactions
trx_list <- list(T1 = list(items = "A",
                           date = 2018,
                           labels = c("l1", "l2")),
                 
                 T2 = list(items = c("A", "B", "D"),
                           date = 2018,
                           labels = "l3"),
                 
                 T3 = list(items = c("B", "C", "D"),
                           date = 2019,
                           labels = c("l2", "l3")),
                 
                 T4 = list(items = c("A", "B", "C", "D"),
                           date = 2020,
                           labels = c("l1", "l2", "l3"))
                 )

# Creation of a TransactionSet object
trx_object_1 <- transaction.set(trx_list,
                                item_key = "items",
                                year_key = "date")
print(trx_object_1)

## -----------------------------------------------------------------------------
# Creation of a dataset for the example
a_dataset <- data.frame(ID = c("T1", "T2", "T3", "T4", "T1", "T2",
                               "T3", "T4", "T2", "T3", "T4", "T4"),
                        items = c("A", "A", "B", "A", "A", "B",
                                  "C", "B", "D", "D", "C", "D"),
                        date = c(2018, 2018, 2019, 2020, 2018, 2018,
                                 2019, 2020, 2018, 2019, 2020, 2020),
                        labels = c("l1", "l3", "l2", "l1", "l2", "l3",
                                   "l3", "l2", "l3", "l2", "l3", "l3"),
                        some.more.data = c(5, 1, 3, 9, 8, 5, 3, 3, 3, 9, 4, 4))
a_dataset

## -----------------------------------------------------------------------------
# Creation of a TransactionSet
trx_object_2 <- make_transactions(a_dataset,
                                  by = "ID",
                                  additional = c("items", "date", "labels"),
                                  unique_values = TRUE,
                                  item_key = "items",
                                  year_key = "date")
print(trx_object_2)

## -----------------------------------------------------------------------------
# Print the second transaction to see what it looks like
print(trx_object_2["data"][[2]])

## -----------------------------------------------------------------------------
ws_variables <- c("JOB.TITLE", "JOB.TASK")

work_situations <- data.frame(WS.ID = c(1, 2, 2, 3, 3),
                              JOB.TITLE = c(44121004, 44142001, 44132032, 44132019, 44132030),
                              JOB.TASK = c("A5440", "A6410", "A5110", "A5260", "A5240"),
                              stringsAsFactors = FALSE)
work_situations

## -----------------------------------------------------------------------------
data_variables <- c("NAME", "ACTIVITY", "JOB.TITLE", "JOB.TASK", "SAMPLE.ID")
key_variables <- c("ID", "CODE", "YEAR")

## -----------------------------------------------------------------------------
trx_object_3 <- make_OE_transactions(measures = oedb_sample,
                                     keys = key_variables,
                                     mode = 2,
                                     work_situations = work_situations,
                                     additional = data_variables)
print(trx_object_3)

## -----------------------------------------------------------------------------
trx_object_4 <- make_OE_transactions(measures = oedb_sample,
                                     keys = key_variables,
                                     mode = 3,
                                     variable_names = ws_variables,
                                     additional = data_variables)
print(trx_object_4)

## -----------------------------------------------------------------------------
trx_object_5 <- make_OE_transactions(measures = oedb_sample,
                                     keys = key_variables,
                                     mode = 1,
                                     work_situations = work_situations,
                                     variable_names = ws_variables,
                                     additional = data_variables)
print(trx_object_5)

## -----------------------------------------------------------------------------
# Access an attribute
trx_object_3["item_key"]

## -----------------------------------------------------------------------------
# The original TransactionSet
print(trx_object_3)

# Remove elements of the transactions by naming those to keep
trx_object_3["names"] <- c("CODE", "NAME")
print(trx_object_3)

## -----------------------------------------------------------------------------
# Access the transactions 1 and 3
trx_object_3[c(1,3)]

## -----------------------------------------------------------------------------
# Access a sub-element of each of the 3 transactions
trx_object_3["CODE"]

## -----------------------------------------------------------------------------
# The original TransactionSet
cat("The original object of class TransactionSet:\n")
print(trx_object_1)

# Subset containing the transactions 1, 2 and 4 (using a numeric vector)
trx_subset_1 <- subset(trx_object_1,
                       c(1, 2, 4))
cat("\nA subset:\n")
print(trx_subset_1)

# Subset containing the transactions 2 and 4 (using a logical vector)
trx_subset_2 <- subset(trx_object_1,
                       c(FALSE, TRUE, FALSE, TRUE))
cat("\nAnother subset:\n")
print(trx_subset_2)

## -----------------------------------------------------------------------------
# Print the names identifying the transactions
cat("Original transaction names:\n")
print(names(trx_object_1["data"]))

# Reorder the first three transactions
reordered_trx_object <- reorder(trx_object_1,
                                c(3, 1, 2, 4))

# Print the names identifying the transactions of the TransactionSet copy
cat("New transaction names after reordering:\n")
print(names(reordered_trx_object["data"]))

## -----------------------------------------------------------------------------
cat("Items contained in the transactions of 'trx_object_1':\n")
get_all_items(trx_object_1)

## -----------------------------------------------------------------------------
cat("List of the itemsets corresponding to the transactions of 'trx_object_1':\n")
get_itemsets(trx_object_1)

## -----------------------------------------------------------------------------
# Items existing in the object TS_instance
cat("Items from the TransactionSet 'TS_instance':\n")
get_all_items(TS_instance)

# Items associated with one specific value of another variable
cat("\nItems from the transactions having the JOB.TITLE value '44132017':\n")
get_items_from_info(TS_instance,
                    info = list(JOB.TITLE = 44132017))

# Items associated with one of the specific values of other variables
cat("\nItems from the transactions having the JOB.TITLE value '44132017'",
    "or '45113004' or having the JOB.TASK value 'A5310':\n")
get_items_from_info(TS_instance,
                    info = list(JOB.TITLE = c(44132017, 45113004),
                                JOB.TASK = "A5310"),
                    presence = "any")

# Items and other data associated with one specific value of another variable
cat("\nItems and values of SAMPLE.ID from the transactions associated with the",
    "JOB.TITLE value '44132017':\n")
get_items_from_info(TS_instance,
                    info = list(JOB.TITLE = 44132017),
                    additional = "SAMPLE.ID")

## -----------------------------------------------------------------------------
# Data associated with one specific item
cat("SAMPLE.ID values from the transactions having the item '3146':\n")
get_info_from_items(TS_instance,
                    items = 3146,
                    info_names = "SAMPLE.ID")

# Data associated with at least one of two specific items
cat("\nJOB.TITLE and JOB.TASK values from the transactions having the item '19' or '25':\n")
get_info_from_items(TS_instance,
                    items = c(19, 25),
                    info_names = c("JOB.TITLE", "JOB.TASK"),
                    presence = "any")

# Data associated with exactly two specific items
cat("\nJOB.TITLE and JOB.TASK values from the transactions having exactly the itemset composed",
    "of '19' and '25':\n")
get_info_from_items(TS_instance,
                    items = c(19, 25),
                    info_names = c("JOB.TITLE", "JOB.TASK"),
                    presence = "exactly")

# Data associated with only items from those specified
cat("\nJOB.TITLE and JOB.TASK values from the transactions having the item '192' or '3146'",
    "and no unspecified item:\n")
get_info_from_items(TS_instance,
                    items = c(192, 3146),
                    info_names = c("JOB.TITLE", "JOB.TASK"),
                    presence = "only")

## -----------------------------------------------------------------------------
# Transactions containing at least two specific items
trx_subset_4 <- get_trx_from_items(TS_instance,
                                   items = c(25, 192),
                                   presence = "all")
cat("Transactions with itemsets composed at least of the items '25' and '192':\n")
print(trx_subset_4)

# Transactions containing at least one of two specific items
trx_subset_5 <- get_trx_from_items(TS_instance,
                                   items = c(25, 192),
                                   presence = "any")
cat("\nTransactions with itemsets composed at least of the item '25' or '192':\n")
print(trx_subset_5)

# Transactions corresponding exactly to a specific itemset
trx_subset_6 <- get_trx_from_items(TS_instance,
                                   items = c(25, 192),
                                   presence = "exactly")
cat("\nTransactions with itemsets composed exactly of the items '25' and '192':\n")
print(trx_subset_6)

# Transactions containing only the sought items
trx_subset_7 <- get_trx_from_items(TS_instance,
                                   items = c(192, 3146),
                                   presence = "only")
cat("\nTransactions with itemsets composed only of the items '192' and '3146':\n")
print(trx_subset_7)

## -----------------------------------------------------------------------------
# Transactions associated with one of the specific values of certain variables
trx_subset_8 <- get_trx_from_info(TS_instance,
                                  info = list(JOB.TITLE = 44132017,
                                              JOB.TASK = "A5310"),
                                  presence = "any")
cat("Transactions containing the JOB.TITLE value '44132017' or the JOB.TASK value 'A5310':\n")
print(trx_subset_8)

# Items associated with all specified values of certain variables
trx_subset_9 <- get_trx_from_info(TS_instance,
                                  info = list(JOB.TITLE = 44132017,
                                              JOB.TASK = "A5310"),
                                  presence = "all")
cat("\nTransactions containing both JOB.TITLE value '44132017' and JOB.TASK value 'A5310':\n")
print(trx_subset_9)

## -----------------------------------------------------------------------------
# Complex transactions
trx_subset_10 <- get_complex_trx(TS_instance)
cat("Transactions with itemsets larger than 1:\n")
print(trx_subset_10)

# Length of each extracted transaction
cat("\nNumber of items of each transaction:\n")
sapply(trx_subset_10["CODE"], length)

## -----------------------------------------------------------------------------
# Simple transactions
trx_subset_11 <- get_simple_trx(TS_instance)
cat("Transactions with itemsets of length 1:\n")
print(trx_subset_11)

# Length of each extracted transaction
cat("\nNumber of items of each transaction:\n")
sapply(trx_subset_11["CODE"], length)

## -----------------------------------------------------------------------------
cat("Transaction itemsets of 'trx_object_1':\n")
get_itemsets(trx_object_1)

## -----------------------------------------------------------------------------
cat("Complexity indexes of the item 'A', then of all items:\n")
complexity_index(trx_object_1, "A")
complexity_index(trx_object_1)

## -----------------------------------------------------------------------------
cat("Complexity ratios of the item 'A', then of all items:\n")
complexity_ratio(trx_object_1, "A")
complexity_ratio(trx_object_1)

## -----------------------------------------------------------------------------
cat("Proportion of complex transactions in 'TS_instance':",
    length(get_complex_trx(TS_instance)) / length(TS_instance))

## -----------------------------------------------------------------------------
cat("Transaction itemsets of 'trx_object_1':\n")
get_itemsets(trx_object_1)

## -----------------------------------------------------------------------------
# Compute co-occurrences for all items
co_occurrence_matrix(trx_object_1)

## -----------------------------------------------------------------------------
# Compute co-occurrences for specific items
co_occurrence_matrix(trx_object_1, c("A", "C", "D"))

## -----------------------------------------------------------------------------
table_on_list(trx_object_1["items"])

## -----------------------------------------------------------------------------
# Compute the proportions of the co-occurrences
co_occurrence_matrix(trx_object_1, proportions = TRUE)

## -----------------------------------------------------------------------------
# Co-occurrence chart on the TransactionSet 'trx_object_1' and all existing items
co_occurrence_chart(trx_object_1)

## -----------------------------------------------------------------------------
# Co-occurrence chart on the TransactionSet 'TS_instance' and all existing items
co_occurrence_chart(TS_instance)

## -----------------------------------------------------------------------------
co_occurrence_chart(TS_instance,
                    items = c(25, 27, 49, 87, 148, 192, 252, 328))

## -----------------------------------------------------------------------------
co_occurrence_chart(TS_instance,
                    items = c(25, 27, 49, 87, 148, 192, 252, 328),
                    proportions = TRUE)

## -----------------------------------------------------------------------------
co_occurrence_chart(TS_instance,
                    min_occ = 2,
                    max_occ = 2)

## -----------------------------------------------------------------------------
co_occurrence_chart(TS_instance,
                    vertex_size = 4,
                    vertex_alpha = 0.4,
                    vertex_margin = 0.15)

## -----------------------------------------------------------------------------
co_occurrence_chart(TS_instance,
                    label_size = 5,
                    label_margin = 0.15)

## ----warning=FALSE------------------------------------------------------------
# Loading the package
library(ggplot2)

# Make changes to the last plot
last_plot() + expand_limits(x = c(-1.5, 1.5),
                            y = c(-1.5, 1.5))

## -----------------------------------------------------------------------------
co_occurrence_chart(TS_instance,
                    edge_looseness = 0,
                    edge_alpha = 0.5)

## ----echo=FALSE, out.extra="class='border_none'"------------------------------
old_par <- par(mar = c(0, 4.1, 0, 2.1))
RColorBrewer::display.brewer.all(type = "seq")
par(old_par)

## -----------------------------------------------------------------------------
co_occurrence_chart(TS_instance,
                    palette = "OrRd",
                    palette_direction = -1)

## ----collapse=TRUE, warning=FALSE---------------------------------------------
# Loading the package
library(corrplot)

# Creation of a matrix
co_occ_matrix <- co_occurrence_matrix(TS_instance)

## ----fig.width=5--------------------------------------------------------------
corrplot(co_occ_matrix,
         is.corr = FALSE,
         col = COL1("Blues"))

## ----fig.width=5--------------------------------------------------------------
corrplot.mixed(co_occ_matrix,
               is.corr = FALSE,
               order = "hclust",
               upper = "color",
               number.digits = 0,
               upper.col = COL1("Blues"),
               lower.col = COL1("Blues"),
               tl.col = "black",
               tl.cex = 0.5)

## -----------------------------------------------------------------------------
# Itemset chart on the TransactionSet 'trx_object_1'
itemset_chart(trx_object_1,
              title = "Itemsets from trx_object_1")

## -----------------------------------------------------------------------------
# Itemset chart on the TransactionSet 'TS_instance'
itemset_chart(TS_instance,
              title = "Itemsets from TS_instance")

## -----------------------------------------------------------------------------
itemset_chart(TS_instance,
              jitter = NA)

## -----------------------------------------------------------------------------
itemset_chart(TS_instance,
              length_one = FALSE)

## -----------------------------------------------------------------------------
TS_instance["names"]

## -----------------------------------------------------------------------------
# Plot the itemset chart of 'TS_instance' and display the identifiers and years
# associated to the transactions
itemset_chart(TS_instance,
              under = "YEAR",
              over = "ID",
              jitter = NA)

## -----------------------------------------------------------------------------
itemset_chart(TS_instance,
              identifiers = "new",
              under = "ID",
              jitter = NA)

## -----------------------------------------------------------------------------
# Write in CSV the TransactionSet 'trx_object_1'
export(trx_object_1)

## ----eval=FALSE---------------------------------------------------------------
#  # Write in a CSV file the TransactionSet 'TS_instance'
#  export(TS_instance,
#         file = "transactions.csv")

## -----------------------------------------------------------------------------
# Coerce a TransactionSet to a data.frame
as(trx_object_1, "data.frame")

## -----------------------------------------------------------------------------
# Coerce a 'TransactionSet' object to a 'transactions' object
arules_trx <- as(trx_object_1, "transactions")
arules_trx

