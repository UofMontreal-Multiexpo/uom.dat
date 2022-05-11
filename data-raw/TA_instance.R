## Preparation of the 'TA_instance' dataset.
## The associated help page needs to be updated if the code here is changed.
## 
## Required datasets: - oedb_sample
##                    - substances_information

#### Creation of TA_instance ####

## Making a list of transactions
to_keep <- c("NAME", "ACTIVITY", "JOB.TITLE", "JOB.TASK", "SAMPLE.ID")
ws <- data.frame(WS.ID = c(1, 2, 2, 3, 3),
                 JOB.TITLE = c(44121004, 44142001, 44132032, 44132019, 44132030),
                 JOB.TASK = c("A5440", "A6410", "A5110", "A5260", "A5240"),
                 stringsAsFactors = FALSE)
ws_vars <- c("JOB.TITLE", "JOB.TASK")

trx <- make_OE_transactions(oedb_sample,
                            keys = c("ID", "CODE", "YEAR"),
                            mode = 1,
                            work_situations = ws,
                            variable_names = ws_vars,
                            additional = to_keep,
                            unique_values = TRUE)

## Associating item identifiers with names and one category
substances <- get_all_items(trx)
families <- substances_information[match(substances,
                                         substances_information$CODE),
                                   "SUBFAMILY"]
families[is.na(families)] <- "Unknown"
names <- substances_information[match(substances,
                                      substances_information$CODE),
                                "NAME"]

items <- data.frame(item = substances,
                    name = names,
                    family = as.factor(families),
                    stringsAsFactors = FALSE)

## Creation of the TransactionAnalyzer
TA_instance <- transaction.analyzer(trx, items)



#### Add prepared data to package ####

#! See file "data-raw/add_prepared_data_to_package.R"


