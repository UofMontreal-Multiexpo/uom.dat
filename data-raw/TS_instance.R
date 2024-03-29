## Preparation of the 'TS_instance' dataset.
## The associated help page need to be updated if the code here is updated.
## 
## Required dataset: oedb_sample

#### Creation of TS_instance ####

## Making a list of transactions by grouping data
trx <- make_transactions(oedb_sample,
                         by = "ID",
                         additional = c("CODE", "YEAR",
                                        "JOB.TITLE", "JOB.TASK", "SAMPLE.ID"))

## Creation of the TransactionSet
TS_instance <- transaction.set(data = trx, item_key = "CODE", year_key = "YEAR")
TS_instance["names"] <- TS_instance["names"][-1]



#### Add prepared data to package ####

#! See file "data-raw/add_prepared_data_to_package.R"


