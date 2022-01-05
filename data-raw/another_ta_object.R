## Preparation of the 'another_ta_object' dataset.
## 
## Input files: - IMIS_clean_20191107_partial.RDS
##              - Thesaurus_agents_clean_family_freq_07112019_partial.txt

#### Creation of another_ta_object ####

# "IMIS_clean_20191107_partial.RDS"
IMIS <- readRDS("./data-raw/IMIS_clean_20191107_partial.RDS")
other_trx <- make_transactions(IMIS,
                               by = c("COMPANY_INDEX", "JOB.TITLE", "YEAR"),
                               additional = "SUBST",
                               item_key = "SUBST",
                               year_key = "YEAR")

# "Thesaurus_agents_clean_family_freq_07112019_partial.txt"
family_subst <- read.delim("./data-raw/Thesaurus_agents_clean_family_freq_07112019_partial.txt",
                           stringsAsFactors = FALSE)
family_subst$Code[nchar(family_subst$Code) == 2] <- paste0("00", family_subst$Code[nchar(family_subst$Code) == 2])
family_subst$Code[nchar(family_subst$Code) == 3] <- paste0("0", family_subst$Code[nchar(family_subst$Code) == 3])

other_substances <- get_all_items(other_trx)
other_families <- family_subst$Family[match(other_substances, family_subst$Code)]
other_families[is.na(other_families)] <- "Unknown"
other_categories <- data.frame(item = other_substances,
                               family = as.factor(other_families),
                               stringsAsFactors = FALSE)

another_ta_object <- transaction.analyzer(other_trx, items = other_categories,
                                          target = "maximally frequent itemsets", count = 100)
# [04s] [00s] [03s] [01m 28s] [04s]
# [03s] [00s] [00s]     [00s] [00s]
# => 01m 42s



#### Weight reduction of another_ta_object ####

# object.size(another_ta_object) / 1024 / 1024
# 423.7 MB (RDS: 25.3 MB)

# Removal of values of attributes unnecessary for the use made of them
another_ta_object@transactions = subset(another_ta_object@transactions, F)
another_ta_object@nodes = data.frame()
another_ta_object@nodes_per_year = matrix()
another_ta_object@n_links = matrix()
another_ta_object@node_links = data.frame()
another_ta_object@nodes_patterns = matrix()
another_ta_object@patterns_per_year = matrix()
another_ta_object@p_links = matrix()

# object.size(another_ta_object) / 1024
# sapply(slotNames(another_ta_object), function(n) object.size(another_ta_object[n]))
# 194.1 KB (RDS: 15 KB)



#### Add prepared data to package ####

#! See file "data-raw/add_prepared_data_to_package.R"


