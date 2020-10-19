## ----setup, include=FALSE-----------------------------------------------------
# fig.align option seems to not be able to be set in the YAML header
knitr::opts_chunk$set(fig.align = "center")

## ----message=FALSE------------------------------------------------------------
# Loading the package
library(oedb.analysis.tools)

# Creation of specific observations
observations <- make_INRS_observations(
  oedb_sample,
  mode = 1,
  work_situations = data.frame(WS_ID = c(1, 2, 2, 3, 3),
                               JOB.TITLE = c(44121004, 44142001, 44132032, 44132019, 44132030),
                               JOB.TASK = c("A5440", "A6410", "A5110", "A5260", "A5240"),
                               stringsAsFactors = FALSE),
  variable_names = c("JOB.TITLE", "JOB.TASK"),
  additional = c("NAME", "CONCENTRATION", "SAMPLE.ID", "JOB.TITLE", "JOB.TASK"),
  unique_values = c("JOB.TITLE", "JOB.TASK")
)

## -----------------------------------------------------------------------------
observations[[2]]

## -----------------------------------------------------------------------------
concentration_values <- lapply(observations,
                               function(obs) setNames(obs$CONCENTRATION, obs$NAME))

# Let's print the second element of the new list,
# corresponding to the second element of the previous list of observations
concentration_values[[2]]

## -----------------------------------------------------------------------------
substances_information[349:353, c("CODE", "NAME", "TOXICITY")]

## -----------------------------------------------------------------------------
classification <- substances_information$TOXICITY
names(classification) <- substances_information$NAME

## -----------------------------------------------------------------------------
class_names <- sort(unique(unlist(classification)))
class_names

## -----------------------------------------------------------------------------
# First, let's check that the value we are going to assign will be unique to substances
# that have no limit
cat("Number of substances having the limit 35:",
    sum(substances_information$LIMIT == 35, na.rm = TRUE),
    "\n")

## -----------------------------------------------------------------------------
# We could also keep a vector defining whether a substance had a limit
no_limit <- is.na(substances_information$LIMIT)

# Extract the limits in a new variable and setting a new limit for missing values
reference_values <- substances_information$LIMIT
reference_values[no_limit] <- 35

## -----------------------------------------------------------------------------
# Naming the vector with the substances the limits are associated with
names(reference_values) <- substances_information$NAME

# Extraction of the names related to the concentrations and removing the duplicates
substance_names <- unique(unlist(lapply(concentration_values, names)))
cat("Number of distinct substances:", length(substance_names))

## ----results="hold"-----------------------------------------------------------
# Extraction of the limits related to the concentrations
reference_values <- reference_values[substance_names]

# Let's see what we have
cat("A subset of the reference values:\n")
reference_values[1:4]

## -----------------------------------------------------------------------------
cat("Number of sets of values:",
    length(concentration_values))

## -----------------------------------------------------------------------------
# Computing the indicators of the MCR approach
mcr_summary(values = concentration_values,
            references = reference_values)

## ----results="hold"-----------------------------------------------------------
# Extraction of the names related to the concentrations of the second observation
substance_names_2 <- unlist(names(concentration_values[[2]]))

# Compute each of the indicators one by one, for the values of the second observation

cat("Hazard quotients:\n")
hazard_quotient(values = concentration_values[[2]],
                references = reference_values[substance_names_2])

cat("\nHazard index:\n")
hazard_index(values = concentration_values[[2]],
             references = reference_values[substance_names_2])

cat("\nMaximum hazard quotient:\n")
maximum_hazard_quotient(values = concentration_values[[2]],
                        references = reference_values[substance_names_2])

cat("\nMaximum cumulative ratio:\n")
maximum_cumulative_ratio(values = concentration_values[[2]],
                         references = reference_values[substance_names_2])

cat("\nReciprocal of the maximum cumulative ratio:\n")
reciprocal_of_mcr(values = concentration_values[[2]],
                  references = reference_values[substance_names_2])

cat("\nMissed toxicity:\n")
missed_toxicity(values = concentration_values[[2]],
                references = reference_values[substance_names_2])

cat("\nTop hazard quotient:\n")
top_hazard_quotient(values = concentration_values[[2]],
                    references = reference_values[substance_names_2],
                    k = 1)

cat("\nMIAT group:\n")
classify_mixture(values = concentration_values[[2]],
                 references = reference_values[substance_names_2])

## -----------------------------------------------------------------------------
mcr_chart(values = concentration_values,
          references = reference_values)

## -----------------------------------------------------------------------------
mcr_chart(values = concentration_values,
          references = reference_values,
          log_transform = FALSE)

## -----------------------------------------------------------------------------
mcr_chart(values = concentration_values,
          references = reference_values,
          regions = TRUE,
          log_transform = FALSE)

## ----warning=FALSE------------------------------------------------------------
library(ggplot2)

last_plot() + coord_cartesian(xlim = c(-0.5, 5),
                              ylim = c(0.5, 8))

## ----warning=FALSE------------------------------------------------------------
# Loading color library for a paleton of 20 colors
library(ggsci)
paleton <- pal_d3("category20")(20)

# Because there are more than 20 references, we need to use extra colors
new_thq_col <- c(paleton[1:20], "red", "green", "blue", "yellow", "black")

mcr_chart(values = concentration_values,
          references = reference_values,
          thq_col = new_thq_col)

## ----warning=FALSE------------------------------------------------------------
mcr_chart(values = concentration_values,
          references = reference_values,
          regions = TRUE,
          regions_col = c("#fdcc8a", "#fef0d9", "#fc8d59", "#d7301f"),
          regions_alpha = 0.1)

## ----warning=FALSE------------------------------------------------------------
mcr_chart(values = concentration_values,
          references = reference_values,
          regions_lab = c(TRUE, TRUE, FALSE, TRUE))

## ----warning=FALSE------------------------------------------------------------
mcr_chart(values = concentration_values,
          references = reference_values,
          regions_lab = c(TRUE, TRUE, FALSE, TRUE),
          regression = TRUE)

## -----------------------------------------------------------------------------
# Frequency of the pairs that produced the top two hazard quotients while hazard index
# is greater than 1
thq_pairs_freq(values = concentration_values,
               references = reference_values)

## -----------------------------------------------------------------------------
# Frequency of element that produced the top hazard quotient with its associated group
thq_freq_by_group(values = concentration_values,
                  references = reference_values)

## -----------------------------------------------------------------------------
table_levels <- names(reference_values)[-19]
# For readability of the example, intentionnaly removal of the 19th name which is quite long

thq_freq_by_group(values = concentration_values,
                  references = reference_values,
                  levels = sort(table_levels))

## -----------------------------------------------------------------------------
# Computing the indicators of the MCR approach
mcr_s <- mcr_summary(values = concentration_values,
                     references = reference_values)

## -----------------------------------------------------------------------------
cor(x = mcr_s$HI,
    y = mcr_s$MCR - 1)

## -----------------------------------------------------------------------------
plot(x = mcr_s$HI,
     y = mcr_s$MCR - 1)

## -----------------------------------------------------------------------------
# Use of the year information associated with the concentration values
# in the original observations
cor(x = sapply(observations, "[[", "YEAR"),
    y = mcr_s$HI)

## -----------------------------------------------------------------------------
plot(x = sapply(observations, "[[", "YEAR"),
     y = mcr_s$HI)

## -----------------------------------------------------------------------------
# Sets of groups and of indicators
groups <- c("I", "II", "IIIA", "IIIB")
indicators <- c("HI", "MCR", "MHQ", "Missed")

# Compute for each group, the summary of the results of each indicator
list_of_summaries <-
  lapply(groups, function(group) {
    sapply(indicators, function(indicator) {
      summary(mcr_s[mcr_s$Group == group, indicator])
    })
  })
names(list_of_summaries) <- groups

list_of_summaries

## -----------------------------------------------------------------------------
classification[349:353]

## -----------------------------------------------------------------------------
# Make the summary for each class
summaries <- mcr_approach_by_class(values = concentration_values,
                                   references = reference_values,
                                   classes = classification,
                                   FUN = mcr_summary)

cat("Number of summaries:", length(summaries))

## ----eval=FALSE---------------------------------------------------------------
#  # Just look at few examples:
#  # summaries of the concentration values of the observations 4 to 6
#  summaries[4:6]

## ----echo=FALSE, results="hold"-----------------------------------------------
# Beautify the display because knitkr::table is not call in a list (even if list of data frames)

# Unfortunatly, loop does not seem to be compatible with the knitr::kable specific data.frame print
s <- 4
cat("[[", s, "]]\n", sep = "") ; s <- s + 1
summaries[[s]] ; cat("\n")
cat("[[", s, "]]\n", sep = "") ; s <- s + 1
summaries[[s]]
cat("[[", s, "]]\n", sep = "")
summaries[[s]]


## -----------------------------------------------------------------------------
# Make one chart for each class
charts <- mcr_approach_by_class(values = concentration_values,
                                references = reference_values,
                                classes = classification,
                                FUN = mcr_chart)

## -----------------------------------------------------------------------------
cat("Number of charts (i.e. number of classes encountered):", length(charts))

## -----------------------------------------------------------------------------
# Plot some of these charts
charts[[1]]
charts[[12]]
charts[["Sensibilisants"]]

## -----------------------------------------------------------------------------
# Add further arguments (regions and log_transform)
charts <- mcr_approach_by_class(values = concentration_values,
                                references = reference_values,
                                classes = classification,
                                FUN = mcr_chart,
                                regions = TRUE,
                                log_transform = FALSE)

# Look at the chart of the second class (and add the class name as a title with ggplot2)
charts[[2]] + ggtitle(names(charts)[2])

## -----------------------------------------------------------------------------
# Build one contingency table for each class and look at some of them
mcr_approach_by_class(values = concentration_values,
                      references = reference_values,
                      classes = classification,
                      FUN = thq_pairs_freq)[10:12]

## -----------------------------------------------------------------------------
# Build one contingency table for each class and look at some of them
mcr_approach_by_class(values = concentration_values,
                      references = reference_values,
                      classes = classification,
                      FUN = thq_freq_by_group)[10:12]

## -----------------------------------------------------------------------------
classification_1 <- list(A = c("C5", "C6", "C8"),
                         B = "C8",
                         C = c("C3", "C8"),
                         D = c("C1", "C3", "C4", "C6"),
                         E = c("C2", "C4", "C5", "C7", "C8"))
classification_1

## -----------------------------------------------------------------------------
# Union with no other argument: identify all classes involved by all elements
union_on_list(classification_1)

## -----------------------------------------------------------------------------
# Union with a set of indices: identify classes involved by specific elements
union_on_list(classification_1,
              indices = c("A", "C"))

## -----------------------------------------------------------------------------
# Intersection with no other argument: identify the common classes between all elements
intersect_on_list(classification_1)

## -----------------------------------------------------------------------------
# Intersection with a set of indices: identify the common classes between specific elements
intersect_on_list(classification_1,
                  indices = c(1, 3))

## -----------------------------------------------------------------------------
# Overall table
table_on_list(classification_1)

## -----------------------------------------------------------------------------
# Table of classes encountered at specific indices
table_on_list(classification_1,
              indices = c(1, 2, 3))

## -----------------------------------------------------------------------------
# Table of classes encountered for specific elements, showing all classes
table_on_list(classification_1,
              indices = c("A", "B", "C"),
              with_zero = TRUE)

## -----------------------------------------------------------------------------
# Preparing three vectors of indices in a matrix
indices_matrix <- matrix(c("A", "B", "C",
                           "A", "B", "D",
                           "B", "C", "E"),
                         nrow = 3,
                         byrow = TRUE)
# Building of tables
table_on_list(classification_1, indices_matrix)

## -----------------------------------------------------------------------------
# Adding names to the three vectors of indices
rownames(indices_matrix) <- c("v1", "v2", "v3")

# Building of tables
table_on_list(classification_1,
              indices_matrix,
              with_zero = TRUE)

## -----------------------------------------------------------------------------
# Preparing three vectors of indices in a list
indices_list <- list(c("A", "B", "C"),
                     c("A", "B"),
                     "A")
# Building of tables
table_on_list(classification_1, indices_list)

## -----------------------------------------------------------------------------
# Building of tables of all classes
table_on_list(classification_1,
              indices_list,
              with_zero = TRUE)

## -----------------------------------------------------------------------------
classification_matrix <- turn_list_into_logical_matrix(classification_1)
classification_matrix

## -----------------------------------------------------------------------------
classification_list <- turn_logical_matrix_into_list(classification_matrix)
classification_list

## -----------------------------------------------------------------------------
classification_2 <- list(C1 = "D",
                         C2 = "E",
                         C3 = c("C", "D"),
                         C4 = c("D", "E"),
                         C5 = c("A", "E"),
                         C6 = c("A", "D"),
                         C7 = "E",
                         C8 = c("A", "B", "C", "E"))
classification_2

## -----------------------------------------------------------------------------
invert_list(classification_2)

## -----------------------------------------------------------------------------
invert_list(classification_1)

## -----------------------------------------------------------------------------
classification_3 <- matrix(c(rep("A", 3), "B", rep("C", 2), rep("D", 4), rep("E", 5),
                             "C5", "C6", "C8",
                             "C8",
                             "C3", "C8",
                             "C1", "C3", "C4", "C6",
                             "C2", "C4", "C5", "C7", "C8"),
                           ncol = 2,
                           dimnames = list(NULL, c("element", "class")))
classification_3

## -----------------------------------------------------------------------------
turn_char_matrix_into_list(classification_3)

## -----------------------------------------------------------------------------
# Naming the observations of which concentration values are from: O1 to O14
names(concentration_values) <- paste0("O", 1:14)

## -----------------------------------------------------------------------------
# Just look at some columns
turn_list_into_logical_matrix(concentration_values,
                              by_name = TRUE)[, c(4:5,8,12:14,23:25)]

## -----------------------------------------------------------------------------
# Extraction of the subsets for the ninth class
subsets <- subset_from_class(values = concentration_values,
                             references = reference_values,
                             classes = classification,
                             class_name = class_names[9])

## ----results="hold"-----------------------------------------------------------
cat("Number of values of each observation:\n")
sapply(concentration_values, length)

cat("\nNumber of values related to the ninth class :\n")
sapply(subsets[["values"]], length)

## ----results="hold"-----------------------------------------------------------
cat("Values of the sixth observation:\n")
concentration_values$O6

cat("\nValues of the sixth observation related to the ninth class:\n")
subsets[["values"]]$O6

## -----------------------------------------------------------------------------
subsets[["references"]]
