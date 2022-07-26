# uom.dat 0.2.1.9000 (development version)

## New features

* The following functions now have an argument `ignore_zero` to choose between ignoring and considering values equal to 0.
  * `top_hazard_quotient`. It returns `NA` if this new argument is set to `TRUE` and all values are equal to 0.
  * `mcr_summary` and `mcr_summary_by_class`. They previously always ignored values equal to 0. A new column named `n_zero` containing the number of values equal to 0 is added to the resulting data frames if this new argument is set to `TRUE`. It is always considered `TRUE` while identifying top hazard quotients.
  * `thq_pairs` and `thq_pairs_by_class`.
  * `thq_by_group` and `thq_by_group_by_class`.

## Minor changes

* The functions `maximum_cumulative_ratio`, `missed_toxicity` and `reciprocal_of_mcr` now return 0 instead of `NaN` if the input values are all equal to 0.

## Fixes

* An error was occurring when calling the function `top_hazard_quotient` by giving sets of values or of HQ containing only values equal to 0 and using the default value of the argument `k`.
* An error was occurring when calling the function `mcr_chart` by giving sets of values containing only values equal to 0.
* Unnamed vectors can now be used as argument `thq_col` in the function `mcr_chart`.

## Documentation

* In the vignette about the MCR approach, the THQ color change example now works as described. It uses the given colors instead of using grey for each THQ name.



# uom.dat 0.2.1 (2022-07-02)

## Fixes

* An error was occurring when calling the method `co_occurrence_matrix` (class `TransactionSet`) by giving an object of class `TransactionSet` containing no transactions and a non-empty vector of items. This case now returns a matrix as expected.
* Unexpected errors were occurring when calling the method `co_occurrence_chart` (either from class `TransactionSet` or `TransactionAnalyzer`) by giving an empty or length-one vector of items. These cases now generate appropriate error messages.
* An error was occurring when initializing an object of class `TransactionAnalyzer` by giving no maximum length as mining parameter if the given minimum length parameter was greater than the number of separate items actually appearing in the transactions.
* Objects of class `TransactionAnalyzer` can now be fully initialized if no patterns can be extracted according to the mining parameters.
* An error was occurring when calling the method `frequency_by_complexity` (class `TransactionAnalyzer`) by giving a list containing no patterns. This case now returns a matrix as expected.
* An error was occurring when calling the method `extract_rules` (class `TransactionAnalyzer`) by giving a support mining parameter greater than 1. This case now returns no rules as expected.
* An error was occuring when calling the method `rules_chart` (class `TransactionAnalzyer`) by giving a data frame of rules containing no rows and leaving the default value (`NULL`) for the `items` argument. This case now creates a graph without any edges between vertices and plots all items of the given `TransactionAnalyzer` object.
* Palettes other than `"Blues"` can now be used in the method `rules_chart` (class `TransactionAnalyzer`) when displaying the confidence or the specificity of rules.

## Documentation

* The help page of the `extract_rules` method (class `TransactionAnalyzer`) now specifies the result in case no rules can be extracted.
* The help page of the `rules_chart` method (class `TransactionAnalyzer`) now only uses valid palettes in examples.



# uom.dat 0.2.0 (2022-06-10)

## Major changes

* Column `"count"` of association rule data frames has been renamed `"frequency"` for consistency with nodes and patterns.
* Column `" "` containing only character strings `"=>"` has been removed from association rule data frames.
* Column `"frequency"` of association rule data frames has been moved to the third position.
* Parameter `count` of `TransactionAnalyzer` has been renamed `min_frequency` for overall consistency. This concerns both the object attribute and the constructor argument (function `transaction.analyzer`).
* Palettes that can be used for coloring confidence values in graphs representing association rules are now the same as for the other characteristics of rules. The previous palettes cannot be used anymore.
* The `RColorBrewer` package must now be installed for some features to work.

## New features

* Pattern supports are now computed and added to pattern data frames as another characteristic.
* Association rule frequencies are now also computed and added to rule data frames when specific itemsets are given for association rule extraction.
* Frequencies can now be plotted in the association rule visualization.
* Additional indicators can be computed and associated with association rules when they are extracted: specificity, accuracy and added value.
* The new indicators (specificity, accuracy and added value) can be plotted in the association rule visualization. The added value is the only one to use a different set of palettes.

## Minor changes

* The type of several attributes of the class `TransactionAnalyzer` and of returns of methods of this class changed from `numeric` to `integer`.
    - Attribute `parameters`: variables `count` and `min_length`.
    - Attribute `nodes` and all methods returning a data frame of nodes: variable `frequency`.
    - Attribute `patterns` and all methods returning a data frame of patterns: variable `year`.
    - Method `frequency_by_complexity` and variables `f.complex` and `f.simple` of the method `spectrum_chart`.
    - Variable `ID` of the methods `spectrum_chart` and `itemset_chart`.
* Class validation (for `TransactionSet` and `TransactionAnalyzer` objects) now goes through the whole process and returns all actual errors instead of being stopped if one is encountered.

## Documentation

* The URLs of the package page and where to report bugs have been added to the package help page (accessible using `help(uom.dat)`).
* Corrections have been applied to the PDF files.
    - Documentation of the methods `itemset_chart` and `get_trx_from_category` of the class `TransactionAnalyzer` have been corrected.
    - The returns of methods that were supposed to be of type `integer` instead of `numeric` have been changed.
    - Mentions of the method `frequency_by_node_commplexity` have been replaced by `frequency_by_complexity`.
* The description of the confidence of an association rule has been corrected (in the help page and the vignette).
* In the vignette about `TransactionAnalyzer`, the explanation of a warning generated by the `arules` package has been removed since this warning is no longer displayed.



# uom.dat 0.1.0 (2022-05-06)

Initial stable release version.


---
