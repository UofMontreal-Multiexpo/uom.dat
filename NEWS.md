# uom.dat 0.1.0.9000 (development version)

## New features

* Pattern supports are now computed and added to pattern data frames as another characteristic.

## Minor changes

* Corrections have been applied to the documentation in PDF files.
    - Documentation of the methods `itemset_chart` and `get_trx_from_category` of the class `TransactionAnalyzer` have been corrected.
    - The returns of methods that were supposed to be of type `integer` instead of `numeric` have been changed.
* The type of several attributes of the class `TransactionAnalyzer` and of returns of methods of this class changed from `numeric` to `integer`.
    - Attribute `parameters`: variables `count` and `min_length`.
    - Attribute `nodes` and all methods returning a data frame of nodes: variable `frequency`.
    - Attribute `patterns` and all methods returning a data frame of patterns: variable `year`.
    - Method `frequency_by_complexity` and variables `f.complex` and `f.simple` of the method `spectrum_chart`.
    - Variable `ID` of the methods `spectrum_chart` and `itemset_chart`.
    



# uom.dat 0.1.0 (2022-05-06)

Initial stable release version.


---
