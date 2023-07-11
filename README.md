# University of Montreal - Data Analysis Tools


## Purpose

The development of these tools is part of a research study led by **Jérôme Lavoué** (Professor at [University of Montreal](https://recherche.umontreal.ca/english/home/), department of environmental health and occupational health) whose title is "Portrait of multiexposure situations in the workplace in Quebec from occupational exposure databases".

This package is about **R processing, analysis and visualization tools** initially designed for processing data from occupational exposure databases, especially for the concept of multiexposure.

Main analytical approaches are **Frequent Itemset Mining**, **Spectrosome**, **Association Rules** and **Maximum Cumulative Ratio**.


## Installation

> **Warning**
> 
> Several versions of the package `ggraph` experience some issues about hierarchical edge bundling charts. Such charts are created in `uom.dat` by the functions `co_occurrence_chart` and `rules_chart`. The correct operation of these functions has been validated with the version 2.0.5 of `ggraph`. Therefore, we recommend the use of this version. To install it, run the following instruction.
> ```r
> remotes::install_version("ggraph", version = "2.0.5")
> ```

The function `install_github` from the package `remotes` can be used to install this package. However, as this repository is private, you need a personal access token for this to work.

A personal access token provides access to the GitHub API. To get one if you don't already have one:

* Go to <https://github.com/settings/tokens>.
* Click on button "Generate new token".
* Fill the "Note" field with something like "Token for private R packages".
* Check the box "repo" (full control of private repositories).
* Click on button "Generate token".
* Copy the given token.

To install the latest version, run the following instruction using your token as the `auth_token` argument.
```r
remotes::install_github("UofMontreal-Multiexpo/uom.dat",
                        auth_token = "my_personal_access_token")
```

To install the development version, use:
```r
remotes::install_github("UofMontreal-Multiexpo/uom.dat",
                        auth_token = "my_personal_access_token",
                        ref = "develop")
```

To install a previous version, run the following instruction, replacing `X.X.X` with the desired version number.
```r
remotes::install_github("UofMontreal-Multiexpo/uom.dat",
                        auth_token = "my_personal_access_token",
                        ref = "vX.X.X")
```


## Documentation

In addition to the manuals of the package, data, classes and functions (accessible by the `help` function), the **inst/doc** directory contains:

* An organized list of the datasets, functions, classes and methods, in the file `list_of_help_pages.html`.

* Explanations with illustrated examples about different types of itemsets, in the file `itemset_mining.html`.

* A description of the class `TransactionSet`, the dataset `TS_instance`, and what to do with transactions, in the file `transaction_sets.html`.

* A description of the class `TransactionAnalyzer`, the dataset `TA_instance`, and what to do with transaction analyzers, in the file `transaction_analyzes.html`.

* A description of the Maximum Cumulative Ratio approach by providing an example of application, in the file `mcr_approach.html`. This one also presents the use of functions for classification data management.

* The detail of the data structures used by the `TransactionSet` class (attributes, method parameters and method returns) in the file `types_of_attributes_and_methods_TS.pdf`. 

* The detail of the data structures used by the `TransactionAnalyzer` class (attributes, method parameters and method returns) in the file `types_of_attributes_and_methods_TA.pdf`.

These files can be accessed using `help(package = "uom.dat")` then clicking on "User guides, package vignettes and other documentation".


## Authors

* [Gauthier Magnin](https://fr.linkedin.com/in/gauthier-magnin) - R programmer analyst.
* [Delphine Bosson-Rieutort](https://espum.umontreal.ca/lespum/departement-de-gestion-devaluation-et-de-politique-de-sante/lequipe-du-departement/personnel-enseignant/professeur/in/in30464/sg/Delphine%20Bosson-Rieutort/) - Assistant professor at University of Montreal School of Public Health (French: *École de Santé Publique de l'Université de Montréal, ESPUM*).


## Collaboration

* [INRS](http://en.inrs.fr/): The French National Research and Safety Institute for the Prevention of Occupational Accidents and Diseases (French: *Institut National de Recherche et de Sécurité pour la prévention des accidents du travail et des maladies professionnelles, INRS*).


---
