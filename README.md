# University of Montreal Data Analysis Tools


## Purpose

The development of these tools is part of a research study led by **Jérôme Lavoué** (Professor at [**University of Montreal**](https://recherche.umontreal.ca/english/home/), department of environmental health and occupational health) whose title is "Portrait of multiexposure situations in the workplace in Quebec from occupational exposure databases".

This package is about **R processing, analysis and visualization tools** initially designed for processing data from occupational exposure databases, especially for the concept of multiexposure.

Main analytical approaches are **Frequent Itemset Mining**, **Spectrosome**, **Association Rules**, **Clustering** and **Maximum Cumulative Ratio**.


## Installation

There are two ways to install the package **uom.dat**.

### A. Installing with the devtools package

The function `install_github` from the `devtools` package can be used to install the present one. However, as this repository is private, you need a personal access token for this function to work.

A personal access token provides access to the GitHub API. To have one if you don't already have one:

* Go to <https://github.com/settings/tokens>.
* Click on button "Generate new token".
* Fill the "Note" field with something like "Token for private R packages".
* Check the box "repo" (Full control of private repositories).
* Click on button "Generate token".
* Copy the given token.

Then you must run the following instruction using your token as the `auth_token` argument:
```r
devtools::install_github("UofMontreal-Multiexpo/uom.dat",
                         auth_token = "my_personal_access_token")
```

### B. Getting all files then installing

1. Clone the repository or download a ZIP copy.
2. In RStudio, open the project file `uom.dat.Rproj`.
3. Install the required packages by running:
   ```r
   install.packages(c("arules", "ggplot2", "ggraph", "ggsci", "graphics",
                      "grDevices", "igraph", "mathjaxr", "methods", "network",
                      "sna", "stats", "utils"))
   ```
4. In the "Build" menu, click on the "Install and Restart" menu item.

You can now close the project **uom.dat** and work on your own one using our package.


## Documentation

In addition to the manuals of the package, data, classes and functions (accessible by the `help` function), the **doc** directory contains:

* An organized list of the datasets, functions, classes and methods, in the file `list_of_help_pages.html`.

* A description of the class `TransactionSet`, the dataset `TS_instance`, and what to do with transactions, in the file `transaction_sets.html`.

* An example of transaction analysis, presented in the file `spectral_analysis_example.nb.html`.

* A description of the Maximum Cumulative Ratio approach by providing an example of application, in the file `mcr_approach.html`. This one also presents the use of functions for classification data management.

* Explanations with illustrated examples about the different types of itemsets, in the file `itemset_mining.html`.

* The detail of the data structures used by the **TransactionSet** class (attributes, method parameters and method returns) in the file `types_of_attributes_and_methods_TS.pdf`. 

* The detail of the data structures used by the **TransactionAnalyzer** class (attributes, method parameters and method returns) in the file `types_of_attributes_and_methods_SA.pdf`.

These files can be accessed using `help(package = "uom.dat")` then clicking on "User guides, package vignettes and other documentation".


## Authors

* [**Gauthier Magnin**](https://fr.linkedin.com/in/gauthier-magnin) - R programmer analyst.

* **Delphine Bosson-Rieutort** - Assistant professor at University of Montreal School of Public Health (French: *École de Santé Publique de l'Université de Montréal, ESPUM*).


## Collaboration

* [INRS](http://en.inrs.fr/): The French National Research and Safety Institute for the Prevention of Occupational Accidents and Diseases (French: *Institut National de Recherche et de Sécurité, INRS*).


---