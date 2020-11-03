# Occupational Exposure Databases analysis tools - Multiexposure


## Purpose

This project is about developing **R processing, analysis and visualization tools** for occupational exposure databases, especially for multiexposure.

The development of these tools is part of a research study led by **Jérôme Lavoué** (Professor at [**University of Montreal**](https://recherche.umontreal.ca/english/home/), department of environmental health and occupational health) whose title is "Portrait of multiexposure situations in the workplace in Quebec from occupational exposure databases".

Main analytical approaches are *Frequent Itemset Mining*, *Association Rules*, *Clustering*, *Maximum Cumulative Ratio* and *Spectrosome*.


## Installation

There are two ways to install the package **oedb.analysis.tools**.

### Installing it with the devtools package

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
devtools::install_github("UofMontreal-Multiexpo/oedb-analysis-tools",
                         auth_token = "my_personal_access_token")
```

### Getting all files then installing

1. Clone the repository or download a ZIP copy.
2. In RStudio, open the project file `oedb-analysis-tools.Rproj`.
3. Install the required packages by running:
   ```r
   install.packages(c("arules", "ggplot2", "ggraph", "ggsci", "graphics",
                      "grDevices", "igraph", "mathjaxr", "methods", "network",
                      "sna", "stats", "utils"))
   ```
4. In the "Build" menu, click on the "Install and Restart" menu item.

You can now close the project **oedb.analysis.tools** and work on your own one using our package.


## Documentation

In addition to the manuals of the package, data and functions (accessible by the `help` function), the **doc** directory contains:

* An organized list of the datasets, functions, classes and methods, in the file `list_of_help_pages.html`.

* An example of spectral analysis, presented in the file `spectral_analysis_example.nb.html`.

* A description of the Maximum Cumulative Ratio approach providing an example of application, in the file `mcr_approach.html`. This one also presents the use of functions for classification data management.

* An explanation with illustrated examples about the different types of itemsets, in the file `frequent_itemsets.html`.

* A presentation of the data structures used by the **SpectralAnalyzer** class (attributes, function parameters and function returns) in the file `types_of_attributes_and_methods.pdf`.

These files can be accessed using `help(package = "oedb.analysis.tools")` then clicking on "User guides, package vignettes and other documentation".


## Authors

* [**Gauthier Magnin**](https://fr.linkedin.com/in/gauthier-magnin) - R programmer analyst.

* **Delphine Bosson-Rieutort** - Assistant professor at University of Montreal School of Public Health (French: *École de Santé Publique de l'Université de Montréal, ESPUM*).


## Collaboration

* [INRS](http://en.inrs.fr/): The French National Research and Safety Institute for the Prevention of Occupational Accidents and Diseases (French: *Institut National de Recherche et de Sécurité, INRS*).


---