# Occupational Exposure Databases analysis tools - Multiexposure


## Purpose

This project is about developing **R processing, analysis and visualization tools** for occupational exposure databases, especially for multiexposure.

The development of these tools is part of a research study led by **Jérôme Lavoué** (Professor at [**University of Montreal**](https://recherche.umontreal.ca/english/home/), department of environmental health and occupational health) whose title is "Portrait of multiexposure situations in the workplace in Quebec from occupational exposure databases".

Main analytical approaches are *Frequent Itemset Mining*, *Association Rules*, *Clustering*, *Maximum Cumulative Ratio* and *Spectrosome*.


## Installation

To install the package **oedb.analysis.tools**:

1. Clone the repository or download a ZIP copy.
2. In RStudio, open the project file `oedb-analysis-tools.Rproj`.
3. Install the required packages by running `install.packages(c("arules", "ggplot2", "ggsci", "graphics", "grDevices", "mathjaxr", "methods", "network", "sna", "stats", "utils"))`.
4. In the "Build" menu, click on the "Install and Restart" item.


## Documentation

In addition to the documentation of the package, data and functions (accessible by the `help` function), the **doc** directory contains:

* An organized list of the datasets, functions, classes and methods, in the file `list_of_help_pages.html`.

* An example of spectral analysis, presented in the file `spectral_analysis_example.nb.html`.

* A description of the Maximum Cumulative Ratio approach providing an example of application, in the file `mcr_approach.html`. This one also presents the use of functions for classification data management.

* A presentation of the data structures used by the **SpectralAnalyzer** class (attributes, function parameters and function returns) in the file `types_of_attributes_and_methods.pdf`.

These files can be accessed using `help(package = "oedb.analysis.tools")` then clicking on "User guides, package vignettes and other documentation".


## Authors

* [**Gauthier Magnin**](https://fr.linkedin.com/in/gauthier-magnin) - R programmer analyst.

* **Delphine Bosson-Rieutort** - Assistant professor at University of Montreal School of Public Health (French: *École de Santé Publique de l'Université de Montréal, ESPUM*).


## Collaboration

* [INRS](http://en.inrs.fr/): The French National Research and Safety Institute for the Prevention of Occupational Accidents and Diseases (French: *Institut National de Recherche et de Sécurité, INRS*).


---