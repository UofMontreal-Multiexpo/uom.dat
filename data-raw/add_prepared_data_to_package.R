## Add prepared data to the package

#### Required functions ####

#' Resave data with the best compression method
#' 
#' Search for the best compression method to save existing '.RData' or '.rda' files and resave them
#'  with this method.
#' 
#' @details
#' Use of the maximum compression level (9).
#' 
#' @template function_not_exported
#' 
#' @param paths A character vector of paths to found data and save files.
#' 
#' @author Gauthier Magnin
#' @seealso [`tools::resaveRdaFiles`], [`tools::checkRdaFiles`].
#' @md
#' @keywords internal
resave_with_best_compression = function(paths){
  
  # Vérification de l'existence du package tools (inclus dans DESCRIPTION.Suggests)
  if (!requireNamespace("tools", quietly = TRUE)) {
    stop("Package \"tools\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  
  # Méthodes de compression
  methods = c("gzip", "bzip2", "xz")
  
  # Pour chaque fichier
  for (p in paths) {
    
    # Pour chaque méthode de compression
    sizes = sapply(methods, function(m) {
      # Compresse les données et retourne la taille du fichier
      tools::resaveRdaFiles(p, compress = m, compression_level = 9)
      return(tools::checkRdaFiles(p)$size)
    })
    names(sizes) = methods
    
    # Sélection de la meilleure méthode de compression
    best = methods[which.min(sizes)]
    
    # Affichage des résultats et du choix optimal
    if (length(paths) != 1) cat("File:", p,"\n")
    cat("File sizes according to compression method:\n")
    print(sizes)
    cat("Use of '", best, "' compression method.\n", sep = "")
    if (p != paths[length(paths)]) cat("\n")
    
    # Recompression selon la meilleur méthode
    tools::resaveRdaFiles(p, compress = best, compression_level = 9)
  }
}



#### Save and compress data ####

save(oedb_sample,               file = "./data/oedb_sample.RData")
save(substances_information,    file = "./data/substances_information.RData")
save(TA_instance,               file = "./data/TA_instance.RData")
save(TS_instance,               file = "./data/TS_instance.RData")

resave_with_best_compression(c("./data/oedb_sample.RData",
                               "./data/substances_information.RData",
                               "./data/TS_instance.RData",
                               "./data/TA_instance.RData"))

usethis::use_data(another_ta_object, internal = TRUE) # R/sysdata.rda
# save(another_ta_object,       file = "./data/another_ta_object.RData")
# resave_with_best_compression("./data/another_ta_object.RData")


