## Add prepared data to the package.
## 
## Output files: - oedb_sample.RData
##               - substances_information.RData
##               - TS_instance.RData
##               - TA_instance.RData
##               - sysdata.rda

#### Required function ####

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
  
  # Checking the existence of the package tools (included in field "Suggests" of the DESCRIPTION file)
  if (!requireNamespace("tools", quietly = TRUE)) {
    stop("Package \"tools\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  # Compression methods
  methods = c("gzip", "bzip2", "xz")
  
  # For each file
  for (p in paths) {
    
    # For each compression method, compress the data and get the file size
    sizes = sapply(methods, function(m) {
      tools::resaveRdaFiles(p, compress = m, compression_level = 9)
      return(tools::checkRdaFiles(p)$size)
    })
    names(sizes) = methods
    
    # Selecting the best compression method
    best = methods[which.min(sizes)]
    
    # Display of results and optimal choice
    if (length(paths) != 1) cat("File:", p,"\n")
    cat("File sizes according to compression method:\n")
    print(sizes)
    cat("Use of '", best, "' compression method.\n", sep = "")
    if (p != paths[length(paths)]) cat("\n")
    
    # Compress again using the best method
    tools::resaveRdaFiles(p, compress = best, compression_level = 9)
  }
}



#### Save and compress data ####

# The following instruction could have been used for exported datesets,
# but the compression is not optimized.
# usethis::use_data(dataset_name, overwrite = TRUE)

save(oedb_sample,               file = "./data/oedb_sample.RData")
save(substances_information,    file = "./data/substances_information.RData")
save(TS_instance,               file = "./data/TS_instance.RData")
save(TA_instance,               file = "./data/TA_instance.RData")

resave_with_best_compression(c("./data/oedb_sample.RData",
                               "./data/substances_information.RData",
                               "./data/TS_instance.RData",
                               "./data/TA_instance.RData"))

usethis::use_data(another_ta_object, internal = TRUE) # R/sysdata.rda


