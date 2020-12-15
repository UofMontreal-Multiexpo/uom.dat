#' @include observation_set.R
NULL


#### Generic functions for structuring observations ####

#' Grouping data
#' 
#' Grouping data into a list of lists called observations.
#' 
#' @details
#' Variables of type `factor` are coerced to `character`.
#' 
#' @param data Data frame containing a set of data to be grouped.
#' @param by Variable names included in `colnames(data)` used to group the data.
#'  Each combination of values of these variables generates one observation.
#' @param additional Names of information to keep when creating observations: vector included in
#'  `colnames(data)`.
#' @param unique_values logical or character.
#'  * If `TRUE`, simplification of the values associated with the variables defined in `additional`
#'    in order to remove duplicates.
#'  * If `FALSE`, keep the duplicates and the correspondence between the values of these variables for
#'    the same line in `data` (as many values as there are rows grouped).
#'  * Otherwise, vector of variable names included in `colnames(data)` for which the removal of
#'    duplicates must be performed.
#' @param item_key,year_key If the output have to be an `ObservationSet`, names of the variables of `data`
#'  containing the items of the observations and the year in which the observations were made.
#' @return List of observations identified or S4 object of class `ObservationSet` containing this list
#'  (depending on whether `item_key` is `NA` or not).
#'  
#' Each observation is named by the combination of values of the variables named in `by` that generated
#'  it. Each observation is a list whose elements correspond to the values of the variables named in `by`
#'  and `additional` which have been grouped together.
#' 
#' @author Gauthier Magnin
#' @seealso For the construction of well-targeted observations specific to the field of occupational
#'  exposure in relation to inspections and similar work situations: [`make_OE_observations`].
#' 
#' @examples
#' to_keep <- c("CODE", "NAME", "ACTIVITY", "JOB.TITLE", "SAMPLE.ID")
#' 
#' ## Make observations without duplicate elements in the additional variables
#' obs_1 <- make_observations(oedb_sample, by = "ID", additional = to_keep)
#' 
#' ## Make observations with duplicate elements correspondence
#' obs_2 <- make_observations(oedb_sample, by = "ID", additional = to_keep,
#'                            unique_values = FALSE)
#' obs_3 <- make_observations(oedb_sample, by = "ID", additional = to_keep,
#'                            unique_values = to_keep[1:2])
#' 
#' ## Make observations as ObservationSet
#' obs_object <- make_observations(oedb_sample, by = "ID",
#'                                 additional = c("CODE", "NAME", "YEAR"),
#'                                 item_key = "CODE", year_key = "YEAR")
#' 
#' @md
#' @export
make_observations = function(data, by, additional = NULL, unique_values = TRUE,
                             item_key = NA, year_key = NA) {
  
  # Regroupement des valeurs des variables "additional" selon les combinaisons possibles de celles de "by"
  # Si unique_values vaut TRUE, aggrégation par la fonction unique() ; c() sinon
  obs = stats::aggregate(data[, additional],
                         by = lapply(by, function(x) data[, x]),
                         FUN = ifelse(is.logical(unique_values) && unique_values, unique, c))
  colnames(obs) = c(by, additional)
  
  # Nommage des lignes selon les combinaisons (valeurs des variables "by" concaténées par ".")
  rownames(obs) = if (length(by) == 1) obs[, by] else apply(obs[, by], 1, paste, collapse = ".")
  
  # Suppression des doublons pour les variables spécifiques
  if (!is.logical(unique_values)) {
    for (var in unique_values) {
      # L'attribution d'une liste à une variable d'une data frame ne fonctionne que via $
      # obs$var <- lapply(obs[, var], unique)
      eval(parse(text = paste0("obs$", var, " <- lapply(obs[, var], unique)")))
    }
  }
  
  # Conversion en character les variables de type factor
  factor_cols = which(sapply(obs, class) == "factor")
  for (col in factor_cols) {
    obs[, col] = as.character(obs[, col])
  }
  
  # Conversion en list de character les variables de type list de factor
  list_cols = names(which(sapply(obs, class) == "list"))
  if (length(list_cols > 0)) {
    if (length(list_cols) == 1) {
      factor_cols = if (obs[, list_cols][[1]] == "factor") list_cols else NULL
    } else {
      factor_cols = list_cols[which(sapply(obs[, list_cols], function(x) typeof(x[[1]])) == "factor")]
    }
    for (col in factor_cols) {
      # obs$col <- lapply(obs[, col], as.character)
      eval(parse(text = paste0("obs$", col, " <- lapply(obs[, col], as.character)")))
    }
  }
  
  return(if (is.na(item_key)) as.list(as.data.frame(t(obs), stringsAsFactors = FALSE))
         else observation.set(as.list(as.data.frame(t(obs), stringsAsFactors = FALSE)),
                              item_key = item_key, year_key = year_key))
}



#### Functions for structuring observations, specific to the Occupational Exposure domain ####

#' Grouping samples about Occupational Exposure
#' 
#' Grouping samples into a list of lists (called observations) specifically for the occupational
#'  exposure domain.
#' An observation corresponds to a set of substances found in the same work situation during the same
#'  inspection.
#' Specific situations are defined by the argument `work_situations`.
#' Non-specific situations are described by existing combinations of variables whose names are defnied
#'  by the argument `variable_names`.
#' 
#' @param measures Set of sampling measures. Data frame that must contain at least three variables for:
#'  * An inspection identifier. The measures resulting from the same inspection must have the same
#'    inspection identifier.
#'  * A code identifying the substance being sampled.
#'  * A year of inspection.
#' @param keys Length-three character vector identifying the names of the three required variables
#'  in the following order: keys for accessing inspection identifiers; substance codes; years.
#' @param mode Mode of creation of observations. One of `1`, `2`, `3`.
#'  \describe{
#'    \item{`1`}{Construction of observations from the situations described by `work_situations`
#'     and existing situations from combinations of values of variables defined by `variable_names`
#'     that would not be described in `work_situations`.}
#'    \item{`2`}{Construction of observations only from the situations described by `work_situations`.
#'     Remaining samples are ignored.}
#'    \item{`3`}{Construction of observations only from combinations of values of variables defined
#'     by `variable_names`.}
#'  }
#' @param work_situations Description of work situations. Data frame that must contain the values
#'  associated with several variables from `measures` forming one work situation.
#'  One column must make possible to identify the different associations of values considered to be the
#'  same work situation. This column must be named `WS.ID` (for **W**ork **S**ituation **ID**entifier)
#'  otherwise the first column is considered to contain such identifiers.
#' @param variable_names Names of variables to be taken into account to consider a work situation,
#'  i.e. vector of names of variables contained in `measures` whose different combinations of
#'  values form the different work situations not described in `work_situations`.
#' @param additional Names of information to keep when creating observations (in addition to those given
#'  by `keys`): vector included in `colnames(measures)`.
#' @param unique_values logical or character.
#'  * If `TRUE`, simplification of the values associated with the variables defined in
#'    `additional` in order to remove duplicates.
#'  * If `FALSE`, keep the duplicates and the correspondence between the values of these variables
#'    for the same sample (as many values as there are samples).
#'  * Otherwise, vector of variable names included in `colnames(measures)` for which the removal of
#'    duplicates must be performed.
#' @return S4 object of class `ObservationSet` containing the list of observations identified.
#'  Each observation is a list containing:
#'  * The codes identifying the substances grouped together.
#'  * The year of the inspection from which the samples are grouped.
#'  * Data related to the samples grouped, concerning the variables defined in the argument `additional`.
#' 
#' @author Gauthier Magnin
#' @seealso For a more generic way to group data: [`make_observations`].
#' 
#' @examples
#' to_keep <- c("CODE", "NAME", "ACTIVITY", "JOB.TITLE", "SAMPLE.ID")
#' ws <- data.frame(WS.ID = c(1, 2, 2, 3, 3),
#'                  JOB.TITLE = c(44121004, 44142001, 44132032, 44132019, 44132030),
#'                  JOB.TASK = c("A5440", "A6410", "A5110", "A5260", "A5240"),
#'                  stringsAsFactors = FALSE)
#' ws_vars <- c("JOB.TITLE", "JOB.TASK")
#' 
#' ## Make observations from 3 specific work situations and as many other
#' ## situations as JOB.TITLE, JOB.TASK pairs
#' obs_1 <- make_OE_observations(oedb_sample, keys = c("ID", "CODE", "YEAR"),
#'                               mode = 1,
#'                               work_situations = ws,
#'                               variable_names = ws_vars,
#'                               additional = to_keep)
#' 
#' ## Make observations from specific work situations only
#' obs_2 <- make_OE_observations(oedb_sample, keys = c("ID", "CODE", "YEAR"),
#'                               mode = 2,
#'                               work_situations = ws,
#'                               additional = to_keep)
#' 
#' ## Make observations where each pair of JOB.TITLE, JOB.TASK is one situation
#' obs_3 <- make_OE_observations(oedb_sample, keys = c("ID", "CODE", "YEAR"),
#'                               mode = 3,
#'                               variable_names = ws_vars,
#'                               additional = to_keep)
#' 
#' @md
#' @export
make_OE_observations = function(measures, keys, mode,
                                work_situations = NULL, variable_names = NULL,
                                additional = NULL, unique_values = TRUE) {
  
  if (length(keys) != 3) stop("keys must be a length-three vector.")
  if (mode > 3 || mode < 1) stop("mode must be 1, 2 or 3.")
  if ((mode == 1 || mode == 2) & is.null(work_situations))
    stop("work_situations must be defined for this mode.")
  if ((mode == 1 || mode == 3) & is.null(variable_names))
    stop("variable_names must be defined for this mode.")
  
  # Liste qui contiendra les observations construites
  observations = list()
  # Identifiants des interventions associés aux prélèvements
  id_interv = unique(measures[, keys[1]])
  
  # Spécification d'ajout des identifiants dans les observations
  additional = c(keys[1], additional)
  if (is.logical(unique_values) && !unique_values) unique_values = keys[1]
  else if (!is.logical(unique_values)) unique_values = c(keys[1], unique_values)
  
  # Pour chaque intervention
  for (id_i in id_interv) {
    # Extraction des prélèvements associés à l'intervention en cours de traitement
    intervention = subset(measures, measures[, keys[1]] == id_i)
    
    if (mode == 1 || mode == 2) {
      # Regroupement des prélèvements correspondant à des situations de travail décrites
      result_mofws = make_obs_from_ws(intervention, keys[-1], work_situations,
                                      additional, unique_values)
      observations = c(observations, result_mofws$observations)
    }
    
    if (mode == 1) {
      # Construction d'observations pour les prélèvements restants
      if (!all(result_mofws$processed)) {
        intervention = subset(intervention, !result_mofws$processed)
        observations = c(observations,
                         make_obs_from_unspecified_ws(intervention, keys[-1], variable_names,
                                                      additional, unique_values))
      }
    } else if (mode == 3) {
      # Regroupement des prélèvements pour chaque combinaisons des variables décrites
      observations = c(observations,
                       make_obs_from_unspecified_ws(intervention, keys[-1], variable_names,
                                                    additional, unique_values))
    }
  }
  
  return(observation.set(data = observations, item_key = keys[2], year_key = keys[3]))
}


#' Grouping samples according to specific work situations
#' 
#' Grouping samples in the form of observations: make observations from work situations.
#' An observation corresponds to a set of substances found in the same work situation.
#' 
#' @inheritParams make_OE_observations
#' @param measures Set of sampling measures. Data frame that must contain at least two variables for:
#'  * A code identifying the substance being sampled.
#'  * A year of inspection.
#' @param keys Length-two character vector identifying the names of the two required variables
#'  in the following order: keys for accessing substance codes; key for accessing years.
#' @return
#'  \describe{
#'    \item{`observations`}{List of observations identified. Each observation is a list containing:
#'      \itemize{
#'        \item{The codes identifying the substances grouped together.}
#'        \item{The year of the inspection from which the samples are grouped.}
#'        \item{Data related to the samples grouped, concerning the variables defined in the argument
#'              `additional`.}
#'      }
#'    }
#'    \item{`processed`}{Vector specifying for each line of `measures` whether it has been included in
#'                       an observation.}
#'  }
#' 
#' @author Gauthier Magnin
#' @seealso [`make_OE_observations`], [`make_obs_from_unspecified_ws`].
#' @md
#' @keywords internal
make_obs_from_ws = function(measures, keys, work_situations,
                            additional = NULL, unique_values = TRUE) {
  
  # Vecteur spécifiant pour chaque prélèvement s'il a été traité ou non
  processed = rep_len(FALSE, nrow(measures))
  
  # Noms des varibles servant à la définition d'une situation de travail
  ID_index = which(colnames(work_situations) == "WS.ID")
  if (length(ID_index) == 0) ID_index = 1
  variable_names = colnames(work_situations)[-ID_index]
  
  # Noms des variables à regrouper avec unique() et des variables à regrouper avec c()
  if (is.logical(unique_values)){
    var_unique = if (unique_values) additional else NULL
  } else {
    var_unique = unique_values
  }
  var_c = setdiff(additional, var_unique)
  
  # Liste qui contiendra les observations construites
  observations = list()
  n_obs = 0
  
  # Pour chaque situation de travail
  for (id_s in unique(work_situations[, ID_index])) {
    situation = subset(work_situations, work_situations[, ID_index] == id_s)
    
    codes = c()
    years = c()
    informations = rep(list(NULL), length(additional))
    names(informations) = additional
    
    # Pour chaque ensemble de valeurs définissant une situation de travail
    for (s in seq_len(nrow(situation))) {
      variable_values = situation[s, -ID_index]
      
      # Sélection des prélèvements correspondant
      to_select = apply(measures, 1,
                        function(measure) {
                          all(measure[variable_names] == variable_values)
                        })
      sub = subset(measures, to_select)
      
      if (nrow(sub) != 0) {
        # Combinaison des substances aux précédentes
        codes = unique(c(codes, sub[, keys[1]]))
        years = unique(c(years, sub[, keys[2]]))
        
        # Gestion des données supplémentaires
        for (var in var_unique) informations[[var]] = unique(c(informations[[var]], sub[, var]))
        for (var in var_c) informations[[var]] = c(informations[[var]], sub[, var])
        
        # Confirmation de correspondance entre ces prélèvements et les situations de travail décrites
        processed[to_select] = TRUE
      }
    }
    
    if(length(codes) != 0) {
      # Une observation supplémentaire
      n_obs = n_obs + 1
      observations[[n_obs]] = c(list(codes, years), informations)
      names(observations[[n_obs]])[c(1,2)] = keys
    }
  }
  
  return(list(observations = observations, processed = processed))
}


#' Grouping samples according to non-specific work situations
#' 
#' Group samples in the form of observations: make observations from unspecified work situations.
#' An observation corresponds to a set of substances found in the same work situation.
#' These situations are described by the possible combinations of values of the variables whose names
#'  are defined in arguments.
#' 
#' @inheritParams make_obs_from_ws
#' @param variable_names Names of variables to be taken into account to consider a work situation,
#'  ie. vector of names of variables contained in `measures` whose different combinations of values
#'  form the different work situations.
#' @return List of observations identified. Each observation is a list containing:
#'  * The codes identifying the substances grouped together.
#'  * The year of the inspection from which the samples are grouped.
#'  * Data related to the samples grouped, concerning the variables defined in the argument `additional`.
#' 
#' @author Gauthier Magnin
#' @seealso [`make_OE_observations`], [`make_obs_from_ws`].
#' @md
#' @keywords internal
make_obs_from_unspecified_ws = function(measures, keys, variable_names,
                                        additional = NULL, unique_values = TRUE) {
  
  # Ensemble des situations de travail effectives
  situations = unique(measures[, variable_names])
  
  # Ajout d'un identifiant quelconque à chacune
  if (length(variable_names) == 1) {
    ws = data.frame(WS.ID = seq_along(situations))
    ws[, variable_names] = situations
  } else {
    ws = cbind(WS.ID = seq_len(nrow(situations)), situations)
  }
  
  # Observations associées aux situations de travail
  return(make_obs_from_ws(measures, keys, ws, additional, unique_values)$observations)
}


