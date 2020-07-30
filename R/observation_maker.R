
#### Fonctions de structuration d'observations génériques ####

#' Grouping data
#' 
#' Grouping data in the form of observations.
#' 
#' @details
#' Variables of type \code{factor} are cast to \code{character}.
#' 
#' @param data Data frame containing a set of data to group together.
#' @param by Variable names included in \code{colnames(data)} used to group the data.
#'  Each combination of values of these different variables generates one observation.
#' @param additional Names of information to keep when creating observations: vector included in
#'  \code{colnames(data)}.
#' @param unique_values logical or character.
#'  \itemize{
#'    \item{If \code{TRUE}, simplification of the values associated with the variables defined in
#'          \code{additional} in order to remove duplicates.}
#'    \item{If \code{FALSE}, keep the duplicates and the correspondence between the values of
#'          these variables for the same line in \code{data} (as many values as there are rows
#'          grouped).}
#'    \item{Otherwise, vector of variable names included in \code{colnames(data)} for which the
#'          removal of duplicates must be performed.}
#'  }
#' @return List of observations identified. Each observation is named by the combination of values
#'  of the variables named in \code{by} that generated it. Each observation is a list whose elements
#'  correspond to the values of the variables named in \code{by} and \code{additional} which have
#'  been grouped together.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{make_INRS_observations}} for the construction of well-targeted observations
#'  specific to the field of occupational exposure in relation to inspections and similar work
#'  situations.
#' @export
make_observations = function(data, by, additional = NULL, unique_values = TRUE) {
  
  # Regroupement des valeurs des variables "additional" selon les combinaisons possibles de celles de "by"
  # Si unique_values vaut TRUE, aggrégation par la fonction unique() ; c() sinon
  obs = aggregate(data[, additional],
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
  if (length(list_cols) == 1) {
    factor_cols = list_cols
  } else {
    factor_cols = list_cols[which(sapply(obs[, list_cols], function(x) class(x[[1]])) == "factor")]
  }
  for (col in factor_cols) {
    # obs$col <- lapply(obs[, col], as.character)
    eval(parse(text = paste0("obs$", col, " <- lapply(obs[, col], as.character)")))
  }
  
  return(as.list(as.data.frame(t(obs))))
}



#### Fonctions de structuration d'observations spécifiques au domaine Occupational Exposure ####

#' Grouping samples
#' 
#' Grouping samples in the form of observations.
#' An observation corresponds to a set of substances found in the same work situation during the same
#'  inspection.
#' Specific situations are defined by the argument \code{work_situations}.
#' Non-specific situations are described by existing combinations of variables whose names are defnied
#'  by the argument \code{variable_names}.
#' 
#' @param measures Set of sampling measures. Data frame that must contain at least the following three
#'  variables:
#'  \describe{
#'    \item{\code{ID}}{Inspection identifier. The measures resulting from the same inspection must
#'          have the same inspection identifier.}
#'    \item{\code{YEAR}}{Year of inspection.}
#'    \item{\code{CODE}}{Code identifying the substance being sampled.}
#'  }
#' @param mode Mode of creation of observations. One of \code{1}, \code{2}, \code{3}.
#'  \describe{
#'    \item{\code{1}}{Construction of observations from the situations described by \code{work_situations}
#'     and existing situations from combinations of values of variables defined by \code{variable_names}
#'     that would not be described in \code{work_situations}.}
#'    \item{\code{2}}{Construction of observations only from the situations described by
#'     \code{work_situations}. Remaining samples are ignored.}
#'    \item{\code{3}}{Construction of observations only from combinations of values of variables
#'     defined by \code{variable_names}.}
#'  }
#' @param work_situations Description of work situations. Data frame that must contain the values
#'  associated with certain variables from \code{measures} forming one work situation.
#'  A \code{WS_ID} (\strong{W}ork \strong{S}ituation \strong{ID}entifier) column must make possible
#'  to identify the different associations of values considered to be the same work situation.
#' @param variable_names Names of variables to be taken into account to consider a work situation,
#'  ie. vector of names of variables contained in \code{measures} whose different combinations of
#'  values form the different work situations not described in \code{work_situations}.
#' @param additional Names of information to keep when creating observations: vector included in
#'  \code{colnames(measures)}.
#' @param unique_values logical or character.
#'  \itemize{
#'    \item{If \code{TRUE}, simplification of the values associated with the variables defined in
#'          \code{additional} in order to remove duplicates.}
#'    \item{If \code{FALSE}, keep the duplicates and the correspondence between the values of
#'          these variables for the same sample (as many values as there are samples).}
#'    \item{Otherwise, vector of variable names included in \code{colnames(measures)} for which the
#'          removal of duplicates must be performed.}
#'  }
#' @return List of observations identified. Each observation is a list containing:
#'  \describe{
#'    \item{\code{CODE}}{The codes identifying the substances grouped together.}
#'    \item{\code{YEAR}}{The year of the inspection from which the samples are grouped.}
#'    \item{\code{...}}{Data related to the samples grouped, concerning the variables defined in
#'                      the argument \code{additional}.}
#'  }
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{make_observations}}.
#' @export
make_INRS_observations = function(measures, mode, work_situations = NULL, variable_names = NULL, additional = NULL, unique_values = TRUE) {
  
  if (mode > 3 || mode < 1) stop("mode must be 1, 2 or 3.")
  if ((mode == 1 || mode == 2) & is.null(work_situations)) stop("work_situations must be defined for this mode.")
  if ((mode == 1 || mode == 3) & is.null(variable_names)) stop("variable_names must be defined for this mode.")
  
  # Liste qui contiendra les observations construites
  observations = list()
  # Identifiants des interventions associés aux prélèvements
  id_interv = unique(measures$ID)
  
  # Spécification d'ajout des identifiants dans les observations
  additional = c("ID", additional)
  if (is.logical(unique_values) && !unique_values) unique_values = "ID"
  else if (!is.logical(unique_values)) unique_values = c("ID", unique_values)
  
  # Pour chaque intervention
  for (id_i in id_interv) {
    # Extraction des prélèvements associés à l'intervention en cours de traitement
    intervention = subset(measures, ID == id_i)
    
    if (mode == 1 || mode == 2) {
      # Regroupement des prélèvements correspondant à des situations de travail décrites
      result_mofws = make_obs_from_ws(intervention, work_situations, additional, unique_values)
      observations = c(observations, result_mofws$observations)
    }
    
    if (mode == 1) {
      # Construction d'observations pour les prélèvements restants
      if (!all(result_mofws$processed)) {
        intervention = subset(intervention, !result_mofws$processed)
        observations = c(observations, make_obs_from_unspecified_ws(intervention, variable_names, additional, unique_values))
      }
    } else if (mode == 3) {
      # Regroupement des prélèvements pour chaque combinaisons des variables décrites
      observations = c(observations, make_obs_from_unspecified_ws(intervention, variable_names, additional, unique_values))
    }
  }
  
  return(observations)
}


#' Grouping samples according to specific work situations
#' 
#' Grouping samples in the form of observations: make observations from work situations.
#' An observation corresponds to a set of substances found in the same work situation.
#' 
#' @param measures Set of sampling measures. Data frame that must contain at least the following two
#'  variables:
#'  \describe{
#'    \item{\code{YEAR}}{Year of inspection.}
#'    \item{\code{CODE}}{Code identifying the substance being sampled.}
#'  }
#' @param work_situations Description of work situations. Data frame that must contain the values
#'  associated with certain variables from \code{measures} forming one work situation.
#'  A \code{WS_ID} (\strong{W}ork \strong{S}ituation \strong{ID}entifier) column must make possible
#'  to identify the different associations of values considered to be the same work situation.
#' @param additional Names of information to keep when creating observations: vector included in
#'  \code{colnames(measures)}.
#' @param unique_values logical or character.
#'  \itemize{
#'    \item{If \code{TRUE}, simplification of the values associated with the variables defined in
#'          \code{additional} in order to remove duplicates.}
#'    \item{If \code{FALSE}, keep the duplicates and the correspondence between the values of
#'          these variables for the same sample (as many values as there are samples).}
#'    \item{Otherwise, vector of variable names included in \code{colnames(measures)} for which the
#'          removal of duplicates must be performed.}
#'  }
#' @return
#'  \describe{
#'    \item{\code{observations}}{List of observations identified.
#'      Each observation is a list containing:
#'      \describe{
#'        \item{\code{CODE}}{The codes identifying the substances grouped together.}
#'        \item{\code{YEAR}}{The year of the inspection from which the samples are grouped.}
#'        \item{\code{...}}{Data related to the samples grouped, concerning the variables defined in
#'                          the argument \code{additional}.}
#'      }
#'    }
#'    \item{\code{processed}}{Vector specifying for each line of \code{measures} whether it has been
#'                            included in an observation.}
#'  }
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{make_observations}}, \code{\link{make_obs_from_unspecified_ws}}.
#' @keywords internal
make_obs_from_ws = function(measures, work_situations, additional = NULL, unique_values = TRUE) {
  
  # Vecteur spécifiant pour chaque prélèvement s'il a été traité ou non
  processed = rep_len(FALSE, nrow(measures))
  
  # Noms des varibles servant à la définition d'une situation de travail
  ID_index = which(colnames(work_situations) == "WS_ID")
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
  for (id_s in unique(work_situations$WS_ID)) {
    situation = subset(work_situations, WS_ID == id_s)
    
    CODE = c()
    YEAR = c()
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
        CODE = unique(c(CODE, sub$CODE))
        YEAR = unique(c(YEAR, sub$YEAR))
        
        # Gestion des données supplémentaires
        for (var in var_unique) informations[[var]] = unique(c(informations[[var]], sub[, var]))
        for (var in var_c) informations[[var]] = c(informations[[var]], sub[, var])
        
        # Définition de ces prélèvements comme ayant une correspondance avec les situations de travail décrites
        processed[to_select] = TRUE
      }
    }
    
    if(length(CODE) != 0) {
      # Une observation supplémentaire
      n_obs = n_obs + 1
      observations[[n_obs]] = c(list(CODE = CODE, YEAR = YEAR),
                                informations)
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
#' @param measures Set of sampling measures. Data frame that must contain at least the following
#'  two variables:
#'  \describe{
#'    \item{\code{YEAR}}{Year of inspection.}
#'    \item{\code{CODE}}{Code identifying the substance being sampled.}
#'  }
#' @param variable_names Names of variables to be taken into account to consider a work situation,
#'  ie. vector of names of variables contained in \code{measures} whose different combinations of
#'  values form the different work situations.
#' @param additional Names of information to keep when creating observations: vector included in
#'  \code{colnames(measures)}.
#' @param unique_values logical or character.
#'  \itemize{
#'    \item{If \code{TRUE}, simplification of the values associated with the variables defined in
#'          \code{additional} in order to remove duplicates.}
#'    \item{If \code{FALSE}, keep the duplicates and the correspondence between the values of
#'          these variables for the same sample (as many values as there are samples).}
#'    \item{Otherwise, vector of variable names included in \code{colnames(measures)} for which the
#'          removal of duplicates must be performed.}
#'  }
#' @return List of observations identified. Each observation is a list containing:
#'    \describe{
#'      \item{\code{CODE}}{The codes identifying the substances grouped together.}
#'      \item{\code{YEAR}}{The year of the inspection from which the samples are grouped.}
#'      \item{\code{...}}{Data related to the samples grouped, concerning the variables defined in
#'                        the argument \code{additional}.}
#'    }
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{make_observations}}, \code{\link{make_obs_from_ws}}.
#' @keywords internal
make_obs_from_unspecified_ws = function(measures, variable_names, additional = NULL, unique_values = TRUE) {
  
  # Ensemble des situations de travail effectives
  situations = unique(measures[, variable_names])
  # Ajout d'un identifiant quelconque à chacune
  if (length(variable_names) == 1) {
    ws = data.frame(WS_ID = seq_along(situations))
    ws[, variable_names] = situations
  } else {
    ws = cbind(WS_ID = seq_len(nrow(situations)), situations)
  }
  
  # Observations associées aux situations de travail
  return(make_obs_from_ws(measures, ws, additional, unique_values)$observations)
}



#### Fonctions de recherche dans une structure d'observations ####

#' Search all items
#' 
#' Extract all the items contained in the observations.
#' 
#' @param observations List of observations on which to do the search.
#' @param key Access key to the items in an observation.
#' @return Vector of all unique items.
#' 
#' @author Gauthier Magnin
#' @export
get_all_items = function(observations, key = "CODE") {
  
  if (!(key %in% names(observations[[1]]))) stop("key must be an existing key in each observation.")
  
  return(sort(unique(unlist(sapply(observations, "[", key)))))
}


#' Search for observations by item
#' 
#' Extract the observations containing one or more sought items.
#' 
#' @param observations List of observations on which to do the search.
#' @param items Sought items.
#' @param target Condition for an observation to be extracted. One of \code{"all"}, \code{"any"}.
#'  \describe{
#'    \item{\code{"all"}}{All the sought items must be part of an observation for this
#'                        observation to be extracted.}
#'    \item{\code{"any"}}{At least one of the sought items must be part of an observation for
#'                        this observation to be extracted.}
#'  }
#' @param key Access key to the items in an observation.
#' @return Subset of the list of observations that match the search criteria.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{get_obs_from_info}}.
#' @export
get_obs_from_items = function(observations, items, target = "all", key = "CODE") {
  
  if (!(target %in% c("all", "any"))) stop("target must be \"all\" or \"any\".")
  if (!(key %in% names(observations[[1]]))) stop("key must be an existing key in each observation.")
  
  func = if (target == "all") all else any
  
  index = which(sapply(lapply(observations, "[[", key), function (x) func(items %in% x)))
  extraction = observations[index]
  
  return(setNames(extraction, index))
}


#' Search for observations by specific information
#' 
#' Extract the observations that match to one or more search criteria.
#' 
#' @param observations List of observations on which to do the search.
#' @param ... arguments of type \code{key = value} where \code{key} refers to the name of one
#'  variable contained in the observations and \code{value} corresponds to the sought value for
#'  this variable.
#' @param target Condition for an observation to be extracted. One of \code{"all"}, \code{"any"}.
#'  \describe{
#'   \item{\code{"all"}}{All the sought information must be part of an observation for this
#'                       observation to be extracted.}
#'   \item{\code{"any"}}{At least one of the sought information must be part of an observation
#'                       for this observation to be extracted.}
#'  }
#' @return Subset of the list of observations that match the search criteria.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{get_obs_from_items}}.
#' @export
get_obs_from_info = function(observations, ..., target = "all") {
  
  if (!(target %in% c("all", "any"))) stop("target must be \"all\" or \"any\".")
  func = if (target == "all") all else any
  
  args = list(...)
  if (length(args) == 0) return(observations)
  
  # Vérification de la correspondance de chaque argument dans chaque observation
  correspondence = sapply(args,
                          function(arg) {
                            lapply(sapply(observations, "[[", names(args)[parent.frame()$i[]]),
                                   function(o) arg %in% o)
                          })
  
  # Indices des observations correspondant aux critères
  if (length(args) == 1 && length(args[[1]]) == 1) {
    index = unlist(correspondence)
  } else {
    # Unlist indépendant pour chaque argument, nécessaire car certaines observations regroupent
    # plusieurs valeurs pour une même variable
    index = apply(t(apply(correspondence, 1, unlist)), 1, func)
  }
  
  return(setNames(observations[index], which(index)))
}


#' Search for items by specific information
#' 
#' Retrieve the items associated with observations corresponding to one or more search criteria.
#' 
#' @param observations List of observations on which to do the search.
#' @param ... arguments of type \code{key = value} where \code{key} refers to the name of one
#'  variable contained in the observations and \code{value} corresponds to the sought value for
#'  this variable.
#' @param target Condition for an item to be extracted from an observation.
#'  One of \code{"all"}, \code{"any"}.
#'  \describe{
#'   \item{\code{"all"}}{All the sought information must be part of an observation for its items to
#'                       be extracted.}
#'   \item{\code{"any"}}{At least one of the sought information must be part of an observation for
#'                       its items to be extracted.}
#'  }
#' @param additional Names of additional information to extract during the search.
#' @param key Access key to the items in an observation.
#' @return Vector of the item codes corresponding to the search if \code{additional} is
#'  equal to \code{NULL}. List of codes and information requested otherwise.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{get_info_from_items}}.
#' @export
get_items_from_info = function(observations, ..., target = "all", additional = NULL, key = "CODE") {
  
  if (!(key %in% names(observations[[1]]))) stop("key must be an existing key in each observation.")
  
  # Observations correspondant aux critères
  obs = get_obs_from_info(observations, ..., target = target)
  
  # Vecteur des items
  if (is.null(additional)) {
    items = unique(unlist(lapply(obs, "[", key)))
    return(sort(items))
  }
  
  # Liste des items et données demandées
  items = c(list(CODE = sort(unique(unlist(lapply(obs, "[[", key))))),
            lapply(additional, function(a) sort(unique(unlist(lapply(obs, "[[", a))))))
  return(setNames(items, c(key, additional)))
}


#' Search for information by item
#' 
#' Retrieve information associated with observations that contain a set of sought items.
#' 
#' @param observations List of observations on which to do the search.
#' @param items Sought items.
#' @param info_names Names of information to extract from observations.
#' @param target Condition for information to be extracted from an observation.
#' One of \code{"all"}, \code{"any"}.
#'  \describe{
#'   \item{\code{"all"}}{All the sought items must be part of an observation for its information
#'                       to be extracted.}
#'   \item{\code{"any"}}{At least one of the sought items must be part of an observation for its
#'                       information to be extracted.}
#'  }
#' @param key Access key to the items in an observation.
#' @return Vector or list of information corresponding to the search.
#'  Vector if only one type of information is to be extracted. List otherwise.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{get_items_from_info}}.
#' @export
get_info_from_items = function(observations, items, info_names, target = "all", key = "CODE") {
  
  # Observations contenant le ou les items recherchés
  obs = get_obs_from_items(observations, items, target, key)
  
  # Vecteur de la variable demandée
  if (length(info_names) == 1) return(sort(unique(unlist(sapply(obs, "[", info_names)))))
  
  # Liste des variables demandées
  to_return = lapply(info_names, function(var) sort(unique(unlist(lapply(obs, "[[", var)))))
  return(setNames(to_return, info_names))
}

