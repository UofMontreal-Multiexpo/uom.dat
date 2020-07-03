
#### Fonctions de structuration d'observations génériques ####

#' Regroupement de données
#' 
#' Regroupe des données sous forme d'observations.
#' 
#' @details
#' Les variables de type \code{factor} sont converties en type \code{character}.
#' 
#' @param data Data frame contenant un ensemble de données à regrouper.
#' @param by Noms de variables inclus dans \code{colnames(data)} servant à regrouper les données
#'  de \code{data}.
#'  Chaque combinaison de valeurs de ces différentes variables engendre une observation.
#' @param additional Noms d'informations à conserver lors de la constitution des observations
#'  (vecteur inclus dans \code{colnames(data)}).
#' @param unique_values logical ou character.
#'  \itemize{
#'    \item{Si \code{TRUE}, simplification des valeurs associées aux variables définies dans
#'          \code{additional} de manière à supprimer les doublons.}
#'    \item{Si \code{FALSE}, conserve les doublons et la correspondance entre les valeurs de ces
#'          variables pour une même ligne de \code{data} (autant de valeurs que de lignes
#'          regroupées).}
#'    \item{Sinon, vecteur des variables inclus dans \code{colnames(data)} pour lesquelles la
#'          suppression des doublons doit être réalisée.}
#'  }
#' @return Liste des observations identifiées. Chaque observation est nommée par la combinaison de
#'  valeurs des variables nommées dans \code{by} qui l'a générée. Chaque observation est une liste
#'  dont les éléments correspondent aux valeurs des variables nommées dans \code{by} et
#'  \code{additional} qui ont été regroupées
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{make_INRS_observations}} pour la construction d'observations bien ciblées
#'  par rapport à des interventions et à des situations de travail similaires.
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

#' Regroupement de prélèvements
#' 
#' Regroupe des prélèvements sous forme d'observations.
#' Une observation correspond à un ensemble de substances retrouvées dans une même situation de travail
#'  lors d'une même intervention.
#' Les situations spécifiques sont définies par l'argument \code{work_situations}.
#' Les situations non spécifiques sont décrites par les combinaisons existantes des variables dont les
#'  noms sont définis par l'argument \code{variable_names}.
#' 
#' @param measures Ensemble de mesures de prélèvements. Data frame devant contenir au moins les
#'  trois variables suivantes :
#'  \describe{
#'    \item{\code{ID}}{Identifiant de l'intervention. Les mesures issues d'une même intervention
#'          doivent avoir le même identifiant d'intervention.}
#'    \item{\code{YEAR}}{Année de réalisation de l'intervention.}
#'    \item{\code{CODE}}{Code identifiant la substance faisant l'objet du prélèvement.}
#'  }
#' @param mode Mode de création des observations. Choix parmi \code{1}, \code{2}, \code{3}.
#'  \describe{
#'   \item{\code{1}}{Construction d'observations à partir des situations décrites par \code{work_situations}
#'    et des situations existantes à partir de combinaisons de \code{variable_names} qui ne seraient pas
#'    décrites dans \code{work_situations}.}
#'  \item{\code{2}}{Construction d'observations uniquement à partir des situations décrites par \code{work_situations}.
#'    Les prélèvements restants sont ignorés.}
#'  \item{\code{3}}{Construction d'observations uniquement à partir des combinaisons de \code{variable_names}.}
#'  }
#' @param work_situations Description de situations de travail. Data frame devant contenir les
#'  valeurs associées à certaines variables de \code{measures} formant une situation de travail.
#'  Une colonne \code{WS_ID} (\strong{W}ork \strong{S}ituation \strong{ID}entifier) doit permettre
#'  d'identifier les différentes associations de valeurs considérées comme une même situation de travail.
#' @param variable_names Noms des variables à prendre en compte pour considérer une situation de travail,
#'  c'est-à-dire, vecteur de noms de variables contenues dans \code{measures} dont les différentes
#'  combinaisons forment les différentes situations de travail non décrites dans \code{work_situations}.
#' @param additional Noms d'informations à conserver lors de la constitution des observations,
#'  c'est-à-dire, vecteur de noms de variables contenues dans \code{measures}.
#' @param unique_values logical ou character.
#'  \itemize{
#'    \item{Si \code{TRUE}, simplification des valeurs associées aux variables définies dans
#'          \code{additional} de manière à supprimer les doublons.}
#'    \item{Si \code{FALSE}, conserve les doublons et la correspondance entre les valeurs de ces
#'          variables pour un même prélèvement (autant de valeurs que de prélèvements).}
#'    \item{Sinon, vecteur des variables inclus dans \code{colnames(measures)} pour lesquelles la
#'          suppression des doublons doit être réalisée.}
#'  }
#' @return Liste des observations identifiées. Chaque observation est une liste contenant :
#'    \describe{
#'      \item{CODE}{Les codes identifiant les substances regroupées.}
#'      \item{YEAR}{L'année de réalisation de l'intervention de laquelle les prélèvements sont regroupées.}
#'      \item{ID}{L'identifiant dede l'intervention de laquelle les prélèvements sont regroupées.}
#'      \item{...}{Les données associées aux prélèvements, concernant les variables définies
#'                 dans l'argument \code{additional}.}
#'    }
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


#' Regroupement de prélèvements selon des situations de travail spécifiques
#' 
#' Regroupe des prélèvements sous forme d'observations (make observations from Work Situation).
#' Une observation correspond à un ensemble de substances retrouvées dans une même situation de travail.
#' 
#' @param measures Ensemble de mesures de prélèvements. Data frame devant contenir au moins les
#'  deux variables suivantes :
#'  \describe{
#'    \item{\code{YEAR}}{Année de réalisation de l'intervention.}
#'    \item{\code{CODE}}{Code identifiant la substance faisant l'objet du prélèvement.}
#'  }
#' @param work_situations Description de situations de travail. Data frame devant contenir les
#'  valeurs associées à certaines variables de \code{measures} formant une situation de travail.
#'  Une colonne \code{WS_ID} (\strong{W}ork \strong{S}ituation \strong{ID}entifier) doit permettre
#'  d'identifier les différentes associations de valeurs considérées comme une même situation de travail.
#' @param additional Noms d'informations à conserver lors de la constitution des observations,
#'  c'est-à-dire, vecteur de noms de variables contenues dans \code{measures}.
#' @param unique_values logical ou character.
#'  \itemize{
#'    \item{Si \code{TRUE}, simplification des valeurs associées aux variables définies dans
#'          \code{additional} de manière à supprimer les doublons.}
#'    \item{Si \code{FALSE}, conserve les doublons et la correspondance entre les valeurs de ces
#'          variables pour un même prélèvement (autant de valeurs que de prélèvements).}
#'    \item{Sinon, vecteur des variables inclus dans \code{colnames(measures)} pour lesquelles la
#'          suppression des doublons doit être réalisée.}
#'  }
#' @return
#'  \describe{
#'    \item{\code{observations}}{La liste des observations identifiées.
#'      Chaque observation est une liste contenant :
#'      \describe{
#'        \item{CODE}{Les codes identifiant les substances regroupées.}
#'        \item{YEAR}{L'année de réalisation de l'intervention de laquelle les prélèvements sont regroupées.}
#'        \item{...}{Les données associées aux prélèvements, concernant les variables définies
#'                   dans l'argument \code{additional}.}
#'      }
#'    }
#'    \item{processed}{Un vecteur spécifiant, pour chaque ligne de \code{measures},
#'                     si elle a été intégrée dans une observation.}
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


#' Regroupement de prélèvements selon des situations de travail non spécifiques
#' 
#' Regroupe des prélèvements sous forme d'observations (make observations from unspecified Work Situation).
#' Une observation correspond à un ensemble de substances retrouvées dans une même situation de travail.
#' Ces situations sont décrites par les combinaisons de valeurs possibles des variables dont les noms
#'  sont définis en paramètre.
#' 
#' @param measures Ensemble de mesures de prélèvements. Data frame devant contenir au moins les
#'  deux variables suivantes :
#'  \describe{
#'    \item{\code{YEAR}}{Année de réalisation de l'intervention.}
#'    \item{\code{CODE}}{Code identifiant la substance faisant l'objet du prélèvement.}
#'  }
#' @param variable_names Noms des variables à prendre en compte pour considérer une situation de travail,
#'  c'est-à-dire, vecteur de noms de variables contenues dans \code{measures} dont les différentes
#'  combinaisons forment les différentes situations de travail.
#' @param additional Noms d'informations à conserver lors de la constitution des observations,
#'  c'est-à-dire, vecteur de noms de variables contenues dans \code{measures}.
#' @param unique_values logical ou character.
#'  \itemize{
#'    \item{Si \code{TRUE}, simplification des valeurs associées aux variables définies dans
#'          \code{additional} de manière à supprimer les doublons.}
#'    \item{Si \code{FALSE}, conserve les doublons et la correspondance entre les valeurs de ces
#'          variables pour un même prélèvement (autant de valeurs que de prélèvements).}
#'    \item{Sinon, vecteur des variables inclus dans \code{colnames(measures)} pour lesquelles la
#'          suppression des doublons doit être réalisée.}
#'  }
#' @return Liste des observations identifiées. Chaque observation est une liste contenant :
#'    \describe{
#'      \item{CODE}{Les codes identifiant les substances regroupées.}
#'      \item{YEAR}{L'année de réalisation de l'intervention de laquelle les prélèvements sont regroupées.}
#'      \item{...}{Les données associées aux prélèvements, concernant les variables définies
#'                 dans l'argument \code{additional}.}
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

#' Recherche l'ensemble des items
#' 
#' Extrait l'intégralité des items contenus dans les observations.
#' 
#' @param observations Liste des observations sur lesquels effectuer la recherche.
#' @param key Clé d'accès aux items de chaque observation.
#' @return Vecteur des identifiants de l'ensemble des items.
#' 
#' @author Gauthier Magnin
#' @export
get_all_items = function(observations, key = "CODE") {
  
  if (!(key %in% names(observations[[1]]))) stop("key must be an existing key in each observation.")
  
  return(sort(unique(unlist(sapply(observations, "[", key)))))
}


#' Recherche d'observations par item
#' 
#' Extrait les observations contenant un ou plusieurs items recherchés.
#' 
#' @param observations Liste des observations sur lesquels effectuer la recherche.
#' @param items Élément·s recherché·s.
#' @param target Condition pour qu'une observation soit extraite. Choix parmi \code{"all"}, \code{"any"}.
#'  \describe{
#'   \item{\code{"all"}}{L'intégralité des items recherchés doivent faire partie d'une observation
#'                       pour que cette observation soit extraite.}
#'   \item{\code{"any"}}{Au moins un des items recherchés doit faire partie d'une observation pour
#'                       que cette observation soit extraite.}
#'  }
#' @param key Clé d'accès aux items de chaque observation.
#' @return Sous-ensemble de la liste d'observations correspondant aux critères de recherche.
#' 
#' @author Gauthier Magnin
#' @seealso \code{\link{get_obs_from_info}}.
#' @export
get_obs_from_items = function(observations, items, target = "all", key = "CODE") {
  
  if (!(target %in% c("all", "any"))) stop("target must be \"all\" or \"any\".")
  if (!(key %in% names(observations[[1]]))) stop("key must be an existing key in each observation.")
  
  func = if (target == "all") all else any
  
  index = which(sapply(sapply(observations, "[[", key), function (x) func(items %in% x)))
  extraction = observations[index]
  
  return(setNames(extraction, index))
}


#' Recherche d'observations par information spécifique
#' 
#' Extrait les observations correspondant à un ou plusieurs critères de recherche.
#' 
#' @param observations Liste des observations sur lesquels effectuer la recherche.
#' @param ... Arguments de type \code{clé = valeur} où \code{clé} fait référence au nom d'une variable
#'  contenue dans les observations et \code{valeur} correspond à la valeur recherchée pour cette variable.
#' @param target Condition pour qu'une observation soit extraite. Choix parmi \code{"all"}, \code{"any"}.
#'  \describe{
#'   \item{\code{"all"}}{L'intégralité des informations recherchées doivent faire partie d'une observation
#'                       pour que cette observation soit extraite.}
#'   \item{\code{"any"}}{Au moins une des informations recherchées doit faire partie d'une observation pour
#'                       que cette observation soit extraite.}
#'  }
#' @return Sous-ensemble de la liste d'observations correspondant aux critères de recherche.
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


#' Recherche d'items par information spécifique
#' 
#' Extrait les items associés aux observations correspondant à un ou plusieurs critères de recherche.
#' 
#' @param observations Liste des observations sur lesquels effectuer la recherche.
#' @param ... Arguments de type \code{clé = valeur} où \code{clé} fait référence au nom d'une variable
#'  contenue dans les observations et \code{valeur} correspond à la valeur recherchée pour cette variable.
#' @param target Condition pour qu'une observation soit extraite. Choix parmi \code{"all"}, \code{"any"}.
#'  \describe{
#'   \item{\code{"all"}}{L'intégralité des informations recherchées doivent faire partie d'une observation
#'                       pour que cette observation soit extraite.}
#'   \item{\code{"any"}}{Au moins une des informations recherchées doit faire partie d'une observation pour
#'                       que cette observation soit extraite.}
#'  }
#' @param additional Noms d'informations supplémentaires à extraire lors de la recherche.
#' @param key Clé d'accès aux items de chaque observation.
#' @return Vecteur des codes des items correspondant à la recherche si \code{additional} vaut \code{NULL}.
#'  Liste des codes et des informations demandées sinon.
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


#' Recherche d'informations par item
#' 
#' Extrait les informations associées aux observations qui contiennent un ensemble d'items recherchés.
#' 
#' @param observations Liste des observations sur lesquels effectuer la recherche.
#' @param items Élément·s recherché·s.
#' @param info_names Noms des informations à extraire des observations.
#' @param target Condition pour qu'une information soit extraite d'une observation.
#' Choix parmi \code{"all"}, \code{"any"}.
#'  \describe{
#'   \item{\code{"all"}}{L'intégralité des items recherchés doivent faire partie d'une observation
#'                       pour que ses informations soient extraites.}
#'   \item{\code{"any"}}{Au moins un des items recherchés doit faire partie d'une observation pour
#'                       que ses informations soient extraites.}
#'  }
#' @param key Clé d'accès aux items de chaque observation.
#' @return Vecteur ou liste des informations correspondant à la recherche.
#'  Vecteur si un seul type d'information est à extraire. Liste sinon.
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

