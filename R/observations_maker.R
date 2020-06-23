
#### Fonctions de structuration d'observations ####

#' Regroupement de prélèvements
#' 
#' Regroupe des prélèvements sous forme d'observations.
#' Une observation correspond à un ensemble de substances retrouvées dans une même situation de travail
#'  lors d'une même intervention.
#' Les situations connues sont décrites par l'argument \code{work_situations}.
#' Les situations non connues sont décrites par les combinaisons existantes des variables dont les noms sont
#'  définis par l'argument \code{variable_names}.
#' 
#' @param measures Ensemble de mesures de prélèvements.
#' @param mode Mode de création des observations. Choix parmi \code{1}, \code{2}, \code{3}.
#'  \describe{
#'   \item{\code{1}}{Construction d'observations à partir des situations décrites par \code{work_situations}
#'    et des situations existantes à partir de combinaisons de \code{variable_names} qui ne seraient pas
#'    décrites dans \code{work_situations}.}
#'  \item{\code{2}}{Construction d'observations uniquement à partir des situations décrites par \code{work_situations}.
#'    Les prélèvements restants sont ignorés.}
#'  \item{\code{3}}{Construction d'observations uniquement à partir des combinaisons de \code{variable_names}.}
#'  }
#' @param work_situations Description de situations de travail.
#' @param variable_names Noms des variables à prendre en compte pour considérer une situation de travail.
#' @param additional Noms des informations à conserver lors de la constitution des observations.
#' @return Liste des observations identifiées.
#' 
#' @author Gauthier Magnin
#' @export
make_observations = function(measures, mode, work_situations = NULL, variable_names = NULL, additional = NULL) {
  
  if (mode > 3 | mode < 1) stop("mode must be 1, 2 or 3.")
  if ((mode == 1 | mode == 2) & is.null(work_situations)) stop("work_situations must be defined for this mode.")
  if ((mode == 1 | mode == 3) & is.null(variable_names)) stop("variable_names must be defined for this mode.")
  
  # Liste qui contiendra les observations construites
  observations = list()
  # Identifiants des interventions associés aux prélèvements
  id_interv = unique(measures$ID)
  
  # Pour chaque intervention
  for (id_i in id_interv) {
    # Extraction des prélèvements associés à l'intervention en cours de traitement
    intervention = subset(measures, ID == id_i)
    
    if (mode == 1 | mode == 2) {
      # Regroupement des prélèvements correspondant à des situations de travail décrites
      result_mofws = make_obs_from_work_situations(intervention, work_situations, additional)
      observations = c(observations, result_mofws[[1]])
    }
    
    if (mode == 1) {
      # Construction d'observations pour les prélèvements restants
      if (!all(result_mofws[[2]])) {
        intervention = subset(intervention, !result_mofws[[2]])
        observations = c(observations, make_obs_from_unspecified_situations(intervention, variable_names, additional))
      }
    } else if (mode == 3) {
      # Regroupement des prélèvements pour chaque combinaisons des variables décrites
      observations = c(observations, make_obs_from_unspecified_situations(intervention, variable_names, additional))
    }
  }
  
  return(observations)
}


#' Regroupement de prélèvements
#' 
#' Regroupe des prélèvements sous forme d'observations.
#' Une observation correspond à un ensemble de substances retrouvées dans une même situation de travail.
#' 
#' @param measures Ensemble de mesures de prélèvements.
#' @param work_situations Description de situations de travail.
#' @param additional Noms des informations à conserver lors de la constitution des observations.
#' @return Liste contenant :
#'  \itemize{
#'    \item{La liste des observations identifiées.}
#'    \item{Un vecteur spécifiant pour chaque ligne de \code{measures} si elle a été intégrée dans une observation.}
#'  }
#' 
#' @author Gauthier Magnin
#' @keywords internal
make_obs_from_work_situations = function(measures, work_situations, additional = NULL) {
  
  # Vecteur spécifiant pour chaque prélèvement s'il a été traité ou non
  processed = rep_len(FALSE, nrow(measures))
  # Noms des varibles servant à la définition d'une situation de travail
  variable_names = colnames(work_situations)[-1]
  
  # Liste qui contiendra les observations construites
  observations = list()
  n_obs = 0
  
  # Pour chaque situation de travail
  for (id_s in unique(work_situations$ID)) {
    situation = subset(work_situations, ID == id_s)
    
    CODE = c()
    NAME = c()
    YEAR = c()
    
    ID = c()
    informations = rep(list(NULL), length(additional))
    names(informations) = additional
    
    # Pour chaque ensemble de variables définissant une situation de travail
    for (s in seq_len(nrow(situation))) {
      variable_values = situation[s, -1]
      
      # Sélection des prélèvements correspondant
      to_select = apply(measures, 1,
                        function(measure) {
                          all(measure[variable_names] == variable_values)
                        })
      sub = subset(measures, to_select)
      
      if (nrow(sub) != 0) {
        # Combinaison des substances aux précédentes
        CODE = unique(c(CODE, sub$AC_CODE))
        NAME = unique(c(NAME, sub$AC_NOM))
        YEAR = unique(c(YEAR, sub$ANNEE))
        
        ID = unique(c(ID, sub$ID))
        for (a in seq_along(additional)) {
          informations[[a]] = c(informations[[a]], unique(eval(parse(text = paste0("sub$", additional[a])))))
          # Exemple : METIER = c(METIER, unique(sub$METIER))
        }
        
        # Définition de ces prélèvements comme ayant une correspondance avec les situations de travail décrites
        processed[to_select] = TRUE
      }
    }
    
    if(length(CODE) != 0) {
      # Une observation supplémentaire
      n_obs = n_obs + 1
      observations[[n_obs]] = c(list(CODE = CODE, NAME = NAME, YEAR = YEAR,
                                     ID = ID),
                                informations)
    }
  }
  
  return(list(observations, processed))
}


#' Regroupement de prélèvements
#' 
#' Regroupe des prélèvements sous forme d'observations.
#' Une observation correspond à un ensemble de substances retrouvées dans une même situation de travail.
#' Ces situations sont décrites par les combinaisons de variables dont les noms sont définis en paramètres.
#' 
#' @param measures Ensemble de mesures de prélèvements.
#' @param variable_names Noms des variables à prendre en compte pour considérer une situation de travail.
#' @param additional Noms des informations à conserver lors de la constitution des observations.
#' @return Liste des observations identifiées.
#' 
#' @author Gauthier Magnin
#' @keywords internal
make_obs_from_unspecified_situations = function(measures, variable_names, additional = NULL) {
  
  # Liste qui contiendra les observations construites
  observations = list()
  
  # Pour chaque prélèvement
  for (p in seq_len(nrow(measures))) {
    measure = measures[p,]
    
    # Définition de la situation de travail associé à ce prélèvement, conformément aux variables spécifiées
    variable_values = tapply(variable_names, seq_along(variable_names),
                             function(x) eval(parse(text = paste0("measure$", x))))
    situation = paste(variable_values, collapse = ".")
    
    # Situation de travail déjà rencontrée pour ce prélèvement ou non
    if (situation %in% names(observations)) {
      # Ajout des nouvelles informations à l'observation correspondant à la situation
      observations[[situation]]$CODE = unique(c(observations[[situation]]$CODE, measure$AC_CODE))
      observations[[situation]]$NAME = unique(c(observations[[situation]]$NAME, measure$AC_NOM))
      observations[[situation]]$YEAR = unique(c(observations[[situation]]$YEAR, measure$ANNEE))
      
      observations[[situation]]$ID = unique(c(observations[[situation]]$ID, measure$ID))
      
      for (a in seq_along(additional)) {
        observations[[situation]][[4 + a]] = unique(c(observations[[situation]][[4 + a]],
                                                      eval(parse(text = paste0("measure$", additional[a])))))
        # Exemple : METIER = unique(c(METIER, measure$METIER))
      }
      
    } else {
      # Gestion des informations additionnelles
      informations = rep(list(NULL), length(additional))
      names(informations) = additional
      
      for (a in seq_along(additional)) {
        informations[[a]] = c(informations[[a]], eval(parse(text = paste0("measure$", additional[a]))))
        # Exemple : METIER = c(METIER, measure$METIER)
      }
      
      # Création d'une nouvelle observation pour cette situation
      observations[[situation]] = c(CODE = measure$AC_CODE,
                                    NAME = measure$AC_NOM,
                                    YEAR = measure$ANNEE,
                                    ID = measure$ID,
                                    informations)
    }
  }
  # Suppression des noms des éléments de la liste
  names(observations) = NULL
  
  return(observations)
}


#' Regroupement de prélèvements
#' 
#' Regroupe des prélèvements sous forme d'observations.
#' Les substances retrouvées dans une même intervention sont regroupées en une même observation.
#' 
#' @param measures Ensemble de mesures de prélèvements.
#' @param additional Noms des informations à conserver lors de la constitution des observations.
#' @return Liste des observations identifiées.
#' 
#' @author Gauthier Magnin
#' @export
turn_interventions_into_observations = function(measures, additional = NULL) {
  # Identifiants des interventions associés aux prélèvements
  id_interv = sort(unique(measures$ID))
  
  observations = tapply(id_interv, id_interv, function(num) {
    
    # Gestion des informations additionnelles
    informations = rep(list(NULL), length(additional))
    names(informations) = additional
    
    for (a in seq_along(additional)) {
      informations[[a]] = unique(eval(parse(text = paste0("measures$", additional[a])))[measures$ID == num])
      # Exemple : METIER = unique(measures$METIER[measures$ID == num])
    }
    
    # Création d'une observation à partir de l'ensemble des prélèvements effectués pendant l'intervention
    return(c(list(CODE = unique(measures$AC_CODE[measures$ID == num]),
                  NAME = unique(measures$AC_NOM[measures$ID == num]),
                  YEAR = unique(measures$ANNEE[measures$ID == num])),
             informations))
  })
  
  return(observations)
}



#### Fonctions de recherche dans une structure d'observations ####

#' Recherche d'observations
#' 
#' Extrait les observations correspondant à un ou plusieurs items recherchés.
#' 
#' @param observations Liste des observations sur lesquels effectuer la recherche.
#' @param items Élément(s) recherché(s).
#' @param target Condition pour qu'une observation soit extraite. Choix parmi \code{"all"}, \code{"any"}.
#'  \describe{
#'   \item{\code{"all"}}{L'intégralité des items recherchés doivent faire partie d'une observation pour que cette observation soit extraite.}
#'   \item{\code{"any"}}{Au moins un des items recherchés doit faire partie d'une observation pour que cette observation soit extraite.}
#'  }
#' @return Sous-ensemble de la liste d'observations correspondant aux critères de recherche.
#' 
#' @author Gauthier Magnin
#' @export
extract_observations_from_items = function(observations, items, target = "all") {
  
  if (!(target %in% c("all", "any"))) stop("target must be \"all\" or \"any\".")
  
  if (target == "all") func = all
  else if (target == "any") func = any
  
  matching_index = which(sapply(sapply(observations, "[[", "CODE"), function (x) func(items %in% x)))
  extraction = observations[matching_index]
  names(extraction) = matching_index
  
  return(extraction)
}


#' Recherche d'observations
#' 
#' Extrait les observations correspondant à un ou plusieurs critères de recherche.
#' 
#' @param observations Liste des observations sur lesquels effectuer la recherche.
#' @param info_names Noms des informations contenues dans les observations, sur lesquels la recherche doit être faite.
#' @param info_values Valeurs recherchées pour les informations décrite par l'argument \code{info_names}.
#' @return Sous-ensemble de la liste d'observations correspondant aux critères de recherche.
#' 
#' @examples
#' extract_observations_from_information(observations, c("METIER", "TACHE"), c(44143009, "A3410"))
#' 
#' @author Gauthier Magnin
#' @export
extract_observations_from_information = function(observations, info_names, info_values) {
  
  if (length(info_names) != length(info_values))
    stop("info_names and info_values must have the same length.")
  
  index = which(apply(sapply(observations, "[", info_names) == info_values, 2, all))
  corresponding = observations[index]
  names(corresponding) = index
  
  return(corresponding)
}


#' Recherche d'items
#' 
#' Extrait les items associés aux observations correspondant à un ou plusieurs critères de recherche.
#' 
#' @param observations Liste des observations sur lesquels effectuer la recherche.
#' @param info_names Noms des informations contenues dans les observations, sur lesquels la recherche doit être faite.
#' @param info_values Valeurs recherchées pour les informations décrite par l'argument \code{info_names}.
#' @return Data frame (code et nom) des items correspondant à la recherche.
#' 
#' @examples
#' extract_items(observations, c("METIER", "TACHE"), c(44143009, "A3410"))
#' 
#' @author Gauthier Magnin
#' @export
extract_items = function(observations, info_names, info_values) {
  
  if (length(info_names) != length(info_values))
    stop("info_names and info_values must have the same length.")
  
  # Observations correspondant aux critères
  index = which(apply(sapply(observations, "[", info_names) == info_values, 2, all))
  corresponding = observations[index]
  
  items = data.frame(CODE = unique(unlist(lapply(corresponding, "[[", "CODE"))),
                     NAME = unique(unlist(lapply(corresponding, "[[", "NAME"))))
  
  return(items[order(items$CODE), ])
}


#' Recherche d'informations
#' 
#' Extrait les informations associées aux observations qui contiennent un ensemble d'items recherché.
#' 
#' @param observations Liste des observations sur lesquels effectuer la recherche.
#' @param items Élément(s) recherché(s).
#' @param info_names Noms des informations à extraire des observations.
#' @return Vecteur ou matrice des informations correspondant à la recherche.
#'  Vecteur si un seul type d'information est à extraire. Matrice sinon.
#' 
#' @examples
#' extract_information(observations, 27, c("METIER", "TACHE"))
#' 
#' @author Gauthier Magnin
#' @export
extract_information = function(observations, items, info_names) {
  
  # Observations contenant le ou les items recherchés
  index = which(sapply(sapply(observations, "[", "CODE"), function(x) all(items %in% x)))
  corresponding = observations[index]
  
  # Vecteur ou matrice de retour, selon le nombre de variables prises en compte
  if (length(info_names) == 1) {
    return(sort(unique(unlist(sapply(corresponding, "[", info_names)))))
  } else {
    information = sapply(corresponding, "[", info_names)
    
    # Cas où plusieurs valeurs sont trouvées pour chaque variable info_names
    if (ncol(information) > 1 || length(unlist(information[1,])) > 1 ) {
      # Unlist indépendant pour chaque info_names nécessaire car certaines observations regroupent
      # plusieurs valeurs par info_names
      information = unique(do.call(cbind, tapply(info_names, info_names,
                                                 function(x) unlist(information[x, ]))))
      rownames(information) = NULL
      
      # Réordonnement si plusieurs résultats après unique()
      if (nrow(information) > 1) {
        # Tri selon chaque colonne (colonnes prises dans l'ordre des variables données dans info_names)
        order = eval(parse(text = paste0("order(",
                                         paste0("information[, ", seq_len(length(info_names)), "]",
                                                collapse = ", "),
                                         ")")))
        information = information[order, ]
      }
    } else {
      information = t(as.character(information))
      colnames(information) = info_names
    }
    
    return(information)
  }
}

