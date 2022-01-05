## Preparation of the 'substances_information' dataset.
## 
## Input files: - famille_agent.csv
##              - agent_vlep.csv
##              - Mixie_data_avec_codes_20200408_partial.xlsx

#### Creation of substances_information (columns CODE, NAME, FAMILY, SUBFAMILY) ####

# "famille_agent.csv"
substances_information = read.csv2("./data-raw/famille_agent.csv",
                                   stringsAsFactors = FALSE)

substances_information$Code = as.numeric(substances_information$Code)
substances_information = substances_information[!is.na(substances_information$Code), ]
substances_information$Code = as.character(substances_information$Code)
substances_information = substances_information[order(substances_information$Grande_famille,
                                                      substances_information$Famille),]

substances_information = substances_information[, c(1,2,4,3)]
colnames(substances_information) = c("CODE", "NAME", "FAMILY", "SUBFAMILY")

# Rows 559 to 571 are moved to rows 580 to 593 and row 630 is moved to row 639.
# Rows 23 and 825 are removed.
# This has no special meaning. Original instructions to create the dataset were
# not kept and these ones allow to obtain the exact same dataset.
substances_information = substances_information[c(1:22, 24:558, 572:592, 559:571,
                                                  593:629, 631:639, 630, 640:824,
                                                  826:nrow(substances_information)), ]

rownames(substances_information) = NULL



#### Column LIMIT ####

# "agent_vlep.csv"
agent_vlep = read.csv2("./data-raw/agent_vlep.csv",
                       stringsAsFactors = FALSE)
agent_vlep$VLEPCT = as.numeric(agent_vlep$VLEPCT)
agent_vlep$VLEP8H = as.numeric(agent_vlep$VLEP8H)

# VLEP-8h if there is one, VLEP-CT otherwise (or NA)
vlep = sapply(substances_information$CODE, function(code) {
  lines = which(agent_vlep$CODE == code)
  if (length(lines) == 0) return(NA)
  
  if (is.na(agent_vlep[lines, "VLEP8H"])) return(agent_vlep[lines, "VLEPCT"])
  return(agent_vlep[lines, "VLEP8H"])
})

substances_information$LIMIT = vlep

# length(vlep)
# sum(is.na(vlep))
# sum(vlep == 1, na.rm = T)
# sum(vlep == 0.75, na.rm = T)
# vlep[is.na(vlep)] = 0.75



#### Column TOXICITY ####

# "Mixie_data_avec_codes_20200408_partial.xlsx"
agent_classes = xlsx::read.xlsx("./data-raw/Mixie_data_avec_codes_20200408_partial.xlsx",
                                sheetIndex = 1, encoding = "UTF-8")

cla = lapply(substances_information$CODE, function(code) {
  code = paste0(paste0(rep("0", nchar(as.character(agent_classes$Code[1])) - nchar(code)), collapse = ""),
                code)
  return(as.character(agent_classes[!is.na(agent_classes$Code) & as.character(agent_classes$Code) == code,
                                    "Classe.toxicologique"]))
})
# Removal of remaining NAs (whereas other elements already have the value character(0))
for (i in which(is.na(cla))) cla[[i]] = character(0)
rm(i)
# Setting NULL if no value
cla = lapply(cla, function(x) if (length(x) == 0) NULL else x)

substances_information$TOXICITY = cla
substances_information = substances_information[, c("CODE", "NAME", "FAMILY", "SUBFAMILY",
                                                    "TOXICITY", "LIMIT")]



#### Add prepared data to package ####

#! See file "data-raw/add_prepared_data_to_package.R"


