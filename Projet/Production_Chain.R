library(igraph)

# Définir un tableau pour les types d'usines
types_usines <- data.frame(
  Type = c(
    "Chantier_navale",
    "Aciérie",
    "Usine_de_fabrication_d'arme_lourde",
    "Fourneau",
    "Usine_de_dynamite",
    "Mine_de_charbon",
    "Mine_de_fer",
    "Carrière_de_salpètre",
    "Usine_de_suif",
    "ferme_de_cochon",
    "Ligne_d'assemblage_de_moteur",
    "Fonderie_de_laiton",
    "Mine_de_cuivre",
    "Mine_de_zinc"
  ),
  TempsDeProduction = c(
    15,
    0.75,
    1,
    0.5,
    1,
    0.25,
    0.25,
    2,
    1,
    1,
    0.75,
    0.75,
    0.5,
    0.5
  ),
  ressource_output= c(
    "croiseur_de_combat",
    "poutre_en_acier",
    "Arme_lourde",
    "acier",
    "dynamite",
    "charbon",
    "fer",
    "salpetre",
    "suif",
    "cochon",
    "moteur_a_vapeur",
    "laiton",
    "cuivre",
    "zinc"
  ),
  ressource_input= c(
    "60:poutre_en_acier 20:moteur 25:arme_lourde",
    "fer charbon",
    "dynamite acier",
    "fer charbon",
    "salpetre suif",
    "void",
    "void",
    "void",
    "cochon",
    "void",
    "laiton acier",
    "cuivre zinc",
    "void",
    "void"
  )
)

# Créer un tableau des relations
relations <- data.frame(Source = character(), Target = character(), Resource = character(), stringsAsFactors = FALSE)

for (i in 1:nrow(types_usines)) {
  inputs <- strsplit(types_usines$ressource_input[i], " ")[[1]]

  for (input in inputs) {
    if (input != "void") {
      resource_name <- sub(".*:", "", input) # Si le format inclut des quantités (e.g., "60:poutre_en_acier")
      source_usine <- types_usines$Type[types_usines$ressource_output == resource_name]
      if (length(source_usine) > 0) {
        relations <- rbind(relations, data.frame(Source = source_usine, Target = types_usines$Type[i], Resource = resource_name))
      }
    }
    print ("L'usine",types_usines$ressource_input[i])
  }
}