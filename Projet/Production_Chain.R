library(igraph)

# Définir un tableau pour les types d'usines
types_usines <- data.frame(
  Type = c(
    "Chantier_navale",
    "Acierie",
    "Usine_de_fabrication_arme_lourde",
    "Fourneau",
    "Usine_de_dynamite",
    "Mine_de_charbon",
    "Mine_de_fer",
    "Carriere_de_salpetre",
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
  ressource_output = c(
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
  ressource_input = c(
    "poutre_en_acier moteur_a_vapeur Arme_lourde",
    "acier",
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
  ),
  Nombre = c(
    2,  # 1 Chantier_navale
    1,  # 1 Acierie
    2,  # 1 Usine_de_fabrication_arme_lourde
    3,  # 3 Fourneaux
    2,  # 1 Usine_de_dynamite
    2,  # 1 Mine_de_charbon
    2,  # 1 Mine_de_fer
    4,  # 1 Carriere_de_salpetre
    2,  # 1 Usine_de_suif
    2,  # 1 ferme_de_cochon
    1,  # 1 Ligne_d'assemblage_de_moteur
    1,  # 1 Fonderie_de_laiton
    1,  # 1 Mine_de_cuivre
    1   # 1 Mine_de_zinc
  ),
  Quantite_ressource = c(
    "1 1 1",  # Chantier_navale
    "1",        # Acierie
    "1 1",     # Usine_de_fabrication_arme_lourde
    "1 1",     # Fourneau
    "1 1",     # Usine_de_dynamite
    "0",         # Mine_de_charbon (Pas de ressource d'entrée)
    "0",         # Mine_de_fer
    "0",         # Carriere_de_salpetre
    "1",        # Usine_de_suif
    "0",         # ferme_de_cochon
    "1 1",     # Ligne_d'assemblage_de_moteur
    "1 1",     # Fonderie_de_laiton
    "0",         # Mine_de_cuivre
    "0"          # Mine_de_zinc
  )
)

# Étendre le tableau pour générer les usines
extended_usines <- do.call(rbind, lapply(1:nrow(types_usines), function(i) {
  n <- types_usines$Nombre[i]
  quantites <- lapply(rep(types_usines$Quantite_ressource[i], n), function(q) {
    if (q == "0") {
      return(numeric(0))  # Pas de ressources nécessaires
    } else {
      as.numeric(unlist(strsplit(q, " ")))
    }
  })
  data.frame(
    Type = paste0(types_usines$Type[i], "_", seq_len(n)),  # Générer des noms uniques
    TempsDeProduction = rep(types_usines$TempsDeProduction[i], n),
    ressource_output = rep(types_usines$ressource_output[i], n),
    ressource_input = rep(types_usines$ressource_input[i], n),
    Quantite_ressource = rep(types_usines$Quantite_ressource[i], n),
    Quantites = I(quantites),
    stringsAsFactors = FALSE
  )
}))

# Résultat final : Tableau étendu
print(extended_usines)

# Créer un tableau des relations
relations <- data.frame(Source = character(), Target = character(), Resource = character(), stringsAsFactors = FALSE)

for (i in 1:nrow(extended_usines)) {
  inputs <- strsplit(extended_usines$ressource_input[i], " ")[[1]]
  #cat("DEBUG: Inputs for", extended_usines$Type[i], ":", inputs, "\n") # Log des inputs
  #inputs <- strsplit(extended_usines$ressource_input[i], " ")[[1]]
  quantities <- extended_usines$Quantites[[i]]

  for (input in inputs) {
    if (input != "void") {

      resource_name <- sub(".*:", "", input) # Si le format inclut des quantités (e.g., "60:poutre_en_acier")
      source_usine <- extended_usines$Type[extended_usines$ressource_output == resource_name]
      cat("DEBUG: Source usine for", resource_name, ":", source_usine, "\n") # Log pour source_usine
      if (length(source_usine) > 0) {
        relations <- rbind(relations, data.frame(Source = source_usine, Target = extended_usines$Type[i], Resource = resource_name))
      }
    }
    print(paste("Type d'usine:", extended_usines$Type[i], "- Temps de production:", extended_usines$TempsDeProduction[i], "Jours"))
  }
}

# Résultat final
print(relations)

# Générer le graphe avec igraph
g <- graph_from_data_frame(relations, directed = TRUE)

# Initialiser les attributs des nœuds
V(g)$statut <- "Normal"      # État initial
V(g)$productivity <- 100      # Productivité initiale
V(g)$stock <- 30              # Stock initial de chaque ressource
V(g)$days_in_state <- 0       # Nombre de jours dans un état non normal
V(g)$production_timer <- 0    # Compteur pour la production

# Fonction pour simuler les états
simulate_states <- function(graph, p_greve, p_maladie, p_incident, t, t_greve = 4, t_maladie = 7, t_incident = 10, plot_interval = 10) {
  size_graph <- vcount(graph)
  resource_output <- setNames(extended_usines$ressource_output, extended_usines$Type)
  resource_input <- setNames(extended_usines$ressource_input, extended_usines$Type)
  production_time <- setNames(extended_usines$TempsDeProduction, extended_usines$Type)
  quantities_list <- setNames(extended_usines$Quantites, extended_usines$Type)

   consumer_indices <- setNames(rep(1, length(unique(extended_usines$ressource_output))), unique(extended_usines$ressource_output))

  # Simulation sur t jours
  for (day in 1:t) {
    cat("\n=== Jour", day, "===\n")
    new_statut <- V(graph)$statut
    new_productivity <- V(graph)$productivity
    new_days_in_state <- V(graph)$days_in_state
    new_stock <- V(graph)$stock
    new_production_timer <- V(graph)$production_timer

    # Parcourir chaque usine pour mettre à jour son état
    for (i in 1:size_graph) {
      if (new_statut[i] == "Normal") {
        # Vérifier les probabilités pour changer d'état
        if (runif(1) < p_greve) {
          new_statut[i] <- "Greve"
          new_days_in_state[i] <- 0
        } else if (runif(1) < p_maladie) {
          new_statut[i] <- "Maladie"
          new_days_in_state[i] <- 0
        } else if (runif(1) < p_incident) {
          new_statut[i] <- "Incident"
          new_days_in_state[i] <- 0
        }
      } else {
        # Si l'usine est dans un état non normal, elle retourne à l'état normal après t_state jours
        new_days_in_state[i] <- new_days_in_state[i] + 1
        #if (new_days_in_state[i] >= t_state) {
        if ((new_statut[i] == "Greve" && new_days_in_state[i] >= t_greve) ||
            (new_statut[i] == "Maladie" && new_days_in_state[i] >= t_maladie) ||
            (new_statut[i] == "Incident" && new_days_in_state[i] >= t_incident)) {
          new_statut[i] <- "Normal"
          new_productivity[i] <- 100
          new_days_in_state[i] <- 0
        }
      }

      # Mettre à jour la productivité selon l'état
      if (new_statut[i] == "Greve") {
        new_productivity[i] <- 25
      } else if (new_statut[i] == "Maladie") {
        new_productivity[i] <- 50
      } else if (new_statut[i] == "Incident") {
        new_productivity[i] <- 0
      }

      # Permettre la production même avec une productivité réduite
      if (new_productivity[i] > 0) {

        inputs <- strsplit(resource_input[V(graph)$name[i]], " ")[[1]]
        quantities <- quantities_list[[V(graph)$name[i]]]   # Quantités nécessaires pour cette usine

        # Calculer le nombre d'unités à produire
        units_produced <- ceiling((1 / production_time[V(graph)$name[i]]) * (new_productivity[i] / 100))

        inputs <- strsplit(resource_input[V(graph)$name[i]], " ")[[1]]
        if (inputs[1] != "void") {

          sufficient_stock <- TRUE  # Assumer que le stock est suffisant initialement

          # Vérifier pour chaque ressource si le stock est suffisant
          for (j in seq_along(inputs)) {
            input_resource <- inputs[j]
            required_quantity <- quantities[j]

            # Calculer la quantité totale disponible pour cette ressource
            total_available <- sum(sapply(neighbors(graph, i, mode = "in"), function(neighbor) {
              if (resource_output[V(graph)$name[neighbor]] == input_resource) {
                return(new_stock[neighbor])
              } else {
                return(0)
              }
            }))

            # Vérifier si le stock est suffisant
            if (total_available < required_quantity) {
              sufficient_stock <- FALSE
              cat("Jour", day, ": Usine", V(graph)$name[i], "n'a pas assez de", input_resource, "pour produire.\n")
              break
            }
          }


          if (sufficient_stock) {
            cat("Jour", day, ": Usine", V(graph)$name[i], "a assez de ressources pour produire.\n")
            # Consommer les ressources en entrée
            for (j in seq_along(inputs)) {
              input_resource <- inputs[j]
              required_quantity <- quantities[j]
              remaining_needed <- required_quantity

              for (neighbor in neighbors(graph, i, mode = "in")) {
                if (resource_output[V(graph)$name[neighbor]] == input_resource && remaining_needed > 0) {
                  available_stock <- new_stock[neighbor]  # Stock disponible
                  to_consume <- min(available_stock, remaining_needed)  # Quantité à consommer
                  if (to_consume > 0) {
                    # Log consommation
                    cat("Jour", day, ": Usine", V(graph)$name[i], "consomme", to_consume, "de", input_resource,
                        "de Usine", V(graph)$name[neighbor], "- Nouveau stock voisin :", new_stock[neighbor] - to_consume, "\n")

                    # Mettre à jour le stock du voisin
                    new_stock[neighbor] <- new_stock[neighbor] - to_consume
                    remaining_needed <- remaining_needed - to_consume

                    # Si les besoins sont satisfaits, arrêter
                    if (remaining_needed <= 0) break
                  }
                }
              }
            }

            cat("DEBUG: Usine", V(graph)$name[i], "- Production timer avant incrément :", new_production_timer[i], "\n")
            new_production_timer[i] <- new_production_timer[i] + 1

            if (new_production_timer[i] >= (production_time[V(graph)$name[i]] / (new_productivity[i] / 100))) {
              # Calculer le nombre d'unités produites
              units_produced <- ceiling((1 / production_time[V(graph)$name[i]]) * (new_productivity[i] / 100))
              new_stock[i] <- new_stock[i] + units_produced
              cat("Jour", day, ": Usine", V(graph)$name[i], "produit", units_produced, "unités de",
                  resource_output[V(graph)$name[i]], "- Nouveau stock :", new_stock[i], "\n")
              new_production_timer[i] <- 0
            }
          }
          else{
            cat("Jour", day, ": Usine", V(graph)$name[i], "n'a pas assez de ressources pour produire. Besoin :",units_produced, "unités.\n")
          }
        } else {
          new_production_timer[i] <- new_production_timer[i] + 1
          if (new_production_timer[i] >= (production_time[V(graph)$name[i]] / (new_productivity[i] / 100))) {
            # Calculer le nombre d'unités produites
            units_produced <- ceiling((1 / production_time[V(graph)$name[i]]) * (new_productivity[i] / 100))
            new_stock[i] <- new_stock[i] + units_produced
            cat("Jour", day, ": Usine", V(graph)$name[i], "produit", units_produced, "unités de",
                resource_output[V(graph)$name[i]], "- Nouveau stock :", new_stock[i], "\n")
            new_production_timer[i] <- 0
          }
        }
      }
    }

    # Mettre à jour les attributs du graphe
    V(graph)$statut <- new_statut
    V(graph)$productivity <- new_productivity
    V(graph)$days_in_state <- new_days_in_state
    V(graph)$stock <- new_stock
    V(graph)$production_timer <- new_production_timer

    # Associer des couleurs selon l'état
    V(graph)$color <- ifelse(V(graph)$statut == "Normal", "green",
                             ifelse(V(graph)$statut == "Greve", "red",
                                    ifelse(V(graph)$statut == "Maladie", "orange", "blue")))

    # Afficher le graphe uniquement à l'intervalle défini
    if (day %% plot_interval == 0 || day == t) {
      cat("Jour", day, "- Etat des usines\n")
      #png("output_graph.png", width = 1200, height = 800)
      plot(
        graph,
        vertex.label = paste(V(graph)$name, "\n", V(graph)$productivity, "%\n",V(graph)$stock),
        vertex.size = 30,
        vertex.label.color = "black",
        vertex.color = V(graph)$color,
        main = paste("Jour", day, "- Etat des usines"),
        layout = layout_with_sugiyama(graph)$layout
      )
    }

    Sys.sleep(0.5)  # Pause entre les jours (réduit pour tester rapidement)
  }
}

  # Vérifier si le graphe est acyclique
if (!is_dag(g)) {
  stop("Le graphe contient des cycles. Veuillez vérifier les relations.")
}

#png("output_graph.png", width = 1200, height = 800)
# Visualisation du graphe
plot(
  g,
  vertex.label = V(g)$name,               # Étiquettes des nœuds (nom des usines)
  vertex.size = 30,                      # Taille des nœuds
  vertex.color = "lightblue",            # Couleur des nœuds
  vertex.label.cex = 1.2,                # Taille du texte des étiquettes
  vertex.label.color = "black",          # Couleur du texte des étiquettes
  edge.label = E(g)$Resource,            # Étiquettes des arêtes (ressources)
  edge.color = "darkgray",               # Couleur des arêtes
  edge.arrow.size = 0.4,                 # Taille des flèches
  layout = layout_with_sugiyama(g)$layout,
  main = "Graphe acyclique oriente des relations entre usines"
)

# Afficher les informations sur le graphe
cat("Nombre de sommets (usines) :", vcount(g), "\n")
cat("Nombre d'arêtes (relations) :", ecount(g), "\n")

# Simulation sur 100 jours avec un plot tous les 10 jours
simulate_states(
  g,
  p_greve = 0.0,    # Probabilité qu'une usine passe en grève
  p_maladie = 0.03,   # Probabilité qu'une usine passe en maladie
  p_incident = 0.01, # Probabilité qu'une usine ait un incident
  t = 200,           # Duree Simulation
  plot_interval = 10# Afficher le graphe tous les x jours
)
