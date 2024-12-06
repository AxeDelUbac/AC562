library(igraph)

createGraph <- function(adj_matrice){
  n <-nrow(adj_matrice)

  g <- make_empty_graph(n, directed = FALSE)

  # Ajouter des arêtes
  for (i in 1:nrow(adj_matrice)) {
    for (j in i:ncol(adj_matrice)) {
      if (adj_matrice[i, j]==1) {
         g <- add_edges(g,c(i,j))
      }
    }
  }

  plot(g)
  return(g)
}


generate_adjacency_matrix <- function(num_nodes, num_edges) {
  # Créer un graphe aléatoire
  graph <- make_empty_graph(n = num_nodes, directed = FALSE)

  # Ajouter des arêtes aléatoires
  edges <- sample(1:num_nodes, 2 * num_edges, replace = TRUE)
  graph <- add_edges(graph, edges)

  # Supprimer les arêtes multiples et les boucles (optionnel)
  graph <- simplify(graph, remove.multiple = TRUE, remove.loops = TRUE)

  # Générer la matrice d'adjacence
  adjacency_matrix <- as.matrix(as_adjacency_matrix(graph, sparse = FALSE))

  # Retourner la matrice d'adjacence
  return(adjacency_matrix)
}

spread_epidemic<- function (graph,n0,p,nday,t)
{
  V(graph)$statut<-rep("S",vcount(graph))
  V(graph)$dayContamination<-rep(0,vcount(graph))
  size_graph<-vcount(graph)
  infected_guy<-sample(1:size_graph,n0)
  for (i in infected_guy)
  {
    V(graph)$statut[i]<-"I"
  }
    # Simulation jour par jour
  for (day in 1:t) {
    # Copier les statuts pour mise à jour
    new_statut <- V(graph)$statut
    new_dayContamination <- V(graph)$dayContamination

    daily_infected <- c()
    daily_recovered <- c()

    for (node in which(V(graph)$statut == "I")) {
      # Infecter les voisins avec probabilité p
      for (neighbor in neighbors(graph, node)) {
        if (V(graph)$statut[neighbor] == "S" && runif(1) < p) {
          new_statut[neighbor] <- "I"  # Devenir infecté
          daily_infected <- c(daily_infected, neighbor)  # Ajouter à la liste d'infectés
        }
      }

      # Augmenter le compteur de jours infectés
      new_dayContamination[node] <- V(graph)$dayContamination[node] + 1

      # Vérifier si la durée d'infection est atteinte
      if (new_dayContamination[node] >= nday) {
        new_statut[node] <- "R"  # Devenir rétabli
        daily_recovered <- c(daily_recovered, node)  # Ajouter à la liste des guéris
      }
    }
    # Mettre à jour les statuts et les compteurs
    V(graph)$statut <- new_statut
    V(graph)$dayContamination <- new_dayContamination

    # Afficher les sommets infectés ce jour
    if (length(daily_infected) > 0) {
      cat("Jour", day, ": Infectés =", unique(daily_infected), "\n")
    } else {
      cat("Jour", day, ": Aucun nouveau sommet infecté\n")
    }
        # Afficher les sommets guéris ce jour
    if (length(daily_recovered) > 0) {
      cat("Jour", day, ": Nœuds guéris =", unique(daily_recovered), "\n")
    } else {
      cat("Jour", day, ": Aucun nœud guéri\n")
    }
      # Associer des couleurs en fonction de l'état des sommets
  V(graph)$color <- ifelse(V(graph)$statut == "S", "green",
                           ifelse(V(graph)$statut == "I", "red", "blue"))
  # Calculer le degré des sommets pour ajuster la taille
  degrees <- degree(graph)

  # Afficher le graphe
  plot(graph,        # Taille des sommets proportionnelle au degré
       vertex.label = V(graph)$name,                # Afficher les noms des sommets
       vertex.color = V(graph)$color)
  }

}

# Générer une matrice d'adjacence pour un graphe avec 5 sommets et 7 arêtes
adj_matrice <- generate_adjacency_matrix(num_nodes = 20, num_edges = 45)

print("Matrice d'adjacence :")
print(adj_matrice)

graph <- createGraph (adj_matrice)
spread_epidemic (graph, 3 , 0.3 , 3 , 20)