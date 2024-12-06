library(igraph)

spread_epidemic<- function (graph,n0,p,nday,t)
{
  V(graph)$statut<-rep("S",vcount(graph))
  V(graph)$dayContamination<-rep(0,vcount(graph))
  V(graph)$R0<-rep(0,vcount(graph))
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
          R0<- V(graph)$R0[V(node)]
          R0<-R0+1
          V(graph)$R0<-rep(R0,V(node))
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
  R0sum<-0
  for (i in 1:vcount(graph))
  {
    R0sum <- R0sum+V(graph)$R0[node]
  }
  print ("La moyenne de contamination par noeud",R0sum/(vcount(V(graph)$statut["I"])))
}

# Générer une matrice d'adjacence pour un graphe avec 5 sommets et 7 arêtes


graph <-erdos.renyi.game(200, 0.01)
spread_epidemic (graph, 10 , 0.3 , 10 , 100)