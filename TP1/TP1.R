degreeList <- function(matrice) {
  resultat<-matrix (1,nrow=nrow(matrice),ncol=1)
  for (i in 1:nrow(matrice)){
    degree<-0
    for (j in 1:ncol(matrice)){
      if (matrice [i,j]==1){
        degree<-degree+1
      }
    }
    resultat[i,1] <-degree
  }
 
  return (resultat) 
}

distributionDegreeList <- function(list) {
  resultat<-matrix (0,nrow=nrow(list),ncol=1)
  for (i in 1:nrow(list)){
    if (list[i,1]>0){
      resultat[list[i,1],1] <-resultat[list[i,1],1]+1
    }
  } 
  return (resultat) 
}

clustering_coef <- function(adj_matrice, ListDedegree) {
  n <- nrow(adj_matrice)
  clustering_coefficients <- numeric(n)

  # Parcourir chaque nœud
  for (i in 1:n) {
    if (ListDedegree[i, 1] >= 2) {
      neighbors <- which(adj_matrice[i, ] == 1)
      k_i <- length(neighbors)

      # Calculer le nombre de liens entre les voisins
      subgraph <- adj_matrice[neighbors, neighbors]
      num_edges <- sum(subgraph) / 2

      # Calculer le coefficient de clustering
      clustering_coefficients[i] <- (2 * num_edges) / (k_i * (k_i - 1))
    } else {
      clustering_coefficients[i] <- 0
    }
  }

  return(clustering_coefficients)
}

BFS <- function(adj_matrice, s) {
  n <- nrow(adj_matrice)
  Ds <- rep(-1, n)
  Ds[s] <- 0
  queue <- c(s)

  while (length(queue) > 0) {
    current_node <- queue[1]
    queue <- queue[-1]

    neighbors <- which(adj_matrice[current_node, ] == 1)
    for (neighbor in neighbors) {
      if (Ds[neighbor] == -1) {
        Ds[neighbor] <- Ds[current_node] + 1
        queue <- c(queue, neighbor)
      }
    }
  }

  return(Ds)
}

BFSstack <- function(adj_matrice, s) {
  n <- nrow(adj_matrice)
  Ds <- rep(-1, n)   # Distance à chaque nœud, initialisée à -1 (non visité)
  Ds[s] <- 0         # Le nœud de départ a une distance de 0
  stack <- numeric(n) # Initialisation d'une pile de taille n
  stack[1] <- s      # Le nœud de départ est placé à la position 1
  read <- 1
  write <- 2
  d <- 0

  while(read!=write){
    i <- stack[read] # Lire le prochain nœud dans la pile
    read <- read + 1

    # Explorer les voisins de i
    neighbors <- which(adj_matrice[i, ] == 1)
    for (j in neighbors) {
      if (Ds[j] == -1) { # Si le voisin n'a pas encore été visité
        Ds[j] <- d + 1
        stack[write] <- j # Placer le voisin dans la pile
        write <- write + 1
      }
    }

    # Incrémenter la distance
    d <- d + 1
  }
  return(Ds)
}

BFSDistanceMatrix <- function(adj_matrice) {
  n <- nrow(adj_matrice)
  D <- matrix(-1, nrow = n, ncol = n)

    for (i in 1:n) {
      #BFS pour obtenir les distances à partir du nœud s
      Ds <- BFS(adj_matrice, i)
      for (j in 1:n) {
        #noeud connecté à aucun autre noeud
        if (Ds[j] != -1) {
          D[i, j] <- Ds[j]
        }
      }
  }

  return(D)
}

diameter <- function(adj_matrice) {
  #Matrice de distance D donnant la distance entre 2 noeuds
  D <- BFSDistanceMatrix(adj_matrice)
  max_distance <- 0

  for (i in 1:nrow(D)) {
    for (j in 1:ncol(D)) {
      if (D[i, j] > max_distance) {
        max_distance <- D[i, j]
      }
    }
  }

  return(max_distance)
}

closenessCentrality <- function(adj_matrice) {
  D <- BFSDistanceMatrix(adj_matrice)
  n <- nrow(D)
  closeness <- numeric(n)

  for (i in 1:n) {
    reachable_distances <- D[i, D[i, ] != -1]
    if (length(reachable_distances) > 1) {
      closeness[i] <- (length(reachable_distances) - 1) / sum(reachable_distances)
    } else {
      closeness[i] <- 0
    }
  }

  return(closeness)
}

betweennessCentrality <- function(adj_matrice) {
  n <- nrow(adj_matrice)
  betweenness <- numeric(n)

  for (s in 1:n) {
    # Utiliser BFS pour obtenir les distances depuis le nœud source s
    queue <- list()
    queue[[1]] <- s
    path_count <- rep(0, n)
    path_count[s] <- 1
    distance <- rep(-1, n)
    distance[s] <- 0
    stack <- list()

    # BFS pour trouver les plus courts chemins depuis le nœud source s
    while (length(queue) > 0) {
      current <- queue[[1]]
      queue <- queue[-1]
      stack <- append(stack, current)

      neighbors <- which(adj_matrice[current, ] == 1)
      for (neighbor in neighbors) {
        if (distance[neighbor] == -1) {
          queue <- append(queue, neighbor)
          distance[neighbor] <- distance[current] + 1
        }
        if (distance[neighbor] == distance[current] + 1) {
          path_count[neighbor] <- path_count[neighbor] + path_count[current]
        }
      }
    }

    # Accumuler les dépendances pour chaque nœud sur les plus courts chemins
    dependency <- rep(0, n)
    while (length(stack) > 0) {
      w <- stack[[length(stack)]]
      stack <- stack[-length(stack)]

      for (v in which(adj_matrice[w, ] == 1)) {
        if (distance[v] == distance[w] - 1) {
          dependency[v] <- dependency[v] + (path_count[v] / path_count[w]) * (1 + dependency[w])
        }
      }

      if (w != s) {
        betweenness[w] <- betweenness[w] + dependency[w]
      }
    }
  }

  return(betweenness)
}

adj_matrice <-matrix(0,nrow=5,ncol=5)
adj_matrice[1,2] <-1
adj_matrice[1,3] <-1
adj_matrice[1,5] <-1
adj_matrice[2,1] <-1
adj_matrice[3,1] <-1
adj_matrice[5,1] <-1
adj_matrice[3,5] <-1
adj_matrice[5,3] <-1
adj_matrice[2,5] <-1
adj_matrice[5,2] <-1
adj_matrice[3,4] <-1
adj_matrice[4,3] <-1

print(adj_matrice)

matrice_inverse <- 1 - adj_matrice

print("Matrice inversée:")
print(matrice_inverse)

#Liste des degrées
ListDedegree<-degreeList(adj_matrice)
print(ListDedegree)

#Distribution des degrées
distribution <- distributionDegreeList(ListDedegree)
print(distribution)

#coefficiant de clustering de chaque point
cluster<- clustering_coef(adj_matrice,ListDedegree)
#Pour Afficher sur plusieur ligne les coefficiant
cluster_matrix <- cbind(1:length(cluster), cluster)
print("Clustering coefficient:")
print (cluster_matrix)

#Algo BFS
s <- 1
#BFS avec un nœud s de départ
distances <- BFS(adj_matrice, s)
print(paste("Distances a partir du noeud", s, ":"))
print(distances)

#BFS avec stack avec un nœud s de départ
distances <- BFSstack(adj_matrice, s)
print(paste("Distances avec stack a partir du noeud", s, ":"))
print(distances)

#BFS stack à un probleme
D_matrix <- BFSDistanceMatrix(adj_matrice)
print("Matrice des distances D:")
print(D_matrix)

# Donne le nombre de noeud maximal du graphe pour aller d'un noeud à un autre
diam <- diameter(adj_matrice)
print(paste("Diametre du graphe:", diam))

# Tester la fonction closenessCentrality
#nb noeud-1 /somme des distance par rapport un noeud
#indication de la centralisation du réseau
closeness <- closenessCentrality(adj_matrice)
print("Centralite de proximite des noeuds:")
print(closeness)

# Tester la fonction betweennessCentrality
#Indique le nombre de fois qu'un noeud apparait sur le plus court chemin entre 2 noeud
#Indique si le noeud est centrale dans le réseaux
betweenness <- betweennessCentrality(adj_matrice)
print("Centralite d'intermediation des noeuds:")
print(betweenness)