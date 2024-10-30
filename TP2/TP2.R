generate_erdos_renyi_graph <- function(n, p) {
  # Initialiser une matrice d'adjacence n x n remplie de 0
  adj_matrix <- matrix(0, n, n)

  # Parcourir toutes les paires de nœuds et ajouter une arête avec probabilité p
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      if (runif(1) <= p) {  # Correction ici
        adj_matrix[i, j] <- 1
        adj_matrix[j, i] <- 1
      }
    }
  }

  # Afficher le graphe en utilisant la matrice d'adjacence
  plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
       xlab = "", ylab = "", axes = FALSE, main = paste("Graphe de Erdos-Renyi (n =", n, ", p =", p, ")"))

  # Générer les coordonnées des nœuds
  node_positions <- data.frame(x = runif(n), y = runif(n))

  # Tracer les arêtes
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      if (adj_matrix[i, j] == 1) {
        lines(c(node_positions$x[i], node_positions$x[j]),
              c(node_positions$y[i], node_positions$y[j]),
              col = "gray")
      }
    }
  }

  # Tracer les nœuds
  points(node_positions$x, node_positions$y, pch = 21, bg = "skyblue", cex = 1.5)

  return(adj_matrix)
}

generate_watts_strogatz_graph <- function(n, k, p) {
  # Vérifier que k est pair et inférieur à n
  if (k %% 2 != 0) stop("k doit être un entier pair pour un graphe de Watts-Strogatz")
  if (k >= n) stop("k doit être inférieur à n")

  # Initialiser une matrice d'adjacence pour le graphe
  adj_matrix <- matrix(0, n, n)

  # Étape 1 : Créer un graphe régulier avec `k` voisins
  half_k <- k / 2
  for (i in 1:n) {
    for (j in 1:half_k) {
      # Calculer les voisins circulaires
      right_neighbor <- (i + j - 1) %% n + 1
      left_neighbor <- (i - j - 1 + n) %% n + 1

      # Vérifier si les indices sont dans les limites
      if (right_neighbor > n || right_neighbor < 1 || left_neighbor > n || left_neighbor < 1) {
        cat("Erreur d'indexation: i =", i, "j =", j, "right_neighbor =", right_neighbor, "left_neighbor =", left_neighbor, "\n")
        next
      }

      # Connexion bidirectionnelle
      adj_matrix[i, right_neighbor] <- 1
      adj_matrix[i, left_neighbor] <- 1
      adj_matrix[right_neighbor, i] <- 1
      adj_matrix[left_neighbor, i] <- 1
    }
  }

  # Étape 2 : Réarranger les arêtes avec probabilité p
  for (i in 1:n) {
    for (j in 1:half_k) {
      if (runif(1) < p) {
        # Supprimer l'arête actuelle
        neighbor <- (i + j - 1) %% n + 1
        if (neighbor > n || neighbor < 1) {
          cat("Erreur d'indexation (réarrangement): i =", i, "j =", j, "neighbor =", neighbor, "\n")
          next
        }
        adj_matrix[i, neighbor] <- 0
        adj_matrix[neighbor, i] <- 0

        # Ajouter une nouvelle arête vers un nœud aléatoire sans auto-boucle
        repeat {
          new_neighbor <- sample(1:n, 1)
          if (new_neighbor != i && adj_matrix[i, new_neighbor] == 0) {
            adj_matrix[i, new_neighbor] <- 1
            adj_matrix[new_neighbor, i] <- 1
            break
          }
        }
      }
    }
     return(adj_matrix)
  }

  # Afficher le graphe
  plot(0, 0, type = "n", xlim = c(-1, 1), ylim = c(-1, 1),
       xlab = "", ylab = "", axes = FALSE, main = paste("Watts-Strogatz Graph (n =", n, ", k =", k, ", p =", p, ")"))

  # Positionner les nœuds en cercle
  theta <- seq(0, 2 * pi, length.out = n + 1)[- (n + 1)]
  node_positions <- data.frame(x = cos(theta), y = sin(theta))
  points(node_positions$x, node_positions$y, pch = 21, bg = "skyblue", cex = 1.5)

  # Tracer les arêtes
  for (i in 1:n) {
    for (j in (i + 1):n) {
      if (adj_matrix[i, j] == 1) {
        lines(c(node_positions$x[i], node_positions$x[j]),
              c(node_positions$y[i], node_positions$y[j]),
              col = "gray")
      }
    }
  }
}

generate_scale_free_graph <- function(n, k, m) {
  # Initialiser une matrice d'adjacence pour le graphe
  adj_matrix <- matrix(0, n, n)

  # Étape 1 : Créer un graphe complet initial de taille k
  for (i in 1:k) {
    for (j in (i + 1):k) {
      adj_matrix[i, j] <- 1
      adj_matrix[j, i] <- 1
    }
  }

  # Étape 2 et suivantes : Ajouter des nœuds et les connecter en fonction des probabilités
  for (new_node in (k + 1):n) {
    # Calculer les degrés des nœuds existants
    degrees <- rowSums(adj_matrix[1:(new_node - 1), 1:(new_node - 1)])
    degree_sum <- sum(degrees)

    # Probabilités pour connecter aux nœuds existants
    probabilities <- degrees / degree_sum

    # Choisir m nœuds parmi les existants en fonction des probabilités de degré
    existing_nodes <- sample(1:(new_node - 1), m, prob = probabilities, replace = FALSE)

    # Connecter le nouveau nœud aux nœuds sélectionnés
    for (node in existing_nodes) {
      adj_matrix[new_node, node] <- 1
      adj_matrix[node, new_node] <- 1
    }
  }

  # Afficher le graphe
  plot(0, 0, type = "n", xlim = c(-1, 1), ylim = c(-1, 1),
       xlab = "", ylab = "", axes = FALSE, main = paste("Graphe Scale-Free (n =", n, ", k =", k, ", m =", m, ")"))

  # Générer des positions aléatoires pour les nœuds
  node_positions <- data.frame(x = runif(n, -1, 1), y = runif(n, -1, 1))

  # Calculer les degrés pour ajuster la taille des nœuds
  node_degrees <- rowSums(adj_matrix)

  # Tracer les arêtes basées sur la matrice d'adjacence
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      if (adj_matrix[i, j] == 1) {
        lines(c(node_positions$x[i], node_positions$x[j]),
              c(node_positions$y[i], node_positions$y[j]),
              col = "gray")
      }
    }
  }

  # Tracer les nœuds en ajustant leur taille en fonction du degré
  points(node_positions$x, node_positions$y, pch = 21,
         bg = "skyblue", cex = 1.5)

  return(adj_matrix)
}


# Fonction pour ajuster la loi P(k) ~ k^(-alpha) dans le cas scale-free
fit_power_law <- function(degrees) {
  # Filtrer pour les degrés non nuls et faire un log-log plot
  degree_counts <- table(degrees)
  degrees_non_zero <- as.numeric(names(degree_counts))
  counts <- as.numeric(degree_counts)

  # Ajustement de la loi de puissance
  fit <- lm(log(counts) ~ log(degrees_non_zero))
  alpha <- -coef(fit)[2]

  # Tracer la distribution avec l'ajustement de la loi de puissance
  plot(log(degrees_non_zero), log(counts), main = "Ajustement de la loi de puissance (Scale-Free)",
       xlab = "log(Degree)", ylab = "log(Frequency)", pch = 16)
  abline(fit, col = "red")

  return(alpha)
}

# Algorithme de Floyd-Warshall pour calculer les plus courts chemins
floyd_warshall <- function(adj_matrix) {
  n <- nrow(adj_matrix)
  dist_matrix <- ifelse(adj_matrix == 1, 1, Inf)
  diag(dist_matrix) <- 0

  for (k in 1:n) {
    for (i in 1:n) {
      for (j in 1:n) {
        dist_matrix[i, j] <- min(dist_matrix[i, j], dist_matrix[i, k] + dist_matrix[k, j])
      }
    }
  }
  return(dist_matrix)
}

# Fonction pour calculer le coefficient de clustering global
clustering_coefficient <- function(adj_matrix) {
  n <- nrow(adj_matrix)
  local_clustering <- numeric(n)

  for (i in 1:n) {
    neighbors <- which(adj_matrix[i, ] == 1)
    k_i <- length(neighbors)

    if (k_i < 2) {
      local_clustering[i] <- 0
    } else {
      subgraph <- adj_matrix[neighbors, neighbors]
      actual_edges <- sum(subgraph) / 2
      possible_edges <- k_i * (k_i - 1) / 2
      local_clustering[i] <- actual_edges / possible_edges
    }
  }

  global_clustering <- mean(local_clustering)
  return(list(global = global_clustering, local = local_clustering))
}

# Fonction pour calculer la closeness centrality
closeness_centrality <- function(dist_matrix) {
  n <- nrow(dist_matrix)
  closeness <- numeric(n)

  for (i in 1:n) {
    reachable <- dist_matrix[i, ][is.finite(dist_matrix[i, ]) & dist_matrix[i, ] > 0]
    if (length(reachable) > 0) {
      closeness[i] <- 1 / mean(reachable)
    } else {
      closeness[i] <- 0
    }
  }
  return(closeness)
}

# Fonction pour calculer la betweenness centrality
betweenness_centrality <- function(adj_matrix) {
  n <- nrow(adj_matrix)
  betweenness <- numeric(n)

  # Utiliser l'algorithme de Floyd-Warshall pour obtenir les plus courts chemins
  dist_matrix <- floyd_warshall(adj_matrix)

  for (k in 1:n) {
    count <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        if (i != k && j != k && is.finite(dist_matrix[i, k]) && is.finite(dist_matrix[k, j]) &&
            dist_matrix[i, j] == dist_matrix[i, k] + dist_matrix[k, j]) {
          count <- count + 1
        }
      }
    }
    betweenness[k] <- count
  }

  return(betweenness)
}

plot_degree_distribution <- function(adj_matrix, title) {
  # Calculer les degrés de chaque nœud
  degrees <- rowSums(adj_matrix)

  # Tracer l'histogramme de la distribution des degrés
  hist(degrees, breaks = seq(0, max(degrees), by = 1), col = "skyblue",
       main = paste("Distribution des degres", title), xlab = "Degre", ylab = "Frequence")

  return(degrees)
}

# Utilisation de la fonction avec des titres sans accents ni caractères spéciaux
n <- 100
p <- 0.05

adj_matrix_er <- generate_erdos_renyi_graph(n, p)

# Calculer les plus courts chemins
dist_matrix_er <- floyd_warshall(adj_matrix_er)

# Calculer le coefficient de clustering global et local
clustering_er <- clustering_coefficient(adj_matrix_er)
cat("Coefficient de clustering global (Erdős-Rényi) :", clustering_er$global, "\n")

# Calculer la closeness centrality pour chaque nœud
closeness_er <- closeness_centrality(dist_matrix_er)
cat("Closeness centralities (Erdős-Rényi) :\n")
print(closeness_er)

# Calculer la betweenness centrality pour chaque nœud
betweenness_er <- betweenness_centrality(adj_matrix_er)
cat("Betweenness centralities (Erdős-Rényi) :\n")
print(betweenness_er)

degrees_er <- plot_degree_distribution(adj_matrix_er, "ErdosRenyi")

n <- 100
k <- 4
p <- 0.05
adj_matrix_ws <- generate_watts_strogatz_graph(n, k, p)

# Calculer les plus courts chemins
dist_matrix_ws <- floyd_warshall(adj_matrix_ws)

# Calculer le coefficient de clustering global et local
clustering_ws <- clustering_coefficient(adj_matrix_ws)
cat("Coefficient de clustering global (watts_strogatz) :", clustering_ws$global, "\n")

# Calculer la closeness centrality pour chaque nœud
closeness_ws <- closeness_centrality(dist_matrix_ws)
cat("Closeness centralities (watts_strogatz) :\n")
print(closeness_ws)

# Calculer la betweenness centrality pour chaque nœud
betweenness_ws <- betweenness_centrality(adj_matrix_ws)
cat("Betweenness centralities (watts_strogatz) :\n")
print(betweenness_ws)

degrees_ws <- plot_degree_distribution(adj_matrix_ws, "WattsStrogatz")

n <- 100
k <- 3
m <- 2
adj_matrix_sf <- generate_scale_free_graph(n, k, m)

# Calculer les plus courts chemins
dist_matrix_sf <- floyd_warshall(adj_matrix_sf)

# Calculer le coefficient de clustering global et local
clustering_sf <- clustering_coefficient(adj_matrix_sf)
cat("Coefficient de clustering global (watts_strogatz) :", clustering_sf$global, "\n")

# Calculer la closeness centrality pour chaque nœud
closeness_sf <- closeness_centrality(dist_matrix_sf)
cat("Closeness centralities (watts_strogatz) :\n")
print(closeness_sf)

# Calculer la betweenness centrality pour chaque nœud
betweenness_sf <- betweenness_centrality(adj_matrix_sf)
cat("Betweenness centralities (watts_strogatz) :\n")
print(betweenness_sf)

degrees_sf <- plot_degree_distribution(adj_matrix_sf, "ScaleFree")

# Ajustement de la loi de puissance pour le graphe scale-free
alpha <- fit_power_law(degrees_sf)
cat("La valeur estimée de alpha pour le graphe scale-free est :", alpha, "\n")