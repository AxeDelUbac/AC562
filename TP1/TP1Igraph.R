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

graph <- createGraph(adj_matrice)

# Compute graph metrics
# 1. Degree
degrees <- degree(graph)
print("Degrees:")
print(degrees)

# 2. Clustering coefficient
clustering_coeff <- transitivity(graph, type = "local")
print("Clustering Coefficients:")
print(clustering_coeff)

# 3. Closeness centrality
closeness_cent <- closeness(graph)
print("Closeness Centrality:")
print(closeness_cent)

# 4. Betweenness centrality
betweenness_cent <- betweenness(graph)
print("Betweenness Centrality:")
print(betweenness_cent)

# Plot graph with node size and color based on metrics
plot(graph,
     vertex.size = degrees * 5 + 10,
     vertex.label = V(graph)$name,
     main = "Graph with Node Size on Degree")