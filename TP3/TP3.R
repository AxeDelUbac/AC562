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

vote <- function(graphG,epsilon,t){
  n=5
  Si <-rbinom(n,1, 0.5)
  print("Si est:")
  print(Si)
  probabilite <- seq(0, n, by = 1)
  for (i in 1:n){
    probabilite[i] <-(1/degree[i])*Si[i]

    probabilite[i] <-(1-2*epsilon)*probalilite+epsilon
  }

  adj_matrice_evoluer <- as_adjacency_matrix(graphG,sparse = FALSE)

  print(adj_matrice_evoluer)
  return(adj_matrice_evoluer)
}

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
#new_adj_matrice <- vote(graph,0.5,10)

p<-0.05
n<-100

graphErdosRenyi <- erdos.renyi.game(n,p,type="gnp",directed=FALSE,FALSE)
plot(graphErdosRenyi)
dim<-1
k<-3
p<-0.01
graphWattsStrogatz <- sample_smallworld(dim,n,k,p,FALSE,FALSE)
plot(graphWattsStrogatz)
t <-seq(0,10, by = 1)


