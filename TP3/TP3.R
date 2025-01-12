
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
  n<-5
  Srr:lkm
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

#graph <- createGraph(adj_matrice)
#new_adj_matrice <- vote(graph,0.5,10)

p<-0.05
n<-100

#q2

tirage_bernoulli <- function(p) {
  resultat <- rbinom(1, 1, p)
  #print (paste("le vote est ",resutat))
  return(resultat)
}

taux_vote <- function(G,nombre_cycle,p) {
  matrice_adj <- as_adjacency_matrix(G)
  count1voter<-0
  nbit <- 0
  nbit <- nbit+2
  for (i in seq_along(V(G))){
    val<- runif(1)
    V(G)$etat[i] <- val
  }
  table1<-c()
  degres <- degree(G)
  table0<-c()
  totalcountervote1<- c()
  e <- 0.01
  for (t in 0:nombre_cycle){
    count1voter<-0
    for (i in seq_along(V(G))){
      N <- degres[i]
      somme<- 0
      for (v in neighbors(G, i)){
        somme <-somme+V(G)$etat[v]
      }
      pdens<-1/N *somme
      f <- (1-2*e)*pdens+e
      vote<-tirage_bernoulli(f)
      while (is.na(vote)){
        vote<-tirage_bernoulli(f)
      }
      if (vote == TRUE){
        table1 <- c(table1,i)
        count1voter<-count1voter+1
      }
      else{
        table0 <- c(table0,i)
      }
    }
    for (c in table0){
      V(G)$etat[c]<-0
    }
    for (c in table1){
      V(G)$etat[c]<-1
    }
    totalcountervote1<- c(totalcountervote1,count1voter)
    nbit<-nbit+1
    print (paste("nbit",nbit))
    print ("processing")
  }
  x_vals <- seq(0, nombre_cycle-1, by = 1)
  
  y_vals <-head (totalcountervote1,nombre_cycle)
  print (length(x_vals))
  print (length(y_vals))
  plot(y_vals, x_vals, col = "red", lwd = 2, main = "Plot of f(x) = sin(x)", xlab = "x", ylab = "f(x)")

}

graphErdosRenyi <-sample_gnp(n, p, directed = FALSE)
plot(graphErdosRenyi , vertex.size=10, vertex.color="skyblue", edge.color="gray", main="Graphe Erdos-Renyi")
dim<-1
k<-3
p<-0.01
taux_vote(graphErdosRenyi,20,p)
graphWattsStrogatz <- sample_smallworld(dim,n,k,p,FALSE,FALSE)
plot(graphWattsStrogatz , vertex.size=10, vertex.color="skyblue", edge.color="gray", main="Graphe Watts-Strogatz")
t <-seq(0,10, by = 1)

#fonction de l'évolution du taux de vote




#q3
n <- 100
Robert <- 0
Mickey <- 1

#G <- barabasi.game(n, m = m, directed = FALSE)


