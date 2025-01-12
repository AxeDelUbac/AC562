library(igraph)

#q1
Vote_matrix<-function(graphG,epsilon,time){
  nb_votant <- vcount(graphG)
  liste_degres <- degree(graphG)
  matrice_adjacence <- as_adjacency_matrix(graphG)

  VOTEMATRIX <- matrix(nrow = nb_votant, ncol = time, byrow = FALSE)
  #écriturre de la première colonne de la matrice
  #les n premiers votants vote pour 0 et les autres pour 1
  median<-nb_votant/2
  for (k in 1:median){
    VOTEMATRIX[k,1] <- 0
  }
  for (k in median+1:median){
    VOTEMATRIX[k,1] <- 1
  }

  #a chaque période on réecris une colone de la matrice VOTEMATRIX
  for (t in 2:time){
    # calcul des votants à la période t:
    for (v in 1:nb_votant)
    {
      nb_voisin <- liste_degres[v]
      #la somme des votes des voisins est :
      sommeVoteVoisin<-0
      for (i in 1:nrow(matrice_adjacence)) {
        if(matrice_adjacence[v,i]==1){
         sommeVoteVoisin<-sommeVoteVoisin + VOTEMATRIX[i,t-1]
        }
      }
      p<-(1/nb_voisin)*sommeVoteVoisin
      Pvote <- (1-2*epsilon)*p+epsilon
      if (Pvote >1)
      {
        print("AAAAAAAAAAAHHHHHHHHHHHHHHHHHHHH!!!!!!!!!!!!!!!")
      }
      resultat<-rbinom(1, size = 1, prob = Pvote)
      if (is.na(resultat))
      {
        print("fait chieeeeeeeeeeeeeeeeeerrrrrrrrrrrrrrrrrrrrrrrrr!!!!!")
      }
      VOTEMATRIX [v,t]<-resultat
    }
    print (t)
  }
  return (VOTEMATRIX)
}

#q2
time <-1
epsilon<-0.01

n <- 100
p <- 0.05
graphErdosRenyi <-sample_gnp(n, p, directed = FALSE)
plot(graphErdosRenyi , vertex.size=10, vertex.color="skyblue", edge.color="gray", main="Graphe Erdos-Renyi")
VOTEMATRIX_ERDOSRENYI<-Vote_matrix(graphErdosRenyi,epsilon,time)
#représenter l'évolution du taux de votes pour le candidat 1:
nbvote1<-matrix(0,nrow = 1, ncol = time, byrow = FALSE)

for (x in 1:time){
  for (y in 1:nrow(VOTEMATRIX_ERDOSRENYI)){
    if (is.na(VOTEMATRIX_ERDOSRENYI[y,x]))
    {
      print ("are you sure bro!!")
    }
    nbvote1[1,x]<-nbvote1[1,x]+VOTEMATRIX_ERDOSRENYI[y,x]
  }
}
x <- 1:time
y <- nbvote1;
plot(x, y, type = "l", col = "blue", lwd = 2, main = "Graphe Erdos-Renyi evolution du vote 1", xlab = "time", ylab = "votant")


dim<-1
k<-3
p<-0.01
graphWattsStrogatz <- sample_smallworld(dim,n,k,p,FALSE,FALSE)
plot(graphWattsStrogatz , vertex.size=10, vertex.color="skyblue", edge.color="gray", main="Graphe Watts-Strogatz")
t <-seq(0,10, by = 1)

VOTEMATRIX_WATTSTROGATS<-Vote_matrix(graphErdosRenyi,epsilon,time)
#représenter l'évolution du taux de votes pour le candidat 1:
nbvote2<-matrix(0,nrow = 1, ncol = time, byrow = FALSE)

for (x in 1:time){
  for (y in 1:nrow(VOTEMATRIX_WATTSTROGATS)){
    if (is.na(VOTEMATRIX_WATTSTROGATS[y,x]))
    {
      print ("are you sure bro!!")
    }
    nbvote2[1,x]<-nbvote2[1,x]+VOTEMATRIX_WATTSTROGATS[y,x]
  }
}
x <- 1:time
y <- nbvote2;
plot(x, y, type = "l", col = "blue", lwd = 2, main = "Graphe Watts strogatz évolution du vote 1", xlab = "time", ylab = "votant")
