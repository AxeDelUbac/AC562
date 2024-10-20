# R code for generating and analyzing random graphs

# 1. Load necessary libraries
library(igraph)

# 2. Functions to generate graphs
generate_erdos_renyi_graph <- function(n, p) {
  return(sample_gnp(n, p, directed = FALSE))
}

generate_watts_strogatz_graph <- function(n, m, p) {
  return(sample_smallworld(1, n, 2 * m, p))
}

generate_scale_free_graph <- function(n, m) {
  return(sample_pa(n, m = m, directed = FALSE))
}

# 3. Generate graphs
n <- 1000

# Erdös-Rényi graphs for p from 0 to 1 with step of 0.05
erdos_renyi_graphs <- lapply(seq(0, 1, by = 0.05), function(p) generate_erdos_renyi_graph(n, p))

# Watts-Strogatz small-world graph with p=0.1 and m=2
watts_strogatz_graph <- generate_watts_strogatz_graph(n, m = 2, p = 0.1)

# Scale-free graph with k=3 and m=2
scale_free_graph <- generate_scale_free_graph(n, m = 2)

plot_graph <- function(graph, title) {
  plot(graph, main = title, vertex.size = 3, vertex.label = NA, edge.arrow.size = 0.5)
}


#plot_graph(erdos_renyi_graphs[[11]], "Erdos-Renyi Graph (p=0.5)")
#plot_graph(watts_strogatz_graph, "Watts-Strogatz Graph (p=0.1, m=2)")
#plot_graph(scale_free_graph, "Scale-Free Graph (m=2)")


# 4. Plot degree distributions
plot_degree_distribution <- function(graph, title) {
  degrees <- degree(graph)
  # Remplacement des accents par des caractères sans accents
  title <- iconv(title, to = "ASCII//TRANSLIT")
  hist(degrees, breaks = 30, main = title, xlab = "Degree", ylab = "Frequency", col = "skyblue")
}

# Plotting degree distributions
plot_degree_distribution(erdos_renyi_graphs[[11]], "Erdos-Renyi Graph Degree Distribution (p=Erdös-Rényi Graph Degree Distribution (p=0.5))")
plot_degree_distribution(watts_strogatz_graph, "Watts-Strogatz Graph Degree Distribution (p=Watts-Strogatz Graph Degree Distribution (p=0.1, m=2))")
plot_degree_distribution(scale_free_graph, "Scale-Free Graph Degree Distribution (m=Scale-Free Graph Degree Distribution (m=2))")

# 5. Show that scale-free graph follows a power law distribution
degrees <- degree(scale_free_graph)
fit <- power.law.fit(degrees)
alpha <- fit$alpha
cat("Estimated alpha for scale-free graph:", alpha, "\n")

# 6. Calculate average length and clustering coefficient for Erdös-Rényi
calculate_average_length <- function(graph) {
  if (is.connected(graph)) {
    return(average.path.length(graph))
  } else {
    return(Inf)  # Graph is not connected
  }
}

calculate_clustering_coefficient <- function(graph) {
  return(transitivity(graph, type = "average"))
}

average_lengths <- sapply(erdos_renyi_graphs, calculate_average_length)
clustering_coefficients <- sapply(erdos_renyi_graphs, calculate_clustering_coefficient)

# Plot average length and clustering coefficient as a function of p
p_values <- seq(0, 1, by = 0.05)

plot(p_values, average_lengths, type = "o", col = "blue", xlab = "Probability p", ylab = "Average Length",
     main = "Average Length vs Probability for Erdos-Renyi Graphs")

plot(p_values, clustering_coefficients, type = "o", col = "red", xlab = "Probability p", ylab = "Clustering Coefficient",
     main = "Clustering Coefficient vs Probability for Erdos-Renyi Graphs")


# 7. Compute clustering coefficient, closeness, betweenness
analyze_graph <- function(graph, name) {
  clustering_coefficient <- transitivity(graph, type = "average")
  closeness <- closeness(graph)
  betweenness <- betweenness(graph)
  hubs <- head(sort(degree(graph), decreasing = TRUE), 5)
  cat("\n", name, "Analysis:\n")
  cat("Clustering Coefficient:", clustering_coefficient, "\n")
  cat("Top 5 Hubs:", hubs, "\n")
}

# Analyze graphs
analyze_graph(erdos_renyi_graphs[[11]], "Erdös-Rényi Graph (p=0.5)")
analyze_graph(watts_strogatz_graph, "Watts-Strogatz Graph (p=0.1, m=2)")
analyze_graph(scale_free_graph, "Scale-Free Graph (m=2)")
