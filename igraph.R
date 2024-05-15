library(igraph)
data<-read.csv("C:/Users/TI/Desktop/iris.csv")
data<-na.omit(data)
# unique species
species <- unique(data$variety)
# Create an empty undirected graph
g <- graph.empty(directed = FALSE)
plot(g)
# Add vertices for each unique species
g <- add_vertices(g, nv = length(species))
V(g)$name <- species  # Set the name attribute to the species names
plot(g)
# Add edges between species based on correlation of petal lengths
for (i in 1:(length(species)-1)) {
  for (j in (i+1):length(species)) {
    # Subset data for each species pair
    subset_data <- data[data$variety %in% c(species[i], species[j]), ]
    # Calculate correlation between petal length for the subset
    correlation <- cor(subset_data$petal.length, subset_data$petal.width)
    # Add edge if correlation is significant
    if (abs(correlation) > 0.5) {
      g <- add_edges(g, c(i, j))
    }
  }
}

# Plot the graph
plot(g, vertex.label.dist = 2, vertex.label.color = "black")

# Assuming you have already created the graph `g` with vertices representing species and edges representing correlations between petal lengths

# Calculate the degree centrality
degree <- degree(g)

# Define a threshold for considering highly connected species
threshold <- quantile(degree, 0.75)  # For example, you can use the 75th percentile

# Get the names of highly connected species
highly_connected_species <- V(g)$name[which(degree >= threshold)]

# Plot the graph highlighting highly connected species
plot(g, vertex.label.dist = 2, vertex.label.color = "black")
highlight <- ifelse(V(g)$name %in% highly_connected_species, "red", "gray")  # Highlight highly connected species in red
plot(g, vertex.label.dist = 2, vertex.label.color = "black", vertex.color = highlight)


# Assuming you have already calculated the highly connected species and stored them in the variable `highly_connected_species`

# Find the indices of highly connected species in the graph
highly_connected_indices <- which(V(g)$name %in% highly_connected_species)

# Create a subgraph containing only highly connected species and their edges
subgraph <- subgraph.edges(g, E(g)[.from(highly_connected_indices) | .to(highly_connected_indices)])

# Plot the subgraph
plot(subgraph, vertex.label.dist = 2, vertex.label.color = "black")

# Plot the original graph with a caption
plot(g, vertex.label.dist = 2, vertex.label.color = "black")
title(main = "Original Graph: Species Correlation Network")

# Plot the subgraph with highly connected species and a caption
plot(subgraph, vertex.label.dist = 2, vertex.label.color = "black")
title(main = "Subgraph: Highly Connected Species")
plot(graph(species),isolates=c("Versicolor" ,"Tulip"   ,   "Orchid" ,    "Rose" ,'lilly'))




#------------------
library(igraph)

# Read the dataset
data <- read.csv("C:/Users/TI/Desktop/iris.csv")
data <- na.omit(data)

# Calculate the mean petal length for each species
mean_petal_length <- tapply(data$petal.width, data$variety, mean)

# Create an empty undirected graph
g <- graph.empty(directed = FALSE)

# Add vertices for each unique species
g <- add_vertices(g, nv = length(unique(data$variety)))
V(g)$name <- unique(data$variety)  # Set the name attribute to the species names

# Add central node representing the entire dataset
g <- add_vertices(g, nv = 1)
V(g)$name[length(V(g))] <- "Iris Dataset"

# Connect central node to species nodes based on petal length similarity
for (i in 1:length(unique(data$variety))) {
  weight <- abs(mean_petal_length[i] - mean(data$petal.length))
  g <- add_edges(g, c(length(V(g)), i), weight = weight)
}
g <- make_star(length(mean_petal_width), center = 1, mode = "undirected")

# Plot the graph
plot(g,  vertex.label=V(eg)$name,main = "Star Graph of Iris Dataset Based on Petal width" , sub='Connect central node to species nodes based on petal length similarity')


#----tree graph
# Read the dataset
data <- read.csv("C:/Users/TI/Desktop/iris.csv")
data <- na.omit(data)
# Calculate mean petal length for each species
mean_petal_length <- tapply(data$petal.length, data$variety, mean)

# Sort species based on mean petal length
sorted_species <- names(sort(mean_petal_length))

# Create an empty directed graph
tree_graph <- graph.empty(directed = TRUE)

# Add vertices for each species
tree_graph <- add_vertices(tree_graph, nv = length(sorted_species))
V(tree_graph)$name <- sorted_species

# Group species with similar petal lengths together
# In this example, we'll connect each species to the one with the closest mean petal length
for (i in 2:length(sorted_species)) {
  closest_species <- sorted_species[i-1]
  tree_graph <- add_edges(tree_graph, c(which(V(tree_graph)$name == closest_species), i))
}

# Plot the tree graph with hierarchical layout
plot(tree_graph, vertex.size = 10, vertex.label.cex = 1, 
     edge.arrow.size = 0.2, edge.curved = 0.2, 
     layout = layout_as_tree,
     main = "Tree Graph Based on Petal Length (Similar Petals at the Top)")


