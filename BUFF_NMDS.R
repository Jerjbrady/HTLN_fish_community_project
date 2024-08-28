#### NMDS Reach data #####
install.packages("ggforce")
library(vegan)
library(cluster)
library(ggplot2)
library(ggforce)
library(dbplyr)
fish_matrix <- read.csv("BUFF_fish_data_pivot.csv")

rownames(fish_matrix) <- fish_matrix$X

df <- fish_matrix[,2:71]

ord <- metaMDS(df)

ord

plot(ord, display = "sites", type = "text")
text(ord, display = "sites", labels = rownames(fish_matrix), cex = 0.7)

 ### stress  <  .05 provides an excellent representation in reduced dimensions, stress < .1 great; stress < .2 good; stress < .3 poor
# Extract NMDS coordinates for sites
site_scores <- scores(ord, display = "sites")

# Extract NMDS coordinates for species
species_scores <- scores(ord, display = "species")


# Perform hierarchical clustering on the NMDS coordinates
hc <- hclust(dist(site_scores), method = "ward.D2")

# Cut the tree into a desired number of clusters (e.g., 3 clusters)
clusters <- cutree(hc, k = 6)

# Convert clusters to a factor for plotting
clusters <- as.factor(clusters)

# Create a data frame for site scores
nmds_site_data <- data.frame(NMDS1 = site_scores[, 1], NMDS2 = site_scores[, 2], 
                             Cluster = clusters, Label = rownames(site_scores))

# Create a data frame for species scores
nmds_species_data <- data.frame(NMDS1 = species_scores[, 1], NMDS2 = species_scores[, 2], 
                                Species = rownames(species_scores))


# Plot NMDS with site labels and species names
ggplot() +
  # Plot site points and hulls
  geom_point(data = nmds_site_data, aes(x = NMDS1, y = NMDS2, color = Cluster), size = 3) +
  geom_mark_hull(data = nmds_site_data, aes(x = NMDS1, y = NMDS2, group = Cluster, fill = Cluster), 
                 concavity = 10, alpha = 0.2) +
  geom_text(data = nmds_site_data, aes(x = NMDS1, y = NMDS2, label = Label), size = 3, vjust = 1.5) +
  # Plot species names
  geom_text(data = nmds_species_data, aes(x = NMDS1, y = NMDS2, label = Species), 
            color = "black", size = 3, vjust = -1, hjust = 0.5) +
  theme_minimal() +
  labs(title = "NMDS with Clusters and Hulls",
       x = "NMDS1",
       y = "NMDS2")

  
