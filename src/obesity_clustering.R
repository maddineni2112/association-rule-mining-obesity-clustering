# =========================
# Loading Required Libraries
# =========================

library(dplyr) # Data manipulation
library(cluster) # Clustering and evaluation
library(ggplot2) # Visualization
library(factoextra) # Visualization


# =========================
# Task 1(b): Pre-processing for KMeans
# =========================

# Read the dataset
obesity <- read.csv("Obesity.csv", stringsAsFactors = FALSE)

# Inspect structure
str(obesity)
summary(obesity)

# -------------------------
# Check for missing values
# -------------------------
colSums(is.na(obesity))

# removing the duplicate rows if any
#but as we not found any in above check, we can skip this test.
obesity <- unique(obesity)

# -------------------------
# Convert ordinal string variables to numeric scales
# -------------------------

# CALC is ordinal:
# no < Sometimes < Frequently < Always
obesity$CALC <- factor(
  obesity$CALC,
  levels = c("no", "Sometimes", "Frequently", "Always"),
  ordered = TRUE
)
obesity$CALC <- as.numeric(obesity$CALC)

# NObeyesdad is ordinal:
# Insufficient_Weight < Normal_Weight < Overweight < Obese
obesity$NObeyesdad <- factor(
  obesity$NObeyesdad,
  levels = c("Insufficient_Weight", "Normal_Weight", "Overweight", "Obese"),
  ordered = TRUE
)
obesity$NObeyesdad <- as.numeric(obesity$NObeyesdad)

# -------------------------
#  Ensure all remaining variables are numeric
# -------------------------
obesity$Age    <- as.numeric(obesity$Age)
obesity$Height <- as.numeric(obesity$Height)
obesity$Weight <- as.numeric(obesity$Weight)
obesity$FCVC   <- as.numeric(obesity$FCVC)
obesity$NCP    <- as.numeric(obesity$NCP)
obesity$CH2O   <- as.numeric(obesity$CH2O)
obesity$FAF    <- as.numeric(obesity$FAF)
obesity$TUE    <- as.numeric(obesity$TUE)

# -------------------------
# Check that all columns are numeric after conversion
# -------------------------
str(obesity)

# -------------------------
# Standardize variables for KMeans
# ( this step is most important step because KMeans uses Euclidean distance)
# -------------------------
obesity_scaled <- scale(obesity)

# -------------------------
# Final preprocessed dataset for clustering
# -------------------------
obesity_kmeans_data <- as.data.frame(obesity_scaled)

# Inspect final data
summary(obesity_kmeans_data)
head(obesity_kmeans_data)

# =========================
# Task 2(a): KMeans with K = 2
# =========================

set.seed(123)

# Apply KMeans with K = 2
kmeans_k2 <- kmeans(obesity_kmeans_data, centers = 2, nstart = 25)

# -------------------------
# Cluster summary
# -------------------------

# Cluster membership for each observation
obesity$Cluster <- as.factor(kmeans_k2$cluster)

# Number of observations in each cluster
cat("Cluster sizes:\n")
print(kmeans_k2$size)

# Cluster centers using standardized data
cat("\nCluster centers (standardized data):\n")
print(kmeans_k2$centers)

# Cluster summary using original data
cat("\nCluster summary using original variables:\n")
cluster_summary <- obesity %>%
  group_by(Cluster) %>%
  summarise(
    Count = n(),
    Age = mean(Age),
    Height = mean(Height),
    Weight = mean(Weight),
    FCVC = mean(FCVC),
    NCP = mean(NCP),
    CH2O = mean(CH2O),
    FAF = mean(FAF),
    TUE = mean(TUE),
    CALC = mean(CALC),
    NObeyesdad = mean(NObeyesdad)
  )

print(cluster_summary)

# -------------------------
# Silhouette measure
# -------------------------

# Distance matrix
dist_matrix <- dist(obesity_kmeans_data)

# Silhouette values
sil_k2 <- silhouette(kmeans_k2$cluster, dist_matrix)

# Print silhouette details
cat("\nSilhouette summary for K = 2:\n")
print(summary(sil_k2))

# Average silhouette width
avg_sil_k2 <- mean(sil_k2[, 3])
cat("\nAverage silhouette width for K = 2:", avg_sil_k2, "\n")

# =========================
# Task 2(b): Tune K using silhouette
# =========================

set.seed(123)

# Choose a reasonable upper limit for K
# Here I use 10 because it is large enough to compare several cluster options
# while still remaining interpretable
k_values <- 2:13

# Store average silhouette values
avg_sil_values <- numeric(length(k_values))

# Calculate silhouette for each K
for (i in seq_along(k_values)) {
  k <- k_values[i]
  
  km_model <- kmeans(obesity_kmeans_data, centers = k, nstart = 25)
  sil <- silhouette(km_model$cluster, dist(obesity_kmeans_data))
  avg_sil_values[i] <- mean(sil[, 3])
}

# Create results table
silhouette_results <- data.frame(
  K = k_values,
  Avg_Silhouette = avg_sil_values
)

cat("Average silhouette values for different K:\n")
print(silhouette_results)

# Find optimal K
optimal_k <- silhouette_results$K[which.max(silhouette_results$Avg_Silhouette)]
cat("\nOptimal K based on maximum average silhouette width:", optimal_k, "\n")

# -------------------------
# Plot average silhouette vs K
# -------------------------
ggplot(silhouette_results, aes(x = K, y = Avg_Silhouette)) +
  geom_line() +
  geom_point(size = 2) +
  labs(
    title = "Average Silhouette Width vs Number of Clusters",
    x = "Number of Clusters (K)",
    y = "Average Silhouette Width"
  ) +
  theme_minimal()

set.seed(123)
kmeans_opt <- kmeans(obesity_kmeans_data, centers = optimal_k, nstart = 25)

# Visualize clusters
fviz_cluster(
  kmeans_opt,
  data = obesity_kmeans_data,
  geom = "point",
  ellipse.type = "convex",
  palette = "jco",
  ggtheme = theme_minimal(),
  main = paste("Cluster Visualization (K =", optimal_k, ")")
)

# =========================
# Task 3(a): Feature contribution plot
# =========================

# Apply KMeans with optimal K (K = 2)
set.seed(123)
kmeans_opt <- kmeans(obesity_kmeans_data, centers = 2, nstart = 25)

# Add cluster labels
obesity_kmeans_data$Cluster <- as.factor(kmeans_opt$cluster)

# Compute mean values of each feature per cluster
library(reshape2)

cluster_means <- obesity_kmeans_data %>%
  group_by(Cluster) %>%
  summarise_all(mean)

# Convert to long format for plotting
cluster_means_long <- melt(cluster_means, id.vars = "Cluster")

# Plot feature contributions
ggplot(cluster_means_long, aes(x = variable, y = value, fill = Cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Feature Contribution by Cluster",
    x = "Features",
    y = "Average Standardized Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
