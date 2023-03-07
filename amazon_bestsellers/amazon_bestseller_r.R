library(tidyverse)
library(ggplot2)
library(reshape2)
library(corrplot)
library(ggcorrplot)
library(dplyr)

df <- read.csv('/Users/ericluong/public_projects/amazon_bestsellers/bestsellers with categories.csv')

# check for na values
sum(is.na(df))

# summary statistics
summary(df)

# histogram of user ratings
ggplot(df, aes(x = `User.Rating`)) + 
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  xlab("User Rating") +
  ylab("Count")

# boxplot of user ratings by genre
ggplot(df, aes(x = Genre, y = `User.Rating`, fill = Genre)) + 
  geom_boxplot() +
  xlab("Genre") +
  ylab("User Rating")

# create a correlation matrix for a subset of variables in df
subset_df <- df[, c("User.Rating", "Reviews", "Price")]
corr_matrix <- cor(subset_df)

# print the correlation matrix
print(corr_matrix)

# create a correlation matrix plot for the numeric columns
corrplot(cor(subset_df), method = "color", type = "upper", tl.col = "black")



# remove any non-numeric characters and convert to integer
df$Year <- as.integer(format(as.Date(gsub("[^0-9]", "", df$Year), "%Y"), "%Y"))


# filter out rows with non-numeric values in 'Reviews' column
df_numeric <- df[!is.na(df$Reviews) & is.numeric(df$Reviews), ]


# calculate mean number of reviews per year by genre
df_mean <- df %>% 
  group_by(Year, Genre) %>% 
  summarise(mean_reviews = mean(Reviews, na.rm = TRUE))

# create line plot of mean reviews per year, colored by genre
ggplot(df_mean, aes(x = Year, y = mean_reviews, color = Genre)) +
  geom_line() +
  labs(x = "Year", y = "Mean Reviews", color = "Genre") +
  ggtitle("Mean Number of Reviews per Year by Genre")


# Group the data by year and genre
df_grouped <- df %>% group_by(Year, Genre) %>% summarise(mean_reviews = mean(Reviews))

# Pivot the data to create a table with years as rows and genres as columns
df_pivot <- df_grouped %>% pivot_wider(names_from = Genre, values_from = mean_reviews, values_fill = 0)

# Plot the grouped bar plot
df_pivot %>% ggplot(aes(x = Year)) + 
  geom_bar(aes(y = Fiction, fill = "Fiction"), stat = "identity") + 
  geom_bar(aes(y = `Non Fiction`, fill = "Non-Fiction"), stat = "identity") + 
  labs(title = "Average Book Reviews by Year and Genre", x = "Year", y = "Average Reviews") + 
  scale_fill_manual(name = "", values = c("Fiction" = "steelblue", "Non-Fiction" = "skyblue")) + 
  guides(fill = guide_legend(reverse = TRUE))













# K-means Analysis

library(tidyverse)
library(ggplot2)
library(scales)
library(gridExtra)

X <- df %>% select(`User.Rating`, Reviews, Price)

# standardize the data
#X_scaled <- scale(X)

# apply K-means clustering
k <- 3 # number of clusters to create
kmeans <- kmeans(X, centers = k, nstart = 25, iter.max = 500, algorithm = "Lloyd")

# visualize the clustering results
plot_cluster <- ggplot(X, aes(x = `User.Rating`, y = Reviews, color = factor(kmeans$cluster))) +
  geom_point() +
  labs(title = "K-Means Clustering Results", x = "User Rating", y = "Number of Reviews") +
  scale_color_discrete(name = "Cluster")
plot_cluster


# create a function to format mean values
format_mean <- function(x) {
  if (is.numeric(x)) {
    comma_format()(round(x, 2))
  } else {
    x
  }
}

# create a dataframe with the cluster centers and their counts
centers_df <- data.frame(t(kmeans$centers))
names(centers_df) <- c("Mean User Rating", "Mean Reviews", "Mean Price")
centers_df$Count <- as.vector(table(kmeans$cluster))

# display the cluster centers dataframe
centers_df

# filter the dataframe to include only books in cluster 1
cluster_1 <- df[kmeans$cluster == 1, ]

# extract the titles, authors, and genres of the books in cluster 1
titles <- cluster_1$Name
authors <- cluster_1$Author
genres <- cluster_1$Genre

# predict the cluster labels for each book
labels <- kmeans$cluster

# filter the dataframe to include only books from cluster 1
cluster_1 <- df[labels == 1, c("Name", "Author", "Genre")]
cluster_2 <- df[labels == 2, c("Name", "Author", "Genre")]
cluster_3 <- df[labels == 3, c("Name", "Author", "Genre")]

# visualize the genre distribution of books in each cluster
plot1 <- cluster_1 %>% 
  count(Genre) %>% 
  ggplot(aes(x = Genre, y = n)) +
  geom_col(fill = "skyblue", color = "black") +
  labs(title = "Genre Distribution of Books in Cluster 1", x = "Genre", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

plot2 <- cluster_2 %>% 
  count(Genre) %>% 
  ggplot(aes(x = Genre, y = n)) +
  geom_col(fill = "skyblue", color = "black") +
  labs(title = "Genre Distribution of Books in Cluster 2", x = "Genre", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

plot3 <- cluster_3 %>% 
  count(Genre) %>% 
  ggplot(aes(x = Genre, y = n)) +
  geom_col(fill = "skyblue", color = "black") +
  labs(title = "Genre Distribution of Books in Cluster 3", x = "Genre", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.05, hjust = 1))

# arrange plots in grid

grid.arrange(plot1, plot2, plot3, ncol = 2)
cluster_1                                   
cluster_2
cluster_3

# summary of clusters
kmeans$centers
table(kmeans$cluster)

# Create a bar plot for User Rating
barplot(kmeans$centers[, 'User.Rating'], main = 'K-Means Centers for User Rating', 
        xlab = 'Cluster', ylab = 'User Rating', ylim = c(0, 5), col = '#F8766D')

# Create a bar plot for Reviews
barplot(kmeans$centers[, 'Reviews'], main = 'K-Means Centers for Reviews', 
        xlab = 'Cluster', ylab = 'Reviews', ylim = c(0, 61000), col = '#00BA38')


# Create a bar plot for Price
barplot(kmeans$centers[, 'Price'], main = 'K-Means Centers for Price', 
        xlab = 'Cluster', ylab = 'Price', ylim = c(0, 20), col = '#619CFF')

