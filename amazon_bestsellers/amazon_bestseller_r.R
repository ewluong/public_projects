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


# Create scatterplot with linear regression line of Reviews vs User Rating
ggplot(df, aes(x = `User.Rating`, y = Reviews)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "User Rating", y = "Number of Reviews") +
  ggtitle("Correlation Between User Rating and Number of Reviews") +
  theme_bw()

# Fit a linear regression model for User.Rating vs Reviews
model = lm(User.Rating ~ Reviews, data = df)
summary(model)

# Regression analysis shows that there is not much correlation between the two

# Group the data by author and calculate the mean user rating for each author
author_reviews <- df %>%
  group_by(Author) %>%
  summarize(mean_reviews = mean(Reviews, na.rm = TRUE)) %>%
  arrange(desc(mean_reviews))

# Select the top 10 authors with the highest mean rating
top_authors <- head(author_reviews, 10)
top_authors <- arrange(desc(top_authors))

# Create a bar chart of the top authors by mean rating
ggplot(top_authors, aes(x = reorder(Author, -mean_reviews), y = mean_reviews)) +
  geom_bar(stat = "identity", fill = "#00BFC4") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Top Authors by Average # of Reviews") +
  xlab("Author") + ylab("Average Reviews")












# NLP Top Words in Titles
library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyverse)
library(SnowballC)
library(tm)
library(wordStem)
library(dplyr)
library(stringr)
library(tm)

# Create a new dataframe with title and reviews columns
df_title <- df %>% select(Name, Reviews)

# Preprocess the book titles by removing stop words and stemming the remaining words
preprocess_text <- function(text) {
  words <- str_split(tolower(text), "\\s+")[[1]]
  words <- words[!words %in% stopwords('english')]
  words <- wordStem(words, language = 'english')
  return(paste(words, collapse = " "))
}

df_title$Title_Processed <- sapply(df_title$Name, preprocess_text)

# Use DocumentTermMatrix to convert the preprocessed text into a bag-of-words representation
corpus <- Corpus(VectorSource(df_title$Title_Processed))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, stemDocument)

# Remove empty documents from the corpus
corpus <- corpus[!(corpus == "")]

# Create the document-term matrix
dtm <- DocumentTermMatrix(corpus)



X <- as.data.frame(as.matrix(dtm))

# Train a linear regression model on the bag-of-words features and the number of reviews
model <- lm(Reviews ~ ., data = cbind(df_title['Reviews'], X))

# Get the coefficients of the trained model and map them back to the vocabulary
coefficients <- data.frame(Coefficient = coef(model)[-1])
rownames(coefficients) <- colnames(X)
coefficients <- coefficients %>%
  mutate(word = rownames(coefficients)) %>%
  arrange(desc(abs(Coefficient))) %>%
  select(word, Coefficient)


# Print the top 10 words or phrases with the highest coefficients
head(coefficients, 10)

# Plot the top 10 words or phrases with the highest coefficients
library(ggplot2)
library(tidyr)
class(coefficients)
top_words <- data.frame(Word = rownames(head(coefficients, 30)), Coefficient = head(coefficients, 30)[,1], row.names = NULL)

top_words <- head(coefficients, 30) %>% rownames_to_column("Word")
top_words <- top_words %>% gather(key = "Category", value = "Value", -Word)

ggplot(top_words, aes(x = Word, y = Value, fill = Category)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Top 10 Words or Phrases with the Highest Coefficients", x = "Word or Phrase", y = "Coefficient")
















# K-means Analysis

library(tidyverse)
library(ggplot2)
library(scales)
library(gridExtra)

X <- df %>% select(`User.Rating`, Reviews, Price)


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

#Based on this finding, we can tell the publisher that high user ratings do not necessarily translate 
#to high sales, and that it may be more important to target books that have a broader appeal and are 
#more likely to receive a large number of reviews and sales, even if they do not have the highest user
#ratings. However, it may also be worth considering ways to increase the visibility and marketing of 
#books in the high rating cluster to boost their sales potential.



# elbow plot to find ideal k-clusters

# Apply K-means clustering for different values of k
wcss <- vector(mode = "numeric", length = 10)
for (i in 1:10) {
  kmeans <- kmeans(X, centers = i, nstart = 25, iter.max = 500, algorithm = "Lloyd")
  wcss[i] <- kmeans$tot.withinss
}

# Plot the WCSS values against the number of clusters (k)
ggplot(data.frame(k = 1:10, wcss = wcss), aes(x = k, y = wcss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  labs(title = "Elbow Plot for K-Means Clustering", x = "Number of Clusters (k)", y = "Within-Cluster Sum of Squares (WCSS)")


# ideal number of clusters is 3-4 clusters according to the elbow plot using the WCSS (within-cluster sum squares) method
