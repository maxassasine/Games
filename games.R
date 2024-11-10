# Load necessary libraries
library(dplyr)     # For data manipulation
library(ggplot2)   # For data visualization
library(forcats)   # For factor reordering
library(tidyr)     # For reshaping data
library(viridis)   # For color scales

# Read data from CSV file
df_games <- read.csv("D:/ACER UFD/games_dataset.csv")

# Clean data: remove NA values
df_games_clean <- na.omit(df_games)

# Convert 'Release.Year' to integer
df_games_clean$Release.Year <- as.integer(df_games_clean$Release.Year)

# Plot distribution of user ratings
ggplot(df_games_clean, aes(x=User.Rating)) +
  geom_histogram(bins=20, fill='blue', color='black', alpha=0.7) +
  labs(title='Distribution of User Ratings', x='User Rating', y='Frequency') +
  theme_minimal()

# Plot genre distribution
ggplot(df_games_clean, aes(x=Genre, fill=Genre)) +
  geom_bar() +
  labs(title='Genre Distribution', x='Genre', y='Count') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d()

# Plot platform distribution
ggplot(df_games_clean, aes(x=fct_infreq(Platform), fill=Platform)) +
  geom_bar() +
  labs(title='Platform Distribution', x='Platform', y='Count') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d()

# Boxplot of User Ratings by Genre
ggplot(df_games_clean, aes(x=Genre, y=User.Rating, fill=Genre)) +
  geom_boxplot() +
  labs(title='User Ratings by Genre', x='Genre', y='User Rating') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d()

# Boxplot of User Ratings by Platform
ggplot(df_games_clean, aes(x=Platform, y=User.Rating, fill=Platform)) +
  geom_boxplot() +
  labs(title='User Ratings by Platform', x='Platform', y='User Rating') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d()

# Calculate average user rating by genre
avg_rating_by_genre <- df_games_clean %>%
  group_by(Genre) %>%
  summarize(avg_rating = mean(User.Rating), .groups = "drop") %>%
  arrange(avg_rating)

# Plot average user ratings by genre
ggplot(avg_rating_by_genre, aes(x=fct_reorder(Genre, avg_rating), y=avg_rating)) +
  geom_bar(stat='identity', fill='purple', color='black') +
  labs(title='Average User Ratings by Genre', x='Genre', y='Average User Rating') +
  theme_minimal()

# Calculate average user rating by platform
avg_rating_by_platform <- df_games_clean %>%
  group_by(Platform) %>%
  summarize(avg_rating = mean(User.Rating), .groups = "drop") %>%
  arrange(avg_rating)

# Plot average user ratings by platform
ggplot(avg_rating_by_platform, aes(x=fct_reorder(Platform, avg_rating), y=avg_rating)) +
  geom_bar(stat='identity', fill='orange', color='black') +
  labs(title='Average User Ratings by Platform', x='Platform', y='Average User Rating') +
  theme_minimal()

# Plot trends in number of releases per year
ggplot(df_games_clean, aes(x=Release.Year)) +
  geom_bar() +
  labs(title='Trends in Number of Game Releases Per Year', x='Release Year', y='Number of Releases') +
  theme_minimal()

# KDE plot of User Ratings by Genre
ggplot(df_games_clean, aes(x=User.Rating, fill=Genre)) +
  geom_density(alpha=0.7) +
  labs(title='Density Plot of User Ratings by Genre', x='User Rating', y='Density') +
  theme_minimal()

# Line plot of average user ratings over time (by Release Year)
avg_rating_over_time <- df_games_clean %>%
  group_by(Release.Year) %>%
  summarize(avg_rating = mean(User.Rating), .groups = "drop")

ggplot(avg_rating_over_time, aes(x=Release.Year, y=avg_rating)) +
  geom_line() +
  labs(title='Average User Ratings Over Time', x='Release Year', y='Average User Rating') +
  theme_minimal()

# Create a table for analysis of platform and genre pairs
platform_genre_counts <- df_games_clean %>%
  group_by(Platform, Genre) %>%
  summarize(count = n(), .groups = "drop")  # Summarize count per platform-genre pair

# Plot using geom_tile for a heatmap of platform and genre pairs
ggplot(platform_genre_counts, aes(x = Platform, y = Genre, fill = count)) +
  geom_tile(color = "white") +
  labs(title = 'Platform and Genre Pair Analysis', x = 'Platform', y = 'Genre', fill = 'Count') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_c()  # Continuous color scale for count
