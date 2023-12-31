# Spotify Song Data PCA Analysis


This project aims to perform Principal Component Analysis (PCA) on a dataset of 2000 Spotify songs to find a low-dimensional representation of the data that captures variability as much as possible, and to visualize the most important components in a 2D plot. The R programming language is used for the analysis.

## Please note: The file PCA Spotify.pdf contains an in-depth explanation for this project. 

## Table of Contents

1. [Data Splitting](#data-splitting)
2. [PCA Analysis](#pca-analysis)
3. [PCA Visualization](#pca-visualization)
4. [Special Artist Analysis](#special-artist-analysis)

## Data Splitting

The dataset is divided into two parts:

1. Labels: Contains the song title and artist.
2. Variables: Contains features like acousticness, danceability, duration, energy, etc.

```R
spotify_labels <- spotify_data[, 1:2]
spotify_vars <- spotify_data[, 3:11]
```

## PCA Analysis

Principal Component Analysis is performed on the scaled variables.

```R
pca <- prcomp(spotify_vars, scale = TRUE)
```

## PCA Visualization

The `ggfortify` package is used to visualize the principal components.

```R
library(ggfortify)
autoplot(pca, data = spotify_vars, loadings = TRUE, loadings.label = TRUE)
```

## Special Artist Analysis

PCA plots are generated to highlight songs by specific artists and titles.

1. Songs by Drake
2. Songs by Arcade Fire


```R
autoplot(pca, data = spotify_vars,  loadings = TRUE, col=ifelse(spotify_labels$artist=="Drake","blue","transparent"),  loadings.label = TRUE)
autoplot(pca, data = spotify_vars,  loadings = TRUE, col=ifelse(spotify_labels$artist=="Arcade Fire","blue","transparent"),  loadings.label = TRUE)
```

## License

MIT License
