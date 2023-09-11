
# IMDb Movie Ratings Prediction

## Table of Contents

1. [Introduction](#introduction)
2. [Data Description](#data-description)
3. [Installation](#installation)
4. [Usage](#usage)
5. [Model](#model)
6. [Results](#results)
7. [Limitations and Future Work](#limitations-and-future-work)
8. [License](#license)

## Introduction

This project aims to predict IMDb ratings for upcoming movies. The prediction model is trained on a dataset containing thousands of movies that have already been rated on IMDb. The model takes into account various features like movie budget, duration, genre, and so on, to make the prediction. The objective is to build a simple yet effective model that can make reasonably accurate predictions.

## Data Description

The dataset contains information on movies, including but not limited to:

- Movie Title
- IMDb Score (Target Variable)
- Movie Budget
- Release Date (Day, Month, Year)
- Duration
- Language
- Country
- Maturity Rating
- Aspect Ratio
- Director
- Main Actors
- Genre

For more details on the dataset, please refer to the data dictionary provided.

## Installation

To get started, you'll need to install the following R packages:

\```
install.packages("car")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("psych")
install.packages("corrplot")
install.packages("mlbench")
install.packages("caret")
install.packages("boot")
install.packages("lmtest")
install.packages("plm")
install.packages("stargazer")
\```

## Usage

Run the R script `imdb_prediction_final.r` to execute the project.

## Model

The project uses a polynomial regression model to predict IMDb ratings. The model was selected based on its ability to provide the highest R-squared value while considering factors like heteroskedasticity, collinearity, and overfitting/underfitting.

## Results

The final model has an R-squared value of 0.41 and an MSE of 0.71 on the test set. The model was validated using various techniques, including p-value analysis for feature selection, NCV tests for non-linearity and heteroskedasticity, and VIF for collinearity.

## Limitations and Future Work

The model can be improved in several ways:

- Incorporating additional data sources like social media sentiment and critics reviews.
- Experimenting with more advanced machine learning models.
- Investigating interaction terms and splines.

## License

This project is licensed under the MIT License

## Contributors

This project was made possible through team work by Sakshi Kumar, Peter-Heglund Lohman and Kabir Nain.
