# Project 4: Movie Recommender App

In this app, we have given users two options to find the recommended movies. 
1. Search by Genre: In this option, user picks up a Genre and we apply the algorim to find the five top popular movies. In this algorithm, we first filter out those movies which have less than 1000 reviews. Among the remaining movies, we find the top five movies based on the average rating.
2. Search by Rating: In this option, user needs to first ranks some movies from the given collection and we then apply the user based collaborative filtering (UBCF) algorithm to predict the top five movies to recommend to users. For this algorithm, we normalize the ratings by "z-score" algorithm. We are using the neighborhood size of 25 and the distance is using the cosine similarity measure. 

The app is published at https://aknemani.shinyapps.io/FindMoviesYouLike/

Note: There may be many functions in the code which are not directly used as they were written for extension of the app to give more flexibility in the future.
