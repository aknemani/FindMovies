## server.R
library(dplyr)
library(ggplot2)
library(recommenderlab)
library(DT)
library(data.table)
library(reshape2)

source('functions/cf_algorithm.R') # collaborative filtering
source('functions/similarity_measures.R') # similarity measures

get_user_ratings = function(value_list) {
  dat = data.table(
    MovieID = sapply(strsplit(names(value_list), "_"),
                     function(x)
                       ifelse(length(x) > 1, x[[2]], NA)),
    Rating = unlist(as.character(value_list))
  )
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
  print(paste0("m", dat$MovieID))
  if (nrow(dat) == 0)
  {
    user_ratings <- -1
    rank_done <<- 0
  }
  else
  {
    rank_done <<- 1
    user_ratings <- sparseMatrix(
      i = rep(1, nrow(dat)),
      j = match(paste0("m", dat$MovieID), colnames(r_mat)),
      x = dat$Rating,
      dims = c(1, ncol(r_mat))
    )
  }
  return(user_ratings)
}


get_movie_genre1 = function(InputGenre = "Action")
{
  #InputGenre = "Action"
  recom = ratings %>%
    group_by(MovieID) %>%
    summarize(ratings_per_movie = n(),
              ave_ratings = round(mean(Rating), dig=3)) %>%
    inner_join(movies, by = 'MovieID') %>%
    filter(ratings_per_movie > 1000) %>%
    filter(grepl(InputGenre, Genres)) %>%
    top_n(5, ave_ratings) %>%
    select('MovieID', 'Title', 'ave_ratings', 'ratings_per_movie', 'Genres') %>%
    arrange(desc(ave_ratings)) #%>%
    #datatable(class = "nowrap hover row-border",escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))
}

# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies,
                  split = "::",
                  fixed = TRUE,
                  useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")
# extract year
movies$Year = as.numeric(unlist(
  lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))
#unique(unlist(strsplit(movies$Genres, split = "|", fixed = TRUE, useBytes = TRUE)))

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID,
                          function(x)
                            paste0(small_image_url, x, '.jpg?raw=true'))

#rating data
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'),
                    sep = ':',
                    colClasses = c('integer', 'NULL'),
                    header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')



i = paste0('u', ratings$UserID)
j = paste0('m', ratings$MovieID)
x = ratings$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)
r_mat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(r_mat) = levels(tmp$i)
colnames(r_mat) = levels(tmp$j)

rank_done = 1


shinyServer(function(input, output, session) {
  # show the movies to be rated
  output$ratings <- renderUI({
    num_rows <- 50
    num_movies <- 5 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(
          width = 2,
          div(style = "text-align:center", img(
            src = movies$image_url[(i - 1) * num_movies + j], height = 150
          )),
          #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
          div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
          div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(
            paste0("select_", movies$MovieID[(i - 1) * num_movies + j]),
            label = "",
            dataStop = 5
          ))
        )) #00c0ef
      })))
    })
  })
  
  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", {
      # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <-
        "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      if(rank_done == 0)
      {
        user_predicted_ids = get_movie_genre1("Action")$MovieID
      }
      else
      {
        r_mat_user = rbind(user_ratings, r_mat)
        r_mat_user_rrm = new('realRatingMatrix', data = r_mat_user) 
        res1 = predict_cf1(r_mat_user_rrm, "ubcf", 5)
        user_predicted_ids = res1$MovieID
      }
      # add user's ratings as first column to rating matrix

      #print(user_predicted_ids)

      recom_results <- data.table(
        Rank = 1:5,
        MovieID = user_predicted_ids,#movies$MovieID[user_predicted_ids],
        Title = movies$Title[match(user_predicted_ids, movies$MovieID)]#,
        #Predicted_rating =  user_results
      )
      print(recom_results)
      
    }) # still busy
    
  }) # clicked on button

  # display the recommendations
  output$results <- renderUI({
    num_rows <- 1
    recom_result <- df()
    num_movies <- min(5, length(recom_result$MovieID))
    print(rank_done)
    if(rank_done == 0)
    {
      num_rows = num_rows + 1
    }
    #print(recom_result)
    lapply(1:num_rows, function(i) {
      image_height = 150
      if(rank_done == 0 && i == 2)
      {
        num_movies = 1
        i = 1000
        image_height = 0
      }
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(
          width = ifelse(rank_done == 0 && i == 1000, 12, 2),
          status = "success",
          solidHeader = TRUE,
          title = ifelse(rank_done == 0 && i == 1000, "You didn't Rate Anything, So Watch Action Movies", paste0("Rank ", (i - 1) * num_movies + j)),
          
          div(style = "text-align:center",
              a(
                img(src = movies$image_url[which(movies$MovieID == recom_result$MovieID[(i - 1) * num_movies + j])], height = image_height)
              )),
          div(style = "text-align:center; font-size: 100%",
              strong(movies$Title[which(movies$MovieID == recom_result$MovieID[(i - 1) * num_movies + j])]))
          
        )
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
  output$RecomMovieList <- renderUI({
    num_rows <- 1
    recom_result <- get_movie_genre1(input$selectedGenre)
    num_movies <- max(1, min(5, length(recom_result$MovieID)))
    #print(recom_result)
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(
          width = ifelse(length(recom_result$MovieID) == 0, 5, 2),
          status = "success",
          solidHeader = TRUE,
          title = ifelse(length(recom_result$MovieID) == 0, "No Recommendation", paste0("Rank ", (i - 1) * num_movies + j)),
          div(style = "text-align:center",
              a(
                img(src = movies$image_url[which(movies$MovieID == recom_result$MovieID[(i - 1) * num_movies + j])], height = 150)
              )),
          div(style = "text-align:center; font-size: 100%",
              strong(movies$Title[which(movies$MovieID == recom_result$MovieID[(i - 1) * num_movies + j])]))
          
        )
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function