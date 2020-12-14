library(dplyr)
library(ggplot2)
library(recommenderlab)
library(DT)
library(data.table)
library(reshape2)

system1 <- function(user_input, movies, ratings) {
  
  genre_list <- c("Action", "Adventure", "Animation", "Children", 
                  "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                  "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                  "Sci-Fi", "Thriller", "War", "Western")
  
  tmp = ratings %>% 
    group_by(MovieID) %>% 
    summarize(ratings_per_movie = n(), ave_ratings = mean(Rating)) %>%
    inner_join(movies, by = 'MovieID')

  genre = tmp[match(user_input, genre_list, ""),]
  recom = arrange(genre, desc(ratings_per_movie))
  
  recom
}

system2 <- function(ratings) {
  train.id = sample(nrow(ratings), floor(nrow(ratings)) * 0.8)
  train = ratings[train.id, ]
  test = ratings[-train.id, ]

  i = paste0(train$UserID)
  j = paste0(train$MovieID)
  x = train$Rating
  tmp = data.frame(i, j, x, stringsAsFactors = T)
  Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
  rownames(Rmat) = levels(tmp$i)
  colnames(Rmat) = levels(tmp$j)
  Rmat = new('realRatingMatrix', data = Rmat)
  random_user = 3333
  
  rec_UBCF = Recommender(Rmat, method = 'UBCF', parameter = list(normalize = 'Z-score', method = 'Cosine', nn = 25))
  recom = predict(rec_UBCF, Rmat[random_user, ], type = 'ratings')
  
  as(recom, 'data.frame')
}
