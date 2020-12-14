library(dplyr)
library(ggplot2)
library(recommenderlab)
library(DT)
library(data.table)
library(reshape2)

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
  random_user = sample(1:6040, 1)
  
  rec_UBCF = Recommender(Rmat, method = 'UBCF', parameter = list(normalize = 'Z-score', method = 'Cosine', nn = 25))
  recom = predict(rec_UBCF, Rmat[random_user, ], type = 'ratings')
  
  as(recom, 'data.frame')
}