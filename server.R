source('https://raw.githubusercontent.com/Ayesha-Raza/Movie_Recommender/main/systems.R')

get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.integer(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

ratings = read.csv('www/ratings.dat', 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
ratings = ratings[(ratings$Rating <= 5),]

shinyServer(function(input, output, session) {
  
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5))))
      })))
    })
  })
  
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { 
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)  
      
      recom_results = system2(ratings)
      
      recom_results
      
    })
    
  })
  
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        movie_id = recom_result$item[(i - 1) * num_movies + j]
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[movies$MovieID==(movie_id)], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[movies$MovieID==(movie_id)])
            )
            
        ) 
      })))
    })
    
  })
  
  df2 <- eventReactive(input$genre, {
    withBusyIndicatorServer("genre", { 
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      input <- input$input
      
      rec <- system1(input, movies, ratings)
      
    })
    
  })
  
  output$rec <- renderUI({
    num_rows <- 2
    num_movies <- 5
    rec <- df2()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = rec$image_url[(i - 1) * num_movies + j], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(rec$Title[(i - 1) * num_movies + j])
            )
            
        )        
      })))
    })
    
  })
  
})