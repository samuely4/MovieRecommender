## server.R

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
}

# read in data
movies = readLines('movies.dat')
movies = strsplit(movies,
                  split = "::",
                  fixed = TRUE,
                  useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID,
                          function(x)
                            paste0(small_image_url, x, '.jpg?raw=true'))

ratings = read.csv('ratings.dat', sep = ':', colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')


shinyServer(function(input, output, session) {
  # show the books to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(
          width = 2,
          div(style = "text-align:center", img(
            src = movies$image_url[(i - 1) * num_movies + j], height = 150
          )),
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
  df_rating <- eventReactive(input$btnRating, {
    withBusyIndicatorServer("btnRating", {
      # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      #print(user_ratings)
      if(nrow(user_ratings) != 0){
        ratingsModf = subset(ratings, select=-c(Timestamp))
        new_user = data.frame(UserID = 0, user_ratings)
        train = rbind(ratingsModf, new_user)
        test = new_user
        i = paste0('u', train$UserID)
        j = paste0('m', train$MovieID)
        x = train$Rating
        tmp_train = data.frame(i, j, x, stringsAsFactors = T)
        Rmat_train = sparseMatrix(as.integer(tmp_train$i), 
                                  as.integer(tmp_train$j), x = tmp_train$x)
        rownames(Rmat_train) = levels(tmp_train$i)
        colnames(Rmat_train) = levels(tmp_train$j)
        Rmat_train = new('realRatingMatrix', data = Rmat_train)
        rec_UBCF = Recommender(Rmat_train, method = 'UBCF',
                               parameter = list(normalize = 'Z-score',
                                                method = 'Cosine',
                                                nn = 25))
        
        recom_UBCF = predict(rec_UBCF,
                             Rmat_train["u0"], type = 'ratings')
        rec_list_UBCF = as(recom_UBCF, 'list')
        
        pred.movieIDs = unlist(lapply(row.names(as.data.frame(rec_list_UBCF[["u0"]])),
                                      function(i) as.numeric(substring(i, 2))))
        pred.ratings = unlist(rec_list_UBCF[["u0"]])
        
        recom_results = data.frame(MovieID = pred.movieIDs, Rating = pred.ratings)
        recom_results = recom_results[order(-recom_results$Rating),]
      }
    }) # still busy
  }) # clicked on button
  
  # Calculate recommendations when the sbumbutton is clicked
  df_genre <- eventReactive(input$btnGenre, {
    withBusyIndicatorServer("btnGenre", {
      # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <-
        "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      # get the user Genre input
      desired_genre = input$Genre
      
      recom_results = ratings %>% 
        group_by(MovieID) %>% 
        summarize(ratings_per_movie = n(), 
                  ave_ratings = round(mean(Rating), dig=3)) %>%
        inner_join(movies, by = 'MovieID') %>%
        filter(ratings_per_movie > 1000) %>%
        filter(grepl(desired_genre, Genres, fixed = TRUE)) %>%
        top_n(10, ave_ratings) %>%
        select('MovieID', 'Title', 'ave_ratings', 'Genres') %>%
        arrange(desc(ave_ratings))
    }) # still busy
    
  }) # clicked on button
  
  
  # display the Ratings recommendations
  output$resultsRating <- renderUI({
    num_rows <- 0
    numMovies <- rep(0, 2)
    noRatings = FALSE
    recom_result <- df_rating()
    # if the user attempted to get recommendation without rating any movies
    if(is.null(recom_result)){
      recom_result <- data.frame(MovieID = c(858, 1198, 3114, 919, 1136, 318, 260, 1278, 912, 50))
      noRatings = TRUE
    }
    # handling all cases related to num_movies
    if(nrow(recom_result) == 0){
      num_rows <- 0
      numMovies = rep(0, 2)
    }else if(nrow(recom_result) <= 5){
      num_rows <- 1
      numMovies = rep(nrow(recom_result), 2)
    }else if(nrow(recom_result) > 5 & nrow(recom_result) < 10){
      num_rows <- 2
      numMovies[1] = 5
      numMovies[2] = nrow(recom_result) - 5
    }else{
      num_rows = 2
      numMovies = rep(5, 2)
    }
    
    noRatingOutput = fluidRow()
    RatingOutput = fluidRow()
    NoRecomOutput = fluidRow()
    if(num_rows != 0){
      if(noRatings){
        noRatingOutput = fluidRow(
          align = "center",
          style = "font-size: 100%; font-weight:Bold",
          "Please rate some of the movies above to get customized recommendations!!!"
        )
      }
      
      RatingOutput = lapply(1:num_rows, function(i) {
        num_movies = numMovies[i]
        list(fluidRow(lapply(1:num_movies, function(j) {
          rankShow = ifelse(num_rows == 2 & num_movies < 5, 
                            paste0("Rank ", (i - 1) * 5 + j),
                            paste0("Rank ", (i - 1) * num_movies + j))
          imgSrcShow = ifelse(num_rows == 2 & num_movies < 5, 
                              movies$image_url[recom_result$MovieID[(i - 1) * 5 + j] == movies$MovieID],
                              movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j] == movies$MovieID])
          movieTitleShow = ifelse(num_rows == 2 & num_movies < 5, 
                                  movies$Title[recom_result$MovieID[(i - 1) * 5 + j] == movies$MovieID],
                                  movies$Title[recom_result$MovieID[(i - 1) * num_movies + j] == movies$MovieID])
          box(
            width = 2,
            status = "success",
            solidHeader = TRUE,
            title = rankShow,
            
            div(style = "text-align:center", 
                a(
                  img(src = imgSrcShow, height = 150)
                )),
            div(style = "text-align:center; font-size: 100%",
                strong(movieTitleShow))
            
          )
        }))) # columns
      }) # rows
    }else{
      NoRecomOutput = fluidRow(
        align = "center",
        style = "font-size: 100%; font-weight:Bold",
        "Sorry, There are no recommendations yet!!!"
      )
    }
    
    div(
      NoRecomOutput,
      noRatingOutput,
      br(),
      RatingOutput
    )
  }) # renderUI function
  
  # display the Genre recommendations
  output$resultsGenre <- renderUI({
    num_rows <- 0
    numMovies <- rep(0, 2)
    recom_result <- df_genre()
    # handling all cases related to num_movies
    if(nrow(recom_result) == 0){
      num_rows <- 0
      numMovies = rep(0, 2)
    }else if(nrow(recom_result) <= 5){
      num_rows <- 1
      numMovies = rep(nrow(recom_result), 2)
    }else if(nrow(recom_result) > 5 & nrow(recom_result) < 10){
      num_rows <- 2
      numMovies[1] = 5
      numMovies[2] = nrow(recom_result) - 5
    }else{
      num_rows = 2
      numMovies = rep(5, 2)
    }
    
    if(num_rows != 0){
      lapply(1:num_rows, function(i) {
        num_movies = numMovies[i]
        list(fluidRow(lapply(1:num_movies, function(j) {
          rankShow = ifelse(num_rows == 2 & num_movies < 5, 
                            paste0("Rank ", (i - 1) * 5 + j),
                            paste0("Rank ", (i - 1) * num_movies + j))
          imgSrcShow = ifelse(num_rows == 2 & num_movies < 5, 
                              movies$image_url[recom_result$MovieID[(i - 1) * 5 + j] == movies$MovieID],
                              movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j] == movies$MovieID])
          movieTitleShow = ifelse(num_rows == 2 & num_movies < 5, 
                                  movies$Title[recom_result$MovieID[(i - 1) * 5 + j] == movies$MovieID],
                                  movies$Title[recom_result$MovieID[(i - 1) * num_movies + j] == movies$MovieID])
          box(
            width = 2,
            status = "success",
            solidHeader = TRUE,
            title = rankShow,
            
            div(style = "text-align:center",
                a(
                  img(src = imgSrcShow, height = 150)
                )),
            div(style = "text-align:center; font-size: 100%",
                strong(movieTitleShow))
            
          )
        }))) # columns
      }) # rows
    }else{
      fluidRow(
        align = "center",
        style = "font-size: 100%; font-weight:Bold",
        "Sorry, There are no recommendations for this genre yet!!!"
      )
    }
  }) # renderUI function
}) # server function