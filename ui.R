## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(ShinyRatingInput)
library(shinyjs)
library(dplyr)
library(data.table)
library(reshape2)


source('helpers.R')


sidebar <- dashboardSidebar(sidebarMenu(
  menuItem(
    "Recommendations By Rating",
    tabName = "RatingRecom",
    icon = icon("star")
  ),
  menuItem(
    "Recommendations By Genre",
    tabName = "GenreRecom",
    icon = icon("film")
  )
))
body <- dashboardBody(includeCSS("Movies.css"),
                      tabItems(
                        tabItem(tabName = "RatingRecom",
                                fluidRow(
                                  box(
                                    width = 12,
                                    title = "Step 1: Rate as many movies as possible",
                                    status = "info",
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    div(class = "rateitems",
                                        uiOutput('ratings'))
                                  )
                                ),
                                fluidRow(
                                  useShinyjs(),
                                  box(
                                    width = 12,
                                    status = "info",
                                    solidHeader = TRUE,
                                    title = "Step 2: Discover Movies you might like",
                                    br(),
                                    withBusyIndicatorUI(
                                      actionButton("btnRating", "Click here to get your recommendations", class = "btn-warning")
                                    ),
                                    br(),
                                    tableOutput("resultsRating")
                                  )
                                )),
                        
                        tabItem(tabName = "GenreRecom",
                                selectInput("Genre",
                                            "Choose a Genre:",
                                            c("Action" = "Action",
                                              "Adventure" = "Adventure",
                                              "Animation" = "Animation",
                                              "Children's" = "Children's",
                                              "Comedy" = "Comedy",
                                              "Crime" = "Crime",
                                              "Documentary" = "Documentary",
                                              "Drama" = "Drama",
                                              "Fantasy" = "Fantasy",
                                              "Film-Noir" = "Film-Noir",
                                              "Horror" = "Horror",
                                              "Musical" = "Musical",
                                              "Mystery" = "Mystery",
                                              "Romance" = "Romance",
                                              "Sci-Fi" = "Sci-Fi",
                                              "Thriller" = "Thriller",
                                              "War" = "War",
                                              "Western" = "Western"
                                            ),
                                tableOutput("drpDwn")
                                ), 
                                fluidRow(
                                  useShinyjs(),
                                  box(
                                    width = 12,
                                    status = "info",
                                    solidHeader = TRUE,
                                    title = "Step 2: Discover Movies you might like",
                                    br(),
                                    withBusyIndicatorUI(
                                      actionButton("btnGenre", "Click here to get your recommendations", class = "btn-warning")
                                    ),
                                    br(),
                                    tableOutput("resultsGenre")
                                  )
                                ))
                      ))
shinyUI(
  dashboardPage(
    title = "Movie Recommender",
    skin = "blue",
    dashboardHeader(title = "Movie Recommender"),
    sidebar = sidebar,
    body = body
  )
)