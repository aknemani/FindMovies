## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

shinyUI(dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Movie Recommender"),
  
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Search By Genre",
      tabName = "GenreSearch",
      icon = icon("video")
    ),
    menuItem(
      "Search By Rating",
      tabName = "RatingsSearch",
      icon = icon("star")
    )
  ), disable = FALSE),
  
  dashboardBody(includeCSS("css/movies.css"),
                tabItems(
                  tabItem(tabName = "RatingsSearch",
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
                              title = "Step 2: Discover movies you might like",
                              br(),
                              withBusyIndicatorUI(
                                actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                              ),
                              br(),
                              tableOutput("results")
                            )
                          )),
                  tabItem(tabName = "GenreSearch",
                          fluidRow(box(
                            selectInput(
                              "selectedGenre",
                              "First load may take 30 seconds",
                              choices = c(
                                "Action",
                                "Adventure",
                                "Animation",
                                "Children's",
                                "Crime",
                                "Comedy",
                                "Documentary",
                                "Drama",
                                "Fantasy",
                                "Film-Noir",
                                "Horror",
                                "Musical",
                                "Mystery",
                                "Romance",
                                "Sci-Fi",
                                "Thriller",
                                "War",
                                "Western"
                              )
                            ),
                            solidHeader = TRUE,
                            title = "Step 1: Select Your Favorite Genre",
                            status = "info"
                          )),
                          fluidRow(
                            useShinyjs(),
                            box(
                              width = 12,
                              status = "info",
                              solidHeader = TRUE,
                              title = "Step 2: Discover Movies You Might Like",
                              br(),
                              tableOutput("RecomMovieList")
                            )
                          ))
                ))
))
