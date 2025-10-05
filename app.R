library(shiny)
library(shinydashboard)
library(DT)
library(jsonlite)
library(dplyr)

# Load games data
games_data <- fromJSON("data/open_research_games.json", simplifyVector = FALSE)

# Convert to data frame for easier handling
games_df <- bind_rows(lapply(names(games_data), function(slug) {
  game <- games_data[[slug]]
  game$slug <- slug
  
  # Convert list fields to character strings
  if (is.list(game$creators)) game$creators <- paste(game$creators, collapse = ", ")
  if (is.list(game$language)) game$language <- paste(game$language, collapse = ", ")
  if (is.list(game$topic_area)) game$topic_area <- paste(game$topic_area, collapse = ", ")
  if (is.list(game$forrt_clusters)) game$forrt_clusters <- paste(game$forrt_clusters, collapse = ", ")
  if (is.list(game$learning_objectives)) game$learning_objectives <- paste(game$learning_objectives, collapse = "; ")
  
  return(game)
}))

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Open Research Games Portal"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Games Portal", tabName = "portal", icon = icon("gamepad")),
      menuItem("Data Management", tabName = "data", icon = icon("database"))
    )
  ),
  
  dashboardBody(
    includeCSS("games.css"),
    
    tabItems(
      # Games Portal Tab
      tabItem(tabName = "portal",
        fluidRow(
          column(12,
            h2("Open Research Games Portal"),
            p("Discover educational games that teach open science practices through interactive gameplay."),
            
            # Search and Filter
            fluidRow(
              column(6,
                textInput("search", "Search Games:", 
                         placeholder = "Search by title, gameplay, topics...")
              ),
              column(6,
                selectInput("cluster_filter", "Filter by FORRT Cluster:",
                           choices = c("All" = "all",
                                     "Replication Crisis" = "Cluster 1",
                                     "Statistical Knowledge" = "Cluster 2", 
                                     "FAIR Data" = "Cluster 6",
                                     "Meta-Research" = "Cluster 8"),
                           selected = "all")
              )
            ),
            
            # Games Display
            div(id = "games-container",
                uiOutput("games_cards")
            )
          )
        )
      ),
      
      # Data Management Tab
      tabItem(tabName = "data",
        fluidRow(
          column(12,
            h2("Games Data Management"),
            p("View and manage the games database."),
            
            DT::dataTableOutput("games_table")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive filtered games
  filtered_games <- reactive({
    df <- games_df
    
    # Apply cluster filter
    if (input$cluster_filter != "all") {
      df <- df[grepl(input$cluster_filter, df$forrt_clusters, ignore.case = TRUE), ]
    }
    
    # Apply search filter
    if (!is.null(input$search) && input$search != "") {
      search_term <- tolower(input$search)
      df <- df[
        grepl(search_term, tolower(df$title)) |
        grepl(search_term, tolower(df$description)) |
        grepl(search_term, tolower(df$gameplay_style)) |
        grepl(search_term, tolower(df$topic_area)) |
        grepl(search_term, tolower(df$forrt_clusters)),
      ]
    }
    
    return(df)
  })
  
  # Render games cards
  output$games_cards <- renderUI({
    games <- filtered_games()
    
    if (nrow(games) == 0) {
      return(div(class = "alert alert-info", "No games found matching your criteria."))
    }
    
    cards <- lapply(1:nrow(games), function(i) {
      game <- games[i, ]
      
      div(class = "col-md-4 mb-4",
        div(class = "card game-card h-100",
          div(class = "card-body",
            h5(class = "card-title", game$title),
            p(class = "card-text", substr(game$description %||% "", 1, 150)),
            
            # Game metadata
            tags$small(
              strong("Gameplay: "), game$gameplay_style, br(),
              strong("Language: "), game$language, br(),
              strong("Playtime: "), game$playtime, br(),
              strong("Topics: "), game$topic_area
            ),
            
            # Buttons
            div(class = "mt-3",
              if (!is.null(game$access) && game$access != "") {
                a(href = game$access, target = "_blank", 
                  class = "btn btn-success btn-sm mr-2",
                  icon("play"), " Play Now")
              },
              actionButton(paste0("details_", i), "Details", 
                          class = "btn btn-outline-info btn-sm")
            )
          )
        )
      )
    })
    
    div(class = "row", cards)
  })
  
  # Handle details button clicks
  observe({
    games <- filtered_games()
    
    for (i in 1:nrow(games)) {
      local({
        my_i <- i
        observeEvent(input[[paste0("details_", my_i)]], {
          game <- games[my_i, ]
          
          showModal(modalDialog(
            title = game$title,
            
            h4("Description"),
            p(game$description),
            
            h4("Game Details"),
            div(class = "row",
              div(class = "col-md-6",
                p(strong("Creator(s): "), game$creators),
                p(strong("Gameplay: "), game$gameplay_style),
                p(strong("Game Type: "), game$game_type),
                p(strong("Playtime: "), game$playtime),
                p(strong("Language: "), game$language)
              ),
              div(class = "col-md-6",
                p(strong("Target Audience: "), game$target_audience),
                p(strong("Format: "), game$delivery_format),
                p(strong("Players: "), game$number_of_players),
                p(strong("License: "), game$licence),
                p(strong("Prerequisites: "), game$prior_knowledge)
              )
            ),
            
            if (!is.null(game$learning_objectives) && game$learning_objectives != "") {
              div(
                h4("Learning Objectives"),
                p(game$learning_objectives)
              )
            },
            
            if (!is.null(game$teaching_integration) && game$teaching_integration != "") {
              div(
                h4("Teaching Integration"),
                p(game$teaching_integration)
              )
            },
            
            if (!is.null(game$testimonials) && game$testimonials != "") {
              div(
                h4("Testimonials"),
                tags$em(p(game$testimonials))
              )
            },
            
            footer = tagList(
              if (!is.null(game$access) && game$access != "") {
                a(href = game$access, target = "_blank", 
                  class = "btn btn-success",
                  icon("play"), " Play This Game")
              },
              modalButton("Close")
            ),
            
            size = "l",
            easyClose = TRUE
          ))
        })
      })
    }
  })
  
  # Data table for management
  output$games_table <- DT::renderDataTable({
    DT::datatable(
      games_df[, c("title", "creators", "gameplay_style", "playtime", "language", "topic_area")],
      options = list(pageLength = 15, scrollX = TRUE),
      filter = "top"
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
