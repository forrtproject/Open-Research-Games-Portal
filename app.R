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
      menuItem("Data Management", tabName = "data", icon = icon("database")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    includeCSS("games.css"),
    
    tabItems(
      # Games Portal Tab
      tabItem(tabName = "portal",
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = tagList(icon("gamepad"), " Open Research Games Portal"),
            p(style = "font-size: 16px; margin-bottom: 20px;",
              "Discover ", strong(paste0(nrow(games_df), " educational games")), 
              " that teach open science practices through interactive gameplay."
            ),
            
            # Enhanced Search and Filter Section
            fluidRow(
              column(4,
                div(style = "margin-bottom: 15px;",
                  textInput("search", 
                           label = tagList(icon("search"), " Search Games:"),
                           placeholder = "Search by title, gameplay, topics..."),
                  tags$small(class = "text-muted", "Search across all game attributes")
                )
              ),
              column(4,
                selectInput("cluster_filter", 
                           label = tagList(icon("filter"), " FORRT Cluster:"),
                           choices = c("All Clusters" = "all",
                                     "Cluster 1: Replication Crisis" = "Cluster 1",
                                     "Cluster 2: Statistical Knowledge" = "Cluster 2", 
                                     "Cluster 6: FAIR Data" = "Cluster 6",
                                     "Cluster 8: Meta-Research" = "Cluster 8"),
                           selected = "all")
              ),
              column(4,
                selectInput("gameplay_filter",
                           label = tagList(icon("gamepad"), " Gameplay Style:"),
                           choices = c("All Styles" = "all"),
                           selected = "all")
              )
            ),
            
            # Results counter and reset
            fluidRow(
              column(6,
                uiOutput("results_count")
              ),
              column(6, align = "right",
                actionButton("reset_filters", "Reset All Filters", 
                           icon = icon("refresh"),
                           class = "btn-warning btn-sm")
              )
            ),
            
            hr(),
            
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
          box(
            width = 12,
            status = "info",
            solidHeader = TRUE,
            title = tagList(icon("database"), " Games Data Management"),
            p(style = "font-size: 16px;",
              "View, search, and export the complete games database."
            ),
            
            # Export buttons
            fluidRow(
              column(12,
                div(style = "margin-bottom: 20px;",
                  downloadButton("download_csv", "Export as CSV", 
                               class = "btn-success",
                               icon = icon("file-csv")),
                  downloadButton("download_excel", "Export as Excel", 
                               class = "btn-success",
                               icon = icon("file-excel"),
                               style = "margin-left: 10px;"),
                  downloadButton("download_json", "Export as JSON", 
                               class = "btn-success",
                               icon = icon("file-code"),
                               style = "margin-left: 10px;")
                )
              )
            ),
            
            DT::dataTableOutput("games_table")
          )
        )
      ),
      
      # About Tab
      tabItem(tabName = "about",
        fluidRow(
          box(
            width = 12,
            status = "success",
            solidHeader = TRUE,
            title = tagList(icon("info-circle"), " About This Portal"),
            
            h3(icon("lightbulb"), " Welcome to the Open Research Games Portal"),
            p(style = "font-size: 16px;",
              "Your gateway to learning open science through play! Our curated collection features ",
              strong(paste0(nrow(games_df), " educational games")), 
              " that make complex research concepts accessible and engaging."
            ),
            
            hr(),
            
            fluidRow(
              column(4,
                div(class = "info-box",
                  h4(icon("gamepad"), " Interactive Learning"),
                  p("Games that teach research methods, statistical thinking, and open science 
                    practices through hands-on experience.")
                )
              ),
              column(4,
                div(class = "info-box",
                  h4(icon("users"), " For Everyone"),
                  p("From high school students to seasoned researchers, find games tailored 
                    to your learning level.")
                )
              ),
              column(4,
                div(class = "info-box",
                  h4(icon("graduation-cap"), " Evidence-Based"),
                  p("Each game is designed with pedagogical principles and learning 
                    objectives in mind.")
                )
              )
            ),
            
            hr(),
            
            h4(icon("compass"), " How to Use This Portal"),
            tags$ul(
              tags$li(strong("Browse:"), " Explore all games using the interactive cards"),
              tags$li(strong("Filter:"), " Use FORRT cluster and gameplay style filters to find relevant games"),
              tags$li(strong("Search:"), " Enter keywords to find specific games by title, topics, or content"),
              tags$li(strong("Play:"), " Click 'Play Now' to start gaming immediately"),
              tags$li(strong("Learn More:"), " Click 'Details' to see comprehensive game information"),
              tags$li(strong("Export:"), " Download the complete database from the Data Management tab")
            ),
            
            hr(),
            
            h4(icon("chart-bar"), " Portal Statistics"),
            fluidRow(
              column(3,
                valueBox(
                  value = nrow(games_df),
                  subtitle = "Total Games",
                  icon = icon("gamepad"),
                  color = "blue"
                )
              ),
              column(3,
                valueBox(
                  value = length(unique(unlist(strsplit(games_df$topic_area, ", ")))),
                  subtitle = "Topic Areas",
                  icon = icon("book"),
                  color = "green"
                )
              ),
              column(3,
                valueBox(
                  value = length(unique(unlist(strsplit(games_df$gameplay_style, ", ")))),
                  subtitle = "Gameplay Styles",
                  icon = icon("puzzle-piece"),
                  color = "yellow"
                )
              ),
              column(3,
                valueBox(
                  value = length(unique(unlist(strsplit(games_df$language, ", ")))),
                  subtitle = "Languages",
                  icon = icon("globe"),
                  color = "purple"
                )
              )
            ),
            
            hr(),
            
            h4(icon("link"), " Links & Resources"),
            p(
              tags$a(href = "https://forrtapps.shinyapps.io/open-research-games-portal/", 
                    target = "_blank",
                    icon("external-link-alt"), " Live App on Shinyapps.io"),
              br(),
              tags$a(href = "https://github.com/forrtproject", 
                    target = "_blank",
                    icon("github"), " FORRT Project on GitHub"),
              br(),
              "Ready to transform your understanding of open science? Let's play and learn together!"
            )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Populate gameplay filter choices dynamically
  observe({
    gameplay_styles <- sort(unique(unlist(strsplit(games_df$gameplay_style, ", "))))
    gameplay_choices <- c("All Styles" = "all", setNames(gameplay_styles, gameplay_styles))
    updateSelectInput(session, "gameplay_filter", choices = gameplay_choices)
  })
  
  # Reset filters
  observeEvent(input$reset_filters, {
    updateTextInput(session, "search", value = "")
    updateSelectInput(session, "cluster_filter", selected = "all")
    updateSelectInput(session, "gameplay_filter", selected = "all")
  })
  
  # Reactive filtered games
  filtered_games <- reactive({
    df <- games_df
    
    # Apply cluster filter
    if (!is.null(input$cluster_filter) && input$cluster_filter != "all") {
      df <- df[grepl(input$cluster_filter, df$forrt_clusters, ignore.case = TRUE), ]
    }
    
    # Apply gameplay filter
    if (!is.null(input$gameplay_filter) && input$gameplay_filter != "all") {
      df <- df[grepl(input$gameplay_filter, df$gameplay_style, ignore.case = TRUE), ]
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
  
  # Results count
  output$results_count <- renderUI({
    count <- nrow(filtered_games())
    total <- nrow(games_df)
    
    if (count == total) {
      tags$div(
        style = "font-size: 16px; padding: 10px;",
        icon("check-circle", class = "text-success"),
        strong(paste0(" Showing all ", total, " games"))
      )
    } else {
      tags$div(
        style = "font-size: 16px; padding: 10px;",
        icon("filter", class = "text-info"),
        strong(paste0(" Showing ", count, " of ", total, " games"))
      )
    }
  })
  
  # Render games cards
  output$games_cards <- renderUI({
    games <- filtered_games()
    
    if (nrow(games) == 0) {
      return(
        div(class = "col-12",
          div(class = "alert alert-info text-center", 
            style = "padding: 40px; margin: 20px 0;",
            icon("search", class = "fa-3x", style = "color: #17a2b8; margin-bottom: 20px;"),
            h4("No games found"),
            p("Try adjusting your filters or search terms to discover more games.")
          )
        )
      )
    }
    
    cards <- lapply(seq_len(nrow(games)), function(i) {
      game <- games[i, ]
      
      # Truncate description - handle NULL, NA, and empty values
      description <- if (!is.null(game$description) && !is.na(game$description) && 
                        nchar(as.character(game$description)) > 120) {
        paste0(substr(game$description, 1, 120), "...")
      } else if (!is.null(game$description) && !is.na(game$description) && 
                 nchar(as.character(game$description)) > 0) {
        as.character(game$description)
      } else {
        "No description available"
      }
      
      div(class = "col-xl-4 col-lg-6 col-md-6 col-sm-12 mb-4",
        div(class = "game-card",
          # Card header with gradient
          div(class = "game-card-header",
            h4(class = "game-card-title", 
              icon("gamepad", class = "game-icon"), 
              game$title
            )
          ),
          
          # Card body
          div(class = "game-card-body",
            # Description
            p(class = "game-description", description),
            
            # Game metadata with icons
            div(class = "game-metadata",
              div(class = "metadata-item",
                icon("puzzle-piece", class = "metadata-icon"),
                tags$span(class = "metadata-label", "Gameplay:"),
                tags$span(class = "metadata-value", 
                  if (!is.null(game$gameplay_style) && !is.na(game$gameplay_style)) {
                    as.character(game$gameplay_style)
                  } else {
                    "N/A"
                  }
                )
              ),
              div(class = "metadata-item",
                icon("clock", class = "metadata-icon"),
                tags$span(class = "metadata-label", "Playtime:"),
                tags$span(class = "metadata-value", 
                  if (!is.null(game$playtime) && !is.na(game$playtime)) {
                    as.character(game$playtime)
                  } else {
                    "N/A"
                  }
                )
              ),
              div(class = "metadata-item",
                icon("globe", class = "metadata-icon"),
                tags$span(class = "metadata-label", "Language:"),
                tags$span(class = "metadata-value", 
                  if (!is.null(game$language) && !is.na(game$language)) {
                    as.character(game$language)
                  } else {
                    "N/A"
                  }
                )
              ),
              div(class = "metadata-item",
                icon("book", class = "metadata-icon"),
                tags$span(class = "metadata-label", "Topics:"),
                tags$span(class = "metadata-value", 
                  if (!is.null(game$topic_area) && !is.na(game$topic_area) && 
                      nchar(as.character(game$topic_area)) > 40) {
                    paste0(substr(game$topic_area, 1, 40), "...")
                  } else if (!is.null(game$topic_area) && !is.na(game$topic_area)) {
                    as.character(game$topic_area)
                  } else {
                    "N/A"
                  }
                )
              )
            )
          ),
          
          # Card footer with buttons
          div(class = "game-card-footer",
            if (!is.null(game$access) && !is.na(game$access) && 
                nchar(as.character(game$access)) > 0) {
              a(href = as.character(game$access), 
                target = "_blank", 
                class = "btn-play",
                icon("play-circle"), 
                " Play Now"
              )
            } else {
              tags$span(class = "btn-play disabled", 
                icon("lock"), 
                " Not Available"
              )
            },
            actionButton(
              paste0("details_", i), 
              tagList(icon("info-circle"), " Details"),
              class = "btn-details"
            )
          )
        )
      )
    })
    
    div(class = "row games-grid", cards)
  })
  
  # Handle details button clicks
  observe({
    games <- filtered_games()
    
    for (i in seq_len(nrow(games))) {
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
      games_df[, c("title", "creators", "gameplay_style", "playtime", "language", "topic_area", "forrt_clusters")],
      options = list(
        pageLength = 15, 
        scrollX = TRUE,
        dom = 'Bfrtip'
      ),
      filter = "top",
      rownames = FALSE
    )
  })
  
  # Export handlers
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("open_research_games_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(games_df, file, row.names = FALSE)
    }
  )
  
  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("open_research_games_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      # Check if openxlsx is available, if not use csv
      if (requireNamespace("openxlsx", quietly = TRUE)) {
        openxlsx::write.xlsx(games_df, file)
      } else {
        # Fallback to CSV if openxlsx not available
        write.csv(games_df, file, row.names = FALSE)
      }
    }
  )
  
  output$download_json <- downloadHandler(
    filename = function() {
      paste0("open_research_games_", Sys.Date(), ".json")
    },
    content = function(file) {
      jsonlite::write_json(games_df, file, pretty = TRUE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
