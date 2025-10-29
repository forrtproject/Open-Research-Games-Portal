# Libraries
library(shiny)
library(shinydashboard)
library(DT)
library(jsonlite)
library(dplyr)

# Load games data
tryCatch({
  games_data <- fromJSON("data/open_research_games.json",
                         simplifyVector = FALSE)
}, error = function(e) {
  stop("Failed to load JSON file: ",
       e$message,
       "\nPlease ensure 'data/open_research_games.json' exists and is valid.")
})

# Define all expected columns
expected_columns <- c(
  "game_id", "title", "creators", "description", "access", "delivery_format",
  "game_type", "gameplay_style", "number_of_players", "target_audience",
  "last_updated", "language", "licence", "topic_area", "forrt_clusters",
  "learning_objectives", "formal_evaluation", "suggested_audience",
  "prior_knowledge", "playtime", "scalability", "teaching_integration",
  "context_specific_elements", "preparation", "testimonials", "entry_id"
)

# Convert to data frame for easier handling
games_df <- bind_rows(lapply(names(games_data), function(slug) {
  game <- games_data[[slug]]
  game$slug <- slug
  # Initialize a list for the game data
  game_row <- list(slug = slug)
  # Handle all expected columns
  for (col in expected_columns) {
    if (col %in% names(game)) {
      if (is.list(game[[col]])) {
        # Convert list fields to character strings
        # with new lines, handle empty lists
        game_row[[col]] <-
          if (length(game[[col]]) > 0 && !all(game[[col]] == "")) {
            paste(game[[col]], collapse = "\n")
          } else {
            "N/A"
          }
      } else {
        # Handle non-list fields, convert NA
        # or empty to "N/A", preserve new lines
        game_row[[col]] <- 
          if (is.null(game[[col]]) || is.na(game[[col]]) || game[[col]] == "") {
            "N/A"
          } else {
            gsub("\r\n|\r", "\n", as.character(game[[col]]))
          }
      }
    } else {
      # Set missing columns to "N/A"
      game_row[[col]] <- "N/A"
    }
  }
  return(game_row) # nolint
}))

# UI
ui <- tagList(
  # scroll to top button functionality
  tags$head(
    # Add viewport meta tag for responsive design
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$script(HTML("
      $(document).ready(function() {
        // Show/hide scroll to top button
        $(window).scroll(function() {
          if ($(this).scrollTop() > 300) {
            $('#scrollToTop').fadeIn();
          } else {
            $('#scrollToTop').fadeOut();
          }
        });
        
        // Smooth scroll to top
        $('#scrollToTop').click(function() {
          $('html, body').animate({scrollTop: 0}, 600);
          return false;
        });
      });
    ")),
    # preserve line breaks in descriptions and metadata
    tags$style(HTML("
      .game-description, .game-metadata, .metadata-item, .game-card-body {
        white-space: pre-wrap; /* Preserve line breaks */
      }
      .game-card-body p {
        margin-bottom: 10px;
      }
      .metadata-value {
        display: block; /* Ensure each item is on a new line */
      }
      /* Responsive adjustments for game cards and grid */
      @media (max-width: 767px) {
        box, h3 {
          padding-top: 60px !important;
          margin-top: 30px;
        }
        

        .game-card {
          margin-bottom: 20px;
        }
        .games-grid > .col-xl-4, 
        .games-grid > .col-lg-6, 
        .games-grid > .col-md-6, 
        .games-grid > .col-sm-12 {
          flex: 0 0 100%;
          max-width: 100%;
        }
      }
    "))
  ),
  # Scroll to top button (outside navbar)
  tags$button(
    id = "scrollToTop",
    class = "scroll-to-top",
    icon("arrow-up"),
    title = "Back to Top"
  ),
  # Main navbar
  navbarPage(
    title = "FORRT Open Research Games Portal",
    id = "main_navbar",
    # Games Portal Tab
    tabPanel(
      title = tagList(icon("gamepad"), "Games Portal"),
      value = "portal",
      includeCSS("games.css"),
      fluidRow(
        box(
          style = "padding: 10px; margin: 10px;",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          h3(icon("gamepad"), " Explore Educational Games"),
          tags$hr(),
          p(style = "font-size: 16px; margin-bottom: 20px;",
            "Discover ", strong(paste0(nrow(games_df), " educational games")), 
            " that teach open science practices through interactive gameplay."
          ),
          # Search and Filter Section
          fluidRow(
            column(4,
              div(style = "margin-bottom: 15px;",
                textInput("search",
                          label = tagList(icon("search"), " Search Games:"),
                          placeholder = "Search by title, gameplay, topics..."),
                tags$small(class = "text-muted",
                           "Search across all game attributes")
              )
            ),
            column(4,
              selectInput("cluster_filter",
                          label = tagList(icon("filter"), " FORRT Cluster:"),
                          choices = c("All Clusters" = "all",
                                  "Cluster 1: Replication Crisis" = "Cluster 1", # nolint
                                  "Cluster 2: Statistical Knowledge" = "Cluster 2",  # nolint
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
          # Games Display
          div(id = "games-container",
            uiOutput("games_cards")
          )
        )
      )
    ),
    # Spreadsheet Tab
    tabPanel(
      title = tagList(icon("database"), "Spreadsheet Table"),
      value = "spreadsheet_tab",
      tags$p(
        "You can view the published Google sheet Version ",
        tags$a(
          href = "https://docs.google.com/spreadsheets/d/e/2PACX-1vRxW5RjnjrJ7KtLo3o8yRjXS8fr3bKOyOwUE_k1b8cN2LRpwkCY3i6Cgo7dZBVFQuyfVywEymMlXRTM/pubhtml?gid=610093275&single=true", # nolint
          "here",
          target = "_blank"
        )
      ),
      tags$iframe(
        src = "https://docs.google.com/spreadsheets/d/e/2PACX-1vRxW5RjnjrJ7KtLo3o8yRjXS8fr3bKOyOwUE_k1b8cN2LRpwkCY3i6Cgo7dZBVFQuyfVywEymMlXRTM/pubhtml?widget=true&amp;headers=false", # nolint
        style = "border:none; width:100%; height:80vh;"
      )
    ),
    # About Tab
    tabPanel(
      title = tagList(icon("info-circle"), "About"),
      value = "about",
      fluidRow(
        box(
          width = 12,
          status = "success",
          solidHeader = TRUE,
          h3(icon("lightbulb"), " Welcome to the Open Research Games Portal "),
          p(style = "font-size: 16px;",
            "The Open Research Games Portal is a crowdsourced database of games and interactive activities for teaching open and reproducible research practices. 
            We recognize that game-based learning makes complex topics more accessible and engaging for students, researchers, educators, and professionals alike. ", # nolint
            br(),
            "The Portal provides detailed information on each game (see the Field Guide below), 
            including pedagogical guidance, user experiences, preparation requirements, and access information, 
            to help you find the right game for your teaching context. 
            Whether you need a lighthearted icebreaker or a learning-intensive curriculum component, 
            you can search by topic, gameplay style, audience, and more. ",
            br(),
            " This is a community resource: anyone can contribute ",
            tags$a(href = "https://forms.gle/PXYBrRhXGiZyi8M99",
                 "by adding new games", target = "_blank", rel = "noopener noreferrer"),
            " or ",
            tags$a(href = "https://forms.gle/MSBWR87GchDo8fED7",
                 "by sharing their experiences with existing ones", target = "_blank", rel = "noopener noreferrer"),
            "."
          ),
          hr(),
          # Key Features
          fluidRow(
            column(4,
              div(class = "info-box",
                h4(icon("gamepad"), " Interactive Learning"),
                p("Games that teach research methods, 
                  statistical thinking, and open science 
                  practices through hands-on experience.")
              )
            ),
            column(4,
              div(class = "info-box",
                h4(icon("users"), " For Everyone"),
                p("From high school students to seasoned researchers,
                 find games tailored 
                  to your learning level.")
              )
            )
          ),
          # How to Use
          h4(icon("compass"), " How to Use This Portal"),
            tags$ul(
            tags$li(strong("Browse:"), " Explore all games using the interactive cards"), # nolint
            tags$li(strong("Filter:"), " Use FORRT cluster and gameplay style filters to find relevant games"), # nolint
            tags$li(strong("Search:"), " Enter keywords to find specific games by title, topics, or content"), # nolint
            tags$li(strong("Play:"), " Click 'Play Now' to start gaming immediately"), # nolint
            tags$li(strong("Learn More:"), " Click 'Details' to see comprehensive game information"), # nolint
            tags$li(strong("Export:"), " Download the complete database from the ", 
                tags$a(href = "https://docs.google.com/spreadsheets/d/1cmydWjD1OuyKxJVfDlv0N3T474zwymfB04yFDZQO-TY/edit?usp=sharing", 
                     "Google Sheet Portal", target = "_blank")) # nolint
            )
          ),
          h2(icon("book"), " Open Research Games Portal - Field Guide"),
          p("This guide explains what information you'll find for each game in the Portal and how it was collected."),
          p(tags$a( 
            href = "https://docs.google.com/spreadsheets/d/1cmydWjD1OuyKxJVfDlv0N3T474zwymfB04yFDZQO-TY/edit?usp=sharing",
            "In the Google Sheets version of the Portal",
            target = "_blank",
            rel = "noopener noreferrer"
          )),
          p("you can see the exact phrasing of the questions under the 'Codebook' tab."),
          h4("Basic Information"),
          tags$ul(
            tags$li(strong("Title -"), " The official name of the game"),
            tags$li(strong("Creators -"), " The individuals or organizations who developed the game"),
            tags$li(strong("Description -"), " An overview of the game's content and purpose, typically using the official description provided by the creators"),
            tags$li(strong("Access -"), " Where to find or purchase the game, including links to open-source versions when available"),
            tags$li(strong("Last Updated -"), " The year the game was most recently updated (helps assess whether content might be outdated)"),
            tags$li(strong("Language -"), " Languages in which the game is available (English, French, German, Spanish, Portuguese, and others)"),
            tags$li(strong("License -"), " The game's licensing terms (e.g., Creative Commons, commercial license, or unknown status)")
          ),
          h4("Game Characteristics"),
          tags$ul(
            tags$li(strong("Delivery Format -"), " How the game is delivered or accessed (digital, physical, hybrid, etc.)"),
            tags$li(strong("Game Type -"), " The category or style of game (board game, card game, simulation, role-play, escape room, puzzle, strategy, party game, storytelling/narrative, platform, debate/discussion focused, quiz, or other). Games may fall into multiple categories."),
            tags$li(strong("Gameplay Style -"), " The social structure of play: cooperative/collaborative, competitive, solo, or team-based competition. Games may feature multiple styles."),
            tags$li(strong("Tone/Learning Intensity -"), " The game's pedagogical approach and atmosphere:"),
            tags$ul(
              tags$li("Lighthearted/Playful - Fun-focused, good for engagement and icebreakers"),
              tags$li("Balanced - Mix of fun and learning"),
              tags$li("Learning-Intensive - Deep focus on concepts and understanding")
            ),
            tags$li(strong("Number of Players -"), " The recommended or official player range"),
            tags$li(strong("Playtime -"), " Typical duration needed to play the game"),
            tags$li(strong("Topic Areas -"), " Open research topics covered by the game, such as: Research Integrity, Open Data, Open Access, Pre-registration, Open Code, Open Peer Review, Big Team Science, Open Educational Resources, Open Methods, Open Hardware, Replication, Statistical Knowledge, Meta-Research, Qualitative Research, General Open Research, Open Software, Recognition and Rewards, Citizen Science, and others."),
            tags$li(strong("FORRT Clusters -"), " Alignment with FORRT's educational framework clusters:"),
            tags$ul(
              tags$li("Cluster 1: Replication Crisis and Credibility Revolution"),
              tags$li("Cluster 2: Conceptual and Statistical Knowledge"),
              tags$li("Cluster 3: Ways of Working"),
              tags$li("Cluster 4: Pre-analysis Planning"),
              tags$li("Cluster 5: Transparency and Reproducibility in Computation and Analysis"),
              tags$li("Cluster 6: FAIR Data and Materials"),
              tags$li("Cluster 7: Publication Sharing"),
              tags$li("Cluster 8: Replication and Meta-Research"),
              tags$li("Cluster 9: Academic Structures and Institutions"),
              tags$li("Cluster 10: Qualitative Research"),
              tags$li("Cluster 11: Research Integrity")
            ),
            tags$li(strong("Learning Objectives -"), " What players are expected to learn or reflect on after playing (e.g., understanding pre-registration, recognizing questionable research practices, or grasping open research principles)"),
            tags$li(strong("Target Audience -"), " The audience specified by the game's creators (if any)")
          ),
          h4("Pedagogical Use & User Experience"),
          tags$ul(
            tags$li(strong("Suggested Audience -"), " Community recommendations for who would benefit most from the game and why (may differ from the official target audience)"),
            tags$li(strong("Prior Knowledge -"), " The level of research knowledge required: basic research understanding, some familiarity with open research practices, or specialized knowledge"),
            tags$li(strong("Formal Evaluation -"), " Academic or empirical research studying the game's effectiveness, with links to studies and outcome summaries when available"),
            tags$li(strong("Scalability -"), " Suggestions for adapting the game for different group sizes, including plenary settings, team play, or solo use"),
            tags$li(strong("Teaching Integration -"), " Ideas for using the game in educational settings: as an intro or closing activity, connecting to course content, or modifying for time constraints"),
            tags$li(strong("Context-Specific Elements -"), " Parts of the game that may need adaptation for different institutional settings or audiences"),
            tags$li(strong("Preparation -"), " What's needed before playing (e.g., moderator preparation, printing, materials purchase, requesting open access versions)"),
            tags$li(strong("Testimonials -"), " User feedback about the game's educational value, engagement, or outcomes")
          ),
          hr(),
          # Portal Statistics
          h4(icon("chart-bar"), " Portal Statistics"),
          fluidRow(
            style = "margin-bottom: 20px; text-align: center; padding: 10px;",
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
                value = length(unique(unlist(strsplit(games_df$topic_area, "\n")))), # nolint
                subtitle = "Topic Areas",
                icon = icon("book"),
                color = "green"
              )
            ),
            column(3,
              valueBox(
                value = length(unique(unlist(strsplit(games_df$gameplay_style, "\n")))), # nolint
                subtitle = "Gameplay Styles",
                icon = icon("puzzle-piece"),
                color = "yellow"
              )
            ),
            column(3,
              valueBox(
                value = length(unique(unlist(strsplit(games_df$language, "\n")))), # nolint
                subtitle = "Languages",
                icon = icon("globe"),
                color = "purple"
              )
            )
          )
        )
      )
    ),
  # Footer with links and resources
  footer = div(
    div(
      class = "about-footer",
      style = "display: flex; flex-wrap: wrap; justify-content: center; 
      align-items: center; gap: 18px; margin: 0 auto 0 auto;
      padding: 0px 10px 10px 10px; border-radius: 10px; 
      box-shadow: 0 2px 8px rgba(0,0,0,0.04); font-size: 1.5rem;",
      
      h4(icon("link"), " Links & Resources"),
      tags$a(href = "https://forrtapps.shinyapps.io/open-research-games-portal/",  # nolint
             target = "_blank",
             icon("external-link-alt"), " Live App"),
      tags$a(href = "https://github.com/forrtproject/Open-Research-Games-Portal",  # nolint
             target = "_blank",
             icon("github"), " GitHub"),
      tags$a(href = "mailto:info@forrt.org",
             target = "_blank",
             icon("phone"), "Email"),
      tags$a(href = "https://forrt.org",
             target = "_blank",
             "FORRT Project"),
      tags$a(href = "https://join.slack.com/t/forrt/shared_invite/zt-alobr3z7-NOR0mTBfD1vKXn9qlOKqaQ",  # nolint
             target = "_blank",
             "Slack")
    ),
    p(style = "font-size: 12px; color: gray; display: 
    block; text-align: center; padding:10px;",
      "© 2024 FORRT Project. All rights reserved.")
  )

)

# Server
server <- function(input, output, session) {
  # Populate gameplay filter choices dynamically
  observe({
    gameplay_styles <- sort(unique(unlist(strsplit(games_df$gameplay_style, "\n")))) # nolint
    gameplay_choices <- c("All Styles" = "all", setNames(gameplay_styles, gameplay_styles)) # nolint
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
      df <- df[grepl(input$cluster_filter, df$forrt_clusters, ignore.case = TRUE), ] # nolint
    }
    # Apply gameplay filter
    if (!is.null(input$gameplay_filter) && input$gameplay_filter != "all") {
      df <- df[grepl(input$gameplay_filter, df$gameplay_style, ignore.case = TRUE), ] # nolint
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
  # No results found message
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
  # Function to display field with new lines as <br/>
  display_field <- function(value) {
    if (is.na(value) || value == "N/A" || value == "") {
      return("N/A")
    }
    HTML(gsub("\n", "<br/>", value))
  }
  # Generate game cards
  cards <- lapply(seq_len(nrow(games)), function(i) {
    game <- games[i, ]
    game_id <- if (!is.null(game$slug)) game$slug else paste0("game_", i)

    # Truncate description for card view
    description <- if (!is.null(game$description) && !is.na(game$description) &&
                         nchar(as.character(game$description)) > 200) {
      paste0(substr(game$description, 1, 200), "...")
    } else if (!is.null(game$description) && !is.na(game$description) &&
               nchar(as.character(game$description)) > 0) {
      as.character(game$description)
    } else {
      "No description available"
    }
    # Card HTML
    div(class = "col-xl-4 col-lg-6 col-md-6 col-sm-12 mb-4",
        div(class = "game-card",
            # Card header with gradient
            div(class = "game-card-header",
                h4(class = "game-card-title",
                   icon("gamepad", class = "game-icon"),
                   display_field(game$title)
                )
            ),
            # Card body
            div(class = "game-card-body", style = "font-size: 1.2rem;",
                # Description
                p(class = "game-description",
                  style = "font-size: 1.15rem;", display_field(description)),
                # Game metadata with icons
                div(class = "game-metadata",
                    div(class = "metadata-item", style = "font-size: 1.2rem;",
                        icon("puzzle-piece", class = "metadata-icon"),
                        tags$span(class = "metadata-label",
                                  style = "font-size: 1.2rem;", "Gameplay:"),
                        tags$span(class = "metadata-value",
                                  style = "font-size: 1.2rem;",
                                  display_field(game$gameplay_style)
                        )
                    ),
                    div(class = "metadata-item", style = "font-size: 1.2rem;",
                        icon("clock", class = "metadata-icon"),
                        tags$span(class = "metadata-label",
                                  style = "font-size: 1.2rem;", "Playtime:"),
                        tags$span(class = "metadata-value",
                                  style = "font-size: 1.2rem;",
                                  display_field(game$playtime)
                        )
                    ),
                    div(class = "metadata-item", style = "font-size: 1.2rem;",
                        icon("globe", class = "metadata-icon"),
                        tags$span(class = "metadata-label", style = "font-size: 1.2rem;", "Language:"),
                        tags$span(class = "metadata-value", style = "font-size: 1.2rem;",
                                  display_field(game$language)
                        )
                    ),
                    div(class = "metadata-item", style = "font-size: 1.2rem;",
                        icon("book", class = "metadata-icon"),
                        tags$span(class = "metadata-label", style = "font-size: 1.2rem;", "Topics:"),
                        tags$span(class = "metadata-value", style = "font-size: 1.2rem;",
                                  if (!is.null(game$topic_area) && !is.na(game$topic_area) &&
                                        nchar(as.character(game$topic_area)) > 40) {
                                    display_field(paste0(substr(game$topic_area, 1, 40), "..."))
                                  } else {
                                    display_field(game$topic_area)
                                  }
                        )
                    )
                )
            ),

                # Card footer with buttons
              div(class = "game-card-footer",
                    if (!is.null(game$access) && !is.na(game$access) &&
                          nchar(as.character(game$access)) > 0) {
                      a(href = game$access,
                        target = "_blank",
                        class = "btn-play",
                        style = "font-size: 1.2rem;",
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
                      paste0("details_", game_id),
                      tagList(icon("info-circle"), " Details"),
                      class = "btn-details",
                      style = "font-size: 1.2rem;"
                    )
                )
            )
        )
  })
  div(class = "row games-grid", cards)
})
  
  # Handle details button clicks - use observeEvent for each game slug
  lapply(seq_len(nrow(games_df)), function(i) {
    local({
      game <- games_df[i, ]
      game_id <- if (!is.null(game$slug)) game$slug else paste0("game_", i)
      
      observeEvent(input[[paste0("details_", game_id)]], {
        # Look up the game from the full dataset by slug
        game_data <- games_df[i, ]
        
        # Function to display field with new lines as <br/>
        display_field <- function(value) {
          if (is.na(value) || value == "N/A" || value == "") {
            return("N/A")
          }
          HTML(gsub("\n", "<br/>", value))
        }
        
        showModal(modalDialog(
          title = display_field(game_data$title),
          
          h4("Description"),
          p(display_field(game_data$description)),
          
          h4("Game Details"),
          div(class = "row",
              div(class = "col-md-6",
                  p(strong("Creator(s): "), display_field(game_data$creators)),
                  p(strong("Gameplay: "), display_field(game_data$gameplay_style)),
                  p(strong("Game Type: "), display_field(game_data$game_type)),
                  p(strong("Playtime: "), display_field(game_data$playtime)),
                  p(strong("Language: "), display_field(game_data$language))
              ),
              div(class = "col-md-6",
                  p(strong("Target Audience: "), display_field(game_data$target_audience)),
                  p(strong("Last Updated: "), display_field(game_data$last_updated)),
                  p(strong("Delivery Format: "), display_field(game_data$delivery_format)),
                  p(strong("Players: "), display_field(game_data$number_of_players)),
                  p(strong("License: "), display_field(game_data$licence)),
                  p(strong("Prerequisites: "), display_field(game_data$prior_knowledge))
              )
          ),
          
          if (!is.null(game_data$learning_objectives) && !is.na(game_data$learning_objectives) && 
              nchar(as.character(game_data$learning_objectives)) > 0) {
            div(
              h4("Learning Objectives"),
              p(display_field(game_data$learning_objectives))
            )
          },
          # scalability, formal_evaluation, context_specific_elements, preparation
          if (!is.null(game_data$scalability) && !is.na(game_data$scalability) && 
              nchar(as.character(game_data$scalability)) > 0) {
            div(
              h4("Scalability"),
              p(display_field(game_data$scalability))
            )
          },
          # teaching_integration
          if (!is.null(game_data$teaching_integration) && !is.na(game_data$teaching_integration) && 
              nchar(as.character(game_data$teaching_integration)) > 0) {
            div(
              h4("Teaching Integration"),
              p(display_field(game_data$teaching_integration))
            )
          },

          if (!is.null(game_data$formal_evaluation) && !is.na(game_data$formal_evaluation) && 
              nchar(as.character(game_data$formal_evaluation)) > 0) {
            div(
              h4("Formal Evaluation"),
              p(display_field(game_data$formal_evaluation))
            )
          },
          # suggested_audience
          if (!is.null(game_data$suggested_audience) && !is.na(game_data$suggested_audience) && 
              nchar(as.character(game_data$suggested_audience)) > 0) {
            div(
              h4("Suggested Audience"),
              p(display_field(game_data$suggested_audience))
            )
          },
          # prior_knowledge
          if (!is.null(game_data$prior_knowledge) && !is.na(game_data$prior_knowledge) && 
              nchar(as.character(game_data$prior_knowledge)) > 0) {
            div(
              h4("Prior Knowledge"),
              p(display_field(game_data$prior_knowledge))
            )
          },

          # context_specific_elements
          if (!is.null(game_data$context_specific_elements) && !is.na(game_data$context_specific_elements) && 
              nchar(as.character(game_data$context_specific_elements)) > 0) {
            div(
              h4("Context-Specific Elements"),
              p(display_field(game_data$context_specific_elements))
            )
          },

          if (!is.null(game_data$preparation) && !is.na(game_data$preparation) && 
              nchar(as.character(game_data$preparation)) > 0) {
            div(
              h4("Preparation"),
              p(display_field(game_data$preparation))
            )
          },
          
          if (!is.null(game_data$testimonials) && !is.na(game_data$testimonials) && 
              nchar(as.character(game_data$testimonials)) > 0) {
            div(
              h4("Testimonials"),
              tags$em(p(display_field(game_data$testimonials)))
            )
          },
          
          footer = tagList(
            if (!is.null(game_data$access) && !is.na(game_data$access) && 
                nchar(as.character(game_data$access)) > 0) {
              a(href = game_data$access, target = "_blank", 
                class = "btn btn-success",
                icon("play"), " Play This Game")
            },
            modalButton("Close")
          ),
          
          size = "l",
          easyClose = TRUE
        ))
      }, ignoreInit = TRUE)
    })
  })
  
}
# Run the app
shinyApp(ui = ui, server = server)
