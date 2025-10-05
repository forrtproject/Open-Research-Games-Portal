# Open Research Games Portal

Welcome to the Open Research Games Portal - your gateway to learning open science through play! Our curated collection features 39 educational games that make complex research concepts accessible and engaging.

### Discover & Play

🎮 Interactive Learning: Games that teach research methods, statistical thinking, and open science practices through hands-on experience.

🎯 For Everyone: From high school students to seasoned researchers, find games tailored to your learning level.

🔬 Evidence-Based: Each game is designed with pedagogical principles and learning objectives in mind.

### How to Use This Portal

- Browse: Explore all games using the cards below
- Filter: Use the FORRT cluster buttons to find games by topic
- Search: Enter keywords to find specific games by title, gameplay, topics, or FORRT clusters
- Play: Click "Play Now" to start gaming immediately
- Learn More: Click "Details" to see comprehensive game information

Ready to transform your understanding of open science? Let's play and learn together!


## Installation & Usage

### Prerequisites
```r
install.packages(c("shiny", "shinydashboard", "DT", "jsonlite", "dplyr"))
```

### Running the App
```r
# Set working directory
setwd("/Users/rdm/Desktop/oss/Open-Research-Games-Portal")

# Run the app
shiny::runApp()
```

### Updating Data
1. Update the Google Sheets with new games
2. Run the R script to regenerate JSON:
```r
source("Open-Research-Games-Portal.R")
```
3. Restart the Shiny app to load new data

## App Features

### Games Portal Tab
- Browse all games in card format
- Search games by keywords
- Filter by FORRT clusters
- View game details in modal popups
- Direct links to play games

### Data Management Tab
- View all games in a sortable table
- Filter and search through the data
