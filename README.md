# [Open Research Games Portal](https://forrt.org/open-research-games-portal/)

Welcome to the Open Research Games Portal - your gateway to learning open science through play! Our curated collection features 45 ++  educational games that make complex research concepts accessible and engaging.

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

Shiny app: [https://forrt.org/open-research-games-portal/](https://forrt.org/open-research-games-portal/)

## Installation & Usage

Use the **repository root** as the working directory (the folder that contains `app.R` and `data/`).

### Prerequisites

**Run the Shiny app** — install:

```r
install.packages(c("shiny", "shinydashboard", "DT", "jsonlite", "dplyr"))
```

**Regenerate `data/open_research_games.json` from Google Sheets** (optional) — the script in `scripts/` also needs:

```r
install.packages(c("googlesheets4", "tidyr", "stringr", "readxl"))
```

### Running the app

```r
setwd("/path/to/Open-Research-Games-Portal")  # repository root
shiny::runApp()
```

### Refreshing data from Google Sheets

1. Update the [Google Sheet](https://docs.google.com/spreadsheets/d/1cmydWjD1OuyKxJVfDlv0N3T474zwymfB04yFDZQO-TY/edit?usp=sharing). Use the **Access(only_link)** column for play URLs; the export script maps that to the **`access`** field in `data/open_research_games.json`.
2. From the **repository root**, run:

```r
source("scripts/Open-Research-Games-Portal.R")
```

The script reads the published CSV (see URLs and fallbacks inside the script) and writes `data/open_research_games.json`. Restart or redeploy the app to pick up changes.

For more information, contact **[info@forrt.org](mailto:info@forrt.org)**.  

This work is part of the **[Framework for Open and Reproducible Research Training (FORRT) Project](https://forrt.org)**.
